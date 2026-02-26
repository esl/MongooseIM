-module(mod_caps).

-xep([{xep, 115}, {version, "1.6.0"}]).

-behaviour(gen_mod).

-type hash() :: {binary(), binary()}.
-type feature() :: binary().
-type caps() :: #{hash := hash(), node => binary(), features => [feature()]}.
-type handle_presence_result() :: skipped | requested | stored | deleted.

-export_type([hash/0, feature/0]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include_lib("kernel/include/logger.hrl").

-define(SERVER_HASH_ALG, ~"sha-1").

%% API
-export([get_features/2, get_resources_with_features/2]).

%% gen_mod callbacks
-export([start/2, stop/1, config_spec/0, supported_features/0, hooks/1]).

%% Hook handlers
-export([disco_info/3,
         disco_local_features/3,
         disco_local_identity/3,
         user_send_presence/3,
         user_terminate/3,
         stream_features/3]).

%% API

-spec get_features(mongooseim:host_type(), jid:jid()) -> [feature()].
get_features(HostType, Jid) ->
    mod_caps_backend:get_jid_features(HostType, Jid).

-spec get_resources_with_features(mongooseim:host_type(), jid:jid()) -> [{jid:lresource(), [feature()]}].
get_resources_with_features(HostType, Jid) ->
    mod_caps_backend:get_bare_jid_features(HostType, Jid).

%% gen_mod callbacks

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_caps_backend:init(HostType, Opts).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mod_caps_backend:stop(HostType),
    delete_server_hash(HostType).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{~"backend" => #option{type = atom,
                                             validate = {module, mod_caps_backend}},
                       ~"iq_response_timeout" => #option{type = integer,
                                                validate = positive}},
             defaults = #{~"backend" => cets,
                          ~"iq_response_timeout" => 5000}}.

-spec supported_features() -> [gen_mod:module_feature()].
supported_features() -> [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 1},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 1},
     {disco_info, HostType, fun ?MODULE:disco_info/3, #{}, 1},
     {c2s_stream_features, HostType, fun ?MODULE:stream_features/3, #{}, 50},
     {s2s_stream_features, HostType, fun ?MODULE:stream_features/3, #{}, 50},
     {user_send_presence, HostType, fun ?MODULE:user_send_presence/3, #{}, 10},
     {user_terminate, HostType, fun ?MODULE:user_terminate/3, #{}, 10}
    ].

%% Hook handlers

-spec disco_local_features(mongoose_disco:feature_acc(),
                           gen_hook:hook_params(),
                           gen_hook:extra()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := Node}, _, #{host_type := HostType}) ->
    case is_server_node_valid(HostType, Node) of
        true -> {ok, Acc#{node := ~""}};
        false -> {ok, mongoose_disco:add_features([?NS_CAPS], Acc)}
    end.

-spec disco_local_identity(mongoose_disco:identity_acc(),
                           gen_hook:hook_params(),
                           gen_hook:extra()) -> {ok, mongoose_disco:identity_acc()}.
disco_local_identity(Acc = #{node := Node}, _, #{host_type := HostType}) ->
    case is_server_node_valid(HostType, Node) of
        true -> {ok, Acc#{node := ~""}};
        false -> {ok, Acc}
    end.

-spec disco_info(mongoose_disco:info_acc(),
                 gen_hook:hook_params(),
                 gen_hook:extra()) -> {ok, mongoose_disco:info_acc()}.
disco_info(Acc = #{node := Node}, _, #{host_type := HostType}) ->
    case is_server_node_valid(HostType, Node) of
        true -> {ok, Acc#{node := ~""}};
        false -> {ok, Acc}
    end.

-spec stream_features(Acc, #{lserver := jid:lserver()}, gen_hook:extra()) -> {ok, Acc} when
    Acc :: [exml:element()].
stream_features(Acc, #{lserver := LServer}, #{host_type := HostType}) ->
    {ok, caps_stream_features(HostType, jid:make(~"", LServer, ~"")) ++ Acc}.

-spec user_send_presence(mongoose_acc:t(), gen_hook:hook_params(), gen_hook:extra()) ->
          {ok, mongoose_acc:t()}.
user_send_presence(Acc, #{c2s_data := C2SData}, #{host_type := HostType}) ->
    Result = process_caps(Acc, C2SData, HostType, inspect_presence(Acc)),
    ?LOG_DEBUG(#{what => mod_caps_user_send_presence,
                 acc => Acc, host_type => HostType, result => Result}),
    {ok, Acc}.

-spec user_terminate(mongoose_acc:t(), gen_hook:hook_params(), gen_hook:extra()) ->
          {ok, mongoose_acc:t()}.
user_terminate(Acc, #{c2s_data := C2SData}, #{host_type := HostType}) ->
    mod_caps_backend:delete_jid_features(HostType, mongoose_c2s:get_jid(C2SData)),
    {ok, Acc}.

%% Helpers

-spec inspect_presence(mongoose_acc:t()) -> caps() | no_caps | skip.
inspect_presence(Acc) ->
    case mongoose_acc:packet(Acc) of
        {#jid{luser = LUser, lserver = LServer},
         #jid{luser = LUser, lserver = LServer, lresource = ~""},
         Stanza} ->
            case mod_presence:get_presence_type(Acc) of
                available -> extract_caps(Stanza);
                unavailable -> no_caps;
                _ -> skip
            end;
        _ ->
            skip
    end.

-spec process_caps(mongoose_acc:t(), mongoose_c2s:data(), mongooseim:host_type(),
                   caps() | no_caps | skip) ->
          handle_presence_result().
process_caps(Acc, C2SData, HostType, Caps = #{hash := Hash}) ->
    Jid = mongoose_acc:from_jid(Acc),
    case mod_caps_backend:get_hash_features(HostType, Hash) of
        {ok, Features} ->
            mod_caps_backend:set_jid_features(HostType, Jid, Features),
            mongoose_hooks:caps_recognised(Acc, C2SData, Features),
            stored;
        {error, not_found} ->
            send_disco_info_request(Acc, C2SData, Caps, Jid),
            requested
    end;
process_caps(Acc, _C2SData, HostType, no_caps) ->
    Jid = mongoose_acc:from_jid(Acc),
    mod_caps_backend:delete_jid_features(HostType, Jid),
    deleted;
process_caps(_Acc, _C2SData, _HostType, skip) ->
    skipped.

-spec send_disco_info_request(mongoose_acc:t(), mongoose_c2s:data(), caps(), jid:jid()) ->
          mongoose_acc:t().
send_disco_info_request(Acc, C2SData, Caps, JID) ->
    IQ = disco_info_request(Caps),
    ServerJID = jid:make_noprep(~"", mongoose_c2s:get_lserver(C2SData), ~""),
    HandlerF = fun(_From, _To, _Acc, Result) ->
                       handle_disco_info_response(Result, Acc, C2SData, Caps)
               end,
    Timeout = gen_mod:get_module_opt(mongoose_acc:host_type(Acc), ?MODULE, iq_response_timeout),
    ejabberd_local:route_iq(ServerJID, JID, Acc, IQ, HandlerF, Timeout).

-spec handle_disco_info_response(timeout | jlib:iq(), mongoose_acc:t(), mongoose_c2s:data(), caps()) ->
          mongoose_acc:t().
handle_disco_info_response(#iq{type = result, sub_el = [QueryEl]}, Acc, C2SData, Caps) ->
    Result = handle_response(Acc, C2SData, Caps, QueryEl),
    ?LOG_DEBUG(#{what => mod_caps_iq_result, acc => Acc, result => Result}),
    Acc;
handle_disco_info_response(Other, Acc, _C2SData, Caps) ->
    ?LOG_NOTICE(#{what => mod_caps_iq_failure, acc => Acc, response => Other, caps => Caps}),
    Acc.

-spec handle_response(mongoose_acc:t(), mongoose_c2s:data(), caps(), exml:element()) -> stored | skipped.
handle_response(Acc, C2SData, Caps, QueryEl) ->
    case verify_hash(Caps, QueryEl) of
        true ->
            Features = exml_query:paths(QueryEl, [{element, ~"feature"}, {attr, ~"var"}]),
            #{hash := Hash} = Caps,
            HostType = mongoose_acc:host_type(Acc),
            FromJid = mongoose_acc:from_jid(Acc),
            mod_caps_backend:set_hash_features(HostType, Hash, Features),
            mod_caps_backend:set_jid_features(HostType, FromJid, Features),
            mongoose_hooks:caps_recognised(Acc, C2SData, Features),
            stored;
        false ->
            skipped
    end.

-spec verify_hash(caps(), exml:element()) -> boolean().
verify_hash(#{hash := {HashAlg, HashValue}}, #xmlel{children = Elements}) ->
    mod_caps_hash:generate(Elements, HashAlg) =:= HashValue.

-spec disco_info_request(caps()) -> jlib:iq().
disco_info_request(#{node := Node, hash := {_HashAlg, HashValue}}) ->
    #iq{type = get,
        xmlns = ?NS_DISCO_INFO,
        sub_el = [#xmlel{name = ~"query",
                         attrs = #{~"xmlns" => ?NS_DISCO_INFO,
                                   ~"node" => <<Node/binary, "#", HashValue/binary>>}}]}.

-doc "Extracts caps from the first 'c' element with caps namespace".
-spec extract_caps(exml:element()) -> caps() | no_caps.
extract_caps(Element) ->
    case exml_query:subelement_with_name_and_ns(Element, ~"c", ?NS_CAPS) of
        #xmlel{attrs = #{~"hash" := HashAlg, ~"ver" := HashValue, ~"node" := Node}} ->
            #{hash => {HashAlg, HashValue}, node => Node};
        _ ->
            no_caps
    end.

-spec caps_stream_features(mongooseim:host_type(), jid:jid()) -> [exml:element()].
caps_stream_features(HostType, ServerJID) ->
    HashValue = make_server_hash(HostType, ServerJID),
    [server_caps_element(HashValue)].

-spec server_caps_element(binary()) -> exml:element().
server_caps_element(HashValue) ->
    #xmlel{name = ~"c",
           attrs = #{~"xmlns" => ?NS_CAPS,
                     ~"hash" => ?SERVER_HASH_ALG,
                     ~"node" => ?MONGOOSE_URI,
                     ~"ver" => HashValue},
           children = []}.

-spec server_disco_elements(mongooseim:host_type(), jid:jid()) -> [exml:element()].
server_disco_elements(HostType, JID) ->
    {result, FeaturesXML} = mongoose_disco:get_local_features(HostType, JID, JID, ~"", ~""),
    IdentityXML = mongoose_disco:get_local_identity(HostType, JID, JID, ~"", ~""),
    InfoXML = mongoose_disco:get_info(HostType, mod_disco, ~"", ~""),
    IdentityXML ++ InfoXML ++ FeaturesXML.

-spec is_server_node_valid(mongooseim:host_type(), binary()) -> boolean().
is_server_node_valid(HostType, Node) ->
    get_server_hash(HostType) =:= extract_server_hash(Node).

-spec extract_server_hash(binary()) -> binary() | no_match.
extract_server_hash(Node) ->
    case binary:split(Node, ~"#", []) of
        [?MONGOOSE_URI, HashValue] ->
            HashValue;
        _ ->
            no_match
    end.

-spec make_server_hash(mongooseim:host_type(), jid:jid()) -> binary().
make_server_hash(HostType, ServerJID) ->
    maybe
        missing ?= get_server_hash(HostType),
        HashValue = generate_server_hash(HostType, ServerJID),
        persistent_term:put({?MODULE, {server_hash, HostType}}, HashValue),
        HashValue
    end.

-spec get_server_hash(mongooseim:host_type()) -> binary() | missing.
get_server_hash(HostType) ->
    try
        persistent_term:get({?MODULE, {server_hash, HostType}})
    catch
        error:badarg -> missing
    end.

-spec delete_server_hash(mongooseim:host_type()) -> ok.
delete_server_hash(HostType) ->
    persistent_term:erase({?MODULE, {server_hash, HostType}}),
    ok.

-spec generate_server_hash(mongooseim:host_type(), jid:jid()) -> binary().
generate_server_hash(HostType, ServerJID) ->
    Elements = server_disco_elements(HostType, ServerJID),
    mod_caps_hash:generate(Elements, ?SERVER_HASH_ALG).
