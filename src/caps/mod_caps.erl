-module(mod_caps).

-xep([{xep, 115}, {version, "1.6.0"}]).

-behaviour(gen_mod).

-type hash() :: {binary(), binary()}.
-type feature() :: binary().
-type state() :: #{hash := hash(),
                   node => binary(),
                   features => [feature()]}.
-type handle_presence_result() :: skipped | requested | stored | deleted.

-export_type([hash/0, feature/0]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include_lib("kernel/include/logger.hrl").

-define(SERVER_HASH_ALG, <<"sha-1">>).

%% gen_mod callbacks
-export([start/2, stop/1, supported_features/0, hooks/1]).

%% hook handlers
-export([disco_info/3,
         disco_local_features/3,
         disco_local_identity/3,
         user_send_presence/3,
         user_receive_presence/3,
         stream_features/3]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    mod_caps_cache:init(HostType),
    mod_caps_state:init(HostType).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mod_caps_state:stop(HostType),
    mod_caps_cache:stop(HostType),
    delete_server_hash(HostType).

-spec supported_features() -> [gen_mod:module_feature()].
supported_features() -> [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{disco_info, HostType, fun ?MODULE:disco_info/3, #{}, 1},
     {disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 1},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 1},
     {c2s_stream_features, HostType, fun ?MODULE:stream_features/3, #{}, 75},
     {s2s_stream_features, HostType, fun ?MODULE:stream_features/3, #{}, 75},
     {user_send_presence, HostType, fun ?MODULE:user_send_presence/3, #{}, 75},
     {user_receive_presence, HostType, fun ?MODULE:user_receive_presence/3, #{}, 75}
    ].

-spec user_send_presence(mongoose_acc:t(), gen_hook:hook_params(), gen_hook:extra()) ->
          {ok, mongoose_acc:t()}.
user_send_presence(Acc, #{c2s_data := C2SData}, #{host_type := HostType}) ->
    Result = handle_presence_from_local_user(Acc, C2SData, HostType),
    ?LOG_DEBUG(#{what => mod_caps_handle_sent_presence, acc => Acc, result => Result}),
    {ok, Acc}.

-spec user_receive_presence(mongoose_acc:t(), gen_hook:hook_params(), gen_hook:extra()) ->
          {ok, mongoose_acc:t()}.
user_receive_presence(Acc, #{c2s_data := C2SData}, #{host_type := HostType}) ->
    Result = handle_presence_from_remote_user(Acc, C2SData, HostType),
    ?LOG_DEBUG(#{what => mod_caps_handle_received_presence, acc => Acc, result => Result}),
    {ok, Acc}.

-spec handle_presence_from_local_user(mongoose_acc:t(), mongoose_c2s:data(),
                                      mongooseim:host_type()) -> handle_presence_result().
handle_presence_from_local_user(Acc, C2SData, HostType) ->
    case mongoose_acc:packet(Acc) of
        {#jid{luser = LUser, lserver = LServer},
         #jid{luser = LUser, lserver = LServer, lresource = <<>>},
         Stanza} ->
            process_caps(Acc, C2SData, HostType, Stanza);
        _ ->
            skipped
    end.

-spec handle_presence_from_remote_user(mongoose_acc:t(), mongoose_c2s:data(),
                                       mongooseim:host_type()) -> handle_presence_result().
handle_presence_from_remote_user(Acc, C2SData, HostType) ->
    maybe
        {From, #jid{lserver = LServer, lresource = <<>>}, Stanza} ?= mongoose_acc:packet(Acc),
        {error, not_found} ?= mongoose_domain_api:get_domain_host_type(From#jid.lserver),
        {ok, HostType} ?= mongoose_domain_api:get_domain_host_type(LServer),
        process_caps(Acc, C2SData, HostType, Stanza)
    else
        _ -> skipped
    end.

-spec process_caps(mongoose_acc:t(), mongoose_c2s:data(), mongooseim:host_type(), exml:element()) ->
          handle_presence_result().
process_caps(Acc, C2SData, HostType, Stanza) ->
    Jid = mongoose_acc:from_jid(Acc),
    case mod_presence:get_presence_type(Acc) of
        available ->
            case extract_caps(Stanza) of
                State = #{hash := Hash} ->
                    case mod_caps_cache:read(HostType, Hash) of
                        {ok, Features} ->
                            mod_caps_state:set(HostType, jid:to_lower(Jid), Features),
                            mongoose_hooks:caps_recognised(Acc, C2SData, Features),
                            stored;
                        {error, not_found} ->
                    send_disco_info_request(Acc, C2SData, State, Jid),
                            requested
                    end;
                _ ->
                    skipped
            end;
        unavailable ->
            mod_caps_state:delete(HostType, jid:to_lower(Jid)),
            deleted
    end.

-spec send_disco_info_request(mongoose_acc:t(), mongoose_c2s:data(), state(), jid:jid()) -> mongoose_acc:t().
send_disco_info_request(Acc, C2SData, State, JID = #jid{lserver = LServer}) ->
    IQ = disco_info_request(State),
    ServerJID = jid:make_noprep(<<>>, LServer, <<>>),
    HandlerF = fun(_From, _To, _Acc, Result) ->
                       handle_disco_info_response(Result, Acc, C2SData, State)
               end,
    ejabberd_local:route_iq(ServerJID, JID, Acc, IQ, HandlerF).

-spec handle_disco_info_response(timeout | jlib:iq(), mongoose_acc:t(), mongoose_c2s:data(), state()) ->
          mongoose_acc:t().
handle_disco_info_response(timeout, Acc, _C2SData, _State) ->
    Acc;
handle_disco_info_response(#iq{type = result, sub_el = [QueryEl]}, Acc, C2SData, State) ->
    Result = handle_response(Acc, C2SData, State, QueryEl),
    ?LOG_DEBUG(#{what => mod_caps_handle_response, acc => Acc, result => Result}),
    Acc.

-spec handle_response(mongoose_acc:t(), mongoose_c2s:data(), state(), exml:element()) -> stored | skipped.
handle_response(Acc, C2SData, State, QueryEl) ->
    case verify_hash(State, QueryEl) of
        true ->
            Features = exml_query:paths(QueryEl, [{element, <<"feature">>}, {attr, <<"var">>}]),
            #{hash := Hash} = State,
            HostType = mongoose_acc:host_type(Acc),
            FromJid = mongoose_acc:from_jid(Acc),
            mod_caps_cache:write(HostType, Hash, Features),
            mod_caps_state:set(HostType, jid:to_lower(FromJid), Features),
            mongoose_hooks:caps_recognised(Acc, C2SData, Features),
            stored;
        false ->
            skipped
    end.

-spec verify_hash(state(), exml:element()) -> boolean().
verify_hash(#{hash := {HashAlg, HashValue}}, #xmlel{children = Elements}) ->
    mod_caps_hash:generate(Elements, HashAlg) =:= HashValue.

-spec disco_info_request(state()) -> jlib:iq().
disco_info_request(#{node := Node, hash := {_HashAlg, HashValue}}) ->
    #iq{type = get,
        xmlns = ?NS_DISCO_INFO,
        sub_el = [#xmlel{name = <<"query">>,
                         attrs = #{<<"xmlns">> => ?NS_DISCO_INFO,
                                   <<"node">> => <<Node/binary, "#", HashValue/binary>>}}]}.

-doc "Extracts caps from the first 'c' element with caps namespace".
-spec extract_caps(exml:element()) -> state() | undefined.
extract_caps(Element) ->
    case exml_query:subelement_with_name_and_ns(Element, <<"c">>, ?NS_CAPS) of
        #xmlel{attrs = #{<<"hash">> := HashAlg, <<"ver">> := HashValue, <<"node">> := Node}} ->
            #{hash => {HashAlg, HashValue}, node => Node};
        _ ->
            undefined
    end.

-spec stream_features(Acc, #{lserver := jid:lserver()}, gen_hook:extra()) -> {ok, Acc} when
    Acc :: [exml:element()].
stream_features(Acc, #{lserver := LServer}, #{host_type := HostType}) ->
    {ok, caps_stream_features(HostType, jid:make(<<>>, LServer, <<>>)) ++ Acc}.

-spec caps_stream_features(mongooseim:host_type(), jid:jid()) -> [exml:element()].
caps_stream_features(HostType, ServerJID) ->
    case make_server_hash(HostType, ServerJID) of
        empty -> [];
        HashValue -> [server_caps_element(HashValue)]
    end.

-spec server_caps_element(binary()) -> exml:element().
server_caps_element(HashValue) ->
    #xmlel{name = <<"c">>,
           attrs = #{<<"xmlns">> => ?NS_CAPS,
                     <<"hash">> => ?SERVER_HASH_ALG,
                     <<"node">> => ?MONGOOSE_URI,
                     <<"ver">> => HashValue},
           children = []}.

-spec server_disco_elements(mongooseim:host_type(), jid:jid()) -> empty | {ok, [exml:element()]}.
server_disco_elements(HostType, JID) ->
    maybe
        {result, FeaturesXML} ?= mongoose_disco:get_local_features(HostType, JID, JID, <<>>, <<>>),
        IdentityXML = mongoose_disco:get_local_identity(HostType, JID, JID, <<>>, <<>>),
        InfoXML = mongoose_disco:get_info(HostType, mod_disco, <<>>, <<>>),
        {ok, IdentityXML ++ InfoXML ++ FeaturesXML}
    end.

-spec disco_local_features(mongoose_disco:feature_acc(),
                           gen_hook:hook_params(),
                           gen_hook:extra()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := Node}, _, #{host_type := HostType}) ->
    case is_server_node_valid(HostType, Node) of
        true -> {ok, Acc#{node := <<>>}};
        false -> {ok, mongoose_disco:add_features([?NS_CAPS], Acc)}
    end.

-spec disco_local_identity(mongoose_disco:identity_acc(),
                           gen_hook:hook_params(),
                           gen_hook:extra()) -> {ok, mongoose_disco:identity_acc()}.
disco_local_identity(Acc = #{node := Node}, _, #{host_type := HostType}) ->
    case is_server_node_valid(HostType, Node) of
        true -> {ok, Acc#{node := <<>>}};
        false -> {ok, Acc}
    end.

-spec disco_info(mongoose_disco:info_acc(),
                 gen_hook:hook_params(),
                 gen_hook:extra()) -> {ok, mongoose_disco:info_acc()}.
disco_info(Acc = #{node := Node}, _, #{host_type := HostType}) ->
    case is_server_node_valid(HostType, Node) of
        true -> {ok, Acc#{node := <<>>}};
        false -> {ok, Acc}
    end.

-spec is_server_node_valid(mongooseim:host_type(), binary()) -> boolean().
is_server_node_valid(HostType, Node) ->
    get_server_hash(HostType) =:= extract_server_hash(Node).

-spec extract_server_hash(binary()) -> binary() | no_match.
extract_server_hash(Node) ->
    case binary:split(Node, <<"#">>, []) of
        [?MONGOOSE_URI, HashValue] ->
            HashValue;
        _ ->
            no_match
    end.

-spec make_server_hash(mongooseim:host_type(), jid:jid()) -> binary() | empty.
make_server_hash(HostType, ServerJID) ->
    maybe
        missing ?= get_server_hash(HostType),
        HashValue = generate_server_hash(HostType, ServerJID),
        persistent_term:put({?MODULE, {server_hash, HostType}}, HashValue),
        HashValue
    end.

-spec get_server_hash(mongooseim:host_type()) -> binary() | empty | missing.
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

-spec generate_server_hash(mongooseim:host_type(), jid:jid()) -> binary() | empty.
generate_server_hash(HostType, ServerJID) ->
    maybe
        {ok, Elements} ?= server_disco_elements(HostType, ServerJID),
        mod_caps_hash:generate(Elements, ?SERVER_HASH_ALG)
    end.
