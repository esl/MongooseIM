-module(mod_caps).
-moduledoc "Handling and caching of entity capabilities according to XEP-0115 and XEP-0390".

-xep([{xep, 115}, {version, "1.6.0"}]).
-xep([{xep, 390}, {version, "0.3.2"}]).

-behaviour(gen_mod).

-type hash() :: {hash_alg(), hash_value()}.
-type hash_alg() :: binary().
-type hash_value() :: binary().
-type version() :: v1  % XEP-0115: Entity Capabilities
                 | v2. % XEP-0390: Entity Capabilities 2.0
-type feature() :: binary().
-type caps() :: #{version := version(), hash := hash(), node => binary(), features => [feature()]}.

-export_type([hash/0, hash_alg/0, hash_value/0, version/0, feature/0]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include_lib("kernel/include/logger.hrl").

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
    mod_caps_backend:get_features(HostType, Jid).

-spec get_resources_with_features(mongooseim:host_type(), jid:jid()) -> [{jid:lresource(), [feature()]}].
get_resources_with_features(HostType, Jid) ->
    mod_caps_backend:get_resources_with_features(HostType, Jid).

%% gen_mod callbacks

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_caps_backend:init(HostType, Opts).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mod_caps_backend:stop(HostType),
    delete_server_hashes(HostType).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{~"backend" => #option{type = atom,
                                             validate = {module, mod_caps_backend}},
                       ~"iq_response_timeout" => #option{type = integer,
                                                         validate = positive},
                       ~"versions" => #list{items = #option{type = atom,
                                                            validate = {enum, [v1, v2]}},
                                            validate = unique_non_empty}},
             defaults = #{~"backend" => cets,
                          ~"iq_response_timeout" => 5000,
                          ~"versions" => [v1, v2]}}.

-spec supported_features() -> [gen_mod:module_feature()].
supported_features() -> [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{disco_info, HostType, fun ?MODULE:disco_info/3, #{}, 1},
     {disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 1},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 1},
     {user_send_presence, HostType, fun ?MODULE:user_send_presence/3, #{}, 10},
     {user_terminate, HostType, fun ?MODULE:user_terminate/3, #{}, 10},
     {c2s_stream_features, HostType, fun ?MODULE:stream_features/3, #{}, 50},
     {s2s_stream_features, HostType, fun ?MODULE:stream_features/3, #{}, 50}].

%% Hook handlers

-spec disco_local_features(mongoose_disco:feature_acc(),
                           gen_hook:hook_params(),
                           gen_hook:extra()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := Node}, _, #{host_type := HostType}) ->
    case is_server_node_valid(HostType, Node) of
        true ->
            Acc1 = mongoose_disco:add_features(lists:map(fun ns/1, versions(HostType)), Acc),
            {ok, Acc1#{node := ~""}};
        false ->
            {ok, Acc}
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
    maybe
        #jid{} ?= Jid = mongoose_c2s:get_jid(C2SData),
        mod_caps_backend:delete_features(HostType, Jid)
    end,
    {ok, Acc}.

%% Helpers

-spec inspect_presence(mongoose_acc:t()) -> caps() | no_caps | skip.
inspect_presence(Acc) ->
    case mongoose_acc:packet(Acc) of
        {#jid{luser = LUser, lserver = LServer},
         #jid{luser = LUser, lserver = LServer, lresource = ~""},
         Stanza} ->
            case mod_presence:get_presence_type(Acc) of
                available -> extract_caps(versions(mongoose_acc:host_type(Acc)), Stanza);
                unavailable -> no_caps;
                _ -> skip
            end;
        _ ->
            skip
    end.

-spec process_caps(mongoose_acc:t(), mongoose_c2s:data(), mongooseim:host_type(),
                   caps() | no_caps | skip) -> unchanged | stored | requested | deleted | skipped.
process_caps(Acc, C2SData, HostType, Caps = #{hash := Hash}) ->
    Jid = mongoose_acc:from_jid(Acc),
    case mod_caps_backend:get_hash_features(HostType, Hash) of
        {ok, Features} ->
            case mod_caps_backend:get_features(HostType, Jid) of
                Features ->
                    unchanged;
                _ ->
                    mod_caps_backend:set_features(HostType, Jid, Features),
                    mongoose_hooks:caps_recognised(Acc, C2SData, Features),
                    stored
            end;
        {error, not_found} ->
            send_disco_info_request(Acc, C2SData, Caps, Jid),
            requested
    end;
process_caps(Acc, _C2SData, HostType, no_caps) ->
    Jid = mongoose_acc:from_jid(Acc),
    mod_caps_backend:delete_features(HostType, Jid),
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
            mod_caps_backend:set_features(HostType, FromJid, Features),
            mongoose_hooks:caps_recognised(Acc, C2SData, Features),
            stored;
        false ->
            skipped
    end.

-spec verify_hash(caps(), exml:element()) -> boolean().
verify_hash(Caps, QueryEl = #xmlel{children = Elements}) ->
    #{version := Version, hash := {HashAlg, HashValue}} = Caps,
    try mod_caps_hash:generate(Elements, Version, HashAlg) of
        HashValue ->
            true;
        OtherValue ->
            ?LOG_INFO(#{what => mod_caps_hash_mismatch,
                        caps => Caps, query_el => QueryEl, hash_value => OtherValue}),
            false
    catch
        error:Reason ->
            ?LOG_NOTICE(#{what => mod_caps_could_not_generate_hash,
                          caps => Caps, query_el => QueryEl, reason => Reason}),
            false
    end.

-spec disco_info_request(caps()) -> jlib:iq().
disco_info_request(Caps) ->
    #iq{type = get,
        xmlns = ?NS_DISCO_INFO,
        sub_el = [#xmlel{name = ~"query",
                         attrs = #{~"xmlns" => ?NS_DISCO_INFO,
                                   ~"node" => capability_hash_node(Caps)}}]}.

-spec capability_hash_node(caps()) -> binary().
capability_hash_node(#{version := v1, node := Node, hash := {_HashAlg, HashValue}}) ->
    <<Node/binary, $#, HashValue/binary>>;
capability_hash_node(#{version := v2, hash := {HashAlg, HashValue}}) ->
    <<(?NS_CAPS_2)/binary, $#, HashAlg/binary, $., HashValue/binary>>.

-doc "Extracts caps from the first 'c' element with caps namespace".
-spec extract_caps([version()], exml:element()) -> caps() | no_caps.
extract_caps([Version | RemVersions], Element) ->
    All = find_caps(Version, Element),
    case lists:search(fun(#{hash := {Alg, _}}) -> mod_caps_hash:is_alg_supported(Alg) end, All) of
        {value, Caps} -> Caps;
        false -> extract_caps(RemVersions, Element)
    end;
extract_caps([], _Element) ->
    no_caps.

-spec find_caps(version(), exml:element()) -> [caps()].
find_caps(v1, Element) ->
    HashElements = exml_query:subelements_with_name_and_ns(Element, ~"c", ?NS_CAPS),
    [#{version => v1, hash => {HashAlg, HashVal}, node => Node}
     || #xmlel{attrs = #{~"hash" := HashAlg, ~"ver" := HashVal, ~"node" := Node}} <- HashElements];
find_caps(v2, Element) ->
    HashElements = exml_query:paths(Element, [{element_with_ns, ~"c", ?NS_CAPS_2},
                                              {element_with_ns, ~"hash", ?NS_HASH}]),
    [#{version => v2, hash => {exml_query:attr(HashEl, ~"algo"), exml_query:cdata(HashEl)}}
     || HashEl <- HashElements].

-spec caps_stream_features(mongooseim:host_type(), jid:jid()) -> [exml:element()].
caps_stream_features(HostType, ServerJID) ->
    [server_caps_element(Vsn, Hash) || Vsn := Hash <- make_server_hashes(HostType, ServerJID)].

-spec server_caps_element(version(), hash()) -> exml:element().
server_caps_element(v1, {HashAlg, HashValue}) ->
    #xmlel{name = ~"c",
           attrs = #{~"xmlns" => ?NS_CAPS,
                     ~"hash" => HashAlg,
                     ~"node" => ?MONGOOSE_URI,
                     ~"ver" => HashValue},
           children = []};
server_caps_element(v2, {HashAlg, HashValue}) ->
    #xmlel{name = ~"c",
           attrs = #{~"xmlns" => ?NS_CAPS_2},
           children = [#xmlel{name = ~"hash",
                              attrs = #{~"xmlns" => ?NS_HASH, ~"algo" => HashAlg},
                              children = [#xmlcdata{content = HashValue}]}]}.

-spec is_server_node_valid(mongooseim:host_type(), binary()) -> boolean().
is_server_node_valid(_HostType, ~"") ->
    true;
is_server_node_valid(HostType, Node) ->
    maybe
        {Version, Hash} ?= extract_server_hash(Node),
        #{Version := Hash} ?= get_server_hashes(HostType),
        true
    else
        _ -> false
    end.

-spec extract_server_hash(binary()) -> {version(), hash()} | no_match.
extract_server_hash(Node) ->
    case binary:split(Node, ~"#", []) of
        [?MONGOOSE_URI, HashValue] ->
            {v1, {server_hash_alg(v1), HashValue}};
        [?NS_CAPS_2, EncodedHash] ->
            case binary:split(EncodedHash, ~".", []) of
                [HashAlg, HashValue] -> {v2, {HashAlg, HashValue}};
                _ -> no_match
            end;
        _ ->
            no_match
    end.

-spec make_server_hashes(mongooseim:host_type(), jid:jid()) -> #{version() => hash()}.
make_server_hashes(HostType, ServerJID) ->
    maybe
        missing ?= get_server_hashes(HostType),
        Hashes = generate_server_hashes(HostType, ServerJID),
        persistent_term:put({?MODULE, {server_hashes, HostType}}, Hashes),
        Hashes
    end.

-spec get_server_hashes(mongooseim:host_type()) -> #{version() => hash()} | missing.
get_server_hashes(HostType) ->
    try
        persistent_term:get({?MODULE, {server_hashes, HostType}})
    catch
        error:badarg -> missing
    end.

-spec generate_server_hashes(mongooseim:host_type(), jid:jid()) -> #{version() => hash()}.
generate_server_hashes(HostType, ServerJID) ->
    DiscoElements = server_disco_elements(HostType, ServerJID),
    #{Version => generate_server_hash(DiscoElements, Version) || Version <- versions(HostType)}.

-spec generate_server_hash([exml:element()], version()) -> hash().
generate_server_hash(DiscoElements, Version) ->
    Alg = server_hash_alg(Version),
    {Alg, mod_caps_hash:generate(DiscoElements, Version, Alg)}.

%% For v2, there could be than one hash alg used, but there are no requirements to do so
-spec server_hash_alg(version()) -> hash_alg().
server_hash_alg(v1) -> ~"sha-1"; % XEP-0115 9.1 specifies SHA-1 as mandatory to implement
server_hash_alg(v2) -> ~"sha-256". % XEP-0390 has no such rule, but uses SHA-256 in examples

-spec ns(version()) -> binary().
ns(v1) -> ?NS_CAPS;
ns(v2) -> ?NS_CAPS_2.

-spec server_disco_elements(mongooseim:host_type(), jid:jid()) -> [exml:element()].
server_disco_elements(HostType, JID) ->
    {result, FeaturesXML} = mongoose_disco:get_local_features(HostType, JID, JID, ~"", ~""),
    IdentityXML = mongoose_disco:get_local_identity(HostType, JID, JID, ~"", ~""),
    InfoXML = mongoose_disco:get_info(HostType, mod_disco, ~"", ~""),
    IdentityXML ++ InfoXML ++ FeaturesXML.

-spec delete_server_hashes(mongooseim:host_type()) -> ok.
delete_server_hashes(HostType) ->
    persistent_term:erase({?MODULE, {server_hashes, HostType}}),
    ok.

-spec versions(mongooseim:host_type()) -> [version()].
versions(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, versions).
