%% @doc Implement Bind2, allowing the inline specification of:
%% - Resource binding
%% - Stream management
-module(mod_bind2).
-xep([{xep, 386}, {version, "0.4.0"}, {status, partial}]).

-include("jlib.hrl").

-define(XMLNS_BIND2, {<<"xmlns">>, ?NS_BIND_2}).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, deps/2, hooks/1, supported_features/0]).

%% hooks handlers
-export([
         sasl2_stream_features/3,
         sasl2_start/3,
         sasl2_success/3
        ]).

%% gen_mod
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    [{mod_sasl2, Opts, hard}].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
     {sasl2_stream_features, HostType, fun ?MODULE:sasl2_stream_features/3, #{}, 50},
     {sasl2_start, HostType, fun ?MODULE:sasl2_start/3, #{}, 50},
     {sasl2_success, HostType, fun ?MODULE:sasl2_success/3, #{}, 50} %% after SM
    ].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].


%% Hooks
-spec sasl2_start(SaslAcc, #{stanza := exml:element()}, gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_start(SaslAcc, #{stanza := El}, _) ->
    case exml_query:path(El, [{element_with_ns, <<"bind">>, ?NS_BIND_2}]) of
        undefined ->
            {ok, SaslAcc};
        BindRequest ->
            {ok, mod_sasl2:put_inline_request(SaslAcc, ?MODULE, BindRequest)}
    end.

-spec sasl2_success(SaslAcc, mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_success(SaslAcc, _, _) ->
    case mod_sasl2:get_inline_request(SaslAcc, ?MODULE) of
        undefined ->
            {ok, SaslAcc};
        #{request := BindRequest} ->
            Resource = generate_lresource(BindRequest),
            SmRequest = get_stream_management_resumption_status(SaslAcc),
            maybe_bind(SaslAcc, Resource, SmRequest)
    end.

-spec sasl2_stream_features(Acc, #{c2s_data := mongoose_c2s:data()}, gen_hook:extra()) ->
    {ok, Acc} when Acc :: [exml:element()].
sasl2_stream_features(Acc, #{c2s_data := C2SData}, _) ->
    Bind2Feature = feature(C2SData),
    {ok, lists:keystore(feature_name(), #xmlel.name, Acc, Bind2Feature)}.


%% Helpers
-spec generate_lresource(exml:element()) -> jid:lresource().
generate_lresource(BindRequest) ->
    GeneratedResource = mongoose_c2s:generate_random_resource(),
    case exml_query:path(BindRequest, [{element, <<"tag">>}, cdata], not_provided) of
        not_provided ->
            GeneratedResource;
        Tag ->
            case jid:resourceprep(Tag) of
                error ->
                    %% The XEP does not specify how to fail in case of a bad resource requested,
                    %% so we decide to ignore the tag and simply generate our own resource
                    GeneratedResource;
                Prefix -> %% https://xmpp.org/extensions/xep-0386.html#identifiers
                    <<Prefix/binary, "/", GeneratedResource/binary>>
            end
    end.

-spec get_stream_management_resumption_status(mongoose_acc:t()) -> mod_sasl2:inline_request().
get_stream_management_resumption_status(SaslAcc) ->
    mod_sasl2:get_inline_request(SaslAcc, mod_stream_management).

-spec maybe_bind(
        SaslAcc, jid:lresource(), undefined | mod_sasl2:inline_request()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
maybe_bind(SaslAcc, _Resource, #{status := success, response := SmResponse}) ->
    Bound = #xmlel{name = <<"bound">>, attrs = [?XMLNS_BIND2], children = [SmResponse]},
    {ok, mod_sasl2:update_inline_request(SaslAcc, ?MODULE, Bound, success)};
maybe_bind(SaslAcc, Resource, SmRequest) ->
    SaslStateData = mod_sasl2:get_state_data(SaslAcc),
    MaybeSmChild = case SmRequest of
                       #{status := failure, response := Response} -> [Response];
                       _ -> []
                   end,
    case do_bind(SaslAcc, SaslStateData, Resource) of
        {ok, SaslAcc1, C2SData} ->
            HostType = mongoose_acc:host_type(SaslAcc),
            Bound = #xmlel{name = <<"bound">>, attrs = [?XMLNS_BIND2], children = MaybeSmChild},
            NewStateData = SaslStateData#{c2s_data => C2SData, c2s_state => session_established},
            SaslAcc2 = mod_sasl2:set_state_data(SaslAcc1, NewStateData),
            SaslAcc3 = mod_sasl2:update_inline_request(SaslAcc2, ?MODULE, Bound, success),
            SaslAcc4 = mongoose_hooks:bind2_enable_features(HostType, SaslAcc3, SaslStateData),
            {ok, SaslAcc4};
        {stop, SaslAcc1} ->
            %% The XEP does not specify what to do if the resource wasn't bound
            %% so we just set failed here and move along with SASL2
            Error = #xmlel{name = <<"failed">>, attrs = [?XMLNS_BIND2], children = MaybeSmChild},
            {ok, mod_sasl2:update_inline_request(SaslAcc1, ?MODULE, Error, failure)}
    end.

-spec do_bind(SaslAcc, mod_sasl2:c2s_state_data(), jid:lresource()) ->
    {ok, SaslAcc, mongoose_c2s:data()} | {stop, SaslAcc}
      when SaslAcc :: mongoose_acc:t().
do_bind(SaslAcc, #{c2s_state := C2SState, c2s_data := C2SData}, Resource) ->
    %% TODO the jid will be in the accumulator, not in the data, we just authenticated!
    C2SData1 = mongoose_c2s:replace_resource(C2SData, Resource),
    HookParams = mongoose_c2s:hook_arg(C2SData1, C2SState, internal, bind2, undefined),
    mongoose_c2s:verify_user_and_open_session(HookParams, session_established, SaslAcc).

-spec feature(mongoose_c2s:data()) -> exml:element().
feature(C2SData) ->
    Inlines = mongoose_hooks:bind2_stream_features(C2SData, []),
    InlineElem = inlines(Inlines),
    #xmlel{name = feature_name(),
           attrs = [?XMLNS_BIND2],
           children = [InlineElem]}.

-spec inlines([exml:element()]) -> exml:element().
inlines(Inlines) ->
    #xmlel{name = <<"inline">>, children = Inlines}.

-spec feature_name() -> binary().
feature_name() ->
    <<"bind">>.
