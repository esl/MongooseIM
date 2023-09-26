%% @doc Implement Bind2, allowing the inline specification of:
%% resource binding, stream management, carbons and csi.
%%
%% There are two steps to this: on sasl2_success, we check if there was a bind2 inline request,
%% and also, if there was a previous SM resumption that has already succeeded – if resumption
%% succeeded, bind2 must be entirely skipped.
%%
%% If we can proceed with binding, we run a similar algorithm that c2s runs itself: we verify the
%% user is allowed to start a session, we open the session changing the c2s data, and we finalise
%% the operation with handle_state_after_packet or maybe_retry_state. But in the case of bind2,
%% once verifications are successful, we need to modify the c2s data to append for example the csi
%% state or the carbons tag in the session info, before the session is actually established, so that
%% session establishment is atomic to changes in the c2s data.
-module(mod_bind2).
-xep([{xep, 386}, {version, "0.4.0"}, {status, partial}]).

-include("jlib.hrl").

-define(XMLNS_BIND_2, {<<"xmlns">>, ?NS_BIND_2}).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, deps/2, hooks/1, supported_features/0]).

%% hooks handlers
-export([
         sasl2_stream_features/3,
         sasl2_start/3,
         sasl2_success/3
        ]).

-export([get_bind_request/1, append_inline_bound_answer/3]).

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


%% Hook handlers
-spec sasl2_stream_features(Acc, #{c2s_data := mongoose_c2s:data()}, gen_hook:extra()) ->
    {ok, Acc} when Acc :: [exml:element()].
sasl2_stream_features(Acc, #{c2s_data := C2SData}, _) ->
    Bind2Feature = feature(C2SData),
    {ok, lists:keystore(feature_name(), #xmlel.name, Acc, Bind2Feature)}.

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
sasl2_success(SaslAcc, _, #{host_type := HostType}) ->
    case mod_sasl2:get_inline_request(SaslAcc, ?MODULE, undefined) of
        undefined ->
            {ok, SaslAcc};
        #{request := BindRequest} ->
            Resource = generate_lresource(BindRequest),
            maybe_bind(SaslAcc, Resource, HostType)
    end.

-spec maybe_bind(SaslAcc, jid:lresource(), mongooseim:host_type()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
maybe_bind(SaslAcc, Resource, HostType) ->
    {SaslAcc1, SaslStateData1, HookParams} = build_new_c2sdata_saslstatedata_and_hookparams(SaslAcc, Resource),
    case mongoose_c2s:verify_user(session_established, HostType, HookParams, SaslAcc1) of
        {ok, SaslAcc2} ->
            Bound = create_bind_response(<<"bound">>),
            SaslAcc3 = mod_sasl2:update_inline_request(SaslAcc2, ?MODULE, Bound, success),
            SaslAcc4 = mongoose_hooks:bind2_enable_features(HostType, SaslAcc3, SaslStateData1),
            #{c2s_data := C2SData2} = SaslStateData2 = mod_sasl2:get_state_data(SaslAcc4),
            {ok, SaslAcc5, C2SData3} = mongoose_c2s:maybe_open_session(session_established, {ok, SaslAcc4}, C2SData2),
            SaslStateData3 = SaslStateData2#{c2s_data => C2SData3, c2s_state => session_established},
            SaslAcc6 = mod_sasl2:set_state_data(SaslAcc5, SaslStateData3),
            {ok, SaslAcc6};
        {stop, SaslAcc1} ->
            bind2_failed(SaslAcc1)
    end.

-spec build_new_c2sdata_saslstatedata_and_hookparams(mongoose_acc:t(), jid:lresource()) ->
    {mongoose_acc:t(), mod_sasl2:c2s_state_data(), mongoose_c2s_hooks:params()}.
build_new_c2sdata_saslstatedata_and_hookparams(SaslAcc, Resource) ->
    SaslStateData1 = mod_sasl2:get_state_data(SaslAcc),
    #{c2s_data := C2SData1, c2s_state := C2SState1} = SaslStateData1,
    C2SData2 = mongoose_c2s:replace_resource(C2SData1, Resource),
    SaslStateData2 = SaslStateData1#{c2s_data => C2SData2},
    HookParams = mongoose_c2s:hook_arg(C2SData2, C2SState1, internal, bind2, undefined),
    {mod_sasl2:set_state_data(SaslAcc, SaslStateData2), SaslStateData2, HookParams}.

bind2_failed(SaslAcc) ->
    %% The XEP does not specify what to do if the resource wasn't bound
    %% so we just set failed here and move along with SASL2
    Error = create_bind_response(<<"failed">>),
    {ok, mod_sasl2:update_inline_request(SaslAcc, ?MODULE, Error, failure)}.

create_bind_response(Answer) ->
    #xmlel{name = Answer, attrs = [?XMLNS_BIND_2]}.

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

-spec feature(mongoose_c2s:data()) -> exml:element().
feature(C2SData) ->
    Inlines = mongoose_hooks:bind2_stream_features(C2SData, []),
    InlineElem = inlines(Inlines),
    #xmlel{name = feature_name(),
           attrs = [?XMLNS_BIND_2],
           children = [InlineElem]}.

-spec inlines([exml:element()]) -> exml:element().
inlines(Inlines) ->
    #xmlel{name = <<"inline">>, children = Inlines}.

-spec feature_name() -> binary().
feature_name() ->
    <<"bind">>.

-spec get_bind_request(mongoose_acc:t()) -> mod_sasl2:inline_request().
get_bind_request(SaslAcc) ->
    mod_sasl2:get_inline_request(SaslAcc, ?MODULE).

-spec append_inline_bound_answer(mongoose_acc:t(), mod_sasl2:inline_request(), exml:element()) ->
    mongoose_acc:t().
append_inline_bound_answer(SaslAcc, #{response := BoundEl = #xmlel{children = Children}}, Response) ->
    BoundEl1 = BoundEl#xmlel{children = [Response | Children]},
    mod_sasl2:append_inline_response(SaslAcc, ?MODULE, BoundEl1).
