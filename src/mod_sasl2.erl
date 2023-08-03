-module(mod_sasl2).
-xep([{xep, 388}, {version, "0.4.0"}, {status, partial}]).

-include("jlib.hrl").
-include("mongoose_logger.hrl").

-define(BIND_RETRIES, 3).
-define(XMLNS_SASL, {<<"xmlns">>, ?NS_SASL}).
-define(XMLNS_SASL2, {<<"xmlns">>, ?NS_SASL_2}).

-behaviour(gen_mod).
-behaviour(gen_statem).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, supported_features/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

%% hooks handlers
-export([c2s_stream_features/3, user_send_xmlel/3]).

-type maybe_binary() :: undefined | binary().
-type mod_state() :: #{authenticated := boolean(),
                       id := not_provided | uuid:uuid(),
                       software := not_provided | binary(),
                       device := not_provided | binary()}.

-type params() :: #{c2s_data => mongoose_c2s:data(), c2s_state => mongoose_c2s:state()}.
-export_type([params/0]).

%% gen_mod
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{c2s_stream_features, HostType, fun ?MODULE:c2s_stream_features/3, #{}, 50}
     | c2s_hooks(HostType)].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
c2s_hooks(HostType) ->
    [{user_send_xmlel, HostType, fun ?MODULE:user_send_xmlel/3, #{}, 50}].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%% gen_statem
callback_mode() ->
    handle_event_function.

-spec init(term()) -> gen_statem:init_result(mongoose_c2s:state(), mongoose_c2s:data()).
init(_) ->
    {stop, this_should_have_never_been_called}.

-spec handle_event(gen_statem:event_type(), term(), mongoose_c2s:state(), mongoose_c2s:data()) ->
    mongoose_c2s:fsm_res().
handle_event(internal, #xmlel{name = <<"authenticate">>} = El,
             {wait_for_feature_before_auth, SaslAcc, Retries} = C2SState, C2SData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL_2 ->
            handle_auth_start(C2SData, C2SState, El, SaslAcc, Retries);
        _ ->
            mongoose_c2s:c2s_stream_error(C2SData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"response">>} = El,
             {wait_for_sasl_response, SaslAcc, Retries} = C2SState, C2SData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL_2 ->
            handle_auth_response(C2SData, C2SState, El, SaslAcc, Retries);
        _ ->
            mongoose_c2s:c2s_stream_error(C2SData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"abort">>} = El,
             C2SState = {_, SaslAcc, Retries}, C2SData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL_2 ->
            handle_sasl_abort(C2SData, C2SState, El, SaslAcc, Retries);
        _ ->
            mongoose_c2s:c2s_stream_error(C2SData, mongoose_xmpp_errors:invalid_namespace())
    end;

handle_event(EventType, EventContent, C2SState, C2SData) ->
    mongoose_c2s:handle_event(EventType, EventContent, C2SState, C2SData).

-spec terminate(term(), mongoose_c2s:state(), mongoose_c2s:data()) -> term().
terminate(Reason, ?EXT_C2S_STATE(C2SState), C2SData) ->
    terminate(Reason, C2SState, C2SData);
terminate(Reason, C2SState, C2SData) ->
    ?LOG_DEBUG(#{what => sasl2_statem_terminate, reason => Reason,
                 c2s_state => C2SState, c2s_data => C2SData}),
    mongoose_c2s:terminate({shutdown, ?MODULE}, C2SState, C2SData).

%% Hooks
-spec c2s_stream_features(Acc, map(), gen_hook:extra()) -> {ok, Acc} when
    Acc :: [exml:element()].
c2s_stream_features(Acc, #{c2s_data := C2SData}, _) ->
    case is_ssl_connection(C2SData)
         andalso lists:keyfind(<<"mechanisms">>, #xmlel.name, Acc) of
        false ->
            {ok, Acc};
        #xmlel{attrs = [?XMLNS_SASL], children = Mechanisms} ->
            Sasl2Feature = feature(C2SData, Mechanisms),
            {ok, lists:keystore(feature_name(), #xmlel.name, Acc, Sasl2Feature)}
    end.

-spec user_send_xmlel(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
     mongoose_c2s_hooks:result().
user_send_xmlel(Acc, Params, _Extra) ->
    El = mongoose_acc:element(Acc),
    case exml_query:attr(El, <<"xmlns">>, false) of
        ?NS_SASL_2 ->
            user_send_sasl2_element(Acc, Params, El);
        _ ->
            {ok, Acc}
    end.

-spec user_send_sasl2_element(mongoose_acc:t(), mongoose_c2s_hooks:params(), exml:element()) ->
     mongoose_c2s_hooks:result().
user_send_sasl2_element(Acc, #{c2s_data := C2SData}, El) ->
    case is_not_sasl2_authenticated_already(C2SData) andalso is_ssl_connection(C2SData) of
        true ->
            %% We need to take control of the state machine to ensure no stanza
            %% out of the established protocol is processed
            Actions = [{push_callback_module, ?MODULE}, {next_event, internal, El}],
            ToAcc = [{actions, Actions}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
        false ->
            Lang = mongoose_c2s:get_lang(C2SData),
            Stanza = mongoose_xmpp_errors:policy_violation(Lang, <<"SALS2 violation">>),
            mongoose_c2s:c2s_stream_error(C2SData, Stanza),
            {stop, mongoose_c2s_acc:to_acc(Acc, hard_stop, sasl2_violation)}
    end.

-spec is_not_sasl2_authenticated_already(mongoose_c2s:data()) -> boolean().
is_not_sasl2_authenticated_already(C2SData) ->
    case get_mod_state(C2SData) of
        #{authenticated := true} -> false;
        _ -> true
    end.

-spec is_ssl_connection(mongoose_c2s:data()) -> boolean().
is_ssl_connection(C2SData) ->
    mongoose_c2s_socket:is_ssl(mongoose_c2s:get_socket(C2SData)).

-spec get_mod_state(mongoose_c2s:data()) -> {error, not_found} | mod_state().
get_mod_state(C2SData) ->
    case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
        {ok, State} -> State;
        Error -> Error
    end.

-spec handle_auth_start(
        mongoose_c2s:data(), mongoose_c2s:state(), exml:element(), mongoose_acc:t(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_auth_start(C2SData, C2SState, El, SaslAcc, Retries) ->
    case capture_mod_state(C2SData, exml_query:subelement(El, <<"user-agent">>, not_provided)) of
        invalid_agent ->
            mongoose_c2s:c2s_stream_error(C2SData, mongoose_xmpp_errors:policy_violation());
        C2SData1 ->
            HostType = mongoose_c2s:get_host_type(C2SData1),
            Mech = get_selected_mech(El),
            ClientIn = get_initial_response(El),
            EventContent = #{event_tag => ?MODULE,
                             event_content => #{stanza => El, mech => Mech, client_in => ClientIn}},
            HookParams = mongoose_c2s:hook_arg(C2SData1, C2SState, internal, EventContent, start),
            SaslAcc1 = mongoose_hooks:sasl2_start(HostType, SaslAcc, HookParams),
            SaslAcc2 = mongoose_c2s_sasl:start(C2SData1, SaslAcc1, Mech, ClientIn),
            handle_sasl_step(HookParams, SaslAcc2, Retries)
    end.

-spec handle_auth_response(
        mongoose_c2s:data(), mongoose_c2s:state(), exml:element(), mongoose_acc:t(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_auth_response(C2SData, C2SState, El, SaslAcc, Retries) ->
    ClientIn = base64:mime_decode(exml_query:cdata(El)),
    EventContent = #{event_tag => ?MODULE,
                     event_content => #{stanza => El, client_in => ClientIn}},
    HookParams = mongoose_c2s:hook_arg(C2SData, C2SState, internal, EventContent, start),
    SaslAcc1 = mongoose_c2s_sasl:continue(C2SData, SaslAcc, ClientIn),
    handle_sasl_step(HookParams, SaslAcc1, Retries).

-spec handle_sasl_abort(
        mongoose_c2s:data(), mongoose_c2s:state(), exml:element(), mongoose_acc:t(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_abort(C2SData, C2SState, El, SaslAcc, Retries) ->
    Jid = mongoose_c2s:get_jid(C2SData),
    Error = #{server_out => <<"aborted">>, maybe_username => Jid},
    EventContent = #{event_tag => ?MODULE, event_content => #{stanza => El}},
    HookParams = mongoose_c2s:hook_arg(C2SData, C2SState, internal, EventContent, start),
    handle_sasl_failure(HookParams, SaslAcc, Error, Retries).

-spec handle_sasl_step(
        mongoose_c2s_hooks:params(), mongoose_c2s_sasl:result(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_step(HookParams, {success, NewSaslAcc, Result}, _Retries) ->
    handle_sasl_success(HookParams, NewSaslAcc, Result);
handle_sasl_step(HookParams, {continue, NewSaslAcc, Result}, Retries) ->
    handle_sasl_continue(HookParams, NewSaslAcc, Result, Retries);
handle_sasl_step(HookParams, {failure, NewSaslAcc, Result}, Retries) ->
    handle_sasl_failure(HookParams, NewSaslAcc, Result, Retries);
handle_sasl_step(HookParams, {error, NewSaslAcc, Result}, Retries) ->
    handle_sasl_error(HookParams, NewSaslAcc, Result, Retries).

-spec handle_sasl_success(
        mongoose_c2s_hooks:params(), mongoose_acc:t(), mongoose_c2s_sasl:success()) ->
    mongoose_c2s:fsm_res().
handle_sasl_success(#{c2s_data := C2SData, c2s_state := C2SState} = HookParams, SaslAcc,
                    #{server_out := MaybeServerOut, jid := Jid, auth_module := AuthMod}) ->
    C2SData1 = mongoose_c2s:set_jid(C2SData, Jid),
    C2SData2 = mongoose_c2s:set_auth_module(C2SData1, AuthMod),
    HostType = mongoose_c2s:get_host_type(C2SData2),
    ?LOG_INFO(#{what => auth_success, text => <<"Accepted SASL authentication">>,
                user => jid:to_binary(Jid), c2s_state => C2SData2}),
    ModState = get_mod_state(C2SData2),
    Actions = [pop_callback_module,
               {next_event, internal, mongoose_c2s_stanzas:stream_header(C2SData2)},
               mongoose_c2s:state_timeout(C2SData2)],
    SuccessStanza = success_stanza(Jid, MaybeServerOut),
    StreamFeaturesStanza = mongoose_c2s_stanzas:stream_features_after_auth(C2SData2),
    C2SState1 = {wait_for_feature_after_auth, ?BIND_RETRIES},
    ToAcc = [{socket_send, [SuccessStanza, StreamFeaturesStanza]},
             {actions, Actions},
             {state_mod, {?MODULE, ModState#{authenticated := true}}},
             {c2s_state, C2SState1}],
    SaslAcc1 = mongoose_c2s_acc:to_acc_many(SaslAcc, ToAcc),
    SaslAcc2 = mongoose_hooks:sasl2_success(HostType, SaslAcc1, HookParams),
    mongoose_c2s:handle_state_after_packet(C2SData2, C2SState, SaslAcc2).

-spec handle_sasl_continue(
        mongoose_c2s_hooks:params(), mongoose_acc:t(), mongoose_c2s_sasl:continue(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_continue(#{c2s_data := C2SData, c2s_state := C2SState} = HookParams, SaslAcc,
                     #{server_out := ServerOut}, Retries) ->
    El = challenge_stanza(ServerOut),
    ToAcc = [{socket_send, El},
             {c2s_state, {wait_for_sasl_response, SaslAcc, Retries}}],
    SaslAcc1 = mongoose_c2s_acc:to_acc_many(SaslAcc, ToAcc),
    mongoose_c2s:handle_state_after_packet(C2SData, C2SState, SaslAcc1).

-spec handle_sasl_failure(
        mongoose_c2s_hooks:params(), mongoose_acc:t(), mongoose_c2s_sasl:failure(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_failure(#{c2s_data := C2SData, c2s_state := C2SState} = HookParams, SaslAcc,
                    #{server_out := ServerOut, maybe_username := Username}, Retries) ->
    LServer = mongoose_c2s:get_lserver(C2SData),
    ?LOG_INFO(#{what => auth_failed, text => <<"Failed SASL authentication">>,
                username => Username, lserver => LServer, c2s_state => C2SData}),
    El = failure_stanza(ServerOut),
    case mongoose_c2s:maybe_retry_state(C2SState) of
        {stop, Reason} ->
            {stop, Reason, C2SData};
        C2SState1 ->
            ToAcc = [{socket_send, El},
                     {c2s_state, {wait_for_feature_before_auth, SaslAcc, Retries}}],
            SaslAcc2 = mongoose_c2s_acc:to_acc_many(SaslAcc, ToAcc),
            mongoose_c2s:handle_state_after_packet(C2SData, C2SState1, SaslAcc2)
    end.

-spec handle_sasl_error(
        mongoose_c2s_hooks:params(), mongoose_acc:t(), mongoose_c2s_sasl:error(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_error(#{c2s_data := C2SData} = HookParams, SaslAcc, #{type := Type, text := Text}, _Retries) ->
    Lang = mongoose_c2s:get_lang(C2SData),
    El = mongoose_xmpp_errors:Type(Lang, Text),
    mongoose_c2s:c2s_stream_error(C2SData, El),
    {stop, mongoose_c2s_acc:to_acc(SaslAcc, hard_stop, sasl2_violation)}.

-spec success_stanza(jid:jid(), maybe_binary()) -> exml:element().
success_stanza(AuthJid, undefined) ->
    AuthorizationId = success_subelement(<<"authorization-identifier">>, jid:to_binary(AuthJid)),
    sasl2_ns_stanza(<<"success">>, [AuthorizationId]);
success_stanza(AuthJid, CData) ->
    AdditionalData = success_subelement(<<"additional-data">>, base64:encode(CData)),
    AuthorizationId = success_subelement(<<"authorization-identifier">>, jid:to_binary(AuthJid)),
    sasl2_ns_stanza(<<"success">>, [AdditionalData, AuthorizationId]).

-spec challenge_stanza(binary()) -> exml:element().
challenge_stanza(ServerOut) ->
    Challenge = #xmlcdata{content = base64:encode(ServerOut)},
    sasl2_ns_stanza(<<"challenge">>, [Challenge]).

-spec failure_stanza(binary()) -> exml:element().
failure_stanza(Reason) ->
    SaslErrorCode = #xmlel{name = Reason, attrs = [?XMLNS_SASL]},
    sasl2_ns_stanza(<<"failure">>, [SaslErrorCode]).

-spec sasl2_ns_stanza(binary(), [exml:element() | exml:cdata()]) -> exml:element().
sasl2_ns_stanza(Name, Children) ->
    #xmlel{name = Name, attrs = [?XMLNS_SASL2], children = Children}.

-spec success_subelement(binary(), binary()) -> exml:element().
success_subelement(Name, AuthId) ->
    #xmlel{name = Name, children = [#xmlcdata{content = AuthId}]}.

%% internal
-spec get_selected_mech(exml:element()) -> binary().
get_selected_mech(El) ->
    exml_query:attr(El, <<"mechanism">>, <<>>).

-spec get_initial_response(exml:element()) -> binary().
get_initial_response(El) ->
    base64:decode(exml_query:path(El, [{element, <<"initial-response">>}, cdata], <<>>)).

-spec capture_mod_state(mongoose_c2s:data(), not_provided | exml:element()) ->
    invalid_agent | mongoose_c2s:data().
capture_mod_state(C2SData, not_provided) ->
    UserAgent = #{authenticated => false, id => not_provided, software => not_provided, device => not_provided},
    mongoose_c2s:merge_mod_state(C2SData, #{?MODULE => UserAgent});
capture_mod_state(C2SData, El) ->
    MaybeId = exml_query:attr(El, <<"id">>, not_provided),
    case if_provided_then_is_not_invalid_uuid_v4(MaybeId) of
        invalid_agent ->
            invalid_agent;
        Value ->
            Software = exml_query:path(El, [{element, <<"software">>}, cdata], not_provided),
            Device = exml_query:path(El, [{element, <<"device">>}, cdata], not_provided),
            UserAgent = #{authenticated => false, id => Value, software => Software, device => Device},
            mongoose_c2s:merge_mod_state(C2SData, #{?MODULE => UserAgent})
    end.

-spec if_provided_then_is_not_invalid_uuid_v4(not_provided | binary()) ->
    invalid_agent | uuid:uuid().
if_provided_then_is_not_invalid_uuid_v4(not_provided) ->
    not_provided;
if_provided_then_is_not_invalid_uuid_v4(Binary) ->
    try
        Uuid = uuid:string_to_uuid(Binary),
        true = uuid:is_v4(Uuid),
        Uuid
    catch
        exit:badarg:_ -> invalid_agent;
        error:badmatch:_ -> invalid_agent
    end.

-spec feature(mongoose_c2s:data(), [exml:element()]) -> exml:element().
feature(C2SData, Mechanisms) ->
    InlineFeatures = mongoose_hooks:sasl2_stream_features(C2SData, []),
    InlineElem = inlines(InlineFeatures),
    #xmlel{name = feature_name(),
           attrs = [?XMLNS_SASL2],
           children = [InlineElem | Mechanisms]}.

-spec inlines([exml:element()]) -> exml:element().
inlines(InlineFeatures) ->
    #xmlel{name = <<"inline">>, children = InlineFeatures}.

-spec feature_name() -> binary().
feature_name() ->
    <<"authentication">>.
