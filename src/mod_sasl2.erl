%% Design Notes
%%
%% This module has three entry points for the statem: `authenticate', `response', and `abort'.
%% All three of them will generate one `OriginalStateData' that will remain unchanged for the whole
%% statem event, and a copy of this value will be stored into the `SaslAcc::mongoose_acc:t()', which
%% hook handlers can modify, and at the end of the processing, they can be compared for changes.
%%
%% This module triggers two hooks for before and after the full sasl2 mechanism is executed.
%% Handlers to these hooks can read the original `OriginalStateData', as well as the values
%% accumulated on the SaslAcc. If a value wants to be updated, it should be careful to fetch the
%% most recent from the accumulator, modify that one, and reinsert, otherwise it can override
%% updates made by previous handlers.
-module(mod_sasl2).
-xep([{xep, 388}, {version, "0.4.0"}, {status, partial}]).

-include("jlib.hrl").
-include("mongoose_logger.hrl").

-define(BIND_RETRIES, 3).
-define(XMLNS_SASL, {<<"xmlns">>, ?NS_SASL}).
-define(XMLNS_SASL_2, {<<"xmlns">>, ?NS_SASL_2}).

-behaviour(gen_mod).
-behaviour(gen_statem).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, supported_features/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

%% hooks handlers
-export([c2s_stream_features/3, user_send_xmlel/3]).

%% helpers
-export([get_inline_request/2, get_inline_request/3, put_inline_request/3,
         append_inline_response/3, update_inline_request/4,
         get_state_data/1, set_state_data/2,
         request_block_future_stream_features/1]).

-type maybe_binary() :: undefined | binary().
-type status() :: pending | success | failure.
-type inline_request() :: #{request := exml:element(),
                            response := undefined | exml:element(),
                            status := status()}.
-type mod_state() :: #{authenticated := boolean(),
                       id := not_provided | uuid:uuid(),
                       software := not_provided | binary(),
                       device := not_provided | binary()}.
-type c2s_state_data() :: #{c2s_state := mongoose_c2s:state(),
                            c2s_data := mongoose_c2s:data(),
                            _ := _}.

-export_type([c2s_state_data/0, inline_request/0]).

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
             ?EXT_C2S_STATE({wait_for_feature_before_auth, SaslAcc, Retries}) = C2SState, C2SData) ->
    %% We don't verify the namespace here because to here we just jumped from user_send_xmlel
    handle_auth_start(C2SData, C2SState, El, SaslAcc, Retries);
handle_event(internal, #xmlel{name = <<"response">>} = El,
             ?EXT_C2S_STATE({wait_for_sasl_response, SaslAcc, Retries}) = C2SState, C2SData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL_2 ->
            handle_auth_response(C2SData, C2SState, El, SaslAcc, Retries);
        _ ->
            mongoose_c2s:c2s_stream_error(C2SData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"abort">>} = El,
             ?EXT_C2S_STATE({_, SaslAcc, Retries}) = C2SState, C2SData) ->
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

%% Hook handlers
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
user_send_sasl2_element(Acc, #{c2s_data := C2SData, c2s_state := C2SState}, El) ->
    case is_not_sasl2_authenticated_already(C2SData) andalso is_ssl_connection(C2SData) of
        true ->
            %% We need to take control of the state machine to ensure no stanza
            %% out of the established protocol is processed
            Actions = [{push_callback_module, ?MODULE}, {next_event, internal, El}],
            ToAcc = [{c2s_state, ?EXT_C2S_STATE(C2SState)}, {actions, Actions}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
        false ->
            Lang = mongoose_c2s:get_lang(C2SData),
            Stanza = mongoose_xmpp_errors:policy_violation(Lang, <<"SALS2 violation">>),
            mongoose_c2s:c2s_stream_error(C2SData, Stanza),
            {stop, mongoose_c2s_acc:to_acc(Acc, hard_stop, sasl2_violation)}
    end.

-spec is_not_sasl2_authenticated_already(mongoose_c2s:data()) -> boolean().
is_not_sasl2_authenticated_already(C2SData) ->
    case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
        {ok, #{authenticated := true}} -> false;
        _ -> true
    end.

-spec is_ssl_connection(mongoose_c2s:data()) -> boolean().
is_ssl_connection(C2SData) ->
    mongoose_c2s_socket:is_ssl(mongoose_c2s:get_socket(C2SData)).

-spec get_mod_state(mongoose_acc:t()) -> {error, not_found} | mod_state().
get_mod_state(SaslAcc) ->
    case mongoose_acc:get_statem_acc(SaslAcc) of
        #{state_mod := #{?MODULE := ModState}} -> ModState;
        _ -> {error, not_found}
    end.

-spec handle_auth_start(
        mongoose_c2s:data(), mongoose_c2s:state(), exml:element(), mongoose_acc:t(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_auth_start(C2SData, C2SState, El, SaslAcc, Retries) ->
    case init_mod_state(exml_query:subelement(El, <<"user-agent">>, not_provided)) of
        invalid_agent ->
            mongoose_c2s:c2s_stream_error(C2SData, mongoose_xmpp_errors:policy_violation());
        ModState ->
            HostType = mongoose_c2s:get_host_type(C2SData),
            Mech = get_selected_mech(El),
            ClientIn = get_initial_response(El),
            OriginalStateData = #{c2s_state => C2SState, c2s_data => C2SData},
            SaslAcc1 = mongoose_c2s_acc:to_acc(SaslAcc, state_mod, {?MODULE, ModState}),
            SaslAcc2 = mongoose_hooks:sasl2_start(HostType, SaslAcc1, El),
            SaslResult = mongoose_c2s_sasl:start(C2SData, SaslAcc2, Mech, ClientIn),
            handle_sasl_step(SaslResult, OriginalStateData, Retries)
    end.

-spec handle_auth_response(
        mongoose_c2s:data(), mongoose_c2s:state(), exml:element(), mongoose_acc:t(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_auth_response(C2SData, C2SState, El, SaslAcc, Retries) ->
    ClientIn = base64:mime_decode(exml_query:cdata(El)),
    OriginalStateData = #{c2s_state => C2SState, c2s_data => C2SData},
    SaslResult = mongoose_c2s_sasl:continue(C2SData, SaslAcc, ClientIn),
    handle_sasl_step(SaslResult, OriginalStateData, Retries).

-spec handle_sasl_abort(
        mongoose_c2s:data(), mongoose_c2s:state(), exml:element(), mongoose_acc:t(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_abort(C2SData, C2SState, _El, SaslAcc, Retries) ->
    Jid = mongoose_c2s:get_jid(C2SData),
    Error = #{server_out => <<"aborted">>, maybe_username => Jid},
    OriginalStateData = #{c2s_state => C2SState, c2s_data => C2SData},
    handle_sasl_failure(SaslAcc, Error, OriginalStateData, Retries).

-spec handle_sasl_step(mongoose_c2s_sasl:result(), c2s_state_data(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_step({success, NewSaslAcc, Result}, OriginalStateData, _Retries) ->
    handle_sasl_success(NewSaslAcc, Result, OriginalStateData);
handle_sasl_step({continue, NewSaslAcc, Result}, OriginalStateData, Retries) ->
    handle_sasl_continue(NewSaslAcc, Result, OriginalStateData, Retries);
handle_sasl_step({failure, NewSaslAcc, Result}, OriginalStateData, Retries) ->
    handle_sasl_failure(NewSaslAcc, Result, OriginalStateData, Retries);
handle_sasl_step({error, NewSaslAcc, #{type := Type}}, OriginalStateData, Retries) ->
    handle_sasl_failure(NewSaslAcc, #{server_out => atom_to_binary(Type),
                                      maybe_username => undefined}, OriginalStateData, Retries).

-spec handle_sasl_success(mongoose_acc:t(), mongoose_c2s_sasl:success(), c2s_state_data()) ->
    mongoose_c2s:fsm_res().
handle_sasl_success(SaslAcc,
                    #{server_out := MaybeServerOut, jid := Jid, auth_module := AuthMod},
                    #{c2s_data := C2SData} = OriginalStateData) ->
    C2SData1 = build_final_c2s_data(C2SData, Jid, AuthMod),
    OriginalStateData1 = OriginalStateData#{c2s_data := C2SData1},
    ?LOG_INFO(#{what => auth_success, text => <<"Accepted SASL authentication">>,
                user => jid:to_binary(Jid), c2s_state => C2SData1}),
    HostType = mongoose_c2s:get_host_type(C2SData1),
    SaslAcc1 = set_state_data(SaslAcc, OriginalStateData1),
    SaslAcc2 = mongoose_hooks:sasl2_success(HostType, SaslAcc1, OriginalStateData1),
    process_sasl2_success(SaslAcc2, OriginalStateData1, MaybeServerOut).

-spec handle_sasl_continue(
        mongoose_acc:t(), mongoose_c2s_sasl:continue(), c2s_state_data(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_continue(SaslAcc,
                     #{server_out := ServerOut},
                     #{c2s_data := C2SData, c2s_state := C2SState},
                     Retries) ->
    El = challenge_stanza(ServerOut),
    ToAcc = [{socket_send, El},
             {c2s_state, ?EXT_C2S_STATE({wait_for_sasl_response, SaslAcc, Retries})}],
    SaslAcc1 = mongoose_c2s_acc:to_acc_many(SaslAcc, ToAcc),
    mongoose_c2s:handle_state_after_packet(C2SData, C2SState, SaslAcc1).

-spec handle_sasl_failure(
        mongoose_acc:t(), mongoose_c2s_sasl:failure(), c2s_state_data(), mongoose_c2s:retries()) ->
    mongoose_c2s:fsm_res().
handle_sasl_failure(SaslAcc,
                    #{server_out := ServerOut, maybe_username := Username},
                    #{c2s_data := C2SData, c2s_state := C2SState},
                    Retries) ->
    LServer = mongoose_c2s:get_lserver(C2SData),
    ?LOG_INFO(#{what => auth_failed, text => <<"Failed SASL authentication">>,
                username => Username, lserver => LServer, c2s_state => C2SData}),
    El = failure_stanza(ServerOut),
    case mongoose_c2s:maybe_retry_state(C2SState) of
        {stop, Reason} ->
            {stop, Reason, C2SData};
        C2SState1 ->
            ToAcc = [{socket_send, El},
                     {c2s_state, ?EXT_C2S_STATE({wait_for_feature_before_auth, SaslAcc, Retries})}],
            SaslAcc2 = mongoose_c2s_acc:to_acc_many(SaslAcc, ToAcc),
            mongoose_c2s:handle_state_after_packet(C2SData, C2SState1, SaslAcc2)
    end.

%% Append to the c2s_data both the new jid and the auth module.
%% Note that further inline requests can later on append a new jid if a resource is negotiated.
-spec build_final_c2s_data(mongoose_c2s:data(), jid:jid(), module()) -> mongoose_c2s:data().
build_final_c2s_data(C2SData, Jid, AuthMod) ->
    C2SData1 = mongoose_c2s:set_jid(C2SData, Jid),
    mongoose_c2s:set_auth_module(C2SData1, AuthMod).

-spec process_sasl2_success(mongoose_acc:t(), c2s_state_data(), maybe_binary()) ->
    mongoose_c2s:fsm_res().
process_sasl2_success(SaslAcc, OriginalStateData, MaybeServerOut) ->
    #{c2s_data := C2SData, c2s_state := C2SState} = get_state_data(SaslAcc),
    SuccessStanza = success_stanza(SaslAcc, C2SData, MaybeServerOut),
    ToAcc = build_to_c2s_acc(SaslAcc, C2SData, OriginalStateData, SuccessStanza),
    SaslAcc1 = mongoose_c2s_acc:to_acc_many(SaslAcc, ToAcc),
    mongoose_c2s:handle_state_after_packet(C2SData, C2SState, SaslAcc1).

%% After auth and inline requests we:
%% - return control to mongoose_c2s (pop_callback_module),
%% - ensure the answer to the sasl2 request is sent in the socket first,
%% - then decide depending on whether an inline request has taken control of the c2s_state if
%%      - do nothing if control was taken
%%      - put the statem in wait_for_feature_after_auth
-spec build_to_c2s_acc(mongoose_acc:t(), mongoose_c2s:data(), c2s_state_data(), exml:element()) ->
    mongoose_c2s_acc:pairs().
build_to_c2s_acc(SaslAcc, C2SData, OriginalStateData, SuccessStanza) ->
    ModState = get_mod_state(SaslAcc),
    MaybeSocketSendStreamFeatures = maybe_flush_stream_features(SaslAcc, C2SData),
    case is_new_c2s_state_requested(SaslAcc, OriginalStateData) of
        false ->
            %% Unless specified by an inline feature, sasl2 would normally put the statem just before bind
            [{socket_send_first, SuccessStanza},
             {c2s_state, {wait_for_feature_after_auth, ?BIND_RETRIES}},
             {actions, [pop_callback_module, mongoose_c2s:state_timeout(C2SData)]},
             {state_mod, {?MODULE, ModState#{authenticated := true}}}
             | MaybeSocketSendStreamFeatures];
        true ->
            [{socket_send_first, SuccessStanza},
             {actions, [pop_callback_module]},
             {state_mod, {?MODULE, ModState#{authenticated := true}}}
             | MaybeSocketSendStreamFeatures]
    end.

-spec request_block_future_stream_features(mongoose_acc:t()) -> mongoose_acc:t().
request_block_future_stream_features(SaslAcc) ->
    mongoose_acc:set(?MODULE, stream_features, false, SaslAcc).

-spec maybe_flush_stream_features(mongoose_acc:t(), mongoose_c2s:data()) ->
    [{flush, mongoose_acc:t()}].
maybe_flush_stream_features(SaslAcc, C2SData) ->
    case mongoose_acc:get(?MODULE, stream_features, true, SaslAcc) of
        true ->
            StreamFeaturesStanza = mongoose_c2s_stanzas:stream_features_after_auth(C2SData),
            Jid = mongoose_c2s:get_jid(C2SData),
            LServer = mongoose_c2s:get_lserver(C2SData),
            HostType = mongoose_c2s:get_host_type(C2SData),
            AccParams = #{lserver => LServer, host_type => HostType,
                          from_jid => jid:make_noprep(<<>>, LServer, <<>>), to_jid => Jid,
                          element => StreamFeaturesStanza},
            Acc = mongoose_acc:strip(AccParams, SaslAcc),
            [{flush, Acc}];
        false ->
            []
    end.

-spec is_new_c2s_state_requested(mongoose_acc:t(), c2s_state_data()) -> boolean().
is_new_c2s_state_requested(SaslAcc, #{c2s_state := OldState}) ->
    #{c2s_state := NewState} = mod_sasl2:get_state_data(SaslAcc),
    OldState =/= NewState.

-spec success_stanza(mongoose_acc:t(), mongoose_c2s:data(), maybe_binary()) -> exml:element().
success_stanza(SaslAcc, C2SData, MaybeCData) ->
    Jid = mongoose_c2s:get_jid(C2SData),
    Inlines = get_acc_sasl2_state(SaslAcc),
    InlineAnswers = get_inline_responses(Inlines),
    case MaybeCData of
        undefined ->
            AuthorizationId = success_subelement(<<"authorization-identifier">>, jid:to_binary(Jid)),
            sasl2_ns_stanza(<<"success">>, [AuthorizationId | InlineAnswers]);
        CData ->
            AdditionalData = success_subelement(<<"additional-data">>, base64:encode(CData)),
            AuthorizationId = success_subelement(<<"authorization-identifier">>, jid:to_binary(Jid)),
            sasl2_ns_stanza(<<"success">>, [AdditionalData, AuthorizationId | InlineAnswers])
    end.

-spec get_inline_responses([inline_request()]) -> [exml:element()].
get_inline_responses(Inlines) ->
    [ Response || {Module, #{status := Status, response := Response}} <- Inlines,
                  ?MODULE =/= Module,
                  pending =/= Status,
                  undefined =/= Response ].

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
    #xmlel{name = Name, attrs = [?XMLNS_SASL_2], children = Children}.

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

-spec init_mod_state(not_provided | exml:element()) -> invalid_agent | mod_state().
init_mod_state(not_provided) ->
    #{authenticated => false, id => not_provided, software => not_provided, device => not_provided};
init_mod_state(El) ->
    MaybeId = exml_query:attr(El, <<"id">>, not_provided),
    case if_provided_then_is_not_invalid_uuid_v4(MaybeId) of
        invalid_agent ->
            invalid_agent;
        Value ->
            Software = exml_query:path(El, [{element, <<"software">>}, cdata], not_provided),
            Device = exml_query:path(El, [{element, <<"device">>}, cdata], not_provided),
            #{authenticated => false, id => Value, software => Software, device => Device}
    end.

-spec if_provided_then_is_not_invalid_uuid_v4(not_provided | binary()) ->
    not_provided | invalid_agent | uuid:uuid().
if_provided_then_is_not_invalid_uuid_v4(not_provided) ->
    not_provided;
if_provided_then_is_not_invalid_uuid_v4(Binary) ->
    try
        Uuid = uuid:string_to_uuid(Binary),
        true = uuid:is_v4(Uuid),
        Uuid
    catch
        exit:badarg:_ -> invalid_agent;
        error:{badmatch, false}:_ -> invalid_agent
    end.

-spec feature(mongoose_c2s:data(), [exml:element()]) -> exml:element().
feature(C2SData, Mechanisms) ->
    InlineFeatures = mongoose_hooks:sasl2_stream_features(C2SData, []),
    InlineElem = inlines(InlineFeatures),
    #xmlel{name = feature_name(),
           attrs = [?XMLNS_SASL_2],
           children = [InlineElem | Mechanisms]}.

-spec inlines([exml:element()]) -> exml:element().
inlines(InlineFeatures) ->
    #xmlel{name = <<"inline">>, children = InlineFeatures}.

-spec feature_name() -> binary().
feature_name() ->
    <<"authentication">>.

-spec get_acc_sasl2_state(mongoose_acc:t()) -> [{module(), inline_request()}].
get_acc_sasl2_state(SaslAcc) ->
    mongoose_acc:get(?MODULE, SaslAcc).

-spec get_inline_request(mongoose_acc:t(), module()) -> inline_request().
get_inline_request(SaslAcc, ModuleRequest) ->
    mongoose_acc:get(?MODULE, ModuleRequest, SaslAcc).

-spec get_inline_request(mongoose_acc:t(), module(), Default) -> Default | inline_request().
get_inline_request(SaslAcc, ModuleRequest, Default) ->
    mongoose_acc:get(?MODULE, ModuleRequest, Default, SaslAcc).

-spec put_inline_request(mongoose_acc:t(), module(), exml:element()) -> mongoose_acc:t().
put_inline_request(SaslAcc, ModuleRequest, XmlRequest) ->
    Request = #{request => XmlRequest, response => undefined, status => pending},
    mongoose_acc:set(?MODULE, ModuleRequest, Request, SaslAcc).

-spec append_inline_response(mongoose_acc:t(), module(), exml:element()) -> mongoose_acc:t().
append_inline_response(SaslAcc, ModuleRequest, XmlResponse) ->
    case mongoose_acc:get(?MODULE, ModuleRequest, undefined, SaslAcc) of
        undefined ->
            SaslAcc;
        Request ->
            Request1 = Request#{response := XmlResponse},
            mongoose_acc:set(?MODULE, ModuleRequest, Request1, SaslAcc)
    end.

-spec update_inline_request(mongoose_acc:t(), module(), exml:element(), status()) -> mongoose_acc:t().
update_inline_request(SaslAcc, ModuleRequest, XmlResponse, Status) ->
    case mongoose_acc:get(?MODULE, ModuleRequest, undefined, SaslAcc) of
        undefined ->
            SaslAcc;
        Request ->
            Request1 = Request#{response := XmlResponse, status := Status},
            mongoose_acc:set(?MODULE, ModuleRequest, Request1, SaslAcc)
    end.

%% Here we extract these values after all modifications by the inline requests
-spec get_state_data(mongoose_acc:t()) -> c2s_state_data().
get_state_data(SaslAcc) ->
    mongoose_acc:get(?MODULE, c2s_state_data, SaslAcc).

-spec set_state_data(mongoose_acc:t(), c2s_state_data()) -> mongoose_acc:t().
set_state_data(SaslAcc, OriginalStateData) ->
    mongoose_acc:set(?MODULE, c2s_state_data, OriginalStateData, SaslAcc).
