-module(mod_stream_management_sasl2).

-define(SM, mod_stream_management).

-include("jlib.hrl").
-include("mongoose_ns.hrl").

-export([hooks/1]).

-export([sasl2_stream_features/3,
         sasl2_start/3,
         sasl2_success/3,
         bind2_stream_features/3,
         bind2_enable_features/3]).

%% Helpers
-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
     {sasl2_stream_features, HostType, fun ?MODULE:sasl2_stream_features/3, #{}, 50},
     {sasl2_start, HostType, fun ?MODULE:sasl2_start/3, #{}, 50},
     {sasl2_success, HostType, fun ?MODULE:sasl2_success/3, #{}, 20},
     {bind2_stream_features, HostType, fun ?MODULE:bind2_stream_features/3, #{}, 50},
     {bind2_enable_features, HostType, fun ?MODULE:bind2_enable_features/3, #{}, 50}
    ].

%% Hook handlers
-spec sasl2_stream_features(Acc, #{c2s_data := mongoose_c2s:data()}, gen_hook:extra()) ->
    {ok, Acc} when Acc :: [exml:element()].
sasl2_stream_features(Acc, _, _) ->
    Resume = #xmlel{name = <<"sm">>, attrs = #{<<"xmlns">> => ?NS_STREAM_MGNT_3}},
    {ok, [Resume | Acc]}.

-spec sasl2_start(SaslAcc, #{stanza := exml:element()}, gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_start(SaslAcc, #{stanza := El}, _) ->
    case exml_query:path(El, [{element_with_ns, <<"resume">>, ?NS_STREAM_MGNT_3}]) of
        undefined ->
            {ok, SaslAcc};
        SmRequest ->
            {ok, mod_sasl2:put_inline_request(SaslAcc, ?MODULE, SmRequest)}
    end.

%% If SASL2 inline stream resumption is requested, that MUST be processed FIRST by the server.
-spec sasl2_success(mongoose_acc:t(), mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
sasl2_success(SaslAcc, _, _) ->
    case mod_sasl2:get_inline_request(SaslAcc, ?MODULE, undefined) of
        undefined ->
            {ok, SaslAcc};
        SmRequest ->
            handle_sasl2_resume(SaslAcc, SmRequest)
    end.

-spec bind2_stream_features(Acc, #{c2s_data := mongoose_c2s:data()}, gen_hook:extra()) ->
    {ok, Acc} when Acc :: [exml:element()].
bind2_stream_features(Acc, _, _) ->
    SmFeature = #xmlel{name = <<"feature">>, attrs = #{<<"var">> => ?NS_STREAM_MGNT_3}},
    {ok, [SmFeature | Acc]}.

-spec bind2_enable_features(SaslAcc, mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
bind2_enable_features(SaslAcc, Params, _) ->
    Inline = #{request := BindRequest} = mod_bind2:get_bind_request(SaslAcc),
    case exml_query:subelement_with_name_and_ns(BindRequest, <<"enable">>, ?NS_STREAM_MGNT_3) of
        undefined ->
            {ok, SaslAcc};
        El ->
            handle_bind_enable(SaslAcc, Params, Inline, El)
    end.

%% If resumption is successful:
%% - MUST skip resource binding (a resumed session already has a resource bound)
%% - MUST entirely ignore the potentially inlined <bind/> request
%% - MUST NOT send stream features in this case (overriding behaviour defined in SASL2)
-spec handle_sasl2_resume(mongoose_acc:t(), mod_sasl2:inline_request()) ->
    mongoose_c2s_hooks:result().
handle_sasl2_resume(SaslAcc, #{request := El}) ->
    #{c2s_state := C2SState, c2s_data := C2SData} = mod_sasl2:get_state_data(SaslAcc),
    case mod_stream_management:handle_resume(C2SData, C2SState, El) of
        {stream_mgmt_error, ErrorStanza} ->
            {ok, mod_sasl2:update_inline_request(SaslAcc, ?MODULE, ErrorStanza, failure)};
        {error, _ErrorStanza, _Reason} -> %% This signifies a stream-error, but we discard those here
            SimpleErrorStanza = mod_stream_management_stanzas:stream_mgmt_failed(<<"bad-request">>),
            {ok, mod_sasl2:update_inline_request(SaslAcc, ?MODULE, SimpleErrorStanza, failure)};
        {error, ErrorStanza} ->
            {ok, mod_sasl2:update_inline_request(SaslAcc, ?MODULE, ErrorStanza, failure)};
        {ok, #{resumed := Resumed, forward := ToForward} = Ret} ->
            SaslAcc1 = mod_sasl2:update_inline_request(SaslAcc, ?MODULE, Resumed, success),
            SaslAcc2 = mod_sasl2:set_state_data(SaslAcc1, Ret),
            SaslAcc3 = mod_sasl2:request_block_future_stream_features(SaslAcc2),
            %% We stop here to completely ignore subsequent inlines (for example BIND2)
            {stop, mongoose_c2s_acc:to_acc(SaslAcc3, socket_send, ToForward)}
    end.

-spec handle_bind_enable(SaslAcc, mod_sasl2:c2s_state_data(), mod_sasl2:inline_request(), exml:element()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
handle_bind_enable(SaslAcc, #{c2s_data := C2SData}, Inline, El) ->
    case mod_stream_management:if_not_already_enabled_create_sm_state(C2SData) of
        error ->
            mod_stream_management:stream_error(SaslAcc, C2SData);
        SmState ->
            do_handle_bind_enable(SaslAcc, C2SData, SmState, Inline, El)
    end.

-spec do_handle_bind_enable(SaslAcc, mongoose_c2s:data(), mod_stream_management:sm_state(), mod_sasl2:inline_request(), exml:element()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
do_handle_bind_enable(SaslAcc, C2SData, SmState, Inline, El) ->
    case exml_query:attr(El, <<"resume">>, false) of
        false ->
            Stanza = mod_stream_management_stanzas:stream_mgmt_enabled(),
            SaslAcc1 = mongoose_c2s_acc:to_acc(SaslAcc, state_mod, {?SM, SmState}),
            SaslAcc2 = mod_bind2:append_inline_bound_answer(SaslAcc1, Inline, Stanza),
            {ok, SaslAcc2};
        Attr when Attr =:= <<"true">>; Attr =:= <<"1">> ->
            Stanza = mod_stream_management:register_smid_return_enabled_stanza(C2SData),
            SaslAcc1 = mongoose_c2s_acc:to_acc(SaslAcc, state_mod, {?SM, SmState}),
            SaslAcc2 = mod_bind2:append_inline_bound_answer(SaslAcc1, Inline, Stanza),
            {ok, SaslAcc2};
        _ ->
            mod_stream_management:stream_error(SaslAcc, C2SData)
    end.
