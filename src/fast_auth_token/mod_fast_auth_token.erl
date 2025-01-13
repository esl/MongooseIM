-module(mod_fast_auth_token).
-xep([{xep, 484}, {version, "0.2.0"}]).
-behaviour(gen_mod).
-include("mongoose_ns.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

%% `gen_mod' callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0]).

%% hooks handlers
-export([sasl2_stream_features/3,
         sasl2_start/3,
         sasl2_success/3,
         remove_user/3,
         remove_domain/3]).

-export([read_tokens/4]).

%% For mocking
-export([utc_now_as_seconds/0,
         generate_unique_token/0]).

-type seconds() :: integer().
-type counter() :: non_neg_integer().
%% Base64 encoded token
-type token() :: binary().
-type agent_id() :: binary().
-type mechanism() :: binary().

-type validity_type() :: days | hours | minutes | seconds.
-type period() :: #{value := non_neg_integer(),
                    unit := days | hours | minutes | seconds}.
-type token_type() :: access | rotate_before_expire.
-type token_slot() :: new | current.
-type add_reason() :: requested | auto_rotate.

-define(REQ, mod_fast_auth_token_request).
-define(FAST, mod_fast_auth_token_fast).

-export_type([tokens_data/0, seconds/0, counter/0, token/0, agent_id/0,
              mechanism/0, token_slot/0, set_current/0]).

-type tokens_data() :: #{
        now_timestamp := seconds(),
        current_token := token() | undefined,
        current_expire := seconds() | undefined,
        current_count := counter() | undefined,
        current_mech := mechanism() | undefined,
        new_token := token() | undefined,
        new_expire := seconds() | undefined,
        new_count := counter() | undefined,
        new_mech := mechanism() | undefined
    }.

-type set_current() :: #{
        current_token := token(),
        current_expire := seconds(),
        current_count := counter(),
        current_mech := mechanism()
    }.

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_fast_auth_token_backend:init(HostType, Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{sasl2_stream_features, HostType, fun ?MODULE:sasl2_stream_features/3, #{}, 50},
     {sasl2_start, HostType, fun ?MODULE:sasl2_start/3, #{}, 50},
     {sasl2_success, HostType, fun ?MODULE:sasl2_success/3, #{}, 50},
     {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"backend">> => #option{type = atom,
                                          validate = {module, ?MODULE}},
                 <<"validity_period">> => validity_periods_spec()},
       defaults = #{<<"backend">> => rdbms}
    }.

validity_periods_spec() ->
    #section{
       items = #{<<"access">> => validity_period_spec(),
                 <<"rotate_before_expire">> => validity_period_spec()},
       defaults = #{<<"access">> => #{value => 3, unit => days},
                    <<"rotate_before_expire">> => #{value => 6, unit => hours}},
       include = always
      }.

validity_period_spec() ->
    #section{
       items = #{<<"value">> => #option{type = integer,
                                        validate = non_negative},
                 <<"unit">> => #option{type = atom,
                                       validate = {enum, [days, hours, minutes, seconds]}}
                },
       required = all
      }.

supported_features() -> [dynamic_domains].

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, #{host_type := HostType}) ->
    mod_fast_auth_token_backend:remove_user(HostType, LUser, LServer),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_domain_api:remove_domain_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    mod_fast_auth_token_backend:remove_domain(HostType, Domain),
    {ok, Acc}.

-spec sasl2_stream_features(Acc, #{c2s_data := mongoose_c2s:data()}, gen_hook:extra()) ->
    {ok, Acc} when Acc :: [exml:element()].
sasl2_stream_features(Acc, _, _) ->
    {ok, [fast() | Acc]}.

fast() ->
    #xmlel{name = <<"fast">>,
           attrs = [{<<"xmlns">>, ?NS_FAST}],
           children = mechanisms_elems(mechanisms())}.

mechanisms_elems(Mechs) ->
    [#xmlel{name = <<"mechanism">>,
           children = [#xmlcdata{content = Mech}]} || Mech <- Mechs].

mechanisms() ->
    %% Mechanisms described in
    %% https://www.ietf.org/archive/id/draft-schmaus-kitten-sasl-ht-09.html
    [% <<"HT-SHA-256-ENDP">>,
     % <<"HT-SHA-256-EXPR">>,
     %% Channel binding: none
     <<"HT-SHA-256-NONE">>].

-spec sasl2_start(SaslAcc, #{stanza := exml:element()}, gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_start(SaslAcc, #{stanza := El}, _) ->
    %% TODO remove this log
    ?LOG_ERROR(#{what => sasl2_startttt, elleee => El, sasla_acc => SaslAcc}),
    Req = exml_query:path(El, [{element_with_ns, <<"request-token">>, ?NS_FAST}]),
    Fast = exml_query:path(El, [{element_with_ns, <<"fast">>, ?NS_FAST}]),
    AgentId = exml_query:path(El, [{element, <<"user-agent">>}, {attr, <<"id">>}]),
    SaslAcc2 = mongoose_acc:set(?MODULE, agent_id, AgentId, SaslAcc),
    SaslAcc3 = maybe_put_inline_request(SaslAcc2, ?REQ, Req),
    {ok, maybe_put_inline_request(SaslAcc3, ?FAST, Fast)}.

maybe_put_inline_request(SaslAcc, _Module, undefined) ->
    SaslAcc;
maybe_put_inline_request(SaslAcc, Module, Request) ->
    mod_sasl2:put_inline_request(SaslAcc, Module, Request).

format_term(X) -> iolist_to_binary(io_lib:format("~0p", [X])).

-spec sasl2_success(SaslAcc, mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_success(SaslAcc, C2SStateData = #{creds := Creds}, #{host_type := HostType}) ->
    %% TODO remove this log
    ?LOG_ERROR(#{what => sasl2_success_debug, sasl_acc => format_term(SaslAcc), c2s_state_data => format_term(C2SStateData),
                 creds => format_term(Creds)}),
    #{c2s_data := C2SData} = C2SStateData,
    #jid{luser = LUser, lserver = LServer} = mongoose_c2s:get_jid(C2SData),
    case check_if_should_add_token(HostType, SaslAcc, Creds) of
        skip ->
            {ok, SaslAcc};
        invalidate ->
            AgentId = mongoose_acc:get(?MODULE, agent_id, undefined, SaslAcc),
            invalidate_token(HostType, LServer, LUser, AgentId),
            {ok, SaslAcc};
        {ok, Mech, AddReason} ->
            AgentId = mongoose_acc:get(?MODULE, agent_id, undefined, SaslAcc),
            %% Attach Token to the response to be used to authentificate
            Response = make_fast_token_response(HostType, LServer, LUser, Mech, AgentId, Creds),
            SaslAcc2 = maybe_init_inline_request(AddReason, SaslAcc),
            SaslAcc3 = mod_sasl2:update_inline_request(SaslAcc2, ?REQ, Response, success),
            {ok, SaslAcc3}
    end.

-spec check_if_should_add_token(HostType :: mongooseim:host_type(),
                                SaslAcc :: mongoose_acc:t(),
                                Creds :: mongoose_credentials:t()) ->
    skip | invalidate | {ok, mechanism(), Reason :: add_reason()}.
check_if_should_add_token(HostType, SaslAcc, Creds) ->
    Parsed = parse_inline_requests(SaslAcc),
    case Parsed of
        #{invalidate := true} ->
            invalidate;
        #{mech := Mech} ->
            case lists:member(Mech, mechanisms()) of
                true ->
                    {ok, Mech, requested};
                false ->
                    skip
            end;
        #{} ->
            maybe_auto_rotate(HostType, Creds)
    end.

-spec parse_inline_requests(SaslAcc :: mongoose_acc:t()) -> map().
parse_inline_requests(SaslAcc) ->
    Req = mod_sasl2:get_inline_request(SaslAcc, ?REQ, undefined),
    Fast = mod_sasl2:get_inline_request(SaslAcc, ?FAST, undefined),
    map_skip_undefined(maps:merge(parse_request(Req), parse_fast(Fast))).

-spec map_skip_undefined(map()) -> map().
map_skip_undefined(Map) ->
    maps:filter(fun(_, Val) -> Val =/= undefined end, Map).

parse_request(#{request := Req = #xmlel{name = <<"request-token">>}}) ->
    Mech = exml_query:attr(Req, <<"mechanism">>),
    #{mech => Mech};
parse_request(undefined) ->
    #{}.

parse_fast(#{request := Fast = #xmlel{name = <<"fast">>}}) ->
    Inv = is_true(exml_query:attr(Fast, <<"invalidate">>)),
    Count = exml_query:attr(Fast, <<"count">>),
    #{invalidate => Inv, count => maybe_parse_integer(Count)};
parse_fast(undefined) ->
    #{}.

is_true(<<"true">>) -> true;
is_true(<<"1">>) -> true;
is_true(_) -> false.

maybe_parse_integer(X) when is_binary(X) ->
    binary_to_integer(X);
maybe_parse_integer(undefined) ->
    undefined.

-spec maybe_auto_rotate(HostType :: mongooseim:host_type(),
                        Creds :: mongoose_credentials:t()) ->
    skip | {ok, mechanism(), Reason :: add_reason()}.
maybe_auto_rotate(HostType, Creds) ->
    %% Creds could contain data from mod_fast_auth_token_generic
    SlotUsed = mongoose_credentials:get(Creds, fast_token_slot_used, undefined),
    DataUsed = mongoose_credentials:get(Creds, fast_token_data, undefined),
    ?LOG_ERROR(#{what => maybe_auto_rotate, slot => SlotUsed, data_used => format_term(DataUsed)}),
    case user_used_token_to_login(SlotUsed) of
        true ->
            case is_used_token_about_to_expire(HostType, SlotUsed, DataUsed) of
                true ->
?LOG_ERROR(#{what => rotate_rotate}),
                    {ok, data_used_to_mech_type(SlotUsed, DataUsed), auto_rotate};
                false ->
                    skip
            end;
        false ->
            skip
    end.

-spec is_used_token_about_to_expire(HostType :: mongooseim:host_type(),
                                    SlotUsed :: token_slot(),
                                    DataUsed :: tokens_data()) -> boolean().
is_used_token_about_to_expire(HostType, SlotUsed, DataUsed) ->
    is_timestamp_about_to_expire(HostType,
                                 slot_to_expire_timestamp(SlotUsed, DataUsed)).

-spec is_timestamp_about_to_expire(HostType :: mongooseim:host_type(),
                                   Timestamp :: seconds()) -> boolean().
is_timestamp_about_to_expire(HostType, Timestamp) ->
    Now = utc_now_as_seconds(),
    TimeBeforeRotate = get_time_to_rotate_before_expire_seconds(HostType),
    SecondsBeforeExpire = Timestamp - Now,
?LOG_ERROR(#{what => is_timestamp_about_to_expire, seconds_before => SecondsBeforeExpire, befor_rot => TimeBeforeRotate}),
    SecondsBeforeExpire =< TimeBeforeRotate.

-spec user_used_token_to_login(token_slot() | undefined) -> boolean().
user_used_token_to_login(SlotUsed) ->
    undefined =/= SlotUsed.

-spec slot_to_expire_timestamp(Slot :: token_slot(), Data :: tokens_data()) -> seconds().
slot_to_expire_timestamp(new, #{new_expire := Timestamp}) ->
    Timestamp;
slot_to_expire_timestamp(current, #{current_expire := Timestamp}) ->
    Timestamp.

-spec data_used_to_mech_type(SlotUsed :: token_slot(),
                             DataUsed :: tokens_data()) -> Mech :: mechanism().
data_used_to_mech_type(new, #{new_mech := Mech}) ->
    Mech;
data_used_to_mech_type(current, #{current_mech := Mech}) ->
    Mech.

-spec maybe_init_inline_request(AddReason, SaslAcc) -> SaslAcc
    when AddReason :: add_reason(),
         SaslAcc :: mongoose_acc:t().
maybe_init_inline_request(requested, SaslAcc) ->
    SaslAcc;
maybe_init_inline_request(auto_rotate, SaslAcc) ->
    %% Add something, so update_inline_request would actually attach data
    mod_sasl2:put_inline_request(SaslAcc, ?REQ, #xmlel{name = <<"auto">>}).

%% Generate expirable auth token and store it in DB
-spec make_fast_token_response(HostType, LServer, LUser, Mech, AgentId, Creds) -> exml:element()
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: agent_id(),
        Mech :: mechanism(),
        Creds :: mongoose_credentials:t().
make_fast_token_response(HostType, LServer, LUser, Mech, AgentId, Creds) ->
    TTLSeconds = get_ttl_seconds(HostType),
    NowTS = ?MODULE:utc_now_as_seconds(),
    ExpireTS = NowTS + TTLSeconds,
    Expire = seconds_to_binary(ExpireTS),
    Token = ?MODULE:generate_unique_token(),
    SetCurrent = maybe_set_current_slot(Creds),
    store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent),
    #xmlel{name = <<"token">>,
           attrs = [{<<"xmlns">>, ?NS_FAST}, {<<"expire">>, Expire},
                    {<<"token">>, Token}]}.

-spec maybe_set_current_slot(Creds :: mongoose_credentials:t()) ->
    SetCurrent :: set_current().
maybe_set_current_slot(Creds) ->
    %% Creds could contain data from mod_fast_auth_token_generic
    SlotUsed = mongoose_credentials:get(Creds, fast_token_slot_used, undefined),
    DataUsed = mongoose_credentials:get(Creds, fast_token_data, undefined),
    case SlotUsed of
        new ->
            token_data_to_set_current(DataUsed);
        _ ->
            false
    end.

-spec token_data_to_set_current(DataUsed :: tokens_data()) -> set_current().
token_data_to_set_current(#{
        new_token := Token,
        new_expire := Expire,
        new_count := Counter,
        new_mech := Mech}) ->
    #{current_token => Token,
      current_expire => Expire,
      current_count => Counter,
      current_mech => Mech}.

-spec seconds_to_binary(seconds()) -> binary().
seconds_to_binary(Secs) ->
    Opts = [{offset, "Z"}, {unit, second}],
    list_to_binary(calendar:system_time_to_rfc3339(Secs, Opts)).

-spec utc_now_as_seconds() -> seconds().
utc_now_as_seconds() ->
    erlang:system_time(second).

-spec get_ttl_seconds(mongooseim:host_type()) -> seconds().
get_ttl_seconds(HostType) ->
    #{value := Value, unit := Unit} = get_validity_period(HostType, access),
    period_to_seconds(Value, Unit).

-spec get_time_to_rotate_before_expire_seconds(mongooseim:host_type()) -> seconds().
get_time_to_rotate_before_expire_seconds(HostType) ->
    #{value := Value, unit := Unit} = get_validity_period(HostType, rotate_before_expire),
    period_to_seconds(Value, Unit).

-spec get_validity_period(mongooseim:host_type(), token_type()) -> period().
get_validity_period(HostType, Type) ->
    gen_mod:get_module_opt(HostType, ?MODULE, [validity_period, Type]).

-spec period_to_seconds(non_neg_integer(), validity_type()) -> seconds().
period_to_seconds(Days, days) -> 24 * 3600 * Days;
period_to_seconds(Hours, hours) -> 3600 * Hours;
period_to_seconds(Minutes, minutes) -> 60 * Minutes;
period_to_seconds(Seconds, seconds) -> Seconds.

-spec generate_unique_token() -> token().
generate_unique_token() ->
    base64:encode(crypto:strong_rand_bytes(25)).

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS,
                      Token, Mech, SetCurrent) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: agent_id(),
        ExpireTS :: seconds(),
        Token :: token(),
        Mech :: mechanism(),
        SetCurrent :: set_current() | false.
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent) ->
    mod_fast_auth_token_backend:store_new_token(
        HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent).

-spec invalidate_token(HostType, LServer, LUser, AgentId) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: agent_id().
invalidate_token(HostType, LServer, LUser, AgentId) ->
    mod_fast_auth_token_backend:invalidate_token(HostType, LServer, LUser, AgentId),
    ok.

-spec read_tokens(HostType, LServer, LUser, AgentId) ->
   {ok, tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: agent_id().
read_tokens(HostType, LServer, LUser, AgentId) ->
    mod_fast_auth_token_backend:read_tokens(HostType, LServer, LUser, AgentId).
