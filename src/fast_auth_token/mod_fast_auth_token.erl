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
-type token_type() :: access.

-export_type([tokens_data/0, seconds/0, counter/0, token/0, agent_id/0,
              mechanism/0]).

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
       items = #{<<"access">> => validity_period_spec()},
       defaults = #{<<"access">> => #{value => 3, unit => days}},
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
    AgentId = exml_query:path(El, [{element, <<"user-agent">>}, {attr, <<"id">>}]),
    SaslAcc2 = mongoose_acc:set(?MODULE, agent_id, AgentId, SaslAcc),
    case exml_query:path(El, [{element_with_ns, <<"request-token">>, ?NS_FAST}]) of
        undefined ->
            {ok, SaslAcc2};
        Request ->
            Mech = exml_query:attr(Request, <<"mechanism">>),
            case Mech of
                <<"HT-SHA-256-NONE">> ->
                    {ok, mod_sasl2:put_inline_request(SaslAcc2, ?MODULE, Request)};
                _ ->
                     {ok, SaslAcc2}
            end
    end.

format_term(X) -> iolist_to_binary(io_lib:format("~0p", [X])).

-spec sasl2_success(SaslAcc, mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_success(SaslAcc, C2SStateData = #{creds := Creds}, #{host_type := HostType}) ->
    %% TODO remove this log
    ?LOG_ERROR(#{what => sasl2_success_debug, sasl_acc => format_term(SaslAcc), c2s_state_data => format_term(C2SStateData),
                 creds => format_term(Creds)}),
    #{c2s_data := C2SData} = C2SStateData,
    #jid{luser = LUser, lserver = LServer} = mongoose_c2s:get_jid(C2SData),
    case mod_sasl2:get_inline_request(SaslAcc, ?MODULE, undefined) of
        undefined ->
            {ok, SaslAcc};
        #{request := Request} ->
            AgentId = mongoose_acc:get(?MODULE, agent_id, undefined, SaslAcc),
            %% Attach Token to the response to be used to authentificate
            Response = make_fast_token_response(HostType, LServer, LUser, Request, AgentId),
            SaslAcc2 = mod_sasl2:update_inline_request(SaslAcc, ?MODULE, Response, success),
            {ok, SaslAcc2}
    end.

%% Generate expirable auth token and store it in DB
make_fast_token_response(HostType, LServer, LUser, Request, AgentId) ->
    Mech = exml_query:attr(Request, <<"mechanism">>),
    TTLSeconds = get_ttl_seconds(HostType),
    NowTS = ?MODULE:utc_now_as_seconds(),
    ExpireTS = NowTS + TTLSeconds,
    Expire = seconds_to_binary(ExpireTS),
    Token = ?MODULE:generate_unique_token(),
    store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech),
    #xmlel{name = <<"token">>,
           attrs = [{<<"xmlns">>, ?NS_FAST}, {<<"expire">>, Expire},
                    {<<"token">>, Token}]}.

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

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: agent_id(),
        ExpireTS :: seconds(),
        Token :: token(),
        Mech :: mechanism().
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech) ->
    mod_fast_auth_token_backend:store_new_token(
        HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech).

-spec read_tokens(HostType, LServer, LUser, AgentId) ->
   {ok, tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: agent_id().
read_tokens(HostType, LServer, LUser, AgentId) ->
    mod_fast_auth_token_backend:read_tokens(HostType, LServer, LUser, AgentId).
