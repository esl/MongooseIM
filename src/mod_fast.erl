-module(mod_fast).
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
         sasl2_success/3]).

-export([read_tokens/4]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{sasl2_stream_features, HostType, fun ?MODULE:sasl2_stream_features/3, #{}, 50},
     {sasl2_start, HostType, fun ?MODULE:sasl2_start/3, #{}, 50},
     {sasl2_success, HostType, fun ?MODULE:sasl2_success/3, #{}, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        defaults = #{}
    }.

supported_features() -> [dynamic_domains].

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

-spec sasl2_success(SaslAcc, mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_success(SaslAcc, C2SStateData, #{host_type := HostType}) ->
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
    TTLSeconds = 100000,
    NowTS = utc_now_as_seconds(),
    ExpireTS = NowTS + TTLSeconds,
    Expire = seconds_to_binary(ExpireTS),
    Token = base64:encode(crypto:strong_rand_bytes(25)),
    store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token),
    #xmlel{name = <<"token">>,
           attrs = [{<<"xmlns">>, ?NS_FAST}, {<<"expire">>, Expire},
                    {<<"token">>, Token}]}.

-spec seconds_to_binary(integer()) -> binary().
seconds_to_binary(Secs) ->
    Opts = [{offset, "Z"}, {unit, second}],
    list_to_binary(calendar:system_time_to_rfc3339(Secs, Opts)).

utc_now_as_seconds() ->
    datetime_to_seconds(calendar:universal_time()).

-spec datetime_to_seconds(calendar:datetime()) -> non_neg_integer().
datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: binary(),
        ExpireTS :: integer(),
        Token :: binary().
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token) ->
    ?LOG_ERROR(#{what => store_new_token, host_type => HostType,
                 lserver => LServer, luser => LUser,
                 expire_ts => ExpireTS, token => Token, agent_id => AgentId}),
    ok.

read_tokens(HostType, LServer, LUser, AgentId) ->
    ?LOG_ERROR(#{what => read_tokens, host_type => HostType,
                 lserver => LServer, luser => LUser, agent_id => AgentId}),
    Data = #{
        current_token => <<"WXZzciBwYmFmdmZnZiBqdmd1IGp2eXFhcmZm">>,
        current_expire => utc_now_as_seconds() + 1000000,
        current_count => 0,
        new_token => <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
        new_expire => utc_now_as_seconds() + 1000000,
        new_count => 0
    },
    {ok, Data}.
