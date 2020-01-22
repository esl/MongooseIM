-module(cyrsasl_oauth).
-author('adrian.stachurski@erlang-solutions.com').

-export([mechanism/0, mech_new/2, mech_step/2]).

-behaviour(cyrsasl).

-record(state, {creds}).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"X-OAUTH">>.

-spec mech_new(Host :: jid:server(),
               Creds :: mongoose_credentials:t()) -> {ok, tuple()}.
mech_new(_Host, Creds) ->
    {ok, #state{creds = Creds}}.

-spec mech_step(State :: tuple(),
                ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                       | {error, binary()}.
mech_step(#state{creds = Creds}, SerializedToken) ->
    %% SerializedToken is a token decoded from CDATA <auth/> body sent by client
    case mod_auth_token:authenticate(SerializedToken) of
        % Validating access token
        {ok, AuthModule, User} ->
            {ok, mongoose_credentials:extend(Creds,
                                             [{username, User},
                                              {auth_module, AuthModule}])};
        % Validating refresh token and returning new tokens
        {ok, AuthModule, User, AccessToken} ->
            {ok, mongoose_credentials:extend(Creds,
                                             [{username, User},
                                              {auth_module, AuthModule},
                                              {sasl_success_response, AccessToken}])};
        {error, {Username, _}} ->
            {error, <<"not-authorized">>, Username};
        {error, _Reason} ->
            {error, <<"not-authorized">>}
    end.
