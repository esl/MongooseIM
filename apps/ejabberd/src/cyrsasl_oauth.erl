-module(cyrsasl_oauth).
-author('adrian.stachurski@erlang-solutions.com').

-export([start/1, stop/0, mech_new/4, mech_step/2]).

-behaviour(cyrsasl).

-record(state, {check_password}).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"X-OAUTH">>, ?MODULE, plain),
    ok.

stop() ->
    ok.

-spec mech_new(Host :: ejabberd:server(),
               GetPassword :: cyrsasl:get_password_fun(),
               CheckPassword :: cyrsasl:check_password_fun(),
               CheckPasswordDigest :: cyrsasl:check_pass_digest_fun()
               ) -> {ok, tuple()}.
mech_new(Host, GetPassword, CheckPassword, CheckPasswordDigest) ->
    {ok, #state{check_password = CheckPassword}}.

-spec mech_step(State :: tuple(),
                ClientIn :: binary()
                ) -> {ok, proplists:proplist()} | {error, binary()}.
mech_step(State, ClientIn) ->
    %% ClientIn is a token decoded from CDATA <auth body sent by client
    case mod_auth_token:validate_token(ClientIn) of
        % Validating access token
        {ok, AuthModule, User} ->
            {ok,[{username, User},
                 {auth_module, AuthModule}]};
        % Validating refresh token and returning new tokens
        {ok, AuthModule, User, Tokens} ->
            {ok,[{username, User},
                 {auth_module, AuthModule},
                 {tokens, Tokens}]};
        _ ->
            {error, <<"not-authorized">>}
    end.
