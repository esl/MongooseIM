-module(cyrsasl_oauth).
-author('adrian.stachurski@erlang-solutions.com').

-export([start/1, stop/0, mech_new/4, mech_step/2, parse/1]).

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
    %% io:format("~n from mech_new ~n. Host:~p~nGetPassword:~p~nCheckPassword:~p~n:CheckPasswordDigest:~p~n",[Host,
    %%                                                                                                        GetPassword,
    %%                                                                                                        CheckPassword,
    %%                                                                                                        CheckPasswordDigest])
    {ok, #state{check_password = CheckPassword}}.

-spec mech_step(State :: tuple(),
                ClientIn :: binary()
                ) -> {ok, proplists:proplist()} | {error, binary()}.
mech_step(State, ClientIn) ->
    AuthzId = mock,
    case prepare(ClientIn) of
        [User, Token] ->
            case mod_auth_token:validate_access_token(Token) of
                {true, AuthModule} -> {ok, [{username, User}, {authzid, AuthzId},
                          {auth_module, AuthModule}]};
                _ ->
                    {error, <<"not-authorized">>, User}
            end;
        _ ->
            {error, <<"bad-protocol">>}
    end.

-spec prepare(binary()) -> 'error' | [binary(),...].
prepare(ClientIn) ->
    io:format( " ~n ---- decoded ---- ~p ~n : ", [ClientIn]),
    parse(ClientIn).

-spec parse(binary()) -> [binary(),...].
parse(S) ->
    io:format("~n parsing: ~p ~n", [S]),
%%    Parsed =  parse(S),
%%    io:format("~n parsed: ~p ~n", [Parsed]),
    [<<"johny">>, S].
