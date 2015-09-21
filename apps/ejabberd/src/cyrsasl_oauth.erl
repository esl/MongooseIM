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
mech_new(_Host, _GetPassword, CheckPassword, _CheckPasswordDigest) ->
    {ok, #state{check_password = CheckPassword}}.

-spec mech_step(State :: tuple(),
                ClientIn :: binary()
                ) -> {ok, proplists:proplist()} | {error, binary()}.
mech_step(State, ClientIn) ->
    {error, <<"bad-protocol">>}.

-spec prepare(binary()) -> 'error' | [binary(),...].
prepare(ClientIn) ->
    'error'.

-spec parse(binary()) -> [binary(),...].
parse(S) ->
    <<"error">>.
