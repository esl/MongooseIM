-module(cyrsasl_scram_sha512).

-export([mechanism/0, mech_new/3, mech_step/2]).

-behaviour(cyrsasl).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"SCRAM-SHA-512">>.

mech_new(Host, Creds, #{} = SocketData) ->
    cyrsasl_scram:mech_new(Host, Creds, SocketData#{sha => sha512, scram_plus => false}).

mech_step(State, ClientIn) ->
    cyrsasl_scram:mech_step(State, ClientIn).
