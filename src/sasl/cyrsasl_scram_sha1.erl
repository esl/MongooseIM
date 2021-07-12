-module(cyrsasl_scram_sha1).

-export([mechanism/0, mech_new/3, mech_step/2]).

-ignore_xref([mech_new/3]).

-behaviour(cyrsasl).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"SCRAM-SHA-1">>.

mech_new(Host, Creds, #{} = SocketData) ->
    cyrsasl_scram:mech_new(Host, Creds, SocketData#{sha => sha, scram_plus => false}).

mech_step(State, ClientIn) ->
    cyrsasl_scram:mech_step(State, ClientIn).
