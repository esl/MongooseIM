-module(cyrsasl_scram_sha1).

-export([mechanism/0, mech_new/3, mech_step/2]).

-include("mongoose.hrl").

-include("jlib.hrl").

-behaviour(cyrsasl).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"SCRAM-SHA-1">>.

mech_new(Host, Creds, _Socket) ->
    cyrsasl_scram:mech_new(Host, Creds, sha).

mech_step(State, ClientIn) ->
    cyrsasl_scram:mech_step(State, ClientIn).
