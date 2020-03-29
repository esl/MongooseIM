-module(cyrsasl_scram_sha256).

-export([mechanism/0, mech_new/2, mech_step/2]).

-include("mongoose.hrl").

-include("jlib.hrl").

-behaviour(cyrsasl).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"SCRAM-SHA-256">>.

mech_new(Host, Creds) ->
    cyrsasl_scram:mech_new(Host, Creds, sha256).

mech_step(State, ClientIn) ->
    cyrsasl_scram:mech_step(State, ClientIn).