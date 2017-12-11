-module(mod_push).

-behaviour(gen_mod).

-export([deps/2, start/2, stop/1]).

deps(_Host, Opts) ->
    [{mod_event_pusher, [{backends, [{push, Opts}]}], hard}].

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.
