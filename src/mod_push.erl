-module(mod_push).

-include("mongoose.hrl").

-behaviour(gen_mod).

-export([deps/2, start/2, stop/1]).

deps(_Host, Opts) ->
    [{mod_event_pusher, [{backends, [{push, Opts}]}], hard}].

start(_Host, _Opts) ->
    ?WARNING_MSG("mod_push is deprecated and will be removed in the future.~n"
                 "Please use mod_event_pusher with push backend.~n"
                 "Refer to mod_event_pusher documentation for more information.", []).

stop(_Host) ->
    ok.
