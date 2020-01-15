-module(mod_http_notification).

-include("mongoose.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([deps/2, start/2, stop/1]).

deps(_Host, Opts) ->
    [{mod_event_pusher, [{backends, [{http, Opts}]}], hard}].

start(_Host, _Opts) ->
    ?WARNING_MSG("mod_http_notification is deprecated and will be removed in the future.~n"
                 "Please use mod_event_pusher with http backend.~n"
                 "Refer to mod_event_pusher documentation for more information.", []).

stop(_Host) ->
    ok.
