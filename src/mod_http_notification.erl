-module(mod_http_notification).

-include("mongoose.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([deps/2, start/2, stop/1]).

deps(_Host, Opts) ->
    [{mod_event_pusher, [{backends, [{http, Opts}]}], hard}].

start(_Host, _Opts) ->
    Text = <<"mod_http_notification is deprecated and will be removed in the future.~n"
             "Please use mod_event_pusher with http backend.~n"
             "Refer to mod_event_pusher documentation for more information.">>,
    ?LOG_WARNING(#{what => module_deprecated, text => Text}).

stop(_Host) ->
    ok.
