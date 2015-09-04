-module(mod_keystore).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

start(Host, _Opts) ->
    ok.

stop(Host) ->
    ok.
