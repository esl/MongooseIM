-module(mod_keystore).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([get_key/2]).

start(Host, _Opts) ->
    [ ejabberd_hooks:add(Hook, Host, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    ok.

stop(Host) ->
    [ ejabberd_hooks:delete(Hook, Host, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    ok.

hook_handlers() ->
    [
     {get_key, get_key, 50}
    ].

get_key(HandlerAcc, KeyID) ->
    123.
