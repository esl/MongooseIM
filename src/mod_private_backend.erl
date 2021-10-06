-module(mod_private_backend).
-export([init/2,
         multi_set_data/4,
         multi_get_data/4,
         get_all_nss/3,
         remove_user/3,
         remove_domain/2]).

-author('arcusfelis@gmail.com').
-behaviour(mod_private).

-define(MAIN_MODULE, mod_private).

init(HostType, Opts) ->
    backend_module:ensure_backend_metrics(?MAIN_MODULE, tracked_funs()),
    Args = [HostType, Opts],
    backend_module:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

tracked_funs() ->
    [multi_get_data, multi_set_data].

multi_set_data(HostType, LUser, LServer, NS2XML) ->
    Args = [HostType, LUser, LServer, NS2XML],
    backend_module:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

multi_get_data(HostType, LUser, LServer, NS2Def) ->
    Args = [HostType, LUser, LServer, NS2Def],
    backend_module:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_all_nss(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    backend_module:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    backend_module:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    backend_module:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
