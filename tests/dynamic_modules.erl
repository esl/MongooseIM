-module(dynamic_modules).

-export([stop/2, start/3, restart/3]).


stop(Domain, Mod) ->
    io:format("stopping ~p", [Mod]),
    case escalus_ejabberd:rpc(code, which, [Mod]) of
        non_existing ->
            ok;
        _ ->
            {atomic, ok} = escalus_ejabberd:rpc(gen_mod, stop_module, [Domain, Mod]),
            ok
    end.

start(Domain, Mod, Args) ->
    io:format("starting ~p", [Mod]),
    case escalus_ejabberd:rpc(gen_mod, start_module, [Domain, Mod, Args]) of
        {badrpc, Reason} ->
            ct:fail("Cannot start module ~p reason ~p", [Mod, Reason]);
        _ -> ok
    end.

restart(Domain, Mod, Args) ->
    stop(Domain, Mod),
    ModStr = atom_to_list(Mod),
    case lists:reverse(ModStr) of
        "cbdo_" ++ Str -> %%check if we need to start odbc module or regular
            stop(Domain, list_to_atom(lists:reverse(Str)));
        Str ->
            stop(Domain, list_to_atom(lists:reverse(Str)++"_odbc"))
    end,
    start(Domain, Mod, Args).

