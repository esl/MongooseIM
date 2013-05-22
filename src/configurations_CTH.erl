-module(configurations_CTH).

-export([init/2,
         pre_init_per_suite/3]).

-record(state, {current}).

init(_Id, [Configs, Node, Interval]) ->
    N = ets:update_counter(configurations_CTH, count, 1),
    D = N/Interval,
    Trunc = erlang:trunc(D),
    Ceil = case D-Trunc of
        0   -> Trunc;
        0.0 -> Trunc;
        _ -> Trunc+1
    end,
    {Current, Variables} = lists:nth(Ceil, Configs),

    {ok, Cwd} = call(Node, file, get_cwd, []),
    Cfg = filename:join([Cwd, "..", "..", "rel", "files", "ejabberd.cfg"]),
    Vars = filename:join([Cwd, "..", "..", "rel", "reltool_vars", "node1_vars.config"]),
    CfgFile = filename:join([Cwd, "etc", "ejabberd.cfg"]),
    {ok, Template} = call(Node, file, read_file, [Cfg]),
    {ok, Default} = call(Node, file, consult, [Vars]),
    NewVars = lists:foldl(fun({Var,Val}, Acc) ->
                    lists:keystore(Var, 1, Acc, {Var,Val})
            end, Default, Variables), 
    LTemplate = binary_to_list(Template),
    NewCfgFile = mustache:render(LTemplate, dict:from_list(NewVars)),
    ok = call(Node, file, write_file, [CfgFile, NewCfgFile]),
    call(Node, application, stop, [ejabberd]),
    call(Node, application, start, [ejabberd]),
    ct:print("Configuration ~p test started.~n", [Current]),
    {ok, #state{current=Current}}.

pre_init_per_suite(_Suite, Config, #state{current=Current}=State) ->
    {add_current(Current, Config), State}.

add_current(Current, Config) ->
    lists:keystore(current_config, 1, Config, {current_config, Current}).

call(Node, M, F, A) ->
    rpc:call(Node, M, F, A).
