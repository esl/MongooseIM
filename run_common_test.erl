-module(run_common_test).
-export([ct/0, ct_cover/0, cover_summary/0]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).
-define(CT_CONFIG, "test.config").
-define(EJABBERD_NODE, 'ejabberd@localhost').

ct() ->
    Result = ct:run_test([{config, [?CT_CONFIG]},
                          {dir, ?CT_DIR},
                          {logdir, ?CT_REPORT}]),
    case Result of
        {error, Reason} ->
            throw({ct_error, Reason});
        _ ->
            ok
    end,
    init:stop(0).

ct_cover() ->
    prepare(),
    ct:run_test([
        {config, [?CT_CONFIG]},
        {dir, ?CT_DIR},
        {logdir, ?CT_REPORT}
    ]),
    analyze(),
    {MS,S,_} = now(),
    FileName = lists:flatten(io_lib:format("run_~b~b.coverdata",[MS,S])),
    io:format("export current cover ~p~n", [cover_call(export, [FileName])]),
    io:format("test finished~n"),

    init:stop(0).

cover_summary() ->
    prepare(),
    Files = rpc:call(?EJABBERD_NODE, filelib, wildcard, ["*.coverdata"]),
    lists:foreach(fun(F) ->
                          io:format("import ~p cover ~p~n", [F, cover_call(import, [F])])
                  end,
                  Files),
    analyze(),
    io:format("summary completed~n"),
    init:stop(0).

prepare() ->
    %%rpc:call(?EJABBERD_NODE, application, stop, [ejabberd]),
    cover_call(start),
    Compiled = cover_call(compile_beam_directory,["lib/ejabberd-2.1.8/ebin"]),
    %%io:format("start ~p~n", [rpc:call(?EJABBERD_NODE, application, start, [ejabberd, permanent])]),
    io:format("Compiled modules ~p~n", [Compiled]).
    %%timer:sleep(10000).

analyze() ->
    Modules = cover_call(modules),
    rpc:call(?EJABBERD_NODE, file, make_dir, ["coverage"]),
    {ok, File} = file:open("cover_summary.txt", write),
    Fun = fun(Module) ->
                  FileName = lists:flatten(io_lib:format("~s.COVER.html",[Module])),
                  FilePath = filename:join(["coverage", FileName]),
                  cover_call(analyse_to_file, [Module, FilePath, [html]]),
                  {ok, {Module, {C, NC}}} = cover_call(analyse, [Module, module]),
                  io:fwrite(File, "~s;~b;~b;~s\n", [Module, C, NC, FilePath])
          end,
    io:format("coverage analyzing~n"),
    lists:foreach(Fun, Modules),
    file:close(File).

cover_call(Function) ->
    cover_call(Function, []).
cover_call(Function, Args) ->
    rpc:call(?EJABBERD_NODE, cover, Function, Args).
