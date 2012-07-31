-module(run_common_test).
-export([ct/0, ct_cover/0, cover_summary/0]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).
-define(CT_CONFIG, "test.config").

tests_to_run() -> [
                   {config, [?CT_CONFIG]},
                   {dir, ?CT_DIR},
                   {logdir, ?CT_REPORT},
                   {suite, muc_SUITE},
                   {group, owner}
                  ].

ct() ->
    Result = ct:run_test(tests_to_run()),
    case Result of
        {error, Reason} ->
            throw({ct_error, Reason});
        _ ->
            ok
    end,
    init:stop(0).

ct_cover() ->
    prepare(),
    ct:run_test(tests_to_run()),
    analyze(),
    {MS,S,_} = now(),
    FileName = lists:flatten(io_lib:format("run_~b~b.coverdata",[MS,S])),
    io:format("export current cover ~p~n", [cover_call(export, [FileName])]),
    io:format("test finished~n"),

    init:stop(0).

cover_summary() ->
    prepare(),
    Files = rpc:call(get_ejabberd_node(), filelib, wildcard, ["*.coverdata"]),
    lists:foreach(fun(F) ->
                          io:format("import ~p cover ~p~n", [F, cover_call(import, [F])])
                  end,
                  Files),
    analyze(),
    io:format("summary completed~n"),
    init:stop(0).

prepare() ->
    cover_call(start),
    Compiled = cover_call(compile_beam_directory,["lib/ejabberd-2.1.8/ebin"]),
    rpc:call(get_ejabberd_node(), application, stop, [ejabberd]),
    StartStatus = rpc:call(get_ejabberd_node(), application, start, [ejabberd, permanent]),
    io:format("start ~p~n", [StartStatus]),
    io:format("Compiled modules ~p~n", [Compiled]).
    %%timer:sleep(10000).

analyze() ->
    Modules = cover_call(modules),
    rpc:call(get_ejabberd_node(), file, make_dir, ["coverage"]),
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
    rpc:call(get_ejabberd_node(), cover, Function, Args).

get_ejabberd_node() ->
    {ok, Props} = file:consult("test.config"),
    {ejabberd_node, Node} = proplists:lookup(ejabberd_node, Props),
    Node.
