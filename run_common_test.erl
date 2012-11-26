-module(run_common_test).
-export([ct/0, ct_cover/0, cover_summary/0]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).

ct_config_file() ->
    {ok, CWD} = file:get_cwd(),
    filename:join([CWD, "test.config"]).

tests_to_run() ->
    [{config, ct_config_file()},
     {dir, ?CT_DIR},
     {logdir, ?CT_REPORT}].
tests_to_run(none) ->
    tests_to_run();
tests_to_run(Node) ->
    [{dir, Node}] ++ tests_to_run().

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
    run_ct_covers(get_tested_nodes()),
    init:stop(0).

run_ct_covers([]) -> 
    %%There is no tested nodes configuration. Assume that ejabberd node is running
    run_ct_cover1();
run_ct_covers([Node | []]) ->
    run_ct_cover(Node),
    run_ejabberd(Node),
    cover_summary(),
    stop_ejabberd(Node);
run_ct_covers([Node | Tail]) ->
    run_ct_cover(Node),
    run_ct_covers(Tail).

run_ct_cover(Node) ->
    io:format("will test node ~p~n", [Node]),
    StartStatus = run_ejabberd(Node),
    io:format("start status ~p~n", [StartStatus]),
    NodeStr = get_node_str(Node),
    os:cmd("cp -R tests "++NodeStr),
    run_ct_cover1(NodeStr),
    StopStatus = stop_ejabberd(Node),
    os:cmd("rm -rf "++NodeStr),
    io:format("stop status ~p~n", [StopStatus]).

run_ct_cover1() ->
    run_ct_cover1(none).

run_ct_cover1(Node) ->
    prepare(),
    ct:run_test(tests_to_run(Node)),
    analyze(Node),
    {MS,S,_} = now(),
    FileName = lists:flatten(io_lib:format("/tmp/ejd_test_run_~b~b.coverdata",[MS,S])),
    io:format("export current cover ~p~n", [cover_call(export, [FileName])]),
    io:format("test finished~n").

cover_summary() ->
    prepare(),
    Files = rpc:call(get_ejabberd_node(), filelib, wildcard, ["/tmp/ejd_test_run_*.coverdata"]),
    lists:foreach(fun(F) ->
                          io:format("import ~p cover ~p~n", [F, cover_call(import, [F])])
                  end,
                  Files),
    analyze(summary),
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

analyze(Node) ->
    Modules = cover_call(modules),
    io:format("node ~s~n", [Node]),
    FilePath = case {Node, file:read_file(?CT_REPORT++"/index.html")} of
        {summary, {ok, IndexFileData}} ->
            R = re:replace(IndexFileData, "<a href=\"all_runs.html\">ALL RUNS</a>", "& <a href=\"cover.html\" style=\"margin-right:5px\">COVER</a>"),
            file:write_file(?CT_REPORT++"/index.html", R),
            ?CT_REPORT++"/cover.html";
        {_, {ok, IndexFileData}} ->
            {match, [PosLen]} = re:run(IndexFileData, "<a href=\"(.*"++Node++".*)\">",[{capture, [1]}]),
            filename:dirname(?CT_REPORT++"/"++binary_to_list(binary:part(IndexFileData, PosLen)))++"/cover.html";
        _ -> skip
    end,
    CoverageDir = filename:dirname(FilePath)++"/coverage",
    rpc:call(get_ejabberd_node(), file, make_dir, ["/tmp/coverage"]),
    {ok, File} = file:open(FilePath, [write]),
    file:write(File, "<html>\n<head></head>\n<body bgcolor=\"white\" text=\"black\" link=\"blue\" vlink=\"purple\" alink=\"red\">\n"),
    file:write(File, "<h1>Coverage for application 'esl-ejabberd'</h1>\n"),
    file:write(File, "<table border=3 cellpadding=5>\n"),
    file:write(File, "<tr><th>Module</th><th>Covered (%)</th><th>Covered (Lines)</th><th>Not covered (Lines)</th><th>Total (Lines)</th></tr>"),
    Fun = fun(Module, {CAcc, NCAcc}) ->
                  FileName = lists:flatten(io_lib:format("~s.COVER.html",[Module])),
                  FilePathC = filename:join(["/tmp/coverage", FileName]),
                  cover_call(analyse_to_file, [Module, FilePathC, [html]]),
                  {ok, {Module, {C, NC}}} = cover_call(analyse, [Module, module]),
                  file:write(File, row(atom_to_list(Module), C, NC, percent(C,NC),"coverage/"++FileName)),
                  {CAcc + C, NCAcc + NC}
          end,
    io:format("coverage analyzing~n"),
    {CSum, NCSum} = lists:foldl(Fun, {0, 0}, Modules),
    os:cmd("cp -R /tmp/coverage "++CoverageDir),
    file:write(File, row("Summary", CSum, NCSum, percent(CSum, NCSum), "#")),
    file:close(File).

cover_call(Function) ->
    cover_call(Function, []).
cover_call(Function, Args) ->
    rpc:call(get_ejabberd_node(), cover, Function, Args).

get_ejabberd_node() ->
    {ok, Props} = file:consult(ct_config_file()),
    {ejabberd_node, Node} = proplists:lookup(ejabberd_node, Props),
    Node.

get_tested_nodes() ->
    {ok, Props} = file:consult(ct_config_file()),
    case proplists:lookup(ejabberd_nodes, Props) of
        none -> [];
        {ejabberd_nodes, Nodes} -> Nodes
    end.

percent(0, _) -> 0;
percent(C, NC) when C /= 0; NC /= 0 -> round(C / (NC+C) * 100);
percent(_, _)                       -> 100.

row(Row, C, NC, Percent, Path) ->
    [
        "<tr>",
        "<td><a href='", Path, "'>", Row, "</a></td>",
        "<td>", integer_to_list(Percent), "%</td>",
        "<td>", integer_to_list(C), "</td>",
        "<td>", integer_to_list(NC), "</td>",
        "<td>", integer_to_list(C+NC), "</td>",
        "</tr>\n"
    ].

get_node_str({Node, _, _}) ->
    get_node_str(Node);
get_node_str(Node) -> atom_to_list(Node).

run_ejabberd({_Node, StartCmd, _}) ->
    do_start_cmd(StartCmd);
run_ejabberd(Node) ->
    do_start_cmd(node_cmd(Node, "start")).

do_start_cmd(StartCmd) ->
    Status = os:cmd(StartCmd),
    timer:sleep(3000),
    Status.

stop_ejabberd({_Node, _, StopCmd}) ->
    os:cmd(StopCmd);
stop_ejabberd(Node) ->
    os:cmd(node_cmd(Node, "stop")).

node_cmd(Node, Cmd) ->
    lists:flatten(io_lib:format("../../dev/ejabberd_~p/bin/ejabberd ~s",
                                [Node, Cmd])).
