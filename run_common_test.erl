-module(run_common_test).
-export([ct/0, ct_cover/0]).

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
    cover_call(start),
    cover_call(compile_beam_directory,["lib/ejabberd-2.1.8/ebin"]),
    %% io:format("Compiled modules ~p~n", [Compiled]),
    ct:run_test([
        {config, [?CT_CONFIG]},
        {dir, ?CT_DIR},
        {logdir, ?CT_REPORT},
    ]),
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
    io:format("coverage analyzing~n", Modules),
    lists:foreach(Fun, Modules),
    file:close(File),
    io:format("test finished~n"),
    init:stop(0).

cover_call(Function) ->
    cover_call(Function, []).
cover_call(Function, Args) ->
    rpc:call(?EJABBERD_NODE, cover, Function, Args).
