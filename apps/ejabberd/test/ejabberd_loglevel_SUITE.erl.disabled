-module(ejabberd_loglevel_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(a2b(Atom), list_to_binary(atom_to_list(Atom))).
-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(ne(A, B), ?assertNot(A == B)).

all() ->
    [
     init_test,
     set_get_loglevel,
     set_custom_loglevel,
     log_at_every_level,
     log_at_custom_level
    ].

end_per_testcase(_TestCase, _Config) ->
    application:stop(lager),
    ok.

%%
%% Tests
%%

init_test(_) ->
    ejabberd_loglevel:init().

set_get_loglevel(C) ->
    %% given
    {ok, Backend} = ejabberd_loglevel_running(),
    %% hint: [0, 1, ..., 5, none, critical, ..., debug]
    LevelsToTest = ([ {L, element(1, L)} || L <- levels() ] ++
                    [ {L, element(2, L)} || L <- levels() ]),
    %% when / then
    [ set_get_loglevel(C, Backend, Expected, Level)
      || {Expected, Level} <- LevelsToTest ].

set_get_loglevel(_, Backend, Expected, Level) ->
    %% when
    [{Backend, ok}] = ejabberd_loglevel:set(Level),
    %% then
    [{Backend, Expected}] = ejabberd_loglevel:get().

set_custom_loglevel(_) ->
    %% given
    {ok, Backend} = ejabberd_loglevel_running(),
    ExampleMod = ejabberd_c2s,
    ExampleLvl = info,
    %% when setting a custom log level for some module
    %% the operation succeeds
    [{Backend, ok}] = ejabberd_loglevel:set_custom(ExampleMod, ExampleLvl).

log_at_every_level(C) ->
    %% given
    ejabberd_loglevel_running(),
    [ begin
          %% when
          ejabberd_loglevel:set(L),
          %% then
          log_at_level(C, {L, LName})
      end || {L, LName} <- levels() ].

log_at_level(C, {0, none}) ->
    %% When log level {0, none} is set and we log on each possible level...
    Before = get_log("log/ejabberd.log"),
    [ log(C, LevelName, "", []) || {_, LevelName} <- levels(), LevelName /= none ],
    %% ...then nothing ends up in the log file.
    %% (polling doesn't make sense in this one case... we have to sleep)
    timer:sleep(timer:seconds(2)),
    After = get_log("log/ejabberd.log"),
    ?eq([], After -- Before);
log_at_level(C, {L, _}) ->
    %% When current log level is L and we log on each possible level...
    Before = get_log("log/ejabberd.log"),
    [ log(C, LevelName, "match-this ~s", [LevelName]) || {_, LevelName} <- levels(), LevelName /= none ],
    %% ...then for each sensible level (i.e. less or equal to current level)
    %% we get a line in the log file.
    ExpectedContents = levels_less_than_or_equal_to(L) -- [<<"none">>],
    After = case get_at_least_n_log_lines("log/ejabberd.log",
                                          length(Before) + length(ExpectedContents),
                                          timer:seconds(5)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,
    LinesDiff = filter_out_non_matching(After -- Before, <<"match-this">>),
    LinesWithExpectedContents = lists:zip(LinesDiff, ExpectedContents),
    [ ?assert('contains?'(Line, ExpectedLevel))
      || {Line, ExpectedLevel} <- LinesWithExpectedContents ].

log_at_custom_level(C) ->
    %% given logging on custom log level for the helper module
    ejabberd_loglevel_running(),
    ejabberd_loglevel:set(1),
    ejabberd_loglevel:set_custom(ejabberd_loglevel_SUITE_helper, 5),
    %% when we log from here and from the helper module
    Before = get_log("log/ejabberd.log"),
    log(C, debug, "suite", []),
    ejabberd_loglevel_SUITE_helper:log(C, debug, "helper module", []),
    %% then
    After = case get_at_least_n_log_lines("log/ejabberd.log", length(Before) + 1, timer:seconds(5)) of
                timeout -> ct:fail("timeout waiting for messages to reach the log file");
                Res -> Res
            end,
    Diff = After -- Before,
    %% ...nothing logged from the suite reaches the log file
    ?eq([], filter_out_non_matching(Diff, <<"suite">>)),
    %% ...logs from the helper module are found in the log file
    [LogLine] = filter_out_non_matching(Diff, <<"helper module">>),
    ?assert('contains?'(LogLine, <<"debug">>)).

%%
%% Helpers
%%

levels() ->
    [{0, none},
     {1, critical},
     {2, error},
     {3, warning},
     {4, info},
     {5, debug}].

ejabberd_loglevel_running() ->
    application:load(lager),
    BackendName = lager_file_backend,
    File = "log/ejabberd.log",
    Before = get_log(File),
    Backend = {BackendName, [{file, File},
                             {level, info},
                             {size, 2097152},
                             {date, "$D0"},
                             {count, 5}]},
    application:set_env(lager, handlers, [Backend]),
    ejabberd_loglevel:init(),
    FileBackend = {BackendName, File},
    true = timeout /= get_at_least_n_log_lines(File, length(Before) + 1, timer:seconds(5)),
    {ok, FileBackend}.

log(_, LevelName, Fmt, Args) ->
    lager:log(LevelName, self(), Fmt, Args).

levels_less_than_or_equal_to(L) ->
    [ ?a2b(LevelName) || {ThisL, LevelName} <- levels(), ThisL =< L ].

'contains?'(String, Pattern) ->
     binary:match(String, [Pattern]) /= nomatch.

get_log(LogFile) ->
    case file:read_file(LogFile) of
        {error, enoent} -> [];
        {ok, Contents} ->
            binary:split(Contents, <<"\n">>, [global, trim])
    end.

filter_out_non_matching(Lines, Pattern) ->
    lists:filter(fun (L) -> 'contains?'(L, Pattern) end, Lines).

get_at_least_n_log_lines(LogFile, NLines, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), get_at_least_n_log_lines),
    get_at_least_n_log_lines(LogFile, NLines, TRef, get_log(LogFile)).

get_at_least_n_log_lines(_LogFile, NLines, TRef, Lines)
  when length(Lines) >= NLines ->
    cancel_timer(TRef),
    Lines;
get_at_least_n_log_lines(LogFile, NLines, TRef, _Lines) ->
    receive
        {timeout, TRef, get_at_least_n_log_lines} ->
            timeout
    after 100 ->
            get_at_least_n_log_lines(LogFile, NLines, TRef, get_log(LogFile))
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
        false ->
            receive {timeout, TRef, get_at_least_n_log_lines} -> ok
            after 0 -> ok end;
        _ ->
            ok
    end.
