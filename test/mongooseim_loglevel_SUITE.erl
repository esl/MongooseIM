-module(mongooseim_loglevel_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         something_is_logged/1,
         set_get_loglevel/1,
         set_custom_loglevel/1,
         log_at_every_level/1,
         log_at_custom_level/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOGFILE, "log/mongooseim.log.1").

all() ->
    [
     something_is_logged,
     set_get_loglevel,
     set_custom_loglevel,
     log_at_every_level,
     log_at_custom_level
    ].

init_per_suite(Config) ->
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, info),
    {ok, Backend} = mongoose_logger_running(),
    [{logger_primary_config, LoggerConfig},
     {logger_backend, Backend}
     | Config].

end_per_suite(Config) ->
    logger:remove_handler(?config(logger_handler, Config)),
    logger:set_primary_config(?config(logger_primary_config, Config)),
    ok.

%%
%% Tests
%%
something_is_logged(Config) ->
    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),
    logger:log(error, "Something"),
    ?assertNotEqual(timeout, get_at_least_n_log_lines(File, length(Before) + 1, timer:seconds(5))).

set_get_loglevel(_Config) ->
    LevelsToTest = ([ {Keyword, Number} || {Number, Keyword} <- levels() ] ++
                    [ {Keyword, Keyword} || {_, Keyword} <- levels() ]),
    Assertion = fun(Expected, Level) ->
                        ?assertEqual(ok, mongoose_logs:set_global_loglevel(Level)),
                        ?assertEqual(Expected, mongoose_logs:get_global_loglevel())
            end,
    [ Assertion(Expected, Level) || {Expected, Level} <- LevelsToTest ].

set_custom_loglevel(_Config) ->
    ExampleMod = mongooseim_loglevel_SUITE_helper,
    ExampleLvl = info,
    ?assertEqual(ok, mongoose_logs:set_module_loglevel(ExampleMod, ExampleLvl)).

log_at_every_level(_Config) ->
    %% given
    [ begin
          %% when
          mongoose_logs:set_global_loglevel(LName),
          %% then
          log_at_level(LName)
      end || {_, LName} <- levels() ].

log_at_level(none) ->
    %% When log level `none` is set and we log on each possible level...
    Before = get_log(?LOGFILE),
    [ logger:log(LevelName, "", []) || {_, LevelName} <- levels(), LevelName /= none ],
    %% ...then nothing ends up in the log file.
    %% (polling doesn't make sense in this one case, we have to sleep)
    Fun = fun() -> get_log(?LOGFILE) -- Before end,
    async_helper:wait_until(Fun, []);
log_at_level(LName) ->
    %% When current log level is L and we log on each possible level...
    Before = get_log(?LOGFILE),
    [ logger:log(LevelName, "match-this ~s", [LevelName]) || {_, LevelName} <- levels(), LevelName /= none ],
    %% ...then for each sensible level (i.e. less or equal to current level)
    %% we get a line in the log file.
    ExpectedContents = levels_less_than_or_equal_to(LName),
    After = case get_at_least_n_log_lines(?LOGFILE,
                                          length(Before) + length(ExpectedContents),
                                          timer:seconds(5)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,
    LinesDiff = filter_out_non_matching(After -- Before, <<"match-this">>),
    LinesWithExpectedContents = lists:zip(LinesDiff, ExpectedContents),
    [ ?assert('contains?'(Line, ExpectedLevel))
      || {Line, ExpectedLevel} <- LinesWithExpectedContents ].

log_at_custom_level(_Config) ->
    %% given logging on custom log level for the helper module
    mongoose_logs:set_global_loglevel(critical),
    mongoose_logs:set_module_loglevel(mongooseim_loglevel_SUITE_helper, debug),
    %% when we log from here and from the helper module
    Before = get_log(?LOGFILE),
    NotSupposedToBeLogged = "This should not be in the logs!",
    ShouldBeSuccessfullyLogged = "This MUST appear in the logs successfully",
    logger:log(debug, NotSupposedToBeLogged),
    mongooseim_loglevel_SUITE_helper:log(debug, ShouldBeSuccessfullyLogged),
    %% then
    After = case get_at_least_n_log_lines(?LOGFILE, length(Before) + 1, timer:seconds(5)) of
                timeout -> ct:fail("timeout waiting for messages to reach the log file");
                Res -> Res
            end,
    Diff = After -- Before,
    %% ...nothing logged from the suite reaches the log file
    ?assertEqual([], filter_out_non_matching(Diff, list_to_binary(NotSupposedToBeLogged))),
    %% ...logs from the helper module are found in the log file
    [LogLine] = filter_out_non_matching(Diff, list_to_binary(ShouldBeSuccessfullyLogged)),
    ?assert('contains?'(LogLine, <<"debug">>)).

%%
%% Helpers
%%

levels() ->
    [
     {-1, none},
     {0, emergency},
     {1, alert},
     {2, critical},
     {3, error},
     {4, warning},
     {5, notice},
     {6, info},
     {7, debug}
    ].

mongoose_logger_running() ->
    File = ?LOGFILE,
    HandlerID = disk_log,
    HandlerModule = logger_disk_log_h,
    HandlerConfig = #{config => #{
                        file => "log/mongooseim.log",
                        type => wrap,
                        max_no_files => 5,
                        max_no_bytes => 2097152
                       }
                     },
    ok = logger:add_handler(HandlerID, HandlerModule, HandlerConfig),
    FileBackend = {HandlerID, File},
    {ok, FileBackend}.

levels_less_than_or_equal_to(LName) ->
    {LNumber, LName} = lists:keyfind(LName, 2, levels()),
    ListOfNamesLessThanOrEqual = lists:filtermap(
        fun({LNum, Name}) when LNum =< LNumber-> {true, Name};
           (_) -> false end, levels()),
    ListsAtoms = lists:delete(none, ListOfNamesLessThanOrEqual),
    lists:map(fun(El) -> atom_to_binary(El, utf8) end, ListsAtoms).

'contains?'(String, Pattern) ->
     binary:match(String, [Pattern]) /= nomatch.

filter_out_non_matching(Lines, Pattern) ->
    lists:filter(fun (L) -> 'contains?'(L, Pattern) end, Lines).

get_log(LogFile) ->
    case file:read_file(LogFile) of
        {error, enoent} -> [];
        {ok, Contents} -> binary:split(Contents, <<"\n">>, [global, trim])
    end.

get_at_least_n_log_lines(LogFile, NLines, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), get_at_least_n_log_lines),
    get_at_least_n_log_lines(LogFile, NLines, TRef, get_log(LogFile)).

get_at_least_n_log_lines(_LogFile, NLines, TRef, Lines)
  when length(Lines) >= NLines ->
    erlang:cancel_timer(TRef),
    Lines;
get_at_least_n_log_lines(LogFile, NLines, TRef, _Lines) ->
    receive
        {timeout, TRef, get_at_least_n_log_lines} -> timeout
    after 100 ->
            get_at_least_n_log_lines(LogFile, NLines, TRef, get_log(LogFile))
    end.
