-module(ejabberd_loglevel_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(a2b(Atom), list_to_binary(atom_to_list(Atom))).
-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(ne(A, B), ?assertNot(A == B)).

all() ->
    [init_test,
     set_get_loglevel,
     set_custom_loglevel,
     log_at_every_level].

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
    After = get_log("log/ejabberd.log"),
    ?eq([], After -- Before);
log_at_level(C, {L, _}) ->
    %% When current log level is L and we log on each possible level...
    Before = get_log("log/ejabberd.log"),
    [ log(C, LevelName, "match-this", []) || {_, LevelName} <- levels(), LevelName /= none ],
    %% (give the file system time to flush)
    timer:sleep(timer:seconds(1)),
    %% ...then for each sensible level (i.e. less or equal to current level)
    %% we get a line in the log file.
    After = get_log("log/ejabberd.log"),
    LinesDiff = filter_out_non_matching(After -- Before, <<"match-this">>),
    ExpectedContents = levels_less_than_or_equal_to(L) -- [<<"none">>],
    LinesWithExpectedContents = lists:zip(LinesDiff, ExpectedContents),
    [ ?assert('contains?'(Line, ExpectedLevel))
      || {Line, ExpectedLevel} <- LinesWithExpectedContents ].

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
    Backend = {BackendName, [{file, File},
                             {level, info},
                             {size, 2097152},
                             {date, "$D0"},
                             {count, 5}]},
    application:set_env(lager, handlers, [Backend]),
    ejabberd_loglevel:init(),
    FileBackend = {BackendName, File},
    {ok, FileBackend}.

log(_, LevelName, Fmt, Args) ->
    lager:log(LevelName, self(), Fmt, Args).

levels_less_than_or_equal_to(L) ->
    [ ?a2b(LevelName) || {ThisL, LevelName} <- levels(), ThisL =< L ].

'contains?'(String, Pattern) ->
     binary:match(String, [Pattern]) /= nomatch.

get_log(LogFile) ->
    {ok, Contents} = file:read_file(LogFile),
    binary:split(Contents, <<"\n">>, [global, trim]).

filter_out_non_matching(Lines, Pattern) ->
    lists:filter(fun (L) -> 'contains?'(L, Pattern) end, Lines).
