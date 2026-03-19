%%% @doc CT hook that collects error logs from MongooseIM nodes during test
%%% execution and writes a report file per test suite.
%%%
%%% Reports are written to an `error_reports/' subfolder in the CT log
%%% directory (ct_report/ct_run.*). Each suite gets its own file named
%%% `SuiteName.log'. Errors are broken down by group and testcase.
%%%
%%% Uses a named instance of log_error_collector (`cth_error_report')
%%% to avoid conflicts with log_error_helper's default instance.
%%%
%%% Usage in *.spec:
%%%   {ct_hooks, [cth_error_report]}.
-module(cth_error_report).

%% CT hook callbacks
-export([id/1, init/2]).
-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_init_per_group/4]).
-export([post_init_per_group/5]).
-export([pre_init_per_testcase/4]).
-export([post_end_per_testcase/5]).
-export([pre_end_per_group/4]).
-export([post_end_per_group/5]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).
-export([terminate/1]).

-import(distributed_helper, [rpc/4, mim/0]).

-define(COLLECTOR, log_error_collector).
-define(INSTANCE, cth_error_report).

-record(state, {
    report_dir :: undefined | string(),
    %% Stack of {GroupName, IsParallel} tuples, innermost first
    groups = [] :: [{atom(), boolean()}],
    parallel_depth = 0 :: non_neg_integer(),
    mark :: undefined | integer(),
    entries = [] :: [entry()],
    total = 0 :: non_neg_integer()
}).

-type entry() :: {section(), [log_error_collector:log_entry()]}.
-type group_info() :: {atom(), boolean()}.
-type section() :: {init_per_suite}
                 | {end_per_suite}
                 | {init_per_group, [group_info()]}
                 | {end_per_group, [group_info()]}
                 | {testcase, [group_info()], atom()}.

%% CT hook callbacks

id(_Opts) ->
    "cth_error_report".

init(_Id, _Opts) ->
    {ok, #state{}}.

pre_init_per_suite(_Suite, Config, State) ->
    try
        mongoose_helper:inject_module(?COLLECTOR),
        rpc(mim(), ?COLLECTOR, start, [?INSTANCE, [error]]),
        Mark = rpc(mim(), ?COLLECTOR, timestamp, []),
        ReportDir = init_report_dir(Config),
        {Config, State#state{report_dir = ReportDir, mark = Mark,
                             groups = [], parallel_depth = 0,
                             entries = [], total = 0}}
    catch Class:Reason:Stacktrace ->
        ct:pal("cth_error_report: failed to start: ~p:~p~n~p",
               [Class, Reason, Stacktrace]),
        {Config, State#state{report_dir = undefined}}
    end.

post_init_per_suite(_Suite, _Config, Return, #state{mark = undefined} = State) ->
    {Return, State};
post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, collect_errors({init_per_suite}, State)}.

pre_init_per_group(_Suite, Group, Config, State) ->
    Parallel = is_parallel_group(Config),
    PD = case Parallel of
        true -> State#state.parallel_depth + 1;
        false -> State#state.parallel_depth
    end,
    {Config, State#state{groups = [{Group, Parallel} | State#state.groups],
                         parallel_depth = PD}}.

post_init_per_group(_Suite, _Group, _Config, Return, #state{mark = undefined} = State) ->
    {Return, State};
post_init_per_group(_Suite, _Group, _Config, Return, State) ->
    {Return, collect_errors({init_per_group, State#state.groups}, State)}.

pre_init_per_testcase(_Suite, _TC, Config, State) ->
    {Config, State}.

post_end_per_testcase(_Suite, _TC, _Config, Return, #state{mark = undefined} = State) ->
    {Return, State};
post_end_per_testcase(_Suite, _TC, _Config, Return, #state{parallel_depth = PD} = State)
  when PD > 0 ->
    %% Inside a parallel group: skip per-testcase collection.
    %% Errors will be attributed to the group's end_per_group instead,
    %% because CT forks hook state for parallel testcases and only one
    %% copy survives, so per-testcase entries would be lost.
    {Return, State};
post_end_per_testcase(_Suite, TC, _Config, Return, State) ->
    {Return, collect_errors({testcase, State#state.groups, TC}, State)}.

pre_end_per_group(_Suite, _Group, Config, State) ->
    {Config, State}.

post_end_per_group(_Suite, _Group, _Config, Return, #state{mark = undefined} = State) ->
    {Return, State};
post_end_per_group(_Suite, _Group, _Config, Return, State) ->
    State1 = collect_errors({end_per_group, State#state.groups}, State),
    [{_, WasParallel} | RestGroups] = State1#state.groups,
    PD = case WasParallel of
        true -> State1#state.parallel_depth - 1;
        false -> State1#state.parallel_depth
    end,
    {Return, State1#state{groups = RestGroups, parallel_depth = PD}}.

pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.

post_end_per_suite(Suite, _Config, Return, #state{report_dir = undefined} = State) ->
    ct:pal("cth_error_report: no report_dir for ~p, skipping", [Suite]),
    {Return, State};
post_end_per_suite(Suite, _Config, Return, State) ->
    try
        State1 = collect_errors({end_per_suite}, State),
        rpc(mim(), ?COLLECTOR, stop, [?INSTANCE]),
        write_report(Suite, State1)
    catch Class:Reason:Stacktrace ->
        ct:pal("cth_error_report: failed to write report for ~p: ~p:~p~n~p",
               [Suite, Class, Reason, Stacktrace])
    end,
    {Return, State#state{report_dir = undefined, mark = undefined,
                         entries = [], total = 0}}.

terminate(_State) ->
    ok.

%% Parallel group detection

-spec is_parallel_group(list()) -> boolean().
is_parallel_group(Config) ->
    Props = proplists:get_value(tc_group_properties, Config, []),
    lists:member(parallel, Props).

%% Error collection

-spec collect_errors(section(), #state{}) -> #state{}.
collect_errors(Section, #state{mark = Mark} = State) ->
    try
        Errors = rpc(mim(), ?COLLECTOR, get_errors_after, [?INSTANCE, Mark]),
        NewMark = rpc(mim(), ?COLLECTOR, timestamp, []),
        Count = length(Errors),
        Entry = {Section, Errors},
        State#state{mark = NewMark,
                    entries = [Entry | State#state.entries],
                    total = State#state.total + Count}
    catch Class:Reason:Stacktrace ->
        ct:pal("cth_error_report: collect_errors failed: ~p:~p~n~p",
               [Class, Reason, Stacktrace]),
        State
    end.

%% Report directory

-spec init_report_dir(list()) -> string().
init_report_dir(Config) ->
    RunDir = path_helper:ct_run_dir(Config),
    ReportDir = filename:join(RunDir, "error_reports"),
    ok = filelib:ensure_dir(filename:join(ReportDir, "dummy")),
    ReportDir.

%% Report writing

-spec write_report(atom(), #state{}) -> ok.
write_report(Suite, #state{report_dir = ReportDir, entries = RevEntries, total = Total}) ->
    File = filename:join(ReportDir, atom_to_list(Suite) ++ ".log"),
    Entries = lists:reverse(RevEntries),
    Content = format_report(Suite, Total, Entries),
    ok = file:write_file(File, Content).

-spec format_report(atom(), non_neg_integer(), [entry()]) -> iolist().
format_report(Suite, Total, Entries) ->
    Header = io_lib:format("Suite: ~p~nTotal errors: ~p~n", [Suite, Total]),
    Body = format_entries(Entries, []),
    [Header | Body].

-spec format_entries([entry()], [group_info()]) -> iolist().
format_entries([], _PrevGroups) ->
    [];
format_entries([{Section, Errors} | Rest], PrevGroups) ->
    Groups = section_groups(Section),
    GroupHeaders = format_group_transitions(PrevGroups, Groups),
    SectionHeader = format_section_header(Section),
    ErrorBody = format_section_errors(Errors),
    [GroupHeaders, SectionHeader, ErrorBody | format_entries(Rest, Groups)].

-spec section_groups(section()) -> [group_info()].
section_groups({init_per_suite}) -> [];
section_groups({end_per_suite}) -> [];
section_groups({init_per_group, Groups}) -> Groups;
section_groups({end_per_group, Groups}) -> Groups;
section_groups({testcase, Groups, _TC}) -> Groups.

%% Insert group headers when we enter a new group level
-spec format_group_transitions([group_info()], [group_info()]) -> iolist().
format_group_transitions(Prev, Curr) ->
    PrevR = lists:reverse(Prev),
    CurrR = lists:reverse(Curr),
    format_group_transitions_impl(PrevR, CurrR).

format_group_transitions_impl([Same | PrevRest], [Same | CurrRest]) ->
    format_group_transitions_impl(PrevRest, CurrRest);
format_group_transitions_impl(_Prev, NewGroups) ->
    [format_group_header(GI) || GI <- NewGroups].

format_group_header({Group, true}) ->
    io_lib:format("~n=== Group: ~p [parallel] ===~n", [Group]);
format_group_header({Group, false}) ->
    io_lib:format("~n=== Group: ~p ===~n", [Group]).

-spec format_section_header(section()) -> iolist().
format_section_header({init_per_suite}) ->
    io_lib:format("~n--- init_per_suite ---~n", []);
format_section_header({end_per_suite}) ->
    io_lib:format("~n--- end_per_suite ---~n", []);
format_section_header({init_per_group, _Groups}) ->
    io_lib:format("~n--- init_per_group ---~n", []);
format_section_header({end_per_group, _Groups}) ->
    io_lib:format("~n--- end_per_group ---~n", []);
format_section_header({testcase, _Groups, TC}) ->
    io_lib:format("~n--- ~p ---~n", [TC]).

-spec format_section_errors([log_error_collector:log_entry()]) -> iolist().
format_section_errors([]) ->
    "(no errors)\n";
format_section_errors(Errors) ->
    Count = io_lib:format("~p error(s)~n", [length(Errors)]),
    Body = lists:map(fun format_entry/1, Errors),
    [Count | Body].

-spec format_entry(log_error_collector:log_entry()) -> iolist().
format_entry({_Timestamp, Level, Msg, Meta}) ->
    Location = format_location(Meta),
    MsgFormatted = format_msg(Msg),
    What = extract_what(Msg),
    WhatStr = case What of
        undefined -> "";
        Atom -> io_lib:format(" what=~p", [Atom])
    end,
    io_lib:format("  [~p]~s~s~n    ~s~n", [Level, Location, WhatStr, MsgFormatted]).

format_location(#{mfa := {M, F, A}}) ->
    io_lib:format(" ~p:~p/~p", [M, F, A]);
format_location(_) ->
    "".

format_msg({string, String}) when is_list(String) ->
    unicode:characters_to_binary(String);
format_msg({string, Binary}) when is_binary(Binary) ->
    Binary;
format_msg({report, Report}) when is_map(Report) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Report]));
format_msg({Format, Args}) when is_list(Format), is_list(Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args));
format_msg(Other) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Other])).

extract_what({report, #{what := What}}) -> What;
extract_what(_) -> undefined.
