%%% @doc CT hook that collects error logs from MongooseIM nodes during test
%%% execution and writes a report file per test suite.
%%%
%%% Reports are written to a `logged_errors/' subfolder in the CT log
%%% directory (ct_report/ct_run.*). Each suite gets its own file named
%%% `SuiteName.log'. Errors are broken down by group and testcase.
%%%
%%% Uses a named instance of log_error_collector (`cth_error_report')
%%% to avoid conflicts with log_error_helper's default instance.
%%%
%%% Tests can declare expected error patterns using expect/1.
%%% In the .log report, unexpected errors are prefixed with
%%% *** UNEXPECTED ***. In the .html report, unexpected errors
%%% are red and expected errors are green.
%%%
%%% Usage in *.spec:
%%%   {ct_hooks, [cth_error_report]}.
%%%
%%% Usage in tests:
%%%   cth_error_report:expect({what, some_expected_error}).
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

%% API for tests
-export([expect/1, max_unexpected_errors_logged/1]).

-import(distributed_helper, [rpc/4, mim/0]).

-define(COLLECTOR, log_error_collector).
-define(INSTANCE, cth_error_report).
-define(PATTERNS_TABLE, cth_error_report_patterns).

-record(state, {
    report_dir :: undefined | string(),
    %% Persists across suites for the summary file
    summary_dir :: undefined | string(),
    %% Stack of {GroupName, IsParallel} tuples, innermost first
    groups = [] :: [{atom(), boolean()}],
    parallel_depth = 0 :: non_neg_integer(),
    mark :: undefined | integer(),
    entries = [] :: [entry()],
    %% Count of unexpected errors in current suite
    unexpected_total = 0 :: non_neg_integer(),
    %% Count of all errors (unexpected + expected) in current suite
    all_total = 0 :: non_neg_integer(),
    %% Accumulated across suites for the final summary
    %% {Suite, AllErrors, UnexpectedErrors}
    suite_results = [] :: [suite_result()]
}).

-type suite_result() :: {atom(), non_neg_integer(), non_neg_integer()}.

-type log_entry() :: log_error_collector:log_entry().
-type entry() :: {section(), Unexpected :: [log_entry()],
                  Expected :: [log_entry()]}.
-type group_info() :: {atom(), boolean()}.
-type pattern() :: log_error_helper:pattern().
-type section() :: {init_per_suite}
                 | {end_per_suite}
                 | {init_per_group, [group_info()]}
                 | {end_per_group, [group_info()]}
                 | {testcase, [group_info()], atom()}.

%% API

%% @doc Declare an expected error pattern for the current suite.
%% Matching errors will be filtered from the report.
%% Uses the same pattern types as log_error_helper:expect/1.
-spec expect(pattern()) -> true.
expect(Pattern) ->
    ets:insert(?PATTERNS_TABLE, {expect, Pattern}).

%% @doc Set the maximum allowed unexpected errors for the current suite.
%% If the count exceeds this limit, end_per_suite will return
%% {error, too_many_unexpected_errors}.
%% Call from init_per_suite or init_per_group.
-spec max_unexpected_errors_logged(non_neg_integer()) -> true.
max_unexpected_errors_logged(N) when is_integer(N), N >= 0 ->
    ets:insert(?PATTERNS_TABLE, {max_unexpected_errors_logged, N}).

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
        SummaryDir = case State#state.summary_dir of
            undefined -> ReportDir;
            Existing -> Existing
        end,
        init_patterns_table(),
        {Config, State#state{report_dir = ReportDir, mark = Mark,
                             summary_dir = SummaryDir,
                             groups = [], parallel_depth = 0,
                             entries = [],
                             unexpected_total = 0, all_total = 0}}
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
    SuiteResult = try
        MaxUnexp = get_max_unexpected_errors_logged(),
        State1 = collect_errors({end_per_suite}, State),
        rpc(mim(), ?COLLECTOR, stop, [?INSTANCE]),
        delete_patterns_table(),
        write_report(Suite, State1),
        Unexp = State1#state.unexpected_total,
        check_unexpected_limit(Suite, Unexp, MaxUnexp,
                               State1#state.report_dir),
        {Suite, State1#state.all_total, Unexp}
    catch Class:Reason:Stacktrace ->
        ct:pal("cth_error_report: failed for ~p: ~p:~p~n~p",
               [Suite, Class, Reason, Stacktrace]),
        delete_patterns_table(),
        {Suite, State#state.all_total,
         State#state.unexpected_total}
    end,
    Results = [SuiteResult | State#state.suite_results],
    {Return, State#state{report_dir = undefined, mark = undefined,
                         entries = [],
                         unexpected_total = 0, all_total = 0,
                         suite_results = Results}}.

terminate(#state{summary_dir = undefined}) ->
    ok;
terminate(#state{suite_results = []}) ->
    ok;
terminate(#state{summary_dir = Dir, suite_results = RevResults}) ->
    write_summary(Dir, lists:sort(RevResults)).

%% Unexpected errors limit check

check_unexpected_limit(_Suite, _Unexp, undefined, _Dir) ->
    ok;
check_unexpected_limit(_Suite, Unexp, Max, _Dir) when Unexp =< Max ->
    ok;
check_unexpected_limit(Suite, Unexp, Max, ReportDir) ->
    Msg = io_lib:format("~p: ~p unexpected ~s logged,"
                        " max allowed: ~p~n",
                        [Suite, Unexp, plural(Unexp, "error"), Max]),
    MarkerFile = filename:join(ReportDir, "limit_exceeded"),
    file:write_file(MarkerFile, Msg, [append]).

%% Parallel group detection

-spec is_parallel_group(list()) -> boolean().
is_parallel_group(Config) ->
    Props = proplists:get_value(tc_group_properties, Config, []),
    lists:member(parallel, Props).

%% Expected patterns table

init_patterns_table() ->
    case ets:whereis(?PATTERNS_TABLE) of
        undefined ->
            ets_helper:new(?PATTERNS_TABLE, [bag]);
        _Tid ->
            ets:delete_all_objects(?PATTERNS_TABLE)
    end.

delete_patterns_table() ->
    case ets:whereis(?PATTERNS_TABLE) of
        undefined -> ok;
        _Tid -> ets_helper:delete(?PATTERNS_TABLE)
    end.

get_patterns() ->
    case ets:whereis(?PATTERNS_TABLE) of
        undefined -> [];
        _Tid -> [P || {expect, P} <- ets:tab2list(?PATTERNS_TABLE)]
    end.

get_max_unexpected_errors_logged() ->
    case ets:whereis(?PATTERNS_TABLE) of
        undefined ->
            undefined;
        _Tid ->
            case ets:match(?PATTERNS_TABLE, {max_unexpected_errors_logged, '$1'}) of
                [[N] | _] -> N;
                [] -> undefined
            end
    end.

%% Error collection

-spec collect_errors(section(), #state{}) -> #state{}.
collect_errors(Section, #state{mark = Mark} = State) ->
    try
        Errors = rpc(mim(), ?COLLECTOR, get_errors_after, [?INSTANCE, Mark]),
        NewMark = rpc(mim(), ?COLLECTOR, timestamp, []),
        {Unexpected, Expected} = classify_errors(Errors, get_patterns()),
        Entry = {Section, Unexpected, Expected},
        State#state{mark = NewMark,
                    entries = [Entry | State#state.entries],
                    unexpected_total = State#state.unexpected_total
                        + length(Unexpected),
                    all_total = State#state.all_total
                        + length(Unexpected) + length(Expected)}
    catch Class:Reason:Stacktrace ->
        ct:pal("cth_error_report: collect_errors failed: ~p:~p~n~p",
               [Class, Reason, Stacktrace]),
        State
    end.

-spec classify_errors([log_entry()], [pattern()]) ->
    {Unexpected :: [log_entry()], Expected :: [log_entry()]}.
classify_errors(Errors, []) ->
    {Errors, []};
classify_errors(Errors, Patterns) ->
    lists:partition(
        fun({_Ts, _Level, Msg, Meta}) ->
            not lists:any(
                fun(P) -> log_error_helper:matches_pattern(Msg, Meta, P) end,
                Patterns)
        end,
        Errors).

%% Report directory

-spec init_report_dir(list()) -> string().
init_report_dir(Config) ->
    RunDir = path_helper:ct_run_dir(Config),
    ReportDir = filename:join(RunDir, "logged_errors"),
    ok = filelib:ensure_dir(filename:join(ReportDir, "dummy")),
    ReportDir.

%% Report writing

-spec write_report(atom(), #state{}) -> ok.
write_report(Suite, #state{report_dir = ReportDir, entries = RevEntries,
                           all_total = AllTotal,
                           unexpected_total = UnexpTotal}) ->
    SuiteStr = atom_to_list(Suite),
    Entries = lists:reverse(RevEntries),
    LogFile = filename:join(ReportDir, SuiteStr ++ ".log"),
    LogContent = format_report_log(Suite, AllTotal, UnexpTotal, Entries),
    ok = file:write_file(LogFile, LogContent),
    HtmlFile = filename:join(ReportDir, SuiteStr ++ ".html"),
    HtmlContent = format_report_html(Suite, AllTotal, UnexpTotal, Entries),
    ok = file:write_file(HtmlFile, HtmlContent).

-spec write_summary(string(), [suite_result()]) -> ok.
write_summary(ReportDir, Results) ->
    %% Sort by unexpected count descending (primary),
    %% alphabetical by suite name (secondary, from input)
    Sorted = lists:sort(
        fun({_, _, U1}, {_, _, U2}) -> U1 >= U2 end, Results),
    write_summary_log(ReportDir, Sorted),
    write_summary_html(ReportDir, Sorted).

write_summary_log(ReportDir, Sorted) ->
    File = filename:join(ReportDir, "summary.log"),
    Header = "Errors Logged During Tests Summary\n\n",
    Lines = lists:map(fun format_summary_line/1, Sorted),
    ok = file:write_file(File, [Header | Lines]).

write_summary_html(ReportDir, Sorted) ->
    File = filename:join(ReportDir, "summary.html"),
    Rows = lists:map(fun format_summary_html_row/1, Sorted),
    Html = [
        "<!DOCTYPE html>\n<html>\n<head>\n"
        "<meta charset=\"utf-8\">\n"
        "<title>Errors Logged During Tests Summary</title>\n"
        "<style>\n"
        "body { font-family: monospace; margin: 2em; }\n"
        "table { border-collapse: collapse; }\n"
        "th, td { border: 1px solid #ccc; padding: 6px 12px;"
        " text-align: left; }\n"
        "th { background: #f0f0f0; }\n"
        "td.num { text-align: right; }\n"
        ".zero { color: green; }\n"
        ".errors { color: red; font-weight: bold; }\n"
        "a { color: inherit; }\n"
        "</style>\n"
        "</head>\n<body>\n",
        "<h1>Errors Logged During Tests Summary</h1>\n",
        "<table>\n<tr><th>Suite</th>"
        "<th>All errors</th>"
        "<th>Unexpected errors</th></tr>\n",
        Rows,
        "</table>\n</body>\n</html>\n"
    ],
    ok = file:write_file(File, Html).

-spec format_summary_line(suite_result()) -> iolist().
format_summary_line({Suite, All, 0}) ->
    io_lib:format("  ~s: ~p errors, 0 unexpected~n", [Suite, All]);
format_summary_line({Suite, All, Unexp}) ->
    io_lib:format("  ~s: ~p errors, ~p unexpected  <---~n",
                  [Suite, All, Unexp]).

-spec format_summary_html_row(suite_result()) -> iolist().
format_summary_html_row({Suite, All, Unexp}) ->
    SuiteStr = atom_to_list(Suite),
    UnexpClass = case Unexp of
        0 -> "num zero";
        _ -> "num errors"
    end,
    io_lib:format("<tr>"
                  "<td><a href=\"~s.html\">~s</a></td>"
                  "<td class=\"num\">~p</td>"
                  "<td class=\"~s\">~p</td>"
                  "</tr>\n",
                  [SuiteStr, SuiteStr, All, UnexpClass, Unexp]).

%% Plain text report

format_report_log(Suite, AllTotal, UnexpTotal, Entries) ->
    Header = io_lib:format(
        "Suite: ~p~nTotal errors: ~p, unexpected: ~p~n",
        [Suite, AllTotal, UnexpTotal]),
    Body = format_entries_log(Entries, []),
    [Header | Body].

format_entries_log([], _PrevGroups) ->
    [];
format_entries_log([{Section, Unexpected, Expected} | Rest], PrevGroups) ->
    Groups = section_groups(Section),
    GroupHeaders = format_group_transitions(PrevGroups, Groups),
    SectionHeader = format_section_header(Section),
    ErrorBody = format_section_errors_log(Unexpected, Expected),
    [GroupHeaders, SectionHeader, ErrorBody
     | format_entries_log(Rest, Groups)].

%% HTML report

format_report_html(Suite, AllTotal, UnexpTotal, Entries) ->
    SuiteStr = atom_to_list(Suite),
    Body = format_entries_html(Entries, []),
    ["<!DOCTYPE html>\n<html>\n<head>\n"
     "<meta charset=\"utf-8\">\n",
     io_lib:format("<title>~s errors</title>\n", [SuiteStr]),
     "<style>\n"
     "body { font-family: monospace; margin: 2em; }\n"
     ".unexpected { color: red; }\n"
     ".expected { color: green; }\n"
     ".section { margin: 1em 0; }\n"
     ".group-header { font-weight: bold; font-size: 1.1em;"
     " margin-top: 1.5em; }\n"
     ".section-header { font-weight: bold; margin-top: 1em; }\n"
     ".entry { margin: 0.3em 0 0.3em 2em;"
     " white-space: pre-wrap; }\n"
     ".meta { font-size: 0.9em; }\n"
     ".note { color: gray; font-style: italic; }\n"
     "</style>\n"
     "</head>\n<body>\n",
     io_lib:format("<h1>~s</h1>\n", [SuiteStr]),
     io_lib:format("<p>Total errors: ~p, unexpected: ~p</p>\n",
                   [AllTotal, UnexpTotal]),
     Body,
     "</body>\n</html>\n"].

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
    io_lib:format("~n~n=== Group: ~p [parallel] ===~n", [Group]);
format_group_header({Group, false}) ->
    io_lib:format("~n~n=== Group: ~p ===~n", [Group]).

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

%% Log section errors

format_section_errors_log([], []) ->
    "(no errors)\n";
format_section_errors_log([], Expected) ->
    N = length(Expected),
    io_lib:format("(~p expected ~s)~n", [N, plural(N, "error")]);
format_section_errors_log(Unexpected, Expected) ->
    ULines = lists:map(fun format_entry_unexpected_log/1, Unexpected),
    ELines = lists:map(fun format_entry_expected_log/1, Expected),
    Total = length(Unexpected) + length(Expected),
    Summary = io_lib:format(
        "~p ~s: ~p unexpected, ~p expected~n",
        [Total, plural(Total, "error"),
         length(Unexpected), length(Expected)]),
    [Summary, ULines, ELines].

format_entry_unexpected_log(Entry) ->
    ["*** UNEXPECTED *** ", format_entry_log(Entry)].

format_entry_expected_log(Entry) ->
    format_entry_log(Entry).

format_entry_log({_Timestamp, Level, Msg, Meta}) ->
    Time = format_time(Meta),
    Node = format_node(Meta),
    MsgFormatted = format_msg(Msg),
    MetaLine = format_meta_line(Meta),
    io_lib:format("[~p] ~s~s~n    ~s~n    ~s~n",
                  [Level, Time, Node, MsgFormatted, MetaLine]).

%% HTML entries

format_entries_html([], _PrevGroups) ->
    [];
format_entries_html([{Section, Unexpected, Expected} | Rest],
                    PrevGroups) ->
    Groups = section_groups(Section),
    GroupHeaders = format_group_transitions_html(PrevGroups, Groups),
    SectionHeader = format_section_header_html(Section),
    ErrorBody = format_section_errors_html(Unexpected, Expected),
    [GroupHeaders, SectionHeader, ErrorBody
     | format_entries_html(Rest, Groups)].

format_group_transitions_html(Prev, Curr) ->
    PrevR = lists:reverse(Prev),
    CurrR = lists:reverse(Curr),
    format_group_transitions_html_impl(PrevR, CurrR).

format_group_transitions_html_impl([Same | PrevRest],
                                   [Same | CurrRest]) ->
    format_group_transitions_html_impl(PrevRest, CurrRest);
format_group_transitions_html_impl(_Prev, NewGroups) ->
    [format_group_header_html(GI) || GI <- NewGroups].

format_group_header_html({Group, true}) ->
    io_lib:format("<br/>\n<div class=\"group-header\">"
                  "Group: ~p [parallel]</div>\n", [Group]);
format_group_header_html({Group, false}) ->
    io_lib:format("<br/>\n<div class=\"group-header\">"
                  "Group: ~p</div>\n", [Group]).

format_section_header_html({init_per_suite}) ->
    "<div class=\"section-header\">init_per_suite</div>\n";
format_section_header_html({end_per_suite}) ->
    "<div class=\"section-header\">end_per_suite</div>\n";
format_section_header_html({init_per_group, _}) ->
    "<div class=\"section-header\">init_per_group</div>\n";
format_section_header_html({end_per_group, _}) ->
    "<div class=\"section-header\">end_per_group</div>\n";
format_section_header_html({testcase, _, TC}) ->
    io_lib:format("<div class=\"section-header\">~p</div>\n",
                  [TC]).

format_section_errors_html([], []) ->
    "<div class=\"note\">(no errors)</div>\n";
format_section_errors_html(Unexpected, Expected) ->
    ULines = [format_entry_html(E, "unexpected") || E <- Unexpected],
    ELines = [format_entry_html(E, "expected") || E <- Expected],
    [ULines, ELines].

format_entry_html({_Timestamp, Level, Msg, Meta}, Class) ->
    Time = format_time(Meta),
    Node = format_node(Meta),
    MsgFmt = html_escape(format_msg(Msg)),
    MetaLine = html_escape(
        iolist_to_binary(format_meta_line(Meta))),
    io_lib:format("<div class=\"entry ~s\">"
                  "[~p] ~s~s<br/>~s<br/>"
                  "<span class=\"meta\">~s</span>"
                  "</div>\n",
                  [Class, Level, Time, Node,
                   MsgFmt, MetaLine]).

format_time(#{time := Time}) ->
    USec = Time rem 1000000,
    Sec = Time div 1000000,
    BaseDate = calendar:datetime_to_gregorian_seconds(
        {{1970, 1, 1}, {0, 0, 0}}),
    UtcGSec = BaseDate + Sec,
    UtcDateTime = calendar:gregorian_seconds_to_datetime(UtcGSec),
    LocalDateTime = calendar:universal_time_to_local_time(
        UtcDateTime),
    LocalGSec = calendar:datetime_to_gregorian_seconds(LocalDateTime),
    OffsetMin = (LocalGSec - UtcGSec) div 60,
    OffsetH = abs(OffsetMin) div 60,
    OffsetM = abs(OffsetMin) rem 60,
    Sign = case OffsetMin >= 0 of true -> $+; false -> $- end,
    {{Y, Mo, D}, {H, Mi, S}} = LocalDateTime,
    io_lib:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0B"
        "~c~2..0B:~2..0B ",
        [Y, Mo, D, H, Mi, S, USec, Sign, OffsetH, OffsetM]);
format_time(_) ->
    "".

format_node(#{node := Node}) ->
    io_lib:format("~s ", [Node]);
format_node(_) ->
    "".

format_meta_line(Meta) ->
    Parts = lists:filtermap(fun(F) -> F(Meta) end,
        [fun format_meta_pid/1, fun format_meta_mfa/1,
         fun format_meta_file/1, fun format_meta_lineno/1]),
    case Parts of
        [] -> "";
        _ -> ["meta: " | lists:join(", ", Parts)]
    end.

format_meta_pid(#{pid := Pid}) ->
    {true, io_lib:format("pid=~p", [Pid])};
format_meta_pid(_) -> false.

format_meta_mfa(#{mfa := {M, F, A}}) ->
    {true, io_lib:format("mfa=~p:~p/~p", [M, F, A])};
format_meta_mfa(_) -> false.

format_meta_file(#{file := File}) ->
    {true, io_lib:format("file=~s", [File])};
format_meta_file(_) -> false.

format_meta_lineno(#{line := Line}) ->
    {true, io_lib:format("line=~p", [Line])};
format_meta_lineno(_) -> false.

format_msg({string, String}) when is_list(String) ->
    unicode:characters_to_binary(String);
format_msg({string, Binary}) when is_binary(Binary) ->
    Binary;
format_msg({report, Report}) when is_map(Report) ->
    unicode:characters_to_binary(pretty_print_map(Report));
format_msg({Format, Args}) when is_list(Format), is_list(Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args));
format_msg(Other) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Other])).

pretty_print_map(Map) ->
    Pairs = maps:to_list(Map),
    Lines = [io_lib:format("  ~0p => ~0p", [K, V]) || {K, V} <- Pairs],
    ["#{\n", lists:join(",\n", Lines), "\n}"].

plural(1, Word) -> Word;
plural(_, Word) -> Word ++ "s".

html_escape(Bin) when is_binary(Bin) ->
    html_escape(binary_to_list(Bin));
html_escape([]) -> [];
html_escape([$< | T]) -> "&lt;" ++ html_escape(T);
html_escape([$> | T]) -> "&gt;" ++ html_escape(T);
html_escape([$& | T]) -> "&amp;" ++ html_escape(T);
html_escape([$" | T]) -> "&quot;" ++ html_escape(T);
html_escape([H | T]) -> [H | html_escape(T)].
