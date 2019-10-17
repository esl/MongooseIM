%%% @doc Writes a markdown file with error information
-module(ct_markdown_errors_hook).

%% @doc Add the following line in your *.spec file to enable this:
%% {ct_hooks, [ct_markdown_errors_hook]}.

%% Callbacks
-export([id/1]).
-export([init/2]).
-export([post_init_per_suite/4,
         post_init_per_group/4,
         post_init_per_testcase/4]).
-export([post_end_per_suite/4,
         post_end_per_group/4,
         post_end_per_testcase/4]).
-record(state, { file, summary_file, truncated_counter_file, suite, limit }).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_markdown_errors_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    File = "/tmp/ct_markdown",
    TrFile = "/tmp/ct_markdown_truncated",
    SummaryFile = "/tmp/ct_summary",
    file:write_file(File, ""),
    file:write_file(SummaryFile, ""),
    file:delete(TrFile),
    {ok, #state{ file = File, summary_file = SummaryFile, truncated_counter_file = TrFile, limit = 25 }}.

post_init_per_suite(SuiteName, Config, Return, State) ->
    State2 = handle_return(SuiteName, init_per_suite, Return, Config, State),
    {Return, State2#state{suite = SuiteName}}.

post_init_per_group(GroupName, Config, Return, State=#state{suite = SuiteName}) ->
    State2 = handle_return(SuiteName, init_per_group, Return, Config, State),
    {Return, State2#state{}}.

post_init_per_testcase(TC, Config, Return, State=#state{suite = SuiteName}) ->
    State2 = handle_return(SuiteName, TC, Return, Config, State),
    {Return, State2}.

post_end_per_suite(SuiteName, Config, Return, State) ->
    State2 = handle_return(SuiteName, end_per_suite, Return, Config, State),
    {Return, State2#state{suite = ''}}.

post_end_per_group(GroupName, Config, Return, State=#state{suite = SuiteName}) ->
    State2 = handle_return(SuiteName, end_per_group, Return, Config, State),
    {Return, State2#state{}}.

%% @doc Called after each test case.
post_end_per_testcase(TC, Config, Return, State=#state{suite = SuiteName}) ->
    State2 = handle_return(SuiteName, TC, Return, Config, State),
    {Return, State2}.

handle_return(SuiteName, Place, Return, Config, State) ->
    try handle_return_unsafe(SuiteName, Place, Return, Config, State)
    catch Class:Error:StackTrace ->
        ct:pal("issue=handle_return_unsafe_failed reason=~p:~p~n"
               "stacktrace=~p", [Class, Error, StackTrace]),
        State
    end.

handle_return_unsafe(SuiteName, Place, Return, Config, State) ->
    case to_error_message(Return) of
        ok ->
            State;
        Error ->
            FullGroupName = full_group_name(Config),
            log_summary(SuiteName, FullGroupName, Place, State),
            F = fun() ->
                log_error(SuiteName, FullGroupName, Place, Error, Config, State)
                end,
            exec_limited_number_of_times(F, State)
    end.

exec_limited_number_of_times(F, State=#state{limit=0, file=File,
                                             truncated_counter_file = TrFile}) ->
    %% Log truncated, increment counter
    TrCounter = old_truncated_counter_value(TrFile),
    file:write_file(TrFile, integer_to_binary(TrCounter+1)),
    State;
exec_limited_number_of_times(F, State=#state{limit=Limit}) ->
    F(),
    State#state{limit=Limit-1}.

old_truncated_counter_value(TrFile) ->
    case file:read_file(TrFile) of
        {ok, Bin} ->
            binary_to_integer(Bin);
        _ ->
            0
    end.

log_summary(SuiteName, GroupName, Place, #state{summary_file = SummaryFile}) ->
    SummaryText = make_summary_text(SuiteName, GroupName, Place),
    file:write_file(SummaryFile, [SummaryText, $\n], [append]),
    ok.

log_error(SuiteName, GroupName, Place, Error, Config, #state{file = File, summary_file = SummaryFile}) ->
    MaybeLogLink = make_log_link(Config),
    LogLink = make_log_link(Config),
    %% Spoler syntax
    %% https://github.com/dear-github/dear-github/issues/166
    %%    <details>
    %%      <summary>Click to expand</summary>
    %%      whatever
    %%    </details>
    SummaryText = make_summary_text(SuiteName, GroupName, Place),
    BinError = iolist_to_binary(io_lib:format("~p", [Error])),
    Content = truncate_binary(1500, reindent(BinError)),
    %% Don't use exml here to avoid escape errors
    Out = <<"<details><summary>", SummaryText/binary, "</summary>\n"
            "\n\n```erlang\n", Content/binary, "\n```\n",
            LogLink/binary, "</details>\n">>,
    file:write_file(File, Out, [append]),
    ok.

make_summary_text(SuiteName, '', '') ->
     atom_to_binary(SuiteName, utf8);
make_summary_text(SuiteName, '', TC) ->
    BSuiteName = atom_to_binary(SuiteName, utf8),
    BTC = atom_to_binary(TC, utf8),
    <<BSuiteName/binary, ":", BTC/binary>>;
make_summary_text(SuiteName, GroupName, TC) ->
    BSuiteName = atom_to_binary(SuiteName, utf8),
    BGroupName = atom_to_binary(GroupName, utf8),
    BTC = atom_to_binary(TC, utf8),
    <<BSuiteName/binary, ":", BGroupName/binary, ":", BTC/binary>>.

make_log_link(Config) ->
    LogFile = proplists:get_value(tc_logfile, Config, ""),
    LogLink =
    case LogFile of
        "" ->
            <<>>;
        _ ->
            <<"\n[Report log](", (list_to_binary(LogFile))/binary, ")\n">>
    end.

to_error_message(Return) ->
    case Return of
        {'EXIT', _} ->
            Return;
        {fail, _} ->
            Return;
        {error, _} ->
            Return;
        {skip, _} ->
            ok;
        _ ->
            ok
    end.

truncate_binary(Len, Bin) ->
    case byte_size(Bin) > Len of
        true ->
            Prefix = binary:part(Bin, {0,Len}),
            <<Prefix/binary, "...">>;
        false ->
            Bin
    end.

reindent(Bin) ->
    %% Use 2 whitespaces instead of 4 for indention
    %% to make more compact look
    binary:replace(Bin, <<"    ">>, <<"  ">>, [global]).


get_group_names(Config) ->
    %% tc_group_path contains something like:
    %% [[{name,muc_rsm_all},parallel],
    %%  [{name,rdbms_muc_all},{repeat_until_all_ok,3},parallel]]
    %% Where muc_rsm_all is subgroup of rdbms_muc_all.
    Path = proplists:get_value(tc_group_path, Config, []),
    TopGroup = proplists:get_value(tc_group_properties, Config, []),
    Names = [ proplists:get_value(name, Props) || Props <- lists:reverse([TopGroup|Path]) ],
    %% Just in case drop undefined
    [Name || Name <- Names, Name =/= undefined].

full_group_name(Config) ->
    %% Groups path, example [main_group, sub_group1, sub_sub_group...]
    Groups = get_group_names(Config),
    join_atoms(Groups).

join_atoms(Atoms) ->
    Strings = [atom_to_list(A) || A <- Atoms],
    list_to_atom(string:join(Strings, ":")).
