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
-record(state, { file, suite, group, limit }).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_markdown_errors_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    File = "/tmp/ct_markdown",
    file:write_file(File, "", []),
    {ok, #state{ file = File, limit = 25 }}.

post_init_per_suite(SuiteName, _Config, Return, State) ->
    State2 = handle_return(SuiteName, '', init_per_suite, Return, State),
    {Return, State2#state{group = '', suite = SuiteName}}.

post_init_per_group(GroupName, _Config, Return, State=#state{suite = SuiteName}) ->
    State2 = handle_return(SuiteName, GroupName, init_per_group, Return, State),
    {Return, State2#state{group = GroupName}}.

post_init_per_testcase(TC, _Config, Return, State=#state{group = GroupName,
                                                         suite = SuiteName}) ->
    State2 = handle_return(SuiteName, GroupName, TC, Return, State),
    {Return, State2}.

post_end_per_suite(SuiteName, _Config, Return, State) ->
    State2 = handle_return(SuiteName, '', end_per_suite, Return, State),
    {Return, State2#state{suite = '', group = ''}}.

post_end_per_group(GroupName, _Config, Return, State=#state{suite = SuiteName}) ->
    State2 = handle_return(SuiteName, GroupName, end_per_group, Return, State),
    {Return, State2#state{group = ''}}.

%% @doc Called after each test case.
post_end_per_testcase(TC, _Config, Return, State=#state{group = GroupName,
                                                        suite = SuiteName}) ->
    State2 = handle_return(SuiteName, GroupName, TC, Return, State),
    {Return, State2}.

handle_return(SuiteName, GroupName, Place, Return, State) ->
    try handle_return_unsafe(SuiteName, GroupName, Place, Return, State)
    catch Class:Error ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("issue=handle_return_unsafe_failed reason=~p:~p~n"
               "stacktrace=~p", [Class, Error, Stacktrace]),
        State
    end.

handle_return_unsafe(SuiteName, GroupName, Place, Return, State) ->
    case to_error_message(Return) of
        ok ->
            State;
        Error ->
            F = fun() ->
                log_error(SuiteName, GroupName, Place, Error, State)
                end,
            exec_limited_number_of_times(F, State)
    end.

exec_limited_number_of_times(F, State=#state{limit=0, file=File}) ->
    %% Log truncated
    file:write_file(File, ".", [append]),
    State;
exec_limited_number_of_times(F, State=#state{limit=Limit}) ->
    F(),
    State#state{limit=Limit-1}.

log_error(SuiteName, GroupName, Place, Error, #state{file = File}) ->
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
            "\n\n```erlang\n", Content/binary, "\n```\n</details>\n">>,
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
