%%% @doc This hook copies mongooseim.log into an html file (with labels),
%%% and inserts links into test case specific html report files. Note that
%%% a temporary html file is created for each link in log_private directory,
%%% for more information see comments for add_log_link_to_line/5 function.
%%%
%%% Hook options must be in proplist format:
%%%   * host - atom, one of the hosts provided in the common test config file.
%%%            Optional parameter, default value: 'mim'
%%%   * log  - list of atoms (suite, group or testcase).
%%%            Optional parameter, default value: [suite]
%%%
%%% examples of the *.spec file configuration:
%%%   * {ct_hooks, [ct_mongoose_log_hook]}.
%%%   * {ct_hooks, [{ct_mongoose_log_hook,[{host, mim2}]}]}.
%%%   * {ct_hooks, [{ct_mongoose_log_hook,[{host, mim3}, {log, [suite, group]}]}]}.
%%%   * {ct_hooks, [{ct_mongoose_log_hook,[{host, fed}, {log, [testcase]}]}]}.
%%%   * {ct_hooks, [{ct_mongoose_log_hook,[{host, reg}, {log, []}]}]}.
%%%
-module(ct_mongoose_log_hook).


%% Callbacks
-export([id/1]).
-export([init/2]).
-export([terminate/1]).

-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-record(state, { print_init_and_done_for_testcases,
                 node_name, reader, writer,
                 current_line_num, out_file, url_file, group, suite,
                 log_flags = [] }).
-include_lib("exml/include/exml.hrl").

%% @doc Return a unique id for this CTH.
id(Opts) ->
    Host = proplists:get_value(host, Opts, mim),
    "ct_mongoose_log_hook_" ++ atom_to_list(Host).

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, Opts) ->
    Node = connect_mim_node(Opts),
    LogFlags = proplists:get_value(log, Opts, [suite]),
    PrintInitDone = proplists:get_value(print_init_and_done_for_testcases, Opts),
    {ok, #state{ node_name=Node, log_flags=LogFlags,
                 print_init_and_done_for_testcases = PrintInitDone }}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(_Suite, Config, State = #state{node_name = undefined}) ->
    {Config, State};
pre_init_per_suite(Suite, Config, State) ->
    maybe_print_log_on_mim_node(suite, starting, Suite, State),
    {Config, State#state{group=no_group, suite=Suite}}.

%% @doc Called before end_per_suite.
post_end_per_suite(_Suite, Config, _Return, State = #state{node_name = undefined}) ->
    {Config, State};
post_end_per_suite(Suite, _Config, Return, State) ->
    maybe_print_log_on_mim_node(suite, finishing, Suite, State),
    {Return, State#state{suite=no_suite}}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group, Config, State = #state{node_name = undefined}) ->
    {Config, State};
pre_init_per_group(Group, Config, State) ->
    maybe_print_log_on_mim_node(group, starting, Group, State),
    {Config, State#state{group=Group}}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group, _Config, Return, State = #state{node_name = undefined}) ->
    {Return, State};
post_end_per_group(Group, _Config, Return, State) ->
    maybe_print_log_on_mim_node(group, finishing, Group, State),
    {Return, State#state{group=no_group}}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC, Config, State = #state{node_name = undefined}) ->
    {Config, State};
pre_init_per_testcase(TC, Config, State=#state{}) ->
    maybe_print_log_on_mim_node(testcase, starting, TC, State),
    Dog = test_server:timetrap(test_server:seconds(10)),
    State3 = ensure_initialized(Config, State),
    State4 = pre_insert_line_numbers_into_report(State3, TC),
    test_server:timetrap_cancel(Dog),
    {Config, State4}.

%% @doc Called after each test case.
post_end_per_testcase(_TC, _Config, Return, State = #state{node_name = undefined}) ->
    {Return, State};
post_end_per_testcase(TC, _Config, Return, State) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    State2 = post_insert_line_numbers_into_report(State, TC),
    test_server:timetrap_cancel(Dog),
    maybe_print_log_on_mim_node(testcase, finishing, TC, State),
    {Return, State2 }.

%% @doc Called when the scope of the CTH is done
terminate(State = #state{node_name = undefined}) ->
    State;
terminate(State) ->
    insert_line_numbers_into_report(State).

% --------------------------------------------

spawn_log_reader(Node) ->
    AbsName = rpc:call(Node, filename, absname, ["log/mongooseim.log.1"], 5000),
    case is_list(AbsName) of
        true ->
            ReaderNode = choose_reader_node(Node),
            open_file_without_linking(ReaderNode, AbsName, [read, binary]);
        _ ->
            {error, {bad_absname, AbsName}}
    end.

%% Optimize reading if on the same host
choose_reader_node(Node) ->
    case are_nodes_from_same_host(Node, node()) of
        true -> node(); %% Both CT and Mongoose are on the same host
        false -> Node   %% Different hosts
    end.

read_new_lines(Reader) ->
    case rpc:call(node(Reader), file, read_line, [Reader], 5000) of
        {ok, Line} ->
            [Line|read_new_lines(Reader)];
        eof ->
            [];
        Other ->
            ct:pal("issue=\"ct_mongoose_log_hook:read_new_lines/1 failed\", reader=~p, reason=~p",
                   [Reader, Other]),
            []
    end.

read_and_write_lines(_Node, Reader, Writer, CurrentLineNum)  when is_integer(CurrentLineNum) ->
    Lines = read_new_lines(Reader),
    write_lines(Lines, CurrentLineNum+1, Writer),
    CurrentLineNum + length(Lines). % new current line

write_lines([Line|Lines], CurrentLineNum, Writer) ->
    Elem = make_elem(CurrentLineNum, Line),
    Data = exml:to_iolist(Elem),
    ok = file:write(Writer, Data),
    write_lines(Lines, CurrentLineNum+1, Writer);
write_lines([], _CurrentLineNum, _Writer) ->
    ok.

make_elem(CurrentLineNum, Line) when is_integer(CurrentLineNum), is_binary(Line) ->
    NextLinkName = make_link_name(CurrentLineNum+1),
    [#xmlcdata{ content = make_content(CurrentLineNum, Line) },
     #xmlel{name = <<"a">>, attrs = #{<<"name">> => NextLinkName}}].

make_link_name(Line) when is_integer(Line) ->
    <<"L", (list_to_binary(integer_to_list(Line)))/binary>>.

make_content(CurrentLineNum, Line) ->
    << (list_to_binary(integer_to_list(CurrentLineNum)))/binary, " ", Line/binary>>.

ensure_initialized(Config, State=#state{node_name=Node, out_file=undefined}) ->
    RunDir = path_helper:ct_run_dir(Config),
    File = atom_to_list(Node) ++ ".log.html",
    %% On disk
    OutFile = filename:join(RunDir, File),
    %% In browser
    UrlFile = ct_logs:uri(filename:join(path_helper:ct_run_dir_in_browser(Config), File)),
    try
        {ok, Reader} = spawn_log_reader(Node),
        %% self() process is temporary
        {ok, Writer} = open_out_file(OutFile),
        file:write(Writer, "<pre>"),
        CurrentLineNum = read_and_write_lines(Node, Reader, Writer, 0),
        ct:pal("issue=\"ct_mongoose_log_hook created log file\", "
               "ct_node=~p, reader=~p, reader_node=~p, writer=~p, out_file=~p",
               [node(), Reader, node(Reader), Writer, OutFile]),
        State#state{reader=Reader, writer=Writer, out_file=OutFile,
                    current_line_num=CurrentLineNum, url_file=UrlFile}
    catch _Class:Reason:Stacktrace ->
            ct:pal("issue=\"Failed to init ct_mongoose_log_hook\"~n node=~p~n "
                   "reason=~p~n stacktrace=~p",
                   [Node, Reason, Stacktrace]),
            State
    end;
ensure_initialized(_Config, State=#state{}) ->
    State.

pre_insert_line_numbers_into_report(State=#state{writer=undefined}, _TC) ->
    State; % Invalid state
pre_insert_line_numbers_into_report(State=#state{node_name=Node, reader=Reader, writer=Writer,
                                             current_line_num=CurrentLineNum, url_file=UrlFile,
                                             group=Group, suite=Suite}, TC) ->
    CurrentLineNum2 = read_and_write_lines(Node, Reader, Writer, CurrentLineNum),
    add_log_link_to_line(UrlFile, CurrentLineNum2, Node, " when started"),
    Message = io_lib:format(
        "<font color=gray>INIT suite=~p group=~p testcase=~p</font>~n",
        [Suite, Group, TC]),
    maybe_file_write(State, Writer, Message),
    State#state{current_line_num=CurrentLineNum2}.

post_insert_line_numbers_into_report(State=#state{writer=undefined}, _TC) ->
    State; % Invalid state
post_insert_line_numbers_into_report(State=#state{node_name=Node, reader=Reader, writer=Writer,
                                             current_line_num=CurrentLineNum, url_file=UrlFile,
                                             group=Group, suite=Suite}, TC) ->
    CurrentLineNum2 = read_and_write_lines(Node, Reader, Writer, CurrentLineNum),
    case CurrentLineNum of
        CurrentLineNum2 ->
            skip; %% Reduce noise in logs because nothing was logged
        _ ->
            add_log_link_to_line(UrlFile, CurrentLineNum2, Node, " when finished")
    end,
    %% Write a message after the main part
    Message = io_lib:format(
        "<font color=gray>DONE suite=~p group=~p testcase=~p</font>~n",
        [Suite, Group, TC]),
    maybe_file_write(State, Writer, Message),
    State#state{current_line_num=CurrentLineNum2}.

insert_line_numbers_into_report(State=#state{node_name=Node, reader=Reader, writer=Writer,
                                             current_line_num=CurrentLineNum}) ->
    CurrentLineNum2 = read_and_write_lines(Node, Reader, Writer, CurrentLineNum),
    State#state{current_line_num=CurrentLineNum2}.

%% Function `escalus_ct:add_log_link(Heading, URL, Type).'
%% allows to add simple links.
%%
%% We can't add link with label (i.e. index.html#LABEL), because it would be escaped.
%%
%% Args:
%% `Heading' - some description for the link
%% `UrlFile' - destination URL to redirect to
%% `Label' - position in the document
add_log_link_to_line(UrlFile, LogLine, Node, ExtraDescription) ->
    Label = "L" ++ integer_to_list(LogLine),
    Heading = "View log from node " ++ atom_to_list(Node) ++ ExtraDescription,
    LinkText = atom_to_list(Node) ++ "#" ++ integer_to_list(LogLine),
    URL = UrlFile ++ "#" ++ Label,
    ct_add_link(Heading, URL, LinkText, "text/html").

%% ct_logs:add_link/3 but without URL escaping
ct_add_link(Heading, URL, LinkText, Type) ->
    Link = io_lib:format(" <a href=\"~ts\" type=~tp>~ts</a>", [URL, Type, LinkText]),
    ct_logs:log(Heading ++ binary_to_list(iolist_to_binary(Link)), "", []).

open_out_file(OutFile) ->
    open_file_without_linking(node(), OutFile, [write, delayed_write]).

%% @doc Open file. The caller process will not be monitored by file_server.
%% So, the file is not closed in case the parent process dies.
open_file_without_linking(Node, File, Opts) when Node =:= node() ->
    Res = apply_in_new_process(file, open, [File, Opts]),
    check_result(Node, File, Res);
open_file_without_linking(Node, File, Opts) ->
    %% Regular rpc:call/4 spawns a temporary process which
    %% cause file process termination.
    %% `block_call' is executed inside a long-living process.
    Res = rpc:block_call(Node, file, open, [File, Opts], 5000),
    check_result(Node, File, Res).

check_result(_Node, _File, {ok, _} = Res) ->
    Res;
check_result(Node, File, Res) ->
    ct:fail({open_file_failed, Node, File, Res}).

%% Spawns a long living process and executes function
apply_in_new_process(M, F, A) ->
    Self = self(),
    Ref = make_ref(),
    ProxyPid = spawn_link(fun() -> Res = erlang:apply(M, F, A),
                                   Self ! {Ref, Res},
                                   receive stop -> stop end
                          end),
    Res1 = receive {Ref, Res} -> Res
           after 5000 -> erlang:error({apply_in_new_process, {M,F,A}}) end,
    unlink(ProxyPid),
    Res1.

are_nodes_from_same_host(Node1, Node2) ->
    compare_host_names(Node1, Node2)
    orelse
    compare_ifs(Node1, Node2).

compare_ifs(Node1, Node2) ->
    {ok, Ifs1} = rpc:call(Node1,inet,getif,[]),
    {ok, Ifs2} = rpc:call(Node2,inet,getif,[]),
    lists:sort(Ifs1) =:= lists:sort(Ifs2).


%% Does not always work. One host can be localhost, another one myhost.
compare_host_names(Node1, Node2) ->
    Host1 = node_to_host(Node1),
    Host2 = node_to_host(Node2),
    Host1 =:= Host2.

node_to_host(Node) when is_atom(Node) ->
    [_Name, Host] = string:tokens(atom_to_list(Node), "@"),
    list_to_atom(Host).

connect_mim_node(HookOpts) ->
    Host = proplists:get_value(host, HookOpts, mim),
    Node = ct:get_config({hosts, Host, node}),
    Cookie = ct:get_config(ejabberd_cookie),
    %% Set cookie permanently
    erlang:set_cookie(Node, Cookie),
    %% this log message lands at misc_io.log.html file
    ct:pal("connecting to the '~p' node (cookie: '~p')", [Node, Cookie]),
    case net_kernel:connect_node(Node) of
        true ->
            Node;
        _ ->
            %% Could happen if we test not with all nodes enabled.
            undefined
    end.

maybe_print_log_on_mim_node(Type, Event, Name, #state{log_flags = LogFlags, node_name = Node}) ->
    ValidEvents = [starting, finishing],
    ValidTypes = [suite, group, testcase],
    case {lists:member(Type, LogFlags),
          lists:member(Event, ValidEvents),
          lists:member(Type, ValidTypes)} of
        {_, _, false} ->
            ct:pal("Invalid logging type: ~p", [Type]);
        {_, false, _} ->
            ct:pal("Invalid logging event: ~p", [Event]);
        {true, _, _} ->
            rpc:call(Node, logger, warning, ["====== ~p ~p ~p", [Event, Name, Type]]);
        _ ->
            ok
    end.

maybe_file_write(#state{print_init_and_done_for_testcases = true}, Writer, Message) ->
    file:write(Writer, Message);
maybe_file_write(_State, _Writer, _Message) ->
    ok.
