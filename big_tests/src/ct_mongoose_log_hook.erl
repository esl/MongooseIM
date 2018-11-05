%%% @doc Copy ejabberd.log into CT reports
-module(ct_mongoose_log_hook).

%% @doc Add the following line in your *.spec file to
%% copy ejabberd.log into CT reports:
%% {ct_hooks, [ct_mongoose_log_hook]}.

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).
-record(state, { node, cookie, reader, writer,
                 current_line_num, out_file, url_file, group, suite,
                 priv_dir }).
-include_lib("exml/include/exml.hrl").

%% @doc Return a unique id for this CTH.
id([Node, _Cookie]) ->
    "ct_mongoose_log_hook_" ++ atom_to_list(Node).

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, [Node0, Cookie0]) ->
    Node = ct:get_config(Node0),
    Cookie = ct:get_config(Cookie0),
    {ok, #state{ node=Node, cookie=Cookie }}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,State) ->
    {Config, State#state{group=no_group, suite=Suite}}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State#state{suite=no_suite}}.

%% @doc Called after end_per_suite.
post_end_per_suite(Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(Group,Config,State) ->
    {Config, State#state{group=Group}}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(Group,Config,State) ->
    {Config, State#state{group=no_group}}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(TC,Config,State=#state{}) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    State2 = keep_priv_dir(Config, State),
    State3 = ensure_initialized(Config, State2),
    State4 = pre_insert_line_numbers_into_report(State3, TC),
    test_server:timetrap_cancel(Dog),
    {Config, State4 }.

%% @doc Called after each test case.
post_end_per_testcase(TC,_Config,Return,State) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    State2 = post_insert_line_numbers_into_report(State, TC),
    test_server:timetrap_cancel(Dog),
    {Return, State2 }.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.
on_tc_skip(_TC, Reason, State) ->
    State.

%% @doc Called when the scope of the CTH is done
terminate(State) ->
    ok.


% --------------------------------------------

spawn_log_reader(Node, Cookie) ->
    %% Set cookie permanently
    erlang:set_cookie(Node, Cookie),
    AbsName = rpc:call(Node, filename, absname, ["log/ejabberd.log"], 5000),
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

read_and_write_lines(Node, Reader, Writer, CurrentLineNum)  when is_integer(CurrentLineNum) ->
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
     #xmlel{name = <<"a">>, attrs = [{<<"name">>, NextLinkName}]}].

make_link_name(Line) when is_integer(Line) ->
    <<"L", (list_to_binary(integer_to_list(Line)))/binary>>.

make_content(CurrentLineNum, Line) ->
    << (list_to_binary(integer_to_list(CurrentLineNum)))/binary, " ", Line/binary>>.

ensure_initialized(Config, State=#state{node=Node, cookie=Cookie, out_file=undefined}) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    RunDir = path_helper:ct_run_dir(Config),
    File = atom_to_list(Node) ++ ".log.html",
    %% On disk
    OutFile = filename:join(RunDir, File),
    %% In browser
    UrlFile = ct_logs:uri(filename:join(path_helper:ct_run_dir_in_browser(Config), File)),
    case spawn_log_reader(Node, Cookie) of
        {ok, Reader} ->
            %% self() process is temporary
            {ok, Writer} = open_out_file(OutFile),
            file:write(Writer, "<pre>"),
            CurrentLineNum = read_and_write_lines(Node, Reader, Writer, 0),
            ct:pal("issue=\"ct_mongoose_log_hook created log file\", "
                   "ct_node=~p, reader=~p, reader_node=~p, writer=~p, out_file=~p",
                   [node(), Reader, node(Reader), Writer, OutFile]),
            State#state{reader=Reader, writer=Writer, out_file=OutFile,
                        current_line_num=CurrentLineNum, url_file=UrlFile};
        Reason ->
            ct:pal("issue=\"Failed to init ct_mongoose_log_hook\", node=~p, reason=~p",
                   [Node, Reason]),
            State#state{out_file=OutFile}
    end;
ensure_initialized(_Config, State=#state{}) ->
    State.

keep_priv_dir(Config, State) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    State#state{priv_dir=PrivDir}.

pre_insert_line_numbers_into_report(State=#state{writer=undefined}, _TC) ->
    State; % Invalid state
pre_insert_line_numbers_into_report(State=#state{node=Node, reader=Reader, writer=Writer,
                                             current_line_num=CurrentLineNum, url_file=UrlFile,
                                             priv_dir=PrivDir, group=Group, suite=Suite}, TC) ->
    CurrentLineNum2 = read_and_write_lines(Node, Reader, Writer, CurrentLineNum),
    add_log_link_to_line(PrivDir, UrlFile, CurrentLineNum2, Node, " when started"),
    Message = io_lib:format(
        "<font color=gray>INIT suite=~p group=~p testcase=~p</font>~n",
        [Suite, Group, TC]),
    file:write(Writer, Message),
    State#state{current_line_num=CurrentLineNum2}.

post_insert_line_numbers_into_report(State=#state{writer=undefined}, _TC) ->
    State; % Invalid state
post_insert_line_numbers_into_report(State=#state{node=Node, reader=Reader, writer=Writer,
                                             current_line_num=CurrentLineNum, url_file=UrlFile,
                                             group=Group, suite=Suite, priv_dir=PrivDir}, TC) ->
    CurrentLineNum2 = read_and_write_lines(Node, Reader, Writer, CurrentLineNum),
    Heading = atom_to_list(Node),
    add_log_link_to_line(PrivDir, UrlFile, CurrentLineNum2, Node, " when finished"),
    %% Write a message after the main part
    Message = io_lib:format(
        "<font color=gray>DONE suite=~p group=~p testcase=~p</font>~n",
        [Suite, Group, TC]),
    file:write(Writer, Message),
    State#state{current_line_num=CurrentLineNum2}.

add_log_link_to_line(PrivDir, UrlFile, LogLine, Node, ExtraDescription) ->
    Label = "L" ++ integer_to_list(LogLine),
    Heading = "View log from node " ++ atom_to_list(Node) ++ ExtraDescription,
    %% We need to invent something unique enough here :)
    LinkName = atom_to_list(Node) ++ "_" ++ integer_to_list(LogLine) ++ ".html",
    add_log_link(Heading, PrivDir, LinkName, UrlFile, Label).

%% Function `escalus_ct:add_log_link(Heading, URL, Type).'
%% allows to add simple links.
%% 
%% We can't add link with label (i.e. index.html#LABEL), because it would be escaped.
%% Let's create an HTML file for each link we want to insert, and insert our custom
%% redirect code inside.
%%
%% Args:
%% `Heading' - some description for the link
%% `PrivDir' - current log_private directory
%% `LinkName' - filename where to write our redirect code inside log_private
%% `UrlFile' - destination URL to redirect to
%% `Label' - position in the document
add_log_link(Heading, PrivDir, LinkName, UrlFile, Label) ->
    URL = UrlFile ++ "#" ++ Label,
    RedirectCode = "<meta http-equiv='refresh' content='0; url=../" ++ URL ++ "' />",
    WhereToWrite = filename:join(PrivDir, LinkName),
    file:write_file(WhereToWrite, RedirectCode),
    escalus_ct:add_log_link(Heading, LinkName, "text/html").

open_out_file(OutFile) ->
    open_file_without_linking(node(), OutFile, [write, delayed_write]).

%% @doc Open file. The caller process will not be monitored by file_server.
%% So, the file is not closed in case the parent process dies.
open_file_without_linking(Node, File, Opts) when Node =:= node() ->
    apply_in_new_process(file, open, [File, Opts]);
open_file_without_linking(Node, File, Opts) ->
    %% Regular rpc:call/4 spawns a temporary process which
    %% cause file process termination.
    %% `block_call' is executed inside a long-living process.
    rpc:block_call(Node, file, open, [File, Opts], 5000).

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
