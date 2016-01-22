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
-record(state, { node, cookie, reader, writer, current_line_num, out_file, url_file }).
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
    {Config, State}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(TC,Config,State=#state{}) ->
    State2 = ensure_initialized(Config, State),
    State3 = pre_insert_line_numbers_into_report(State2),
    {Config, State3 }.

%% @doc Called after each test case.
post_end_per_testcase(TC,_Config,Return,State) ->
    State2 = post_insert_line_numbers_into_report(State, TC),
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
    ReaderNode = choose_reader_node(Node),
    %% Regular rpc:call/4 spawn a process
    rpc:block_call(ReaderNode, file, open, [AbsName, [read, binary]], 5000).

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
        _ -> % ignore errors
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
    %% Remove: *SUITE.logs/run.*/log_private/
    RunDir = filename:absname(filename:join([PrivDir, "..", "..", ".."])),
    File = atom_to_list(Node) ++ ".log.html",
    %% On disk
    OutFile = filename:join(RunDir, File),
    %% In browser
    UrlFile = ct_logs:uri(filename:join(["..", "..", File])),
    case spawn_log_reader(Node, Cookie) of
        {ok, Reader} ->
            %% self() process is temporary
            {ok, Writer} = open_out_file(OutFile),
            file:write(Writer, "<pre>"),
            CurrentLineNum = read_and_write_lines(Node, Reader, Writer, 0),
            ct:pal("issue=\"ct_mongoose_log_hook created log file\", "
                   "reader=~p, writer=~p, out_file=~p", [Reader, Writer, OutFile]),
            State#state{reader=Reader, writer=Writer, out_file=OutFile,
                        current_line_num=CurrentLineNum, url_file=UrlFile};
        _ ->
            ct:pal("Failed to init ct_mongoose_log_hook for ~p", [Node]),
            State#state{out_file=OutFile}
    end;
ensure_initialized(_Config, State=#state{}) ->
    State.

pre_insert_line_numbers_into_report(State=#state{writer=undefined}) ->
    State; % Invalid state
pre_insert_line_numbers_into_report(State=#state{node=Node, reader=Reader, writer=Writer,
                                             current_line_num=CurrentLineNum, url_file=UrlFile}) ->
    CurrentLineNum2 = read_and_write_lines(Node, Reader, Writer, CurrentLineNum),
    Heading = atom_to_list(Node),
    URL = UrlFile ++ "#L" ++ integer_to_list(CurrentLineNum2+1),
    ct_logs:log(Heading, "<a href=\"~ts\">~p#~p</a>\n", [URL, Node, CurrentLineNum2+1]),
    Same = CurrentLineNum =:= CurrentLineNum2,
    case Same of
        true ->
            skip;
        _ ->
            file:write(Writer, "<hr/>")
    end,
    State#state{current_line_num=CurrentLineNum2}.

post_insert_line_numbers_into_report(State=#state{writer=undefined}, _TC) ->
    State; % Invalid state
post_insert_line_numbers_into_report(State=#state{node=Node, reader=Reader, writer=Writer,
                                             current_line_num=CurrentLineNum, url_file=UrlFile}, TC) ->
    CurrentLineNum2 = read_and_write_lines(Node, Reader, Writer, CurrentLineNum),
    Heading = atom_to_list(Node),
    URL = UrlFile ++ "#L" ++ integer_to_list(CurrentLineNum2),
    Same = CurrentLineNum =:= CurrentLineNum2,
    case Same of
        true ->
            skip;
        _ ->
            ct_logs:log(Heading, "<a href=\"~ts\">~p#~p (new log lines)</a>\n",
                        [URL, Node, CurrentLineNum2]),
            %% Write a message after the main part
            MessageIfNotEmpty = io_lib:format("^^^^^^^^^^~p^^^^^^^^^^~n", [TC]),
            file:write(Writer, MessageIfNotEmpty),
            file:write(Writer, "<hr/>")
    end,
    State#state{current_line_num=CurrentLineNum2}.

open_out_file(OutFile) ->
    Self = self(),
    Ref = make_ref(),
    ProxyPid = spawn_link(fun() -> Res = file:open(OutFile, [write]),
                                   Self ! {Ref, Res},
                                   receive stop -> stop end
                          end),
    Res1 = receive {Ref, Res} -> Res
           after 5000 -> erlang:error({open_file_timeout, OutFile}) end,
    unlink(ProxyPid),
    Res1.

are_nodes_from_same_host(Node1, Node2) ->
    node_to_host(Node1) =:= node_to_host(Node2).

node_to_host(Node) when is_atom(Node) ->
    [$@|Host] = lists:dropwhile(fun(X) -> X =/= $@ end, atom_to_list(Node)),
    list_to_atom(Host).
