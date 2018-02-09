%%%----------------------------------------------------------------------
%%% File    : ejabberd_ctl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd command line admin tool
%%% Created : 11 Jan 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%% @headerfile "ejabberd_ctl.hrl"

%%% @doc Management of mongooseimctl commands and frontend to ejabberd commands.
%%%
%%% An mongooseimctl command is an abstract function identified by a
%%% name, with a defined number of calling arguments, that can be
%%% defined in any Erlang module and executed using mongooseimctl
%%% administration script.
%%%
%%% Note: strings cannot have blankspaces
%%%
%%% Does not support commands that have arguments with ctypes: list, tuple
%%%
%%% TODO: Update the guide
%%% TODO: Mention this in the release notes
%%% Note: the commands with several words use now the underline: _
%%% It is still possible to call the commands with dash: -
%%% but this is deprecated, and may be removed in a future version.


-module(ejabberd_ctl).
-author('alexey@process-one.net').

-export([start/0,
         init/0,
         process/1,
         process2/2,
         register_commands/3,
         unregister_commands/3]).

-include("ejabberd_ctl.hrl").
-include("ejabberd_commands.hrl").
-include("mongoose.hrl").

-type format() :: integer | string | binary | {list, format()}.
-type format_type() :: binary() | string() | char().
-type cmd() :: {CallString :: string(), Args :: [string()], Desc :: string()}.

-define(ASCII_SPACE_CHARACTER, $\s).
-define(PRINT(Format, Args), io:format(lists:flatten(Format), Args)).
-define(TIME_HMS_FORMAT, "~B days ~2.10.0B:~2.10.0B:~2.10.0B").
-define(a2l(A), atom_to_list(A)).

%%-----------------------------
%% Module
%%-----------------------------

-spec start() -> none().
start() ->
    case init:get_plain_arguments() of
        [SNode | Args0] ->
        Args = args_join_xml(args_join_strings(Args0)),
            SNode1 = case string:tokens(SNode, "@") of
                         [_Node, _Server] ->
                             SNode;
                         _ ->
                             case net_kernel:longnames() of
                                 true ->
                                     SNode ++ "@" ++ inet_db:gethostname() ++
                                         "." ++ inet_db:res_option(domain);
                                 false ->
                                     SNode ++ "@" ++ inet_db:gethostname();
                                 _ ->
                                     SNode
                             end
                     end,
            Node = list_to_atom(SNode1),
            Status = case rpc:call(Node, ?MODULE, process, [Args]) of
                         {badrpc, Reason} ->
                             ?PRINT("Failed RPC connection to the node ~p: ~p~n",
                                    [Node, Reason]),
                             %% TODO: show minimal start help
                             ?STATUS_BADRPC;
                         S ->
                             S
                     end,
            halt(Status);
        _ ->
            print_usage(),
            halt(?STATUS_USAGE)
    end.


-spec init() -> atom() | ets:tid().
init() ->
    ets:new(ejabberd_ctl_cmds, [named_table, set, public]),
    ets:new(ejabberd_ctl_host_cmds, [named_table, set, public]).


%%-----------------------------
%% mongooseimctl Command managment
%%-----------------------------

-spec register_commands(CmdDescs :: [tuple()] | tuple(),
                        Module :: atom(),
                        Function :: atom()) -> 'ok'.
register_commands(CmdDescs, Module, Function) ->
    ets:insert(ejabberd_ctl_cmds, CmdDescs),
    ejabberd_hooks:add(ejabberd_ctl_process, global, Module, Function, 50),
    ok.


-spec unregister_commands(CmdDescs :: [any()],
                          Module :: atom(),
                          Function :: atom()) -> 'ok'.
unregister_commands(CmdDescs, Module, Function) ->
    lists:foreach(fun(CmdDesc) ->
                          ets:delete_object(ejabberd_ctl_cmds, CmdDesc)
                  end, CmdDescs),
    ejabberd_hooks:delete(ejabberd_ctl_process, global, Module, Function, 50),
    ok.


%%-----------------------------
%% Process
%%-----------------------------

%% @doc The commands status, stop and restart are defined here to ensure
%% they are usable even if ejabberd is completely stopped.
-spec process(_) -> integer().
process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    MongooseStatus = get_mongoose_status(),
    ?PRINT("~s", [format_status([{node, node()}, {internal_status, InternalStatus},
                                 {provided_status, ProvidedStatus},
                                 {mongoose_status, MongooseStatus},
                                 {os_pid, os:getpid()}, get_uptime(),
                                 {dist_proto, get_dist_proto()},
                                 {logs, get_log_files()}])]),
    case MongooseStatus of
        not_running -> ?STATUS_ERROR;
        {running, _, _Version} -> ?STATUS_SUCCESS
    end;
process(["stop"]) ->
    %%ejabberd_cover:stop(),
    init:stop(),
    ?STATUS_SUCCESS;
process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;
process(["mnesia"]) ->
    ?PRINT("~p~n", [mnesia:system_info(all)]),
    ?STATUS_SUCCESS;
process(["mnesia", "info"]) ->
    mnesia:info(),
    ?STATUS_SUCCESS;
process(["mnesia", Arg]) when is_list(Arg) ->
    case catch mnesia:system_info(list_to_atom(Arg)) of
        {'EXIT', Error} -> ?PRINT("Error: ~p~n", [Error]);
        Return -> ?PRINT("~p~n", [Return])
    end,
    ?STATUS_SUCCESS;


%% @doc The arguments --long and --dual are not documented because they are
%% automatically selected depending in the number of columns of the shell
process(["help" | Mode]) ->
    {MaxC, ShCode} = get_shell_info(),
    case Mode of
        [] ->
            print_usage(dual, MaxC, ShCode),
            ?STATUS_USAGE;
        ["--dual"] ->
            print_usage(dual, MaxC, ShCode),
            ?STATUS_USAGE;
        ["--long"] ->
            print_usage(long, MaxC, ShCode),
            ?STATUS_USAGE;
        ["--tags"] ->
            print_usage_tags(MaxC, ShCode),
            ?STATUS_SUCCESS;
        ["--tags", Tag] ->
            print_usage_tags(Tag, MaxC, ShCode),
            ?STATUS_SUCCESS;
        ["help"] ->
            print_usage_help(MaxC, ShCode),
            ?STATUS_SUCCESS;
        [CmdString | _] ->
            CmdStringU = re:replace(CmdString, "-", "_", [global, {return, list}]),
            print_usage_commands(CmdStringU, MaxC, ShCode),
            ?STATUS_SUCCESS
    end;
process(Args) ->
    AccessCommands = get_accesscommands(),
    {String, Code} = process2(Args, AccessCommands),
    case String of
        [] -> ok;
        _ ->
            io:format("~s~n", [String])
    end,
    Code.


-spec process2(Args :: [string()],
               AccessCommands :: [ejabberd_commands:access_cmd()]
               ) -> {String::string(), Code::integer()}.
process2(["--auth", User, Server, Pass | Args], AccessCommands) ->
    process2(Args, {list_to_binary(User), list_to_binary(Server), list_to_binary(Pass)},
             AccessCommands);
process2(Args, AccessCommands) ->
    process2(Args, noauth, AccessCommands).


%% @private
process2(Args, Auth, AccessCommands) ->
    case try_run_ctp(Args, Auth, AccessCommands) of
        {String, wrong_command_arguments} when is_list(String) ->
            io:format(lists:flatten(["\n" | String]++["\n"])),
            [CommandString | _] = Args,
            process(["help" | [CommandString]]),
            {lists:flatten(String), ?STATUS_ERROR};
        {String, Code} when is_list(String) and is_integer(Code) ->
            {lists:flatten(String), Code};
        String when is_list(String) ->
            {lists:flatten(String), ?STATUS_SUCCESS};
        Other ->
            {"Erroneous result: " ++ io_lib:format("~p", [Other]), ?STATUS_ERROR}
    end.


-spec get_accesscommands() -> [char() | tuple()].
get_accesscommands() ->
    case ejabberd_config:get_local_option(mongooseimctl_access_commands) of
        ACs when is_list(ACs) -> ACs;
        _ -> []
    end.


%%-----------------------------
%% Command calling
%%-----------------------------

-spec try_run_ctp(Args :: [string()],
                  Auth :: ejabberd_commands:auth(),
                  AccessCommands :: [ejabberd_commands:access_cmd()]
                  ) -> string() | integer() | {string(), integer()}.
try_run_ctp(Args, Auth, AccessCommands) ->
    try ejabberd_hooks:run_fold(ejabberd_ctl_process, false, [Args]) of
        false when Args /= [] ->
            try_call_command(Args, Auth, AccessCommands);
        false ->
            print_usage(),
            {"", ?STATUS_USAGE};
        Status ->
            {"", Status}
    catch
        exit:Why ->
            print_usage(),
            {io_lib:format("Error in mongooseimctl process: ~p", [Why]), ?STATUS_USAGE};
        Error:Why ->
            %% In this case probably ejabberd is not started, so let's show Status
            process(["status"]),
            ?PRINT("~n", []),
            {io_lib:format("Error in mongooseimctl process: '~p' ~p", [Error, Why]), ?STATUS_USAGE}
    end.


-spec try_call_command(Args :: [string()],
                       Auth :: ejabberd_commands:auth(),
                       AccessCommands :: [ejabberd_commands:access_cmd()]
                       ) -> string() | integer() | {string(), integer()}.
try_call_command(Args, Auth, AccessCommands) ->
    try call_command(Args, Auth, AccessCommands) of
        {error, command_unknown} ->
            {io_lib:format("Error: command ~p not known.", [hd(Args)]), ?STATUS_ERROR};
        Res ->
            Res
    catch
        A:Why ->
            Stack = erlang:get_stacktrace(),
            {io_lib:format("Problem '~p ~p' occurred executing the command.~nStacktrace: ~p", [A, Why, Stack]), ?STATUS_ERROR}
    end.


-spec call_command(Args :: [string()],
                   Auth :: ejabberd_commands:auth(),
                   AccessCommands :: [ejabberd_commands:access_cmd()]
                   ) -> string() | integer() | {string(), integer()}
                     | {error, any()}.
call_command([CmdString | Args], Auth, AccessCommands) ->
    CmdStringU = re:replace(CmdString, "-", "_", [global, {return, list}]),
    Command = list_to_atom(CmdStringU),
    case ejabberd_commands:get_command_format(Command) of
        {error, command_unknown} ->
            {error, command_unknown};
        {ArgsFormat, ResultFormat} ->
            case (catch format_args(Args, ArgsFormat)) of
                ArgsFormatted when is_list(ArgsFormatted) ->
                    Result = ejabberd_commands:execute_command(AccessCommands, Auth, Command,
                                                               ArgsFormatted),
                    format_result(Result, ResultFormat);
                {'EXIT', {function_clause, [{lists, zip, [A1, A2], _FileInfo} | _]}} ->
                    {NumCompa, TextCompa} =
                        case {length(A1), length(A2)} of
                            {L1, L2} when L1 < L2 -> {L2-L1, "less argument"};
                            {L1, L2} when L1 > L2 -> {L1-L2, "more argument"}
                        end,
                    {io_lib:format("Error: the command ~p requires ~p ~s.",
                                   [CmdString, NumCompa, TextCompa]),
                     wrong_command_arguments}
            end
    end.


%%-----------------------------
%% Format arguments
%%-----------------------------

%% @private
-spec args_join_xml([string()]) -> [string()].
args_join_xml([]) ->
    [];
args_join_xml([ [ $< | _ ] = Arg | RArgs ]) ->
    case bal(Arg, $<, $>) of
        true ->
            [Arg | args_join_xml(RArgs)];
        false ->
            [NextArg | RArgs1] = RArgs,
            args_join_xml([Arg ++ " " ++ NextArg | RArgs1])
    end;
args_join_xml([ Arg | RArgs ]) ->
    [ Arg | args_join_xml(RArgs) ].


%% @private
-spec args_join_strings([string()]) -> [string()].
args_join_strings([]) ->
    [];
args_join_strings([ "\"", NextArg | RArgs ]) ->
    args_join_strings([ "\"" ++ NextArg | RArgs ]);
args_join_strings([ [ $" | _ ] = Arg | RArgs ]) ->
    case lists:nthtail(length(Arg)-2, Arg) of
        [C1, $"] when C1 /= ?ASCII_SPACE_CHARACTER ->
            [ string:substr(Arg, 2, length(Arg)-2) | args_join_strings(RArgs) ];
        _ ->
            [NextArg | RArgs1] = RArgs,
            args_join_strings([Arg ++ " " ++ NextArg | RArgs1])
    end;
args_join_strings([ Arg | RArgs ]) ->
    [ Arg | args_join_strings(RArgs) ].


%% @private
-spec bal(string(), char(), char()) -> boolean().
bal(String, Left, Right) ->
    bal(String, Left, Right, 0).


%% @private
-spec bal(string(), L :: char(), R :: char(), Bal :: integer()) -> boolean().
bal([], _Left, _Right, Bal) ->
    Bal == 0;
bal([?ASCII_SPACE_CHARACTER, _NextChar | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal);
bal([Left | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal-1);
bal([Right | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal+1);
bal([_C | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal).


%% @private
-spec format_args(Args :: [any()],
                  ArgsFormat :: [format()]) -> [any()].
format_args(Args, ArgsFormat) ->
    lists:foldl(
      fun({{_ArgName, ArgFormat}, Arg}, Res) ->
              Formatted = format_arg(Arg, ArgFormat),
              Res ++ [Formatted]
      end,
      [],
      lists:zip(ArgsFormat, Args)).


%% @private
-spec format_arg(string(), format()) -> format_type().
format_arg(Arg, integer) ->
    format_arg2(Arg, "~d");
format_arg("", string) ->
    "";
format_arg(Arg, string) ->
    NumChars = integer_to_list(string:len(Arg)),
    Parse = "~" ++ NumChars ++ "c",
    format_arg2(Arg, Parse);
format_arg(Arg, binary) ->
    list_to_binary(format_arg(Arg, string));
format_arg(Arg, {list, Type}) ->
    [format_arg(Token, Type) || Token <- string:tokens(Arg, ";")].


%% @private
-spec format_arg2(Arg :: string(),
                  Parse :: nonempty_string()
                  ) -> [[any()] | char()] | char().
format_arg2(Arg, Parse)->
    {ok, [Arg2], _RemainingArguments} = io_lib:fread(Parse, Arg),
    Arg2.

%%-----------------------------
%% Format result
%%-----------------------------
-spec format_result(In :: tuple() | atom() | integer() | string() | binary(),
                    {_, 'atom'|'integer'|'string'|'binary'}
                    ) -> string() | {string(), _}.
format_result({error, ErrorAtom}, _) ->
    {io_lib:format("Error: ~p", [ErrorAtom]), make_status(error)};
format_result(Atom, {_Name, atom}) ->
    io_lib:format("~p", [Atom]);
format_result(Int, {_Name, integer}) ->
    io_lib:format("~p", [Int]);
format_result(String, {_Name, string}) ->
    io_lib:format("~s", [String]);
format_result(Binary, {_Name, binary}) ->
    io_lib:format("~s", [Binary]);
format_result(Code, {_Name, rescode}) ->
    {"", make_status(Code)};
format_result({Code, Text}, {_Name, restuple}) ->
    {io_lib:format("~s", [Text]), make_status(Code)};
%% The result is a list of something: [something()]
format_result([], {_Name, {list, _ElementsDef}}) ->
    "";
format_result([FirstElement | Elements], {_Name, {list, ElementsDef}}) ->
    %% Start formatting the first element
    [format_result(FirstElement, ElementsDef) |
     %% If there are more elements, put always first a newline character
     lists:map(
       fun(Element) ->
               ["\n" | format_result(Element, ElementsDef)]
       end,
       Elements)];
%% The result is a tuple with several elements: {something1(), something2(), ...}
%% NOTE: the elements in the tuple are separated with tabular characters,
%% if a string is empty, it will be difficult to notice in the shell,
%% maybe a different separation character should be used, like ;;?
format_result(ElementsTuple, {_Name, {tuple, ElementsDef}}) ->
    ElementsList = tuple_to_list(ElementsTuple),
    [{FirstE, FirstD} | ElementsAndDef] = lists:zip(ElementsList, ElementsDef),
    [format_result(FirstE, FirstD) |
     lists:map(
       fun({Element, ElementDef}) ->
               ["\t" | format_result(Element, ElementDef)]
       end,
       ElementsAndDef)].


-spec make_status(ok | true | _) -> 0 | 1.
make_status(ok) -> ?STATUS_SUCCESS;
make_status(true) -> ?STATUS_SUCCESS;
make_status(_Error) -> ?STATUS_ERROR.


-spec get_list_commands()
      -> [{Call :: string(), Args :: [string()], Desc :: string()}].
get_list_commands() ->
    try ejabberd_commands:list_commands() of
        Commands ->
            [tuple_command_help(Command)
             || {N, _, _}=Command <- Commands,
                %% Don't show again those commands, because they are already
                %% announced by ejabberd_ctl itself
                N /= status, N /= stop, N /= restart]
    catch
        exit:_ ->
            []
    end.


-spec tuple_command_help(ejabberd_commands:list_cmd()) -> cmd().
tuple_command_help({Name, Args, Desc}) ->
    Arguments = [atom_to_list(ArgN) || {ArgN, _ArgF} <- Args],
    CallString = atom_to_list(Name),
    {CallString, Arguments, Desc}.


-spec get_list_ctls() -> [cmd()].
get_list_ctls() ->
    case catch ets:tab2list(ejabberd_ctl_cmds) of
        {'EXIT', _} -> [];
        Cs -> [{NameArgs, [], Desc} || {NameArgs, Desc} <- Cs]
    end.

format_status([{node, Node}, {internal_status, IS}, {provided_status, PS},
               {mongoose_status, MS}, {os_pid, OSPid}, {uptime, UptimeHMS},
               {dist_proto, DistProto}, {logs, LogFiles}]) ->
    ( ["MongooseIM node ", ?a2l(Node), ":\n",
       "    operating system pid: ", OSPid, "\n",
       "    Erlang VM status: ", ?a2l(IS), " (of: starting | started | stopping)\n",
       "    boot script status: ", io_lib:format("~p", [PS]), "\n",
       "    version: ", case MS of
                          {running, App, Version} -> [Version, " (as ", ?a2l(App), ")"];
                          not_running -> "unavailable - neither ejabberd nor mongooseim is running"
                        end, "\n",
       "    uptime: ", io_lib:format(?TIME_HMS_FORMAT, UptimeHMS), "\n",
       "    distribution protocol: ", DistProto, "\n"] ++
      ["    logs: none - maybe enable logging to a file in app.config?\n" || LogFiles == [] ] ++
      ["    logs:\n" || LogFiles /= [] ] ++ [
      ["        ", LogFile, "\n"] || LogFile <- LogFiles ] ).

%%-----------------------------
%% Print help
%%-----------------------------

%% Bold
-define(B1, "\e[1m").
-define(B2, "\e[22m").
-define(B(S), case ShCode of true -> [?B1, S, ?B2]; false -> S end).

%% Underline
-define(U1, "\e[4m").
-define(U2, "\e[24m").
-define(U(S), case ShCode of true -> [?U1, S, ?U2]; false -> S end).

print_usage() ->
    {MaxC, ShCode} = get_shell_info(),
    print_usage(dual, MaxC, ShCode).


-spec print_usage('dual' | 'long'
                , MaxC :: integer()
                , ShCode :: boolean()) -> 'ok'.
print_usage(HelpMode, MaxC, ShCode) ->
    AllCommands =
        [
         {"status", [], "Get MongooseIM status"},
         {"stop", [], "Stop MongooseIM"},
         {"restart", [], "Restart MongooseIM"},
         {"help", ["[--tags [tag] | com?*]"], "Show help (try: mongooseimctl help help)"},
         {"mnesia", ["[info]"], "show information of Mnesia system"}] ++
        get_list_commands() ++
        get_list_ctls(),

    ?PRINT(
       ["Usage: ", ?B("mongooseimctl"), " [--node ", ?U("nodename"), "] [--auth ",
        ?U("user"), " ", ?U("host"), " ", ?U("password"), "] ",
        ?U("command"), " [", ?U("options"), "]\n"
        "\n"
        "Available commands in this MongooseIM node:\n"], []),
    print_usage_commands(HelpMode, MaxC, ShCode, AllCommands),
    ?PRINT(
       ["\n"
        "Examples:\n"
        "  mongooseimctl restart\n"
        "  mongooseimctl --node mongooseim@host restart\n"],
       []).


-spec print_usage_commands(HelpMode :: 'dual' | 'long',
                           MaxC :: integer(),
                           ShCode :: boolean(),
                           Commands :: [cmd(), ...]) -> 'ok'.
print_usage_commands(HelpMode, MaxC, ShCode, Commands) ->
    CmdDescsSorted = lists:keysort(1, Commands),

    %% What is the length of the largest command?
    {CmdArgsLenDescsSorted, Lens} =
        lists:mapfoldl(
          fun({Cmd, Args, Desc}, Lengths) ->
                  Len =
                      length(Cmd) +
                      lists:foldl(fun(Arg, R) ->
                                          R + 1 + length(Arg)
                                  end,
                                  0,
                                  Args),
                  {{Cmd, Args, Len, Desc}, [Len | Lengths]}
          end,
          [],
          CmdDescsSorted),
    MaxCmdLen = case Lens of
                    [] -> 80;
                    _ -> lists:max(Lens)
                end,

    %% For each command in the list of commands
    %% Convert its definition to a line
    FmtCmdDescs = format_command_lines(CmdArgsLenDescsSorted, MaxCmdLen, MaxC, ShCode, HelpMode),

    ?PRINT([FmtCmdDescs], []).


%% @doc Get some info about the shell: how many columns of width and guess if
%% it supports text formatting codes.
-spec get_shell_info() -> {integer(), boolean()}.
get_shell_info() ->
    case io:columns() of
        {ok, C} -> {C-2, true};
        {error, enotsup} -> {78, false}
    end.


%% @doc Split this command description in several lines of proper length
-spec prepare_description(DescInit :: non_neg_integer(),
                          MaxC :: integer(),
                          Desc :: string()) -> [[[any()]], ...].
prepare_description(DescInit, MaxC, Desc) ->
    Words = string:tokens(Desc, " "),
    prepare_long_line(DescInit, MaxC, Words).


-spec prepare_long_line(DescInit :: non_neg_integer(),
                        MaxC :: integer(),
                        Words :: [nonempty_string()]
                        ) -> [[[any()]], ...].
prepare_long_line(DescInit, MaxC, Words) ->
    MaxSegmentLen = MaxC - DescInit,
    MarginString = lists:duplicate(DescInit, ?ASCII_SPACE_CHARACTER), % Put spaces
    [FirstSegment | MoreSegments] = split_desc_segments(MaxSegmentLen, Words),
    MoreSegmentsMixed = mix_desc_segments(MarginString, MoreSegments),
    [FirstSegment | MoreSegmentsMixed].


-spec mix_desc_segments(MarginStr :: [any()],
                        Segments :: [[[any(), ...]]]) -> [[[any()], ...]].
mix_desc_segments(MarginString, Segments) ->
    [["\n", MarginString, Segment] || Segment <- Segments].

split_desc_segments(MaxL, Words) ->
    join(MaxL, Words).


%% @doc Join words in a segment, but stop adding to a segment if adding this
%% word would pass L
-spec join(L :: number(), Words :: [nonempty_string()]) -> [[[any(), ...]], ...].
join(L, Words) ->
    join(L, Words, 0, [], []).


-spec join(L :: number(),
           Words :: [nonempty_string()],
           LenLastSeg :: non_neg_integer(),
           LastSeg :: [nonempty_string()],
           ResSeg :: [[[any(), ...]]] ) -> [[[any(), ...]], ...].
join(_L, [], _LenLastSeg, LastSeg, ResSeg) ->
    ResSeg2 = [lists:reverse(LastSeg) | ResSeg],
    lists:reverse(ResSeg2);
join(L, [Word | Words], LenLastSeg, LastSeg, ResSeg) ->
    LWord = length(Word),
    case LWord + LenLastSeg < L of
        true ->
            %% This word fits in the last segment
            %% If this word ends with "\n", reset column counter
            case string:str(Word, "\n") of
                0 ->
                    join(L, Words, LenLastSeg+LWord+1, [" ", Word | LastSeg], ResSeg);
                _ ->
                    join(L, Words, LWord+1, [" ", Word | LastSeg], ResSeg)
            end;
        false ->
            join(L, Words, LWord, [" ", Word], [lists:reverse(LastSeg) | ResSeg])
    end.


-spec format_command_lines(CALD :: [{[any()], [any()], number(), _}, ...],
                           MaxCmdLen :: integer(),
                           MaxC :: integer(),
                           ShCode :: boolean(),
                           'dual' | 'long') -> [[any(), ...], ...].
format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, dual)
  when MaxC - MaxCmdLen < 40 ->
    %% If the space available for descriptions is too narrow, enforce long help mode
    format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, long);
format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, dual) ->
    lists:map(
      fun({Cmd, Args, CmdArgsL, Desc}) ->
              DescFmt = prepare_description(MaxCmdLen+4, MaxC, Desc),
              ["  ", ?B(Cmd), " ", [[?U(Arg), " "] || Arg <- Args], string:chars(?ASCII_SPACE_CHARACTER, MaxCmdLen - CmdArgsL + 1),
               DescFmt, "\n"]
      end, CALD);
format_command_lines(CALD, _MaxCmdLen, MaxC, ShCode, long) ->
    lists:map(
      fun({Cmd, Args, _CmdArgsL, Desc}) ->
              DescFmt = prepare_description(8, MaxC, Desc),
              ["\n  ", ?B(Cmd), " ", [[?U(Arg), " "] || Arg <- Args], "\n", "        ",
               DescFmt, "\n"]
      end, CALD).


%%-----------------------------
%% Print Tags
%%-----------------------------

print_usage_tags(MaxC, ShCode) ->
    ?PRINT("Available tags and commands:", []),
    TagsCommands = ejabberd_commands:get_tags_commands(),
    lists:foreach(
      fun({Tag, Commands} = _TagCommands) ->
              ?PRINT(["\n\n  ", ?B(Tag), "\n     "], []),
              Words = lists:sort(Commands),
              Desc = prepare_long_line(5, MaxC, Words),
              ?PRINT(Desc, [])
      end,
      TagsCommands),
    ?PRINT("\n\n", []).


print_usage_tags(Tag, MaxC, ShCode) ->
    ?PRINT(["Available commands with tag ", ?B(Tag), ":", "\n"], []),
    HelpMode = long,
    TagsCommands = ejabberd_commands:get_tags_commands(),
    CommandsNames = case lists:keysearch(Tag, 1, TagsCommands) of
                        {value, {Tag, CNs}} -> CNs;
                        false -> []
                    end,
    CommandsList = lists:map(
                     fun(NameString) ->
                             C = ejabberd_commands:get_command_definition(list_to_atom(NameString)),
                             #ejabberd_commands{name = Name,
                                                args = Args,
                                                desc = Desc} = C,
                             tuple_command_help({Name, Args, Desc})
                     end,
                     CommandsNames),
    print_usage_commands(HelpMode, MaxC, ShCode, CommandsList),
    ?PRINT("\n", []).


%%-----------------------------
%% Print usage of 'help' command
%%-----------------------------

print_usage_help(MaxC, ShCode) ->
    LongDesc =
        ["The special 'help' mongooseimctl command provides help of MongooseIM commands.\n\n"
         "The format is:\n  ", ?B("mongooseimctl"), " ", ?B("help"), " [", ?B("--tags"), " ", ?U("[tag]"), " | ", ?U("com?*"), "]\n\n"
         "The optional arguments:\n"
         "  ", ?B("--tags"), "      Show all tags and the names of commands in each tag\n"
         "  ", ?B("--tags"), " ", ?U("tag"), "  Show description of commands in this tag\n"
         "  ", ?U("command"), "     Show detailed description of the command\n"
         "  ", ?U("com?*"), "       Show detailed description of commands that match this glob.\n"
         "              You can use ? to match a simple character, \n"
         "              and * to match several characters.\n"
         "\n",
         "Some example usages:\n",
         " mongooseimctl help\n",
         " mongooseimctl help --tags\n",
         " mongooseimctl help --tags accounts\n",
         " mongooseimctl help register\n",
         " mongooseimctl help regist*\n",
         "\n",
         "Please note that 'mongooseimctl help' shows all MongooseIM commands, \n",
         "even those that cannot be used in the shell with mongooseimctl.\n",
         "Those commands can be identified because the description starts with: *"],
    ArgsDef = [],
    C = #ejabberd_commands{
      name = help,
      desc = "Show help of MongooseIM commands",
      longdesc = lists:flatten(LongDesc),
      args = ArgsDef,
      module = none,
      function = none,
      result = {help, string}},
    print_usage_command("help", C, MaxC, ShCode).


%%-----------------------------
%% Print usage command
%%-----------------------------

%% @spec (CmdSubString::string(), MaxC::integer(), ShCode::boolean()) -> ok
print_usage_commands(CmdSubString, MaxC, ShCode) ->
    %% Get which command names match this substring
    AllCommandsNames = [atom_to_list(Name) || {Name, _, _} <- ejabberd_commands:list_commands()],
    Cmds = filter_commands(AllCommandsNames, CmdSubString),
    case Cmds of
        [] -> io:format("Error: not command found that match: ~p~n", [CmdSubString]);
        _ -> print_usage_commands2(lists:sort(Cmds), MaxC, ShCode)
    end.


print_usage_commands2(Cmds, MaxC, ShCode) ->
    %% Then for each one print it
    lists:mapfoldl(
      fun(Cmd, Remaining) ->
              print_usage_command(Cmd, MaxC, ShCode),
              case Remaining > 1 of
                  true -> ?PRINT([" ", lists:duplicate(MaxC, 126), " \n"], []);
                  false -> ok
              end,
              {ok, Remaining-1}
      end,
      length(Cmds),
      Cmds).


filter_commands(All, SubString) ->
    case lists:member(SubString, All) of
        true -> [SubString];
        false -> filter_commands_regexp(All, SubString)
    end.


filter_commands_regexp(All, Glob) ->
    RegExp = xmerl_regexp:sh_to_awk(Glob),
    lists:filter(
      fun(Command) ->
              case re:run(Command, RegExp, [{capture, none}]) of
              match ->
                  true;
              nomatch ->
                  false
              end
      end,
      All).


%% @spec (Cmd::string(), MaxC::integer(), ShCode::boolean()) -> ok
print_usage_command(Cmd, MaxC, ShCode) ->
    Name = list_to_atom(Cmd),
    case ejabberd_commands:get_command_definition(Name) of
        command_not_found ->
            io:format("Error: command ~p not known.~n", [Cmd]);
        C ->
            print_usage_command(Cmd, C, MaxC, ShCode)
    end.


print_usage_command(Cmd, C, MaxC, ShCode) ->
    #ejabberd_commands{
                     tags = TagsAtoms,
                     desc = Desc,
                     longdesc = LongDesc,
                     args = ArgsDef,
                     result = ResultDef} = C,

    NameFmt = ["  ", ?B("Command Name"), ": ", Cmd, "\n"],

    %% Initial indentation of result is 13 = length("  Arguments: ")
    Args = [format_usage_ctype(ArgDef, 13) || ArgDef <- ArgsDef],
    ArgsMargin = lists:duplicate(13, ?ASCII_SPACE_CHARACTER),
    ArgsListFmt = case Args of
                      [] -> "\n";
                      _ -> [ [Arg, "\n", ArgsMargin] || Arg <- Args]
                  end,
    ArgsFmt = ["  ", ?B("Arguments"), ": ", ArgsListFmt],

    %% Initial indentation of result is 11 = length("  Returns: ")
    ResultFmt = format_usage_ctype(ResultDef, 11),
    ReturnsFmt = ["  ", ?B("Returns"), ": ", ResultFmt],

    TagsFmt = ["  ", ?B("Tags"), ": ", prepare_long_line(8, MaxC, [atom_to_list(TagA) || TagA <- TagsAtoms])],

    DescFmt = ["  ", ?B("Description"), ": ", prepare_description(15, MaxC, Desc)],

    LongDescFmt = case LongDesc of
                      "" -> "";
                      _ -> ["", prepare_description(0, MaxC, LongDesc), "\n\n"]
                  end,

    ?PRINT(["\n", NameFmt, "\n", ArgsFmt, "\n", ReturnsFmt, "\n\n", TagsFmt, "\n\n", DescFmt, "\n\n", LongDescFmt], []).


format_usage_ctype(Type, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==rescode) or (Type==restuple) or (Type==binary)->
    io_lib:format("~p", [Type]);
format_usage_ctype({Name, Type}, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==rescode) or (Type==restuple) or (Type==binary)->
    io_lib:format("~p::~p", [Name, Type]);
format_usage_ctype({Name, {list, ElementDef}}, Indentation) ->
    NameFmt = atom_to_list(Name),
    Indentation2 = Indentation + length(NameFmt) + 4,
    ElementFmt = format_usage_ctype(ElementDef, Indentation2),
    [NameFmt, "::[ ", ElementFmt, " ]"];
format_usage_ctype({Name, {tuple, ElementsDef}}, Indentation) ->
    NameFmt = atom_to_list(Name),
    Indentation2 = Indentation + length(NameFmt) + 4,
    ElementsFmt = format_usage_tuple(ElementsDef, Indentation2),
    [NameFmt, "::{ " | ElementsFmt].


format_usage_tuple([], _Indentation) ->
    [];
format_usage_tuple([ElementDef], Indentation) ->
    [format_usage_ctype(ElementDef, Indentation), " }"];
format_usage_tuple([ElementDef | ElementsDef], Indentation) ->
    ElementFmt = format_usage_ctype(ElementDef, Indentation),
    MarginString = lists:duplicate(Indentation, ?ASCII_SPACE_CHARACTER), % Put spaces
    [ElementFmt, ", \n", MarginString, format_usage_tuple(ElementsDef, Indentation)].

get_mongoose_status() ->
    case lists:keyfind(mongooseim, 1, application:which_applications()) of
        false ->
            not_running;
        {_, _, Version} ->
            {running, mongooseim, Version}
    end.

get_uptime() ->
    {MilliSeconds, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(MilliSeconds div 1000),
    {uptime, [D, H, M, S]}.

get_dist_proto() ->
    %% See kernel/src/net_kernel.erl
    case init:get_argument(proto_dist) of
        {ok, [Proto]} -> Proto;
        _ -> "inet_tcp"
    end.

%%-----------------------------
%% Lager specific helpers
%%-----------------------------

get_log_files() ->
    Handlers = case catch sys:get_state(lager_event) of
                   {'EXIT', _} -> [];
                   Hs when is_list(Hs) -> Hs
               end,
    [ file_backend_path(State)
      || {lager_file_backend, _File, State} <- Handlers ].

file_backend_path(LagerFileBackendState) when element(1, LagerFileBackendState) =:= state ->
    element(2, LagerFileBackendState).
