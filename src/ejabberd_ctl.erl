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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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


-module(ejabberd_ctl).
-author('alexey@process-one.net').

-export([start/0,
         process/1]).

-ignore_xref([process/1, start/0]).

-include("ejabberd_ctl.hrl").
-include("mongoose_logger.hrl").

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
        [SNode | Args] ->
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
                                 {logs, mongoose_logs:get_log_files()}])]),
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
process(["join_cluster", Arg]) when is_list(Arg) ->
    join_cluster(Arg);
process(["join_cluster" | _]) ->
    cluster_command_usage();
process(["leave_cluster"]) ->
    leave_cluster();
process(["remove_from_cluster", Arg]) when is_list(Arg) ->
    remove_from_cluster(Arg);
process(["remove_from_cluster" | _]) ->
    cluster_command_usage();
process(["graphql", Arg]) when is_list(Arg) ->
    Doc = list_to_binary(Arg),
    Ep = mongoose_graphql:get_endpoint(admin),
    Result = mongoose_graphql:execute_cli(Ep, undefined, Doc),
    handle_graphql_result(Result);
process(["graphql" | _]) ->
    ?PRINT("This command requires one string type argument!\n", []),
    ?STATUS_ERROR;
process(Args) ->
    case mongoose_graphql_commands:process(Args) of
        #{status := executed, result := Result} ->
            handle_graphql_result(Result);
        #{status := error, reason := no_args} = Ctx ->
            print_usage(Ctx),
            ?STATUS_USAGE;
        #{status := error} = Ctx ->
            ?PRINT(error_message(Ctx) ++ "\n\n", []),
            print_usage(Ctx),
            ?STATUS_ERROR;
        #{status := usage} = Ctx ->
            print_usage(Ctx),
            ?STATUS_SUCCESS % not STATUS_USAGE, as that would tell the script to print general help
    end.

-spec error_message(mongoose_graphql_commands:context()) -> iolist().
error_message(#{reason := unknown_command, command := Command}) ->
    io_lib:format("Unknown command '~s'", [Command]);
error_message(#{reason := invalid_args}) ->
    "Could not parse the command arguments";
error_message(#{reason := unknown_category}) ->
    "Unknown category";
error_message(#{reason := {unknown_arg, ArgName}, command := Command}) ->
    io_lib:format("Unknown argument '~s' for command '~s'", [ArgName, Command]);
error_message(#{reason := {invalid_arg_value, ArgName, ArgValue}, command := Command}) ->
    io_lib:format("Invalid value '~s' of argument '~s' for command '~s'",
                  [ArgValue, ArgName, Command]);
error_message(#{reason := {missing_args, MissingArgs}, command := Command}) ->
    io_lib:format("Missing mandatory arguments for command '~s': ~s",
                  [Command, ["'", lists:join("', '", MissingArgs), "'"]]).

-spec print_usage(mongoose_graphql_commands:context()) -> any().
print_usage(#{category := Category, command := Command, args_spec := ArgsSpec}) ->
    print_usage_command(Category, Command, ArgsSpec);
print_usage(#{category := Category, commands := Commands}) ->
    print_usage_category(Category, Commands);
print_usage(_) ->
    {MaxC, ShCode} = get_shell_info(),
    print_usage(MaxC, ShCode).

handle_graphql_result({ok, Result}) ->
    JSONResult = mongoose_graphql_response:term_to_pretty_json(Result),
    ?PRINT("~s\n", [JSONResult]),
    case Result of
        #{errors := _} -> ?STATUS_ERROR;
        _ -> ?STATUS_SUCCESS
    end;
handle_graphql_result({error, Reason}) ->
    {_Code, Error} = mongoose_graphql_errors:format_error(Reason),
    JSONResult = jiffy:encode(#{errors => [Error]}, [pretty]),
    ?PRINT("~s\n", [JSONResult]),
    ?STATUS_ERROR.

%%-----------------------------
%% Format arguments
%%-----------------------------
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
    print_usage(MaxC, ShCode).


-spec print_usage(MaxC :: integer(), ShCode :: boolean()) -> ok.
print_usage(MaxC, ShCode) ->
    ?PRINT(["Usage: ", ?B("mongooseimctl"), " [", ?U("category"), "] ", ?U("command"),
            " [", ?U("arguments"), "]\n\n"
            "Most MongooseIM commands are grouped into the following categories:\n"], []),
    print_categories(MaxC, ShCode),
    ?PRINT(["\nTo list the commands in a particular category:\n  mongooseimctl ", ?U("category"),
            "\n"], []),
    ?PRINT(["\nThe following basic system management commands do not have a category:\n"], []),
    print_usage_commands(MaxC, ShCode, basic_commands()).

-spec basic_commands() -> [cmd()].
basic_commands() ->
    [{"status", [], "Get MongooseIM status"},
     {"stop", [], "Stop MongooseIM"},
     {"restart", [], "Restart MongooseIM"},
     {"graphql", ["query"], "Execute GraphQL query or mutation"}].

-spec print_categories(MaxC :: integer(), ShCode :: boolean()) -> ok.
print_categories(MaxC, ShCode) ->
    SortedSpecs = lists:sort(maps:to_list(mongoose_graphql_commands:get_specs())),
    Categories = [{binary_to_list(Category), [], binary_to_list(Desc)}
                  || {Category, #{desc := Desc}} <- SortedSpecs],
    print_usage_commands(MaxC, ShCode, Categories).

-spec print_usage_category(mongoose_graphql_commands:category(),
                           mongoose_graphql_commands:command_map()) -> ok.
print_usage_category(Category, Commands) ->
    {MaxC, ShCode} = get_shell_info(),
    ?PRINT(["Usage: ", ?B("mongooseimctl"), " ", Category, " ", ?U("command"), " ", ?U("arguments"), "\n"
            "\n"
            "The following commands are available in the category '", Category, "':\n"], []),
    CmdSpec = [{binary_to_list(Command), [], binary_to_list(Desc)}
               || {Command, #{desc := Desc}} <- maps:to_list(Commands)],
    print_usage_commands(MaxC, ShCode, CmdSpec),
    ?PRINT(["\nTo list the arguments for a particular command:\n"
            "  mongooseimctl ", Category, " ", ?U("command"), " --help", "\n"], []).

-spec print_usage_command(mongoose_graphql_commands:category(),
                          mongoose_graphql_commands:command(),
                          [mongoose_graphql_commands:arg_spec()]) -> ok.
print_usage_command(Category, Command, ArgsSpec) ->
    {MaxC, ShCode} = get_shell_info(),
    ?PRINT(["Usage: ", ?B("mongooseimctl"), " ", Category, " ", Command, " ", ?U("arguments"), "\n"
            "\n",
            "Each argument has the format: --", ?U("name"), " ", ?U("value"), "\n",
            "Available arguments are listed below with the corresponding GraphQL types:\n"], []),
    %% Reuse the function initially designed for printing commands for now
    %% This will be replaced with new logic when old commands are dropped
    Args = [{binary_to_list(Name), [], mongoose_graphql_commands:wrap_type(Wrap, Type)}
            || #{name := Name, type := Type, wrap := Wrap} <- ArgsSpec],
    print_usage_commands(MaxC, ShCode, Args),
    ?PRINT(["\nScalar values do not need quoting unless they contain special characters or spaces.\n"
            "Complex input types are passed as JSON maps or lists, depending on the type.\n"
            "When a type is followed by '!', the corresponding argument is required.\n"], []).

-spec print_usage_commands(MaxC :: integer(),
                           ShCode :: boolean(),
                           Commands :: [cmd(), ...]) -> 'ok'.
print_usage_commands(MaxC, ShCode, Commands) ->
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
    FmtCmdDescs = format_command_lines(CmdArgsLenDescsSorted, MaxCmdLen, MaxC, ShCode),
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
    Words = string:tokens(Desc, " \n"),
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
                           ShCode :: boolean()) -> [[any(), ...], ...].
format_command_lines(CALD, MaxCmdLen, MaxC, ShCode) when MaxC - MaxCmdLen < 40 ->
    % Long mode
    lists:map(
        fun({Cmd, Args, _CmdArgsL, Desc}) ->
            DescFmt = prepare_description(8, MaxC, Desc),
            ["\n  ", ?B(Cmd), " ", [[?U(Arg), " "] || Arg <- Args], "\n", "        ",
             DescFmt, "\n"]
        end, CALD);

format_command_lines(CALD, MaxCmdLen, MaxC, ShCode) ->
    % Dual mode
    lists:map(
        fun({Cmd, Args, CmdArgsL, Desc}) ->
            DescFmt = prepare_description(MaxCmdLen+4, MaxC, Desc),
            ["  ", ?B(Cmd), " ", [[?U(Arg), " "] || Arg <- Args],
             string:chars(?ASCII_SPACE_CHARACTER, MaxCmdLen - CmdArgsL + 1),
             DescFmt, "\n"]
        end, CALD).

%%-----------------------------
%% Print usage command
%%-----------------------------

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
%% Cluster management commands
%%-----------------------------

join_cluster(NodeString) ->
    handle_cluster_operation(join_cluster, [NodeString]).

leave_cluster() ->
    handle_cluster_operation(leave_cluster, []).

remove_from_cluster(NodeString) ->
    handle_cluster_operation(remove_from_cluster, [NodeString]).

handle_cluster_operation(Operation, Args) ->
    case apply(mongoose_server_api, Operation, Args) of
        {ok, Result} ->
            ?PRINT("~s\n", [Result]),
            ?STATUS_SUCCESS;
        {_, Result} ->
            ?PRINT("Error: \"~s\"\n", [Result]),
            ?STATUS_ERROR
    end.

cluster_command_usage() ->
    ?PRINT("This command requires one argument: other node's name\n", []),
    ?STATUS_ERROR.
