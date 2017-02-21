%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_sessions.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_admin_extra_sessions).
-author('badlop@process-one.net').

-export([
    commands/0,

    num_resources/2,
    resource_num/3,
    kick_session/4,
    kick_this_session/4,
    prepare_reason/1,
    status_num/2, status_num/1,
    status_list/2, status_list/1,
    connected_users_info/0,
    connected_users_info/1,
    set_presence/7,
    user_sessions_info/2
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-type status() :: binary().
-type u_s_r_p_st() :: { User    :: ejabberd:user(),
                        Server  :: ejabberd:server(),
                        Res     :: ejabberd:resource(),
                        Prio    :: integer(),
                        Status  :: status()}.
-type formatted_user_info() :: {USR :: string(),
                                Conn :: string(),
                                IPS :: string(),
                                Port :: inet:port_number(),
                                Prio :: integer(),
                                NodeS :: string(),
                                Uptime :: integer()}.
-type usr_nowpid_p_i() :: {ejabberd:simple_jid(),
                           {Now :: erlang:timestamp(), Pid :: identifier()},
                           Prio :: integer(),
                           Info :: string()}.

%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    SessionDisplay = {list,
                      {sessions, {tuple,
                                  [{jid, string},
                                   {connection, string},
                                   {ip, string},
                                   {port, integer},
                                   {priority, integer},
                                   {node, string},
                                   {uptime, integer}
                                  ]}}
                     },

    [
        #ejabberd_commands{name = num_resources, tags = [session],
                           desc = "Get the number of resources of a user",
                           module = ?MODULE, function = num_resources,
                           args = [{user, binary}, {host, binary}],
                           result = {resources, integer}},
        #ejabberd_commands{name = resource_num, tags = [session],
                           desc = "Resource string of a session number",
                           module = ?MODULE, function = resource_num,
                           args = [{user, binary}, {host, binary}, {num, integer}],
                           result = {resource, binary}},
        #ejabberd_commands{name = kick_session, tags = [session],
                           desc = "Kick a user session",
                           module = ?MODULE, function = kick_session,
                           args = [{user, binary}, {host, binary},
                                   {resource, binary}, {reason, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = status_num_host, tags = [session, stats],
                           desc = "Number of logged users with this status in host",
                           module = ?MODULE, function = status_num,
                           args = [{host, binary}, {status, binary}],
                           result = {users, integer}},
        #ejabberd_commands{name = status_num, tags = [session, stats],
                           desc = "Number of logged users with this status",
                           module = ?MODULE, function = status_num,
                           args = [{status, binary}],
                           result = {users, integer}},
        #ejabberd_commands{name = status_list_host, tags = [session],
                           desc = "List of users logged in host with their statuses",
                           module = ?MODULE, function = status_list,
                           args = [{host, binary}, {status, binary}],
                           result = {users, {list,
                                             {userstatus, {tuple, [
                                {user, string},
                                {host, string},
                                {resource, string},
                                {priority, integer},
                                {status, string}
                                ]}}
                                            }}},
        #ejabberd_commands{name = status_list, tags = [session],
                           desc = "List of logged users with this status",
                           module = ?MODULE, function = status_list,
                           args = [{status, binary}],
                           result = {users, {list,
                                             {userstatus, {tuple, [
                                {user, string},
                                {host, string},
                                {resource, string},
                                {priority, integer},
                                {status, string}
                                ]}}
                                            }}},
        #ejabberd_commands{name = connected_users_info,
                           tags = [session],
                           desc = "List all established sessions and their information",
                           module = ?MODULE, function = connected_users_info,
                           args = [],
                           result = {connected_users_info, SessionDisplay}},
        #ejabberd_commands{name = connected_users_vhost,
                           tags = [session],
                           desc = "Get the list of established sessions in a vhost",
                           module = ?MODULE, function = connected_users_info,
                           args = [{host, binary}],
                           result = {connected_users_vhost, SessionDisplay}},
        #ejabberd_commands{name = user_sessions_info,
                           tags = [session],
                           desc = "Get information about all sessions of a user",
                           module = ?MODULE, function = user_sessions_info,
                           args = [{user, binary}, {host, binary}],
                           result = {user_sessions_info, SessionDisplay}},

        #ejabberd_commands{name = set_presence,
                           tags = [session],
                           desc = "Set presence of a session",
                           module = ?MODULE, function = set_presence,
                           args = [{user, binary}, {host, binary},
                                   {resource, binary}, {type, binary},
                                   {show, binary}, {status, binary},
                                   {priority, binary}],
                           result = {res, rescode}}
        ].

%%%
%%% Sessions
%%%

-spec num_resources(ejabberd:user(), ejabberd:server()) -> non_neg_integer().
num_resources(User, Host) ->
    length(ejabberd_sm:get_user_resources(User, Host)).


-spec resource_num(ejabberd:user(), ejabberd:server(), integer()) -> binary() | string().
resource_num(User, Host, Num) ->
    Resources = ejabberd_sm:get_user_resources(User, Host),
    case (0<Num) and (Num=<length(Resources)) of
        true ->
            lists:nth(Num, Resources);
        false ->
            lists:flatten(io_lib:format("Error: Wrong resource number: ~p", [Num]))
    end.


-spec kick_session(ejabberd:user(), ejabberd:server(), ejabberd:resource(),
        ReasonText :: binary()) -> ok.
kick_session(User, Server, Resource, ReasonText) ->
    kick_this_session(User, Server, Resource, prepare_reason(ReasonText)),
    ok.


-spec kick_this_session(ejabberd:user(), ejabberd:server(), ejabberd:resource(),
        Reason :: binary()) -> ok | {error, lager_not_started}.
kick_this_session(User, Server, Resource, Reason) ->
    ejabberd_sm:route(
        jid:make(<<"">>, <<"">>, <<"">>),
        jid:make(User, Server, Resource),
        {broadcast, {exit, Reason}}).


-spec prepare_reason(binary() | string()) -> binary().
prepare_reason(<<>>) ->
    <<"Kicked by administrator">>;
prepare_reason([Reason]) ->
    prepare_reason(Reason);
prepare_reason(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
prepare_reason(Reason) when is_binary(Reason) ->
    Reason;
prepare_reason(StringList) ->
    prepare_reason(string:join(StringList, "_")).


-spec status_num('all' | ejabberd:server(), status()) -> non_neg_integer().
status_num(Host, Status) ->
    length(get_status_list(Host, Status)).


-spec status_num(status()) -> non_neg_integer().
status_num(Status) ->
    status_num(all, Status).


-spec status_list('all' | ejabberd:server(), status()) -> [u_s_r_p_st()].
status_list(Host, Status) ->
    Res = get_status_list(Host, Status),
    [{U, S, R, P, St} || {U, S, R, P, St} <- Res].


-spec status_list(binary()) -> [u_s_r_p_st()].
status_list(Status) ->
    status_list(all, Status).


-spec get_status_list('all' | ejabberd:server(), status()) -> [u_s_r_p_st()].
get_status_list(Host, StatusRequired) ->
    Sessions0 = case Host of
        all -> ejabberd_sm:get_full_session_list();
        _ -> ejabberd_sm:get_vh_session_list(Host)
    end,
    Sessions = [ {catch ejabberd_c2s:get_presence(Pid), Server, Priority}
                 || {{_, Server, _}, {_, Pid}, Priority, _} <- Sessions0 ],

    %% Filter by status
    Fstatus = case StatusRequired of
        all -> fun(_, _) -> true end;
        _ -> fun(A, B) -> A == B end
    end,
    [{User, Server, Resource, Priority, StatusText}
     || {{User, Resource, Status, StatusText}, Server, Priority} <- Sessions,
        apply(Fstatus, [Status, StatusRequired])].


-spec connected_users_info() -> [formatted_user_info()].
connected_users_info() ->
    connected_users_info(all).


-spec connected_users_info('all' | ejabberd:server()) -> [formatted_user_info()].
connected_users_info(Host) ->
    USRIs = case Host of
        all -> ejabberd_sm:get_full_session_list();
        _ -> ejabberd_sm:get_vh_session_list(Host)
    end,
    lists:map(fun format_user_info/1, USRIs).


-spec set_presence(ejabberd:user(), ejabberd:server(), ejabberd:resource(),
        Type :: binary(), Show :: binary() | string(), Status :: binary() | string(),
        Prio :: binary() | string()) -> 'ok'.
set_presence(User, Host, Resource, Type, Show, Status, Priority) ->
    Pid = ejabberd_sm:get_session_pid(User, Host, Resource),
    USR = <<User/binary, $@, Host/binary, $/, Resource/binary>>,
    US = <<User/binary, $@, Host/binary>>,
    Message = {xmlstreamelement,
               #xmlel{ name = <<"presence">>,
                      attrs = [{<<"from">>, USR}, {<<"to">>, US}, {<<"type">>, Type}],
                      children = [#xmlel{ name = <<"show">>,
                                          children = [#xmlcdata{content = Show}]},
                                  #xmlel{ name = <<"status">>,
                                          children = [#xmlcdata{content = Status}]},
                                  #xmlel{ name = <<"priority">>,
                                          children = [#xmlcdata{content = Priority}]}]}},
    p1_fsm_old:send_event(Pid, Message).


-spec user_sessions_info(ejabberd:user(), ejabberd:server()) -> [formatted_user_info()].
user_sessions_info(User, Host) ->
    Resources = ejabberd_sm:get_user_resources(User, Host),
    lists:foldl(fun(Res, Acc) ->
                case ejabberd_sm:get_session(User, Host, Res) of
                    offline -> Acc;
                    Session -> [format_user_info(Session)|Acc]
                end
        end, [], Resources).


-spec format_user_info(usr_nowpid_p_i()) -> formatted_user_info().
format_user_info(Usr) ->
    format_user_info(Usr, calendar:datetime_to_gregorian_seconds({date(), time()})).


-spec format_user_info(usr_nowpid_p_i(), CurrentSec :: non_neg_integer()) -> formatted_user_info().
format_user_info({{U, S, R}, {Now, Pid}, Priority, Info}, CurrentSec) ->
    Conn = proplists:get_value(conn, Info),
    {Ip, Port} = proplists:get_value(ip, Info),
    IPS = inet_parse:ntoa(Ip),
    NodeS = atom_to_list(node(Pid)),
    Uptime = CurrentSec - calendar:datetime_to_gregorian_seconds(
            calendar:now_to_local_time(Now)),
    {[U, $@, S, $/, R], atom_to_list(Conn), IPS, Port, Priority, NodeS, Uptime}.
