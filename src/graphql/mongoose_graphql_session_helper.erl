-module(mongoose_graphql_session_helper).

-export([format_session/1, format_sessions/1, format_status_user/1, format_status_users/1]).

-ignore_xref([format_session/1, format_status_user/1]).

-type session_data() :: map().
-type session_list() :: [{ok, session_data()}].
-type status_user_data() :: map().
-type status_user_list() :: [{ok, status_user_data()}].

-export_type([session_data/0, session_list/0, status_user_data/0, status_user_list/0]).

-spec format_sessions([mongoose_session_api:session_info()]) -> session_list().
format_sessions(Sessions) ->
    lists:map(fun(S) -> {ok, format_session(S)} end, Sessions).

-spec format_session(mongoose_session_api:session_info()) -> session_data().
format_session({USR, Conn, Address, Prio, Node, Uptime}) ->
    {IP, Port} = from_address(Address),
    #{<<"user">> => jid:to_binary(USR),
      <<"connection">> => from_conn(Conn),
      <<"ip">> => IP,
      <<"port">> => Port,
      <<"priority">> => Prio,
      <<"node">> => atom_to_binary(Node),
      <<"uptime">> => Uptime}.

from_conn(undefined) -> null;
from_conn(Conn) -> atom_to_binary(Conn).

from_address(undefined) ->
    {null, null};
from_address({IP, Port}) ->
    {iolist_to_binary(inet:ntoa(IP)), Port}.

-spec format_status_users([mongoose_session_api:status_user_info()]) -> status_user_list().
format_status_users(Sessions) ->
    lists:map(fun(S) -> {ok, format_status_user(S)} end, Sessions).

-spec format_status_user(mongoose_session_api:status_user_info()) -> status_user_data().
format_status_user({JID, Prio, StatusText}) ->
    #{<<"user">> => jid:to_binary(JID),
      <<"priority">> => Prio,
      <<"text">> => iolist_to_binary(StatusText)}.
