%% @doc Provide an interface for frontends (like graphql or ctl) to manage sessions.
-module(mongoose_session_api).

-export([list_sessions/0,
         list_sessions/1,
         count_sessions/0,
         count_sessions/1,
         list_user_sessions/1,
         list_resources/1,
         list_user_resources/1,
         num_resources/1,
         get_user_resource/2,
         list_status_users/1,
         list_status_users/2,
         num_status_users/1,
         num_status_users/2,
         set_presence/5,
         kick_session/2,
         kick_sessions/2,
         prepare_reason/1]).

-ignore_xref([prepare_reason/1,
               get_user_resources/2,
               list_user_sessions/1,
               num_resources/1]).

-include("session.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("exml/include/exml.hrl").

-type status() :: binary().
-type session() :: #session{}.
-type status_user_info() :: {JID :: jid:jid(),
                             Prio :: ejabberd_sm:priority(),
                             Status :: status()}.

-type session_info() :: {USR :: jid:jid(),
                         Conn :: atom(),
                         IPS :: inet:ip_address(),
                         Port :: inet:port_number(),
                         Prio :: ejabberd_sm:priority(),
                         NodeS :: node(),
                         Uptime :: integer()}.

-type res_number_result() :: {ok | wrong_res_number, binary()}.

-type kick_user_result() :: #{binary() => term()}.

-type set_presence_result() :: {ok | empty_resource, binary()}.

-export_type([res_number_result/0,
              set_presence_result/0,
              kick_user_result/0,
              status_user_info/0,
              session_info/0,
              status/0]).

-spec list_sessions() -> {ok, [session_info()]}.
list_sessions() ->
    USRIs = ejabberd_sm:get_full_session_list(),
    {ok, lists:map(fun format_user_info/1, USRIs)}.

-spec list_sessions(jid:server()) -> {ok, [session_info()]}.
list_sessions(Host) ->
    USRIs = ejabberd_sm:get_vh_session_list(Host),
    {ok, lists:map(fun format_user_info/1, USRIs)}.

-spec count_sessions() -> {ok, non_neg_integer()}.
count_sessions() ->
    {ok, ejabberd_sm:get_total_sessions_number()}.

-spec count_sessions(jid:server()) -> {ok, non_neg_integer()}.
count_sessions(Host) ->
    {ok, ejabberd_sm:get_vh_session_number(Host)}.

-spec list_resources(jid:server()) -> {ok, [jid:literal_jid()]}.
list_resources(Host) ->
    Lst = ejabberd_sm:get_vh_session_list(Host),
    {ok, [jid:to_binary(USR) || #session{usr = USR} <- Lst]}.

-spec list_user_resources(jid:jid()) -> {ok, [jid:literal_jid()]}.
list_user_resources(JID) ->
    {ok, ejabberd_sm:get_user_resources(JID)}.

-spec list_user_sessions(jid:jid()) -> {ok, [session_info()]}.
list_user_sessions(JID) ->
    Resources = ejabberd_sm:get_user_resources(JID),
    {ok, lists:foldl(
             fun(Res, Acc) ->
                 RJID = jid:replace_resource(JID, Res),
                 case ejabberd_sm:get_session(RJID) of
                     offline -> Acc;
                     Session -> [format_user_info(Session) | Acc]
                 end
             end,
             [],
             Resources)}.

-spec num_resources(jid:jid()) -> {ok, non_neg_integer()}.
num_resources(JID) ->
    {ok, length(ejabberd_sm:get_user_resources(JID))}.

-spec get_user_resource(jid:jid(), integer()) -> res_number_result().
get_user_resource(JID, Num) ->
    Resources = ejabberd_sm:get_user_resources(JID),
    case (0 < Num) and (Num =< length(Resources)) of
        true ->
            {ok, lists:nth(Num, Resources)};
        false ->
            {wrong_res_number, iolist_to_binary(io_lib:format("Wrong resource number: ~p", [Num]))}
    end.

-spec num_status_users(jid:server(), status()) -> {ok, non_neg_integer()}.
num_status_users(Host, Status) ->
    {ok, Sessions} = list_status_users(Host, Status),
    {ok, length(Sessions)}.

-spec num_status_users(status()) -> {ok, non_neg_integer()}.
num_status_users(Status) ->
    {ok, Sessions} = list_status_users(Status),
    {ok, length(Sessions)}.

-spec list_status_users(jid:server(), status()) -> {ok, [status_user_info()]}.
list_status_users(Host, Status) ->
    Sessions = ejabberd_sm:get_vh_session_list(Host),
    {ok, get_status_list(Sessions, Status)}.

-spec list_status_users(status()) -> {ok, [status_user_info()]}.
list_status_users(Status) ->
    Sessions = ejabberd_sm:get_full_session_list(),
    {ok, get_status_list(Sessions, Status)}.

-spec set_presence(jid:jid(), Type :: binary(), Show :: binary(),
                   Status :: binary(), Prio :: binary()) -> set_presence_result().
set_presence(#jid{lresource = <<>>}, _Type, _Show, _Status, _Priority) ->
    {empty_resource, <<"The resource is empty. You need to provide a full JID">>};
set_presence(JID, Type, Show, Status, Priority) ->
    Pid = ejabberd_sm:get_session_pid(JID),
    USR = jid:to_binary(JID),
    US = jid:to_binary(jid:to_bare(JID)),

    Children = maybe_pres_status(Status,
                                 maybe_pres_priority(Priority,
                                                     maybe_pres_show(Show, []))),
    Message = {xmlstreamelement,
               #xmlel{name = <<"presence">>,
                      attrs = [{<<"from">>, USR}, {<<"to">>, US} | maybe_type_attr(Type)],
                      children = Children}},
    ok = p1_fsm_old:send_event(Pid, Message),
    {ok, <<"Presence set successfully">>}.

-spec kick_sessions(jid:jid(), binary()) -> {ok, [kick_user_result()]}.
kick_sessions(JID, Reason) ->
    {ok, lists:map(
             fun(Resource) ->
                 FullJID = jid:replace_resource(JID, Resource),
                 case kick_session(FullJID, Reason) of
                     {ok, Result} ->
                         {ok, Result};
                     {Code, Message} ->
                         {ok, #{<<"jid">> => FullJID,
                                <<"kicked">> => false,
                                <<"code">> => atom_to_binary(Code),
                                <<"message">> => Message}}
                 end
             end,
             ejabberd_sm:get_user_resources(JID))}.

-spec kick_session(jid:jid(), binary() | null) -> {ok, kick_user_result()} | {atom(), binary()}.
kick_session(JID, ReasonText) ->
    case ejabberd_c2s:terminate_session(JID, prepare_reason(ReasonText)) of
        no_session ->
            {no_session, <<"No active session">>};
        {exit, _Reason} ->
            {ok, #{<<"jid">> => JID,
                   <<"kicked">> => true,
                   <<"message">> => <<"Session kicked">>}}
    end.

-spec prepare_reason(binary() | null) -> binary().
prepare_reason(Reason) when Reason == <<>>; Reason == null ->
    <<"Kicked by administrator">>;
prepare_reason(Reason) when is_binary(Reason) ->
    Reason.

%% Internal

-spec get_status_list([session()], status()) -> [status_user_info()].
get_status_list(Sessions0, StatusRequired) ->
    Sessions = [{catch ejabberd_c2s:get_presence(Pid), S, P}
                || #session{sid = {_, Pid}, usr = {_, S, _}, priority = P} <- Sessions0],

    [{jid:make_noprep(User, Server, Resource), Priority, StatusText}
     || {{User, Resource, Status, StatusText}, Server, Priority} <- Sessions,
        Status == StatusRequired].

-spec format_user_info(ejabberd_sm:session()) -> session_info().
format_user_info(#session{sid = {Microseconds, Pid}, usr = Usr,
                          priority = Priority, info = Info}) ->
    Conn = maps:get(conn, Info),
    {Ip, Port} = maps:get(ip, Info),
    Node = node(Pid),
    Uptime = (erlang:system_time(microsecond) - Microseconds) div 1000000,
    {Usr, Conn, Ip, Port, Priority, Node, Uptime}.

-spec maybe_type_attr(binary())-> list().
maybe_type_attr(<<"available">>) ->
    [];
maybe_type_attr(Type) ->
    [{<<"type">>, Type}].

-spec maybe_pres_show(binary(), list()) -> list().
maybe_pres_show(Show, Children) when Show =:= <<>>;
                                     Show =:= <<"online">> ->
    Children;
maybe_pres_show(Show, Children) ->
    [#xmlel{name = <<"show">>,
            children = [#xmlcdata{content = Show}]} | Children].

-spec maybe_pres_priority(binary(), list()) -> list().
maybe_pres_priority(<<>>, Children) ->
    Children;
maybe_pres_priority(Prio, Children) ->
    [#xmlel{name = <<"priority">>,
            children = [#xmlcdata{content = Prio}]} | Children].

-spec maybe_pres_status(binary(), list()) -> list().
maybe_pres_status(<<>>, Children) ->
    Children;
maybe_pres_status(Status, Children) ->
    [#xmlel{name = <<"status">>,
            children = [#xmlcdata{content = Status}]} | Children].
