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
                         Address :: {inet:ip_address(), inet:port_number()} | undefined,
                         Prio :: ejabberd_sm:priority(),
                         NodeS :: node(),
                         Uptime :: integer()}.

-type res_number_result() :: {ok | wrong_res_number | user_not_found, binary()}.

-type kick_user_result() :: #{binary() => term()}.

-type set_presence_result() :: {ok | empty_resource | user_not_found, binary()}.

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

-spec list_sessions(jid:server()) -> {ok, [session_info()]} | {domain_not_found, binary()}.
list_sessions(Host) ->
    case check_domain(Host) of
        ok ->
            USRIs = ejabberd_sm:get_vh_session_list(Host),
            {ok, lists:map(fun format_user_info/1, USRIs)};
        Error ->
            Error
    end.

-spec count_sessions() -> {ok, non_neg_integer()}.
count_sessions() ->
    {ok, ejabberd_sm:get_total_sessions_number()}.

-spec count_sessions(jid:server()) -> {ok, non_neg_integer()} | {domain_not_found, binary()}.
count_sessions(Host) ->
    case check_domain(Host) of
        ok ->
            {ok, ejabberd_sm:get_vh_session_number(Host)};
        Error ->
            Error
    end.

-spec list_resources(jid:server()) -> {ok, [jid:literal_jid()]} | {domain_not_found, binary()}.
list_resources(Host) ->
    case check_domain(Host) of
        ok ->
            Lst = ejabberd_sm:get_vh_session_list(Host),
            {ok, [jid:to_binary(USR) || #session{usr = USR} <- Lst]};
        Error ->
            Error
    end.

-spec list_user_resources(jid:jid()) -> {ok, [jid:literal_jid()]} | {user_not_found, binary()}.
list_user_resources(JID) ->
    case check_user(JID) of
        ok ->
            {ok, ejabberd_sm:get_user_resources(JID)};
        Error ->
            Error
    end.

-spec list_user_sessions(jid:jid()) -> {ok, [session_info()]} | {user_not_found, binary()}.
list_user_sessions(JID) ->
    case check_user(JID) of
        ok ->
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
                    Resources)};
        Error ->
            Error
    end.

-spec num_resources(jid:jid()) -> {ok, non_neg_integer()} | {user_not_found, binary()}.
num_resources(JID) ->
    case check_user(JID) of
        ok ->
            {ok, length(ejabberd_sm:get_user_resources(JID))};
        Error ->
            Error
    end.

-spec get_user_resource(jid:jid(), integer()) -> res_number_result().
get_user_resource(JID, Num) ->
    case check_user(JID) of
        ok ->
            Resources = ejabberd_sm:get_user_resources(JID),
            case (0 < Num) and (Num =< length(Resources)) of
                true ->
                    {ok, lists:nth(Num, Resources)};
                false ->
                    {wrong_res_number,
                     iolist_to_binary(io_lib:format("Wrong resource number: ~p", [Num]))}
            end;
        Error ->
            Error
    end.

-spec num_status_users(jid:server(), status()) -> {ok, non_neg_integer()}
                                                | {domain_not_found, binary()}.
num_status_users(Host, Status) ->
    case check_domain(Host) of
        ok ->
            {ok, Sessions} = list_status_users(Host, Status),
            {ok, length(Sessions)};
        Error ->
            Error
    end.

-spec num_status_users(status()) -> {ok, non_neg_integer()}.
num_status_users(Status) ->
    {ok, Sessions} = list_status_users(Status),
    {ok, length(Sessions)}.

-spec list_status_users(jid:server(), status()) -> {ok, [status_user_info()]}
                                                 | {domain_not_found, binary()}.
list_status_users(Host, Status) ->
    case check_domain(Host) of
        ok ->
            Sessions = ejabberd_sm:get_vh_session_list(Host),
            {ok, get_status_list(Sessions, Status)};
        Error ->
            Error
    end.

-spec list_status_users(status()) -> {ok, [status_user_info()]}.
list_status_users(Status) ->
    Sessions = ejabberd_sm:get_full_session_list(),
    {ok, get_status_list(Sessions, Status)}.

-spec set_presence(jid:jid(), Type :: binary(), Show :: binary(),
                   Status :: binary(), Prio :: binary()) -> set_presence_result().
set_presence(#jid{lresource = <<>>}, _Type, _Show, _Status, _Priority) ->
    {empty_resource, <<"The resource is empty. You need to provide a full JID">>};
set_presence(JID, Type, Show, Status, Priority) ->
    case check_user(JID) of
        ok ->
            Pid = ejabberd_sm:get_session_pid(JID),
            USR = jid:to_binary(JID),
            US = jid:to_binary(jid:to_bare(JID)),

            Children = maybe_pres_status(Status,
                                         maybe_pres_priority(Priority,
                                                             maybe_pres_show(Show, []))),
            Message = #xmlel{name = <<"presence">>,
                             attrs = [{<<"from">>, USR}, {<<"to">>, US} | maybe_type_attr(Type)],
                             children = Children},
            ok = mod_presence:set_presence(Pid, Message),
            {ok, <<"Presence set successfully">>};
        Error ->
            Error
    end.

-spec kick_sessions(jid:jid(), binary()) -> {ok, [kick_user_result()]} | {user_not_found, binary()}.
kick_sessions(JID, Reason) ->
    case check_user(JID) of
        ok ->
            {ok, lists:map(
                    fun(Resource) ->
                        FullJID = jid:replace_resource(JID, Resource),
                        case kick_session_internal(FullJID, Reason) of
                            {ok, Result} ->
                                {ok, Result};
                            {Code, Message} ->
                                {ok, #{<<"jid">> => FullJID,
                                        <<"kicked">> => false,
                                        <<"code">> => atom_to_binary(Code),
                                        <<"message">> => Message}}
                        end
                    end,
                    ejabberd_sm:get_user_resources(JID))};
        Error ->
            Error
    end.

-spec kick_session(jid:jid(), binary() | null) -> {ok, kick_user_result()}
                                                | {no_session | user_not_found, binary()}.
kick_session(JID, Reason) ->
    case check_user(JID) of
        ok ->
            kick_session_internal(JID, Reason);
        Error ->
            Error
    end.

-spec kick_session_internal(jid:jid(), binary() | null) -> {ok, kick_user_result()}
                                                         | {no_session, binary()}.
kick_session_internal(JID, Reason) ->
    case ejabberd_sm:terminate_session(JID, prepare_reason(Reason)) of
        no_session ->
            {no_session, <<"No active session">>};
        ok ->
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
    Sessions = [ {catch mod_presence:get_presence(Pid), S, P}
                 || #session{sid = {_, Pid}, usr = {_, S, _}, priority = P} <- Sessions0],

    [{jid:make_noprep(User, Server, Resource), Priority, StatusText}
     || {{User, Resource, Status, StatusText}, Server, Priority} <- Sessions,
        Status == StatusRequired].

-spec format_user_info(ejabberd_sm:session()) -> session_info().
format_user_info(#session{sid = {Microseconds, Pid}, usr = Usr,
                          priority = Priority, info = Info}) ->
    Conn = maps:get(conn, Info, undefined),
    Address = maps:get(ip, Info, undefined),
    Node = node(Pid),
    Uptime = (erlang:system_time(microsecond) - Microseconds) div 1000000,
    {Usr, Conn, Address, Priority, Node, Uptime}.

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

-spec check_domain(jid:server()) -> ok | {domain_not_found, binary()}.
check_domain(Domain) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, _} -> ok;
        {error, not_found} -> {domain_not_found, <<"Domain not found">>}
    end.

-spec check_user(jid:jid()) -> ok | {user_not_found, binary()}.
check_user(JID = #jid{lserver = LServer}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case ejabberd_auth:does_user_exist(HostType, JID, stored) of
                true -> ok;
                false -> {user_not_found, <<"Given user does not exist">>}
            end;
        {error, not_found} ->
            {user_not_found, <<"User's domain does not exist">>}
    end.
