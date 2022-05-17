%% @doc Provide an interface for frontends (like graphql or ctl) to manage last activity.
-module(mod_last_api).

-export([get_last/1, set_last/3, count_active_users/2]).

-include("jlib.hrl").

-type info() :: #{timestamp := mod_last:timestamp(), status := mod_last:status()}.
-type timestamp() :: mod_last:timestamp().
-type status() :: mod_last:status().

-define(USER_NOT_FOUND_RESULT(User, Server),
        {user_does_not_exist, io_lib:format("User ~s@~s does not exist", [User, Server])}).

-spec set_last(jid:jid(), timestamp(), status()) -> {ok, info()} | {user_does_not_exist, iolist()}.
set_last(#jid{luser = User, lserver = Server} = JID, Timestamp, Status) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            {ok, HostType} = mongoose_domain_api:get_host_type(Server),
            ok = mod_last:store_last_info(HostType, User, Server, Timestamp, Status),
            {ok, #{timestamp => Timestamp, status => Status}};
        false ->
            ?USER_NOT_FOUND_RESULT(User, Server)
    end.

-spec get_last(jid:jid()) ->
    {ok, info()} | {last_not_found | user_does_not_exist, iolist()}.
get_last(#jid{luser = User, lserver = Server} = JID) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            {ok, HostType} = mongoose_domain_api:get_host_type(Server),
            case mod_last:get_last_info(HostType, User, Server) of
                {ok, Timestamp, Status} ->
                    {ok, #{timestamp => Timestamp, status => Status}};
                not_found ->
                    {last_not_found, "Given user's last info not found"}
            end;
        false ->
            ?USER_NOT_FOUND_RESULT(User, Server)
    end.

-spec count_active_users(jid:server(), timestamp()) ->
    {ok, pos_integer()} |{domain_not_found, iolist()}.
count_active_users(Domain, Timestamp) ->
    LDomain = jid:nameprep(Domain),
    case mongoose_domain_api:get_host_type(LDomain) of
        {ok, HostType} ->
            {ok, mod_last:count_active_users(HostType, LDomain, Timestamp)};
        {error, not_found} ->
            {domain_not_found, "Domain not found"}
    end.
