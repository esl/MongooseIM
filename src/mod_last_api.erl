%% @doc Provide an interface for frontends (like graphql or ctl) to manage last activity.
-module(mod_last_api).

-export([get_last/1, set_last/3, count_active_users/2]).

-export([list_old_users/1, list_old_users/2,
         remove_old_users/1, remove_old_users/2]).

-include("jlib.hrl").
-include("mongoose.hrl").

-type old_user() :: {jid:jid(), null | timestamp()}.
-type info() :: #{timestamp := mod_last:timestamp(), status := mod_last:status()}.
-type timestamp() :: mod_last:timestamp().
-type status() :: mod_last:status().
-type host_type() :: mongooseim:host_type().

-define(USER_NOT_FOUND_RESULT(User, Server),
        {user_does_not_exist, io_lib:format("User ~s@~s does not exist", [User, Server])}).
-define(DOMAIN_NOT_FOUND_RESULT, {domain_not_found, <<"Domain not found">>}).

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
    {ok, info()} | {last_not_found | user_does_not_exist, iodata()}.
get_last(#jid{luser = User, lserver = Server} = JID) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            {ok, HostType} = mongoose_domain_api:get_host_type(Server),
            case mod_last:get_last_info(HostType, User, Server) of
                {ok, Timestamp, Status} ->
                    {ok, #{timestamp => Timestamp, status => Status}};
                not_found ->
                    {last_not_found, <<"Given user's last info not found">>}
            end;
        false ->
            ?USER_NOT_FOUND_RESULT(User, Server)
    end.

-spec count_active_users(jid:server(), timestamp()) ->
    {ok, non_neg_integer()} | {domain_not_found, binary()}.
count_active_users(Domain, Timestamp) ->
    LDomain = jid:nameprep(Domain),
    case mongoose_domain_api:get_host_type(LDomain) of
        {ok, HostType} ->
            {ok, mod_last:count_active_users(HostType, LDomain, Timestamp)};
        {error, not_found} ->
            ?DOMAIN_NOT_FOUND_RESULT
    end.

-spec list_old_users(timestamp()) -> {ok, [old_user()]}.
list_old_users(Timestamp) ->
    OldUsers = lists:append([list_old_users(HostType, Domain, Timestamp) ||
                             HostType <- ?ALL_HOST_TYPES,
                             Domain <- mongoose_domain_api:get_domains_by_host_type(HostType)]),
    {ok, OldUsers}.

-spec list_old_users(jid:server(), timestamp()) ->
    {ok, [old_user()]} | {domain_not_found, binary()}.
list_old_users(Domain, Timestamp) ->
    LDomain = jid:nameprep(Domain),
    case mongoose_domain_api:get_host_type(LDomain) of
        {ok, HostType} ->
            {ok, list_old_users(HostType, LDomain, Timestamp)};
        {error, not_found} ->
            ?DOMAIN_NOT_FOUND_RESULT
    end.

-spec remove_old_users(timestamp()) -> {ok, [old_user()]}.
remove_old_users(Timestamp) ->
    {ok, OldUsers} = list_old_users(Timestamp),
    ok = remove_users(OldUsers),
    {ok, OldUsers}.

-spec remove_old_users(jid:server(), timestamp()) ->
    {ok, [old_user()]} | {domain_not_found, binary()}.
remove_old_users(Domain, Timestamp) ->
    case list_old_users(Domain, Timestamp) of
        {ok, OldUsers} ->
            ok = remove_users(OldUsers),
            {ok, OldUsers};
        Error ->
            Error
    end.

%% Internal

-spec list_old_users(host_type(), jid:lserver(), timestamp()) -> [old_user()].
list_old_users(HostType, Domain, Timestamp) ->
    Users = ejabberd_auth:get_vh_registered_users(Domain),
    lists:filtermap(fun(U) -> prepare_old_user(HostType, U, Timestamp) end, Users).

-spec prepare_old_user(host_type(), jid:simple_bare_jid(), timestamp()) ->
    false | {true, old_user()}.
prepare_old_user(HostType, {LU, LS}, Timestamp) ->
    JID = jid:make_bare(LU, LS),
    case ejabberd_sm:get_user_resources(JID) of
        [] ->
            case mod_last:get_last_info(HostType, LU, LS) of
                {ok, UserTimestamp, _} when UserTimestamp < Timestamp ->
                    {true, {JID, UserTimestamp}};
                not_found ->
                    {true, {JID, null}};
                _ ->
                    false
            end;
        _ ->
            false
    end.

-spec remove_users([old_user()]) -> ok.
remove_users(Users) ->
    lists:foreach(fun({JID, _}) -> ok = ejabberd_auth:remove_user(JID) end, Users).
