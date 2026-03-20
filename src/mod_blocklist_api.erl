-module(mod_blocklist_api).

%% API
-export([add_user/2, remove_user/1, list_blocked_users/2, count_blocked_users/1]).

-include("jlib.hrl").
-include("session.hrl").

%% API

-spec add_user(jid:jid(), mod_blocklist:reason()) -> ok | {domain_not_found | user_not_found, iolist()}.
add_user(#jid{luser = LUser, lserver = LServer} = JID, Reason) ->
    maybe
        {ok, HostType} ?= mongoose_domain_api:get_host_type(LServer),
        true ?= ejabberd_auth:does_user_exist(JID),
        mod_blocklist_backend:upsert_block(HostType, LUser, LServer, Reason),
        terminate_sessions(JID, make_reason(Reason))
    else
        {error, _} -> {domain_not_found, "User's domain does not exist"};
        false -> {user_not_found, "User does not exist"}
    end.

-spec remove_user(jid:jid()) -> {ok, boolean()} | {domain_not_found, iolist()}.
remove_user(#jid{luser = LUser, lserver = LServer}) ->
    maybe
        {ok, HostType} ?= mongoose_domain_api:get_host_type(LServer),
        {ok, mod_blocklist_backend:remove_block(HostType, LUser, LServer)}
    else
        {error, _} -> {domain_not_found, "User's domain does not exist"}
    end.

-spec list_blocked_users(jid:lserver(), mod_blocklist_backend:list_opts()) ->
    {ok, [{jid:literal_jid(), mod_blocklist:reason()}]} | {domain_not_found, iolist()}.
list_blocked_users(Domain, Opts) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            Users = mod_blocklist_backend:list_blocked_users(HostType, Domain, Opts),
            {ok, [{jid:to_binary({LUser, Domain}), Reason} || {LUser, Reason} <- Users]};
        {error, _} ->
            {domain_not_found, "Domain does not exist"}
    end.

-spec count_blocked_users(jid:lserver()) -> {ok, non_neg_integer()} | {domain_not_found, iolist()}.
count_blocked_users(Domain) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            {ok, mod_blocklist_backend:count_blocked_users(HostType, Domain)};
        {error, _} ->
            {domain_not_found, "Domain does not exist"}
    end.

%% Helpers

terminate_sessions(JID, Reason) ->
    lists:foreach(
        fun(#session{sid = {_, Pid}}) -> ejabberd_sm:terminate_session(Pid, Reason) end,
        ejabberd_sm:get_raw_sessions(JID)).

make_reason(undefined) -> <<"Kicked by administrator">>;
make_reason(Reason) -> Reason.
