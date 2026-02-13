-module(mod_blocklist_api).

%% API
-export([add_user/2, remove_user/1]).

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

%% Helpers

terminate_sessions(JID, Reason) ->
    lists:foreach(
        fun(#session{sid = {_, Pid}}) -> ejabberd_sm:terminate_session(Pid, Reason) end,
        ejabberd_sm:get_raw_sessions(JID)).

make_reason(undefined) -> <<"Kicked by administrator">>;
make_reason(Reason) -> Reason.
