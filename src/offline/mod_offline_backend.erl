%% Just a proxy interface module between the main mod_offline module and
%% the backend modules (i.e. mod_offline_mnesia, mod_offline_rdbms...).
-module(mod_offline_backend).

-export([init/2,
         pop_messages/2,
         fetch_messages/2,
         write_messages/4,
         count_offline_messages/4,
         remove_expired_messages/2,
         remove_old_messages/3,
         remove_user/3,
         remove_domain/2]).

-define(MAIN_MODULE, mod_offline).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback pop_messages(mongooseim:host_type(), jid:jid()) ->
    {ok, [mod_offline:msg()]} | {error, any()}.

-callback fetch_messages(mongooseim:host_type(), jid:jid()) ->
    {ok, [mod_offline:msg()]} | {error, any()}.

-callback write_messages(mongooseim:host_type(), jid:luser(), jid:lserver(), [mod_offline:msg()]) ->
    ok | {error, any()}.

-callback count_offline_messages(mongooseim:host_type(), jid:luser(), jid:lserver(), Limit :: mod_offline:msg_count()) ->
    mod_offline:msg_count().

-callback remove_expired_messages(mongooseim:host_type(), jid:lserver()) ->
    {ok, mod_offline:msg_count()} | {error, any()}.

-callback remove_old_messages(mongooseim:host_type(), jid:lserver(), mod_offline:timestamp()) ->
    {ok, mod_offline:msg_count()} | {error, any()}.

-callback remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.

-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.

-optional_callbacks([remove_domain/2]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [pop_messages, write_messages], Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec pop_messages(mongooseim:host_type(), jid:jid()) ->
    {ok, [mod_offline:msg()]} | {error, any()}.
pop_messages(HostType, UserJID) ->
    Args = [HostType, UserJID],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec fetch_messages(mongooseim:host_type(), jid:jid()) ->
    {ok, [mod_offline:msg()]} | {error, any()}.
fetch_messages(HostType, UserJID) ->
    Args = [HostType, UserJID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec write_messages(mongooseim:host_type(), jid:luser(), jid:lserver(), [mod_offline:msg()]) ->
    ok | {error, any()}.
write_messages(HostType, LUser, LServer, Msgs) ->
    Args = [HostType, LUser, LServer, Msgs],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec count_offline_messages(mongooseim:host_type(), jid:luser(), jid:lserver(), Limit :: mod_offline:msg_count()) ->
    mod_offline:msg_count().
count_offline_messages(HostType, LUser, LServer, Limit) ->
    Args = [HostType, LUser, LServer, Limit],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_expired_messages(mongooseim:host_type(), jid:lserver()) ->
    {ok, mod_offline:msg_count()} | {error, any()}.
remove_expired_messages(HostType, LServer) ->
    Args = [HostType, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_old_messages(mongooseim:host_type(), jid:lserver(), mod_offline:timestamp()) ->
    {ok, mod_offline:msg_count()} | {error, any()}.
remove_old_messages(HostType, LServer, Timestamp) ->
    Args = [HostType, LServer, Timestamp],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    case mongoose_backend:is_exported(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, 2) of
        true ->
            mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args);
        false -> 
            ok
    end.
