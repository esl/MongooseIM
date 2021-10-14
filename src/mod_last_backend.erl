%% Just a proxy interface module between the main mod_last module and
%% the backend modules (i.e. mod_last_rdbms, mod_last_mnesia...).
-module(mod_last_backend).

-export([init/2,
         get_last/3,
         count_active_users/3,
         set_last_info/5,
         remove_user/3,
         remove_domain/2]).

-define(MAIN_MODULE, mod_last).

-callback init(mod_last:host_type(), gen_mod:module_opts()) -> ok.

-callback get_last(mod_last:host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | {error, term()} | not_found.

-callback count_active_users(mod_last:host_type(), jid:lserver(), mod_last:timestamp()) ->
    non_neg_integer().

-callback set_last_info(
            mod_last:host_type(),
            jid:luser(),
            jid:lserver(),
            mod_last:timestamp(),
            mod_last:status()) -> ok | {error, term()}.

-callback remove_user(mod_last:host_type(), jid:luser(), jid:lserver()) ->
    ok | {error, term()}.

-callback remove_domain(mod_last:host_type(), jid:lserver()) ->
    ok | {error, term()}.

-spec init(mod_last:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [get_last, set_last_info],
    mongoose_backend:init_per_host_type(HostType, ?MAIN_MODULE, TrackedFuns),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_last(mod_last:host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | {error, term()} | not_found.
get_last(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec count_active_users(mod_last:host_type(), jid:lserver(), mod_last:timestamp()) ->
    non_neg_integer().
count_active_users(HostType, LServer, Timestamp) ->
    Args = [HostType, LServer, Timestamp],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_last_info(
            mod_last:host_type(),
            jid:luser(),
            jid:lserver(),
            mod_last:timestamp(),
            mod_last:status()) -> ok | {error, term()}.
set_last_info(HostType, LUser, LServer, Timestamp, Status) ->
    Args = [HostType, LUser, LServer, Timestamp, Status],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(mod_last:host_type(), jid:luser(), jid:lserver()) ->
    ok | {error, term()}.
remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mod_last:host_type(), jid:lserver()) ->
    ok | {error, term()}.
remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).