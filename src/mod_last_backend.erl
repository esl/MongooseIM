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

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback get_last(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | {error, term()} | not_found.

-callback count_active_users(mongooseim:host_type(), jid:lserver(), mod_last:timestamp()) ->
    non_neg_integer().

-callback set_last_info(
            mongooseim:host_type(),
            jid:luser(),
            jid:lserver(),
            mod_last:timestamp(),
            mod_last:status()) -> ok | {error, term()}.

-callback remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    ok | {error, term()}.

-callback remove_domain(mongooseim:host_type(), jid:lserver()) ->
    ok | {error, term()}.

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [get_last, set_last_info],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_last(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | {error, term()} | not_found.
get_last(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec count_active_users(mongooseim:host_type(), jid:lserver(), mod_last:timestamp()) ->
    non_neg_integer().
count_active_users(HostType, LServer, Timestamp) ->
    Args = [HostType, LServer, Timestamp],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_last_info(
            mongooseim:host_type(),
            jid:luser(),
            jid:lserver(),
            mod_last:timestamp(),
            mod_last:status()) -> ok | {error, term()}.
set_last_info(HostType, LUser, LServer, Timestamp, Status) ->
    Args = [HostType, LUser, LServer, Timestamp, Status],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    ok | {error, term()}.
remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) ->
    ok | {error, term()}.
remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
