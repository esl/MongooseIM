%% Just a proxy interface module between the main mod_roster module and
%% the backend modules (i.e. mod_roster_rdbms, mod_roster_mnesia...).
-module(mod_roster_backend).
-export([init/2,
         transaction/2,
         read_roster_version/3,
         write_roster_version/5,
         get_roster/3,
         get_roster_entry/6,
         get_subscription_lists/3,
         roster_subscribe_t/2,
         update_roster_t/2,
         del_roster_t/4,
         remove_user_t/3,
         remove_domain_t/2]).

-define(MAIN_MODULE, mod_roster).

%% ----------------------------------------------------------------------
%% Callbacks
%% (exactly the same as specs in this module)

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback transaction(mongooseim:host_type(), fun(() -> any())) ->
    {aborted, any()} | {atomic, any()} | {error, any()}.

-callback read_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    binary() | error.

-callback write_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver(),
                               mod_roster:transaction_state(), mod_roster:version()) -> ok.

-callback get_roster(mongooseim:host_type(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].

-callback get_roster_entry(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact(),
                           mod_roster:transaction_state(), mod_roster:entry_format()) ->
    mod_roster:roster() | does_not_exist | error.

-callback get_subscription_lists(mongoose_acc:t(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].

-callback roster_subscribe_t(mongooseim:host_type(), mod_roster:roster()) -> ok.

-callback update_roster_t(mongooseim:host_type(), mod_roster:roster()) -> ok.

-callback del_roster_t(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact()) -> ok.

-callback remove_user_t(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.

-callback remove_domain_t(mongooseim:host_type(), jid:lserver()) -> ok.

-optional_callbacks([remove_domain_t/2]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [read_roster_version,
                   write_roster_version,
                   get_roster,
                   get_roster_entry,
                   get_subscription_lists,
                   roster_subscribe_t,
                   update_roster_t,
                   del_roster_t],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec transaction(mongooseim:host_type(), fun(() -> any())) ->
    {aborted, any()} | {atomic, any()} | {error, any()}.
transaction(HostType, F) ->
    Args = [HostType, F],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).


-spec read_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    binary() | error.
read_roster_version(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec write_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver(),
                               mod_roster:transaction_state(), mod_roster:version()) -> ok.
write_roster_version(HostType, LUser, LServer, TransactionState, Ver) ->
    Args = [HostType, LUser, LServer, TransactionState, Ver],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_roster(mongooseim:host_type(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_roster(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_roster_entry(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact(),
                           mod_roster:transaction_state(), mod_roster:entry_format()) ->
    mod_roster:roster() | does_not_exist | error.
get_roster_entry(HostType, LUser, LServer, LJid, TransactionState, Format) ->
    Args = [HostType, LUser, LServer, LJid, TransactionState, Format],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_subscription_lists(mongoose_acc:t(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_subscription_lists(Acc, LUser, LServer) ->
    Args = [Acc, LUser, LServer],
    mongoose_backend:call_tracked(mongoose_acc:host_type(Acc), ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec roster_subscribe_t(mongooseim:host_type(), mod_roster:roster()) -> ok.
roster_subscribe_t(HostType, Roster) ->
    Args = [HostType, Roster],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec update_roster_t(mongooseim:host_type(), mod_roster:roster()) -> ok.
update_roster_t(HostType, Roster) ->
    Args = [HostType, Roster],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec del_roster_t(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact()) -> ok.
del_roster_t(HostType, LUser, LServer, LJid) ->
    Args = [HostType, LUser, LServer, LJid],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user_t(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user_t(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain_t(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain_t(HostType, LServer) ->
    Args = [HostType, LServer],
    case mongoose_backend:is_exported(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, 2) of
        true ->
            mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args);
        false ->
            ok
    end.
