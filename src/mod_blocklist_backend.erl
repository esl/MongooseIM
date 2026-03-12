-module(mod_blocklist_backend).

%% API
-export([init/2,
         get_block/3,
         upsert_block/4,
         remove_block/3,
         remove_domain/2,
         count_blocked_users/2,
         list_blocked_users/3,
         % Debugging function, not exposed in the API
         clear_all/1]).

-ignore_xref([clear_all/1]).

-define(MAIN_MODULE, mod_blocklist).

-type list_opts() :: #{
    limit => non_neg_integer(),
    offset => non_neg_integer()
}.
-export_type([list_opts/0]).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback get_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> {ok, mod_blocklist:reason()} | not_found.

-callback upsert_block(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_blocklist:reason()) -> ok.

-callback remove_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().

-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.

-callback count_blocked_users(mongooseim:host_type(), jid:lserver()) -> non_neg_integer().

-callback list_blocked_users(mongooseim:host_type(), jid:lserver(), list_opts()) ->
    [{jid:luser(), mod_blocklist:reason()}].

-callback clear_all(mongooseim:host_type()) -> ok.

%% API

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [upsert_block, remove_block], Opts),
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Opts]).

-spec get_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> {ok, mod_blocklist:reason()} | not_found.
get_block(HostType, LUser, LServer) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, LUser, LServer]).

-spec upsert_block(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_blocklist:reason()) -> ok.
upsert_block(HostType, LUser, LServer, Reason) ->
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, LUser, LServer, Reason]).

-spec remove_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().
remove_block(HostType, LUser, LServer) ->
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, LUser, LServer]).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, Domain) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Domain]).

-spec count_blocked_users(mongooseim:host_type(), jid:lserver()) -> non_neg_integer().
count_blocked_users(HostType, Domain) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Domain]).

-spec list_blocked_users(mongooseim:host_type(), jid:lserver(), list_opts()) -> [{jid:luser(), mod_blocklist:reason()}].
list_blocked_users(HostType, Domain, Opts) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Domain, Opts]).

-spec clear_all(mongooseim:host_type()) -> ok.
clear_all(HostType) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]).

