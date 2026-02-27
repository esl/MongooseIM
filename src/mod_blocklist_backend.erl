-module(mod_blocklist_backend).

%% API
-export([init/2, get_block/3, upsert_block/4, remove_block/3, remove_domain/2]).

-define(MAIN_MODULE, mod_blocklist).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback get_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> {ok, mod_blocklist:reason()} | not_found.

-callback upsert_block(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_blocklist:reason()) -> ok.

-callback remove_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().

-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.

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

