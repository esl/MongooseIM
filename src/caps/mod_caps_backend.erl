-module(mod_caps_backend).

-export([init/2,
         stop/1,
         get_hash_features/2,
         get_jid_features/2,
         get_bare_jid_features/2,
         set_hash_features/3,
         set_jid_features/3,
         delete_hash_features/2,
         delete_jid_features/2]).

-callback init(mongooseim:host_type()) -> ok.
-callback stop(mongooseim:host_type()) -> ok.
-callback get_hash_features(mongooseim:host_type(), mod_caps:hash()) ->
    {ok, [mod_caps:feature()]} | {error, not_found}.
-callback get_jid_features(mongooseim:host_type(), jid:jid()) ->
    [mod_caps:feature()].
-callback get_bare_jid_features(mongooseim:host_type(), jid:jid()) ->
    [{jid:lresource(), [mod_caps:feature()]}].
-callback set_hash_features(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
-callback set_jid_features(mongooseim:host_type(), jid:jid(), [mod_caps:feature()]) -> ok.
-callback delete_hash_features(mongooseim:host_type(), mod_caps:hash()) -> ok.
-callback delete_jid_features(mongooseim:host_type(), jid:jid()) -> ok.

-ignore_xref([get_jid_features/2,
              delete_hash_features/2]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MODULE, tracked_funs(), Opts),
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec get_hash_features(mongooseim:host_type(), mod_caps:hash()) ->
          {ok, [mod_caps:feature()]} | {error, not_found}.
get_hash_features(HostType, Hash) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Hash]).

-spec get_jid_features(mongooseim:host_type(), jid:jid()) -> [mod_caps:feature()].
get_jid_features(HostType, Jid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

-spec get_bare_jid_features(mongooseim:host_type(), jid:jid()) ->
    [{jid:lresource(), [mod_caps:feature()]}].
get_bare_jid_features(HostType, Jid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

-spec set_hash_features(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
set_hash_features(HostType, Hash, Features) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Hash, Features]).

-spec set_jid_features(mongooseim:host_type(), jid:jid(), [mod_caps:feature()]) -> ok.
set_jid_features(HostType, Jid, Features) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid, Features]).

-spec delete_hash_features(mongooseim:host_type(), mod_caps:hash()) -> ok.
delete_hash_features(HostType, Hash) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Hash]).

-spec delete_jid_features(mongooseim:host_type(), jid:jid()) -> ok.
delete_jid_features(HostType, Jid) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

tracked_funs() ->
    [set_hash_features, set_jid_features, delete_hash_features, delete_jid_features].
