-module(mod_caps_backend).

-export([init/2, stop/1,
         get_features/2, get_resources_with_features/2, set_features/3, delete_features/2,
         get_hash_features/2, set_hash_features/3, delete_hash_features/2]).

-callback init(mongooseim:host_type()) -> ok.
-callback stop(mongooseim:host_type()) -> ok.

-callback get_features(mongooseim:host_type(), jid:jid()) -> [mod_caps:feature()].
-callback get_resources_with_features(mongooseim:host_type(), jid:jid()) ->
    [{jid:lresource(), [mod_caps:feature()]}].
-callback set_features(mongooseim:host_type(), jid:jid(), [mod_caps:feature()]) -> ok.
-callback delete_features(mongooseim:host_type(), jid:jid()) -> ok.

-callback get_hash_features(mongooseim:host_type(), mod_caps:hash()) ->
    {ok, [mod_caps:feature()]} | {error, not_found}.
-callback set_hash_features(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
-callback delete_hash_features(mongooseim:host_type(), mod_caps:hash()) -> ok.

-ignore_xref([delete_hash_features/2]). % Unused at the moment

%% API: init/stop

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MODULE, tracked_funs(), Opts),
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

%% API: jid to features

-spec get_features(mongooseim:host_type(), jid:jid()) -> [mod_caps:feature()].
get_features(HostType, Jid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

-spec get_resources_with_features(mongooseim:host_type(), jid:jid()) ->
    [{jid:lresource(), [mod_caps:feature()]}].
get_resources_with_features(HostType, Jid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

-spec set_features(mongooseim:host_type(), jid:jid(), [mod_caps:feature()]) -> ok.
set_features(HostType, Jid, Features) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid, Features]).

-spec delete_features(mongooseim:host_type(), jid:jid()) -> ok.
delete_features(HostType, Jid) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

%% API: hash to features

-spec get_hash_features(mongooseim:host_type(), mod_caps:hash()) ->
          {ok, [mod_caps:feature()]} | {error, not_found}.
get_hash_features(HostType, Hash) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Hash]).

-spec set_hash_features(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
set_hash_features(HostType, Hash, Features) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Hash, Features]).

-spec delete_hash_features(mongooseim:host_type(), mod_caps:hash()) -> ok.
delete_hash_features(HostType, Hash) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Hash]).

%% Helpers

tracked_funs() ->
    [set_features, delete_features, set_hash_features, delete_hash_features].
