-module(mod_caps_cache).
-moduledoc "API and behaviour for caching entity capabilities".

-export([init/1, stop/1, read/2, write/3, delete/2]).

-callback init(mongooseim:host_type()) -> ok.
-callback stop(mongooseim:host_type()) -> ok.
-callback read(mongooseim:host_type(), mod_caps:hash()) ->
    {ok, [mod_caps:feature()]} | {error, not_found}.
-callback write(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
-callback delete(mongooseim:host_type(), mod_caps:hash()) -> ok.

-spec init(mongooseim:host_type()) -> ok.
init(HostType) ->
    TrackedFuns = [write, delete],
    mongoose_backend:init(HostType, ?MODULE, TrackedFuns, #{backend => cets}),
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec read(mongooseim:host_type(), mod_caps:hash()) ->
          {ok, [mod_caps:feature()]} | {error, not_found}.
read(HostType, Key) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Key]).

-spec write(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
write(HostType, Key, Features) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Key, Features]).

-spec delete(mongooseim:host_type(), mod_caps:hash()) -> ok.
delete(HostType, Key) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Key]).
