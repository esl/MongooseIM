-module(mod_caps_state).
-moduledoc "API and behaviour for caching entity capabilities".

-export([init/1, stop/1, get/2, get_resources/2, set/3, delete/2]).

-callback init(mongooseim:host_type()) -> ok.
-callback stop(mongooseim:host_type()) -> ok.
-callback get(mongooseim:host_type(), jid:ljid()) -> [mod_caps:feature()].
-callback get_resources(mongooseim:host_type(), jid:simple_bare_jid()) ->
    [{jid:ljid(), [mod_caps:feature()]}].
-callback set(mongooseim:host_type(), jid:ljid(), [mod_caps:feature()]) -> ok.
-callback delete(mongooseim:host_type(), jid:ljid()) -> ok.

-spec init(mongooseim:host_type()) -> ok.
init(HostType) ->
    TrackedFuns = [set, delete],
    mongoose_backend:init(HostType, ?MODULE, TrackedFuns, #{backend => cets}),
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec get(mongooseim:host_type(), jid:ljid()) -> [mod_caps:feature()].
get(HostType, Key) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Key]).

-spec get_resources(mongooseim:host_type(), jid:simple_bare_jid()) ->
          [{jid:lresource(), [mod_caps:feature()]}].
get_resources(HostType, Key) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Key]).

-spec set(mongooseim:host_type(), jid:ljid(), [mod_caps:feature()]) -> ok.
set(HostType, Key, Features) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Key, Features]).

-spec delete(mongooseim:host_type(), jid:ljid()) -> ok.
delete(HostType, Key) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Key]).
