-module(mod_pubsub_cache_backend).

-export([start/2,
         stop/1,
         upsert_last_item/5,
         delete_last_item/2,
         get_last_item/2]).

-ignore_xref([stop/1]).

-define(MAIN_MODULE, mod_pubsub_cache).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-callback start(jid:lserver()) -> ok.

-callback stop() -> ok.

-callback upsert_last_item(ServerHost :: binary(),
                           Nidx :: mod_pubsub:nodeIdx(),
                           ItemID :: mod_pubsub:itemId(),
                           Publisher :: jid:jid(),
                           Payload :: mod_pubsub:payload()) -> ok | {error, Reason :: term()}.

-callback delete_last_item(ServerHost :: binary(),
                           Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.

-callback get_last_item(ServerHost :: binary(),
                        Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, LastItem :: mod_pubsub:pubsubLastItem()} | {error, Reason :: term()}.

-spec start(jid:lserver(), gen_mod:module_opts()) -> ok.
start(ServerHost, Opts = #{last_item_cache := CacheBackend}) ->
    % mongoose_backend extracts the "backend" option, but the cache backend is under the "last_item_cache" key
    % the "backend" from Opts relates to the backend for mod_pubsub
    OptsWithBackend = Opts#{backend => CacheBackend},
    TrackedFuns = [upsert_last_item, delete_last_item, get_last_item],
    mongoose_backend:init(ServerHost, ?MAIN_MODULE, TrackedFuns, OptsWithBackend),
    Args = [ServerHost],
    mongoose_backend:call(ServerHost, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec stop(jid:lserver()) -> ok.
stop(ServerHost) ->
    mongoose_backend:call(ServerHost, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec upsert_last_item(ServerHost :: binary(),
                       Nidx :: mod_pubsub:nodeIdx(),
                       ItemID :: mod_pubsub:itemId(),
                       Publisher :: jid:jid(),
                       Payload :: mod_pubsub:payload()) -> ok | {error, Reason :: term()}.
upsert_last_item(ServerHost, Nidx, ItemID, Publisher, Payload) ->
    Args = [ServerHost, Nidx, ItemID, Publisher, Payload],
    mongoose_backend:call_tracked(ServerHost, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec delete_last_item(ServerHost :: binary(),
                       Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.
delete_last_item(ServerHost, Nidx) ->
    Args = [ServerHost, Nidx],
    mongoose_backend:call_tracked(ServerHost, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_last_item(ServerHost :: binary(),
                    Nidx :: mod_pubsub:nodeIdx()) ->
                       {ok, LastItem :: mod_pubsub:pubsubLastItem()} | {error, Reason :: term()}.
get_last_item(ServerHost, Nidx) ->
    Args = [ServerHost, Nidx],
    mongoose_backend:call_tracked(ServerHost, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
