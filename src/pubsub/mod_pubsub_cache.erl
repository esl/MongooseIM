-module(mod_pubsub_cache).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-callback start(jid:lserver()) -> ok.

-callback stop() -> ok.

-callback upsert_last_item(ServerHost :: binary(),
                           Nidx :: mod_pubsub:nodeIdx(),
                           ItemID :: mod_pubsub:itemId(),
                           Publisher:: jid:jid(),
                           Payload :: mod_pubsub:payload()) -> ok | {error, Reason :: term()}.

-callback delete_last_item(ServerHost :: binary(),
                           Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.

-callback get_last_item(ServerHost :: binary(),
                        Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, LastItem :: mod_pubsub:pubsubLastItem()} | {error, Reason :: term()}.

