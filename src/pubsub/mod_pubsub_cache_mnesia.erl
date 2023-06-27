-module(mod_pubsub_cache_mnesia).

-behaviour(mod_pubsub_cache_backend).

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/1, stop/0]).

-export([
         upsert_last_item/5,
         get_last_item/2,
         delete_last_item/2]).

%% ------------------------ Backend start/stop ------------------------

-spec start(jid:lserver()) -> ok.
start(_) ->
    create_table(),
    ok.

-spec stop() -> ok.
stop() ->
    ok.

-spec upsert_last_item(ServerHost :: binary(),
                       Nidx :: mod_pubsub:nodeIdx(),
                       ItemID :: mod_pubsub:itemId(),
                       Publisher :: jid:jid(),
                       Payload::mod_pubsub:payload()) -> ok | {error, Reason :: term()}.
upsert_last_item(_ServerHost, Nidx, ItemId, Publisher, Payload) ->
    CreatedAt = os:system_time(microsecond),
    try mnesia:dirty_write(
        #pubsub_last_item{
        nodeid = Nidx,
        itemid = ItemId,
        creation = {CreatedAt, {Publisher#jid.luser, Publisher#jid.lserver, <<>>}},
        payload = Payload}
    ) of
        ok -> ok
    catch
        exit:{aborted, Reason} -> {error, Reason}
    end.

-spec get_last_item(ServerHost :: binary(),
                    Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, mod_pubsub:pubsubLastItem()} | {error, Reason :: term()}.
get_last_item(_ServerHost, Nidx) ->
    try mnesia:dirty_read({pubsub_last_item, Nidx}) of
        [LastItem] -> {ok, LastItem};
        [] -> {error, no_items}
    catch
        exit:{aborted, Reason} -> {error, Reason}
    end.

-spec delete_last_item(ServerHost :: binary(),
                       Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.
delete_last_item(_ServerHost, Nidx) ->
    try mnesia:dirty_delete({pubsub_last_item, Nidx}) of
        ok -> ok
    catch
        exit:{aborted, Reason} -> {error, Reason}
    end.

%% ------------------------ Helpers ----------------------------

create_table() ->
    mongoose_mnesia:create_table(
        pubsub_last_item,
        [
            {ram_copies, [node()]},
            {attributes, record_info(fields, pubsub_last_item)}
        ]).
