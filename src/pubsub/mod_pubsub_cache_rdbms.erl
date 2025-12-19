-module(mod_pubsub_cache_rdbms).

-behaviour(mod_pubsub_cache_backend).

-include("pubsub.hrl").
-include("jlib.hrl").
-include("mongoose_logger.hrl").

-export([start/1, stop/0]).

-export([
         upsert_last_item/5,
         delete_last_item/2,
         get_last_item/2]).
%% ------------------------ Backend start/stop ------------------------

-spec start(jid:lserver()) -> ok.
start(Host) ->
    InsertFields = [<<"nidx">>, <<"itemid">>, <<"created_luser">>,
                    <<"created_lserver">>, <<"created_at">>, <<"payload">>],
    UpdateFields = [<<"itemid">>, <<"created_luser">>, <<"created_lserver">>,
                    <<"created_at">>, <<"payload">>],
    mongoose_rdbms:prepare(pubsub_get_last_item, pubsub_last_item, [nidx],
        <<"SELECT nidx, itemid, created_luser, created_at, created_lserver, payload "
          "FROM pubsub_last_item WHERE nidx = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_last_item, pubsub_last_item, [nidx],
        <<"DELETE FROM pubsub_last_item WHERE nidx = ?">>),
    rdbms_queries:prepare_upsert(Host, pubsub_last_item_upsert, pubsub_last_item,
                                 InsertFields,
                                 UpdateFields,
                                 [<<"nidx">>]),
    ok.

-spec stop() -> ok.
stop() -> ok.

%% ------------------- Pubusub last item ------------------------------

-spec upsert_last_item(ServerHost :: jid:lserver(),
                       Nidx :: mod_pubsub:nodeIdx(),
                       ItemID :: mod_pubsub:itemId(),
                       Publisher :: jid:jid(),
                       Payload :: mod_pubsub:payload()) -> ok | {error, Reason :: term()}.
upsert_last_item(ServerHost, Nidx, ItemID, Publisher, Payload) ->
    {ModifiedLUser, ModifiedLServer,
     CreatedAt, PayloadBin} = prepare_upsert_params(Publisher, Payload),
    UpdateParams = [ItemID, ModifiedLUser, ModifiedLServer, CreatedAt, PayloadBin],
    InsertParams = [Nidx | UpdateParams],
    Res = rdbms_queries:execute_upsert(ServerHost, pubsub_last_item_upsert, InsertParams, UpdateParams),
    convert_rdbms_response(Res).

-spec delete_last_item(ServerHost :: binary(),
                       Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.
delete_last_item(ServerHost, Nidx) ->
    Res = mongoose_rdbms:execute_successfully(ServerHost, pubsub_delete_last_item, [Nidx]),
    convert_rdbms_response(Res).

-spec get_last_item(ServerHost :: binary(),
                    Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, LastItem :: mod_pubsub:pubsubLastItem()} | {error, Reason :: term()}.
get_last_item(ServerHost, Nidx) ->
    Res = mongoose_rdbms:execute_successfully(ServerHost, pubsub_get_last_item, [Nidx]),
    convert_rdbms_response(Res).

%%====================================================================
%% Helpers
%%====================================================================

convert_rdbms_response({selected, []}) ->
    {error, no_items};
convert_rdbms_response({selected, [SelectedItem]}) ->
    LastItem = item_to_record(SelectedItem),
    {ok, LastItem};
convert_rdbms_response({updated, _}) ->
    ok;
convert_rdbms_response(Response) ->
    ?LOG_ERROR(#{what => pubsub_rdbms_cache_failed, reason => Response}),
    {error, pubsub_rdbms_cache_failed}.

prepare_upsert_params(Publisher, Payload) ->
    PayloadXML = #xmlel{name = <<"item">>, children = Payload},
    CreatedAt = os:system_time(microsecond),
    BinaryPayload = exml:to_binary(PayloadXML),
    ModifiedLUser = Publisher#jid.luser,
    ModifiedLServer = Publisher#jid.lserver,
    {
        ModifiedLUser, ModifiedLServer,
        CreatedAt, BinaryPayload
    }.

item_to_record({NodeIdx, ItemId, CreatedLUser, CreatedAt, CreatedLServer, PayloadDB}) ->
    PayloadXML = mongoose_rdbms:unescape_binary(global, PayloadDB),
    {ok, #xmlel{children = Payload}} = exml:parse(PayloadXML),
    Creation = {mongoose_rdbms:result_to_integer(CreatedAt),
                {CreatedLUser, CreatedLServer, <<>>}},
    #pubsub_last_item{itemid = ItemId,
                      nodeid = NodeIdx,
                      creation = Creation,
                      payload = Payload}.
