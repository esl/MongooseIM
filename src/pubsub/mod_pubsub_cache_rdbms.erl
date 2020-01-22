-module(mod_pubsub_cache_rdbms).

-behaviour(mod_pubsub_cache).

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
    UniqueKeyValues = [Nidx],
    Res = rdbms_queries:execute_upsert(ServerHost, pubsub_last_item_upsert,
                                       InsertParams, UpdateParams, UniqueKeyValues),
    convert_rdbms_response(Res).

-spec delete_last_item(ServerHost :: binary(),
                       Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.
delete_last_item(ServerHost, Nidx) ->
    DeleteQuerySQL = delete_pubsub_last_item(Nidx),
    Res = mongoose_rdbms:sql_query(ServerHost, DeleteQuerySQL),
    convert_rdbms_response(Res).

-spec get_last_item(ServerHost :: binary(),
                    Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, LastItem :: mod_pubsub:pubsubLastItem()} | {error, Reason :: term()}.
get_last_item(ServerHost, Nidx) ->
    ReadQuerySQL = get_pubsub_last_item(Nidx),
    Res = mongoose_rdbms:sql_query(ServerHost, ReadQuerySQL),
    convert_rdbms_response(Res).

-spec get_pubsub_last_item(mod_pubsub:nodeIdx()) -> iolist().
get_pubsub_last_item(Nidx) ->
    ["SELECT nidx, itemid, created_luser, created_at, created_lserver, payload FROM pubsub_last_item"
     " WHERE nidx = ", esc_int(Nidx), ";"].

-spec delete_pubsub_last_item(mod_pubsub:nodeIdx()) -> iolist().
delete_pubsub_last_item(Nidx) ->
    ["DELETE FROM pubsub_last_item"
    " WHERE nidx = ", esc_int(Nidx), ";"].

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
    ?ERROR_MSG("RDBMS cache failed with: ~p", [Response]),
    {error, pubsub_rdbms_cache_failed}.

esc_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

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
    Creation = {usec:to_now(mongoose_rdbms:result_to_integer(CreatedAt)),
                {CreatedLUser, CreatedLServer, <<>>}},
    #pubsub_last_item{itemid = ItemId,
                      nodeid = NodeIdx,
                      creation = Creation,
                      payload = Payload}.
