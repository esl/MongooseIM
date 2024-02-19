%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc RDBMS backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_cassandra_arch).
-behaviour(mongoose_cassandra).
-behaviour(ejabberd_gen_mam_archive).
-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1, hooks/1]).

%% MAM hook handlers
-export([archive_size/3,
         archive_message/3,
         lookup_messages/3,
         remove_archive/3,
         get_mam_muc_gdpr_data/3]).

%% mongoose_cassandra callbacks
-export([prepared_queries/0]).

%% ----------------------------------------------------------------------
%% Imports

%% UID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

%% Other
-import(mod_mam_utils,
        [maybe_min/2,
         maybe_max/2,
         apply_start_border/2,
         apply_end_border/2]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-record(mam_muc_ca_filter, {
          room_jid,
          with_nick,
          nick_name,
          start_id,
          end_id
         }).

-record(mam_muc_message, {
          id :: non_neg_integer(),
          room_jid :: binary(),
          from_jid :: binary() | undefined,
          nick_name :: binary(),
          with_nick :: binary(),
          message :: binary() | undefined
         }).

-callback encode(binary()) -> binary().
-callback decode(binary()) -> binary().

%% ----------------------------------------------------------------------
%% Types

-type filter() :: #mam_muc_ca_filter{}.
-type message_id() :: non_neg_integer().
-type host_type() :: mongooseim:host_type().

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(host_type()) -> ok.
stop(_HostType) ->
    ok.

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{mam_muc_archive_message, HostType, fun ?MODULE:archive_message/3, #{}, 50},
     {mam_muc_archive_size, HostType, fun ?MODULE:archive_size/3, #{}, 50},
     {mam_muc_lookup_messages, HostType, fun ?MODULE:lookup_messages/3, #{}, 50},
     {mam_muc_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 50},
     {get_mam_muc_gdpr_data, HostType, fun ?MODULE:get_mam_muc_gdpr_data/3, #{}, 50}].

%% ----------------------------------------------------------------------
%% mongoose_cassandra_worker callbacks

prepared_queries() ->
    [
     {insert_offset_hint_query, insert_offset_hint_query_cql()},
     {prev_offset_query, prev_offset_query_cql()},
     {insert_query, insert_query_cql()},
     {fetch_user_messages_query, fetch_user_messages_cql()},
     {select_for_removal_query, select_for_removal_query_cql()},
     {remove_archive_query, remove_archive_query_cql()},
     {remove_archive_offsets_query, remove_archive_offsets_query_cql()}]
        ++ extract_messages_queries()
        ++ extract_messages_r_queries()
        ++ calc_count_queries()
        ++ list_message_ids_queries().

%%====================================================================
%% Internal functions
%%====================================================================

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec archive_size(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: integer(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, room := jid:jid()},
    Extra :: gen_hook:extra().
archive_size(Size, #{room := RoomJID}, #{host_type := HostType}) when is_integer(Size) ->
    PoolName = pool_name(HostType),
    Borders = Start = End = WithNick = undefined,
    Filter = prepare_filter(RoomJID, Borders, Start, End, WithNick, undefined),
    {ok, calc_count(PoolName, RoomJID, HostType, Filter)}.


%% ----------------------------------------------------------------------
%% INSERT MESSAGE

insert_query_cql() ->
    "INSERT INTO mam_muc_message "
        "(id, room_jid, from_jid, nick_name, with_nick, message) "
        "VALUES (?, ?, ?, ?, ?, ?)".

-spec archive_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok | {error, term()},
    Params :: mod_mam:archive_message_params(),
    Extra :: gen_hook:extra().
archive_message(_Result, Params, #{host_type := HostType}) ->
    try
        {ok, archive_message2(Params, HostType)}
    catch _Type:Reason ->
        {ok, {error, Reason}}
    end.

archive_message2(#{message_id := MessID,
                   local_jid := LocJID = #jid{},
                   remote_jid := FromJID = #jid{},
                   source_jid := #jid{lresource = BNick},
                   packet := Packet}, HostType) ->
    BLocJID = mod_mam_utils:bare_jid(LocJID),
    BFromJID = mod_mam_utils:bare_jid(FromJID),
    BPacket = packet_to_stored_binary(HostType, Packet),
    Messages = [#mam_muc_message{
                 id        = MessID,
                 room_jid  = BLocJID,
                 from_jid  = BWithFromJID,
                 nick_name = BNick,
                 message   = BPacket,
                 with_nick = BWithNick
                } || {BWithNick, BWithFromJID} <- [{<<>>, BFromJID}, {BNick, <<>>}]],
    write_messages(HostType, Messages).

write_messages(HostType, Messages) ->
    PoolName = pool_name(HostType),
    RoomJID = undefined,
    MultiParams = [message_to_params(M) || M <- Messages],
    mongoose_cassandra:cql_write_async(PoolName, RoomJID, ?MODULE, insert_query, MultiParams).

message_to_params(#mam_muc_message{
                     id        = MessID,
                     room_jid  = BLocJID,
                     from_jid  = BFromJID,
                     nick_name = BNick,
                     with_nick = BWithNick,
                     message   = BPacket
                    }) ->
    #{id => MessID, room_jid => BLocJID, from_jid => BFromJID,
      nick_name => BNick, with_nick => BWithNick, message => BPacket}.


%% ----------------------------------------------------------------------
%% REMOVE ARCHIVE

remove_archive_query_cql() ->
    "DELETE FROM mam_muc_message WHERE room_jid = ? AND with_nick = ?".

remove_archive_offsets_query_cql() ->
    "DELETE FROM mam_muc_message_offset WHERE room_jid = ? AND with_nick = ?".

select_for_removal_query_cql() ->
    "SELECT DISTINCT room_jid, with_nick FROM mam_muc_message WHERE room_jid = ?".

-spec remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, room := jid:jid()},
    Extra :: gen_hook:extra().
remove_archive(Acc, #{room := RoomJID}, #{host_type := HostType}) ->
    BRoomJID = mod_mam_utils:bare_jid(RoomJID),
    PoolName = pool_name(HostType),
    Params = #{room_jid => BRoomJID},
    %% Wait until deleted

    DeleteFun =
        fun(Rows, _AccIn) ->
                mongoose_cassandra:cql_write(PoolName, RoomJID, ?MODULE,
                                             remove_archive_query, Rows),
                mongoose_cassandra:cql_write(PoolName, RoomJID, ?MODULE,
                                             remove_archive_offsets_query, Rows)
        end,

    mongoose_cassandra:cql_foldl(PoolName, RoomJID, ?MODULE,
                                 select_for_removal_query, Params, DeleteFun, []),
    {ok, Acc}.

%% ----------------------------------------------------------------------
%% SELECT MESSAGES

-spec lookup_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {ok, mod_mam:lookup_result()},
    Params :: mam_iq:lookup_params(),
    Extra :: gen_hook:extra().
lookup_messages({error, _Reason} = Result, _Params, _Extra) ->
    {ok, Result};
lookup_messages(_Result, #{search_text := <<_/binary>>}, _Extra) ->
    {ok, {error, 'not-supported'}};
lookup_messages(_Result,
                #{owner_jid := RoomJID, rsm := RSM, borders := Borders,
                  start_ts := Start, end_ts := End, with_jid := WithJID,
                  search_text := undefined, page_size := PageSize,
                  is_simple := IsSimple, message_id := MsgID},
                #{host_type := HostType}) ->
    try
        WithNick = maybe_jid_to_nick(WithJID),
        PoolName = pool_name(HostType),
        {ok, lookup_messages2(PoolName, HostType,
                         RoomJID, RSM, Borders,
                         Start, End, WithNick,
                         PageSize, MsgID, IsSimple)}
    catch _Type:Reason:Stacktrace ->
            {ok, {error, {Reason, {stacktrace, Stacktrace}}}}
    end.

maybe_jid_to_nick(#jid{lresource = BNick}) -> BNick;
maybe_jid_to_nick(undefined) -> undefined.


lookup_messages2(PoolName, HostType,
                 RoomJID = #jid{}, RSM, Borders,
                 Start, End, WithNick,
                 PageSize, MsgID, _IsSimple = true) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(RoomJID, Borders, Start, End, WithNick, MsgID),
    lookup_messages_simple(PoolName, HostType, RoomJID, RSM, PageSize, Filter);
lookup_messages2(PoolName, HostType,
                 RoomJID = #jid{}, RSM, Borders,
                 Start, End, WithNick,
                 PageSize, MsgID, _IsSimple) ->
    %% Query with offset calculation
    %% We cannot just use RDBMS code because "LIMIT X, Y" is not supported by cassandra
    %% Not all queries are optimal. You would like to disable something for production
    %% once you know how you will call bd
    Strategy = rsm_to_strategy(RSM),
    Filter = prepare_filter(RoomJID, Borders, Start, End, WithNick, MsgID),
    case Strategy of
        last_page ->
            lookup_messages_last_page(PoolName, HostType, RoomJID, RSM, PageSize, Filter);
        by_offset ->
            lookup_messages_by_offset(PoolName, HostType, RoomJID, RSM, PageSize, Filter);
        first_page ->
            lookup_messages_first_page(PoolName, HostType, RoomJID, RSM, PageSize, Filter);
        before_id ->
            lookup_messages_before_id(PoolName, HostType, RoomJID, RSM, PageSize, Filter);
        after_id ->
            lookup_messages_after_id(PoolName, HostType, RoomJID, RSM, PageSize, Filter)
    end.

rsm_to_strategy(#rsm_in{direction = before, id = undefined}) ->
    last_page;
rsm_to_strategy(#rsm_in{direction = undefined, index = 0}) ->
    first_page;
rsm_to_strategy(#rsm_in{direction = undefined, index = Offset}) when is_integer(Offset) ->
    by_offset;
rsm_to_strategy(#rsm_in{direction = before, id = Id}) when is_integer(Id) ->
    before_id;
rsm_to_strategy(#rsm_in{direction = aft, id = Id}) when is_integer(Id) ->
    after_id;
rsm_to_strategy(#rsm_in{}) ->
    first_page;
rsm_to_strategy(undefined) ->
    first_page.

lookup_messages_simple(PoolName, HostType, RoomJID,
                       #rsm_in{direction = aft, id = ID},
                       PageSize, Filter) ->
    %% Get last rows from result set
    MessageRows = extract_messages(PoolName, RoomJID, HostType, after_id(ID, Filter), PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, HostType, RoomJID)}};
lookup_messages_simple(PoolName, HostType, RoomJID,
                       #rsm_in{direction = before, id = ID},
                       PageSize, Filter) ->
    MessageRows = extract_messages(PoolName, RoomJID, HostType, before_id(ID, Filter), PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, HostType, RoomJID)}};
lookup_messages_simple(PoolName, HostType, RoomJID,
                       #rsm_in{direction = undefined, index = Offset},
                       PageSize, Filter) ->
    %% Apply offset
    StartId = offset_to_start_id(PoolName, RoomJID, Filter,
                                 Offset), %% POTENTIALLY SLOW AND NOT SIMPLE :)
    MessageRows = extract_messages(PoolName, RoomJID, HostType, from_id(StartId, Filter), PageSize,
                                   false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, HostType, RoomJID)}};
lookup_messages_simple(PoolName, HostType, RoomJID,
                       _,
                       PageSize, Filter) ->
    MessageRows = extract_messages(PoolName, RoomJID, HostType, Filter, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, HostType, RoomJID)}}.

lookup_messages_last_page(PoolName, HostType, RoomJID,
                          #rsm_in{direction = before, id = undefined},
                          0, Filter) ->
    %% Last page
    TotalCount = calc_count(PoolName, RoomJID, HostType, Filter),
    {ok, {TotalCount, TotalCount, []}};
lookup_messages_last_page(PoolName, HostType, RoomJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(PoolName, RoomJID, HostType, Filter, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows, HostType, RoomJID)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(PoolName, RoomJID, HostType, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows, HostType, RoomJID)}}
    end.

lookup_messages_by_offset(PoolName, HostType, RoomJID,
                          #rsm_in{direction = undefined, index = Offset},
                          0, Filter) when is_integer(Offset) ->
    %% By offset
    TotalCount = calc_count(PoolName, RoomJID, HostType, Filter),
    {ok, {TotalCount, Offset, []}};
lookup_messages_by_offset(PoolName, HostType, RoomJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) when is_integer(Offset) ->
    %% By offset
    StartId = offset_to_start_id(PoolName, RoomJID, Filter, Offset), %% POTENTIALLY SLOW
    MessageRows = extract_messages(PoolName, RoomJID, HostType, from_id(StartId, Filter), PageSize,
                                   false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows, HostType, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(PoolName, RoomJID, HostType, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(MessageRows, HostType, RoomJID)}}
    end.

lookup_messages_first_page(PoolName, HostType, RoomJID,
                           _,
                           0, Filter) ->
    %% First page, just count
    TotalCount = calc_count(PoolName, RoomJID, HostType, Filter),
    {ok, {TotalCount, 0, []}};
lookup_messages_first_page(PoolName, HostType, RoomJID,
                           _,
                           PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(PoolName, RoomJID, HostType, Filter, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows, HostType, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(PoolName, RoomJID, HostType, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(MessageRows, HostType, RoomJID)}}
    end.

lookup_messages_before_id(PoolName, HostType, RoomJID,
                          RSM = #rsm_in{direction = before, id = ID},
                          PageSize, Filter) ->
    TotalCount = calc_count(PoolName, RoomJID, HostType, Filter),
    Offset = calc_offset(PoolName, RoomJID, HostType, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(PoolName, RoomJID, HostType, to_id(ID, Filter),
                                   PageSize + 1, true),
    Result = {TotalCount, Offset, rows_to_uniform_format(MessageRows, HostType, RoomJID)},
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result).

lookup_messages_after_id(PoolName, HostType, RoomJID,
                         RSM = #rsm_in{direction = aft, id = ID},
                         PageSize, Filter) ->
    PoolName = pool_name(HostType),
    TotalCount = calc_count(PoolName, RoomJID, HostType, Filter),
    Offset = calc_offset(PoolName, RoomJID, HostType, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(PoolName, RoomJID, HostType, from_id(ID, Filter),
                                   PageSize + 1, false),
    Result = {TotalCount, Offset, rows_to_uniform_format(MessageRows, HostType, RoomJID)},
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result).


after_id(ID, Filter = #mam_muc_ca_filter{start_id = AfterID}) ->
    Filter#mam_muc_ca_filter{start_id = maybe_max(ID + 1, AfterID)}.

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter = #mam_muc_ca_filter{end_id = BeforeID}) ->
    Filter#mam_muc_ca_filter{end_id = maybe_min(ID - 1, BeforeID)}.

to_id(ID, Filter = #mam_muc_ca_filter{end_id = BeforeID}) ->
    Filter#mam_muc_ca_filter{end_id = maybe_min(ID, BeforeID)}.

from_id(ID, Filter = #mam_muc_ca_filter{start_id = AfterID}) ->
    Filter#mam_muc_ca_filter{start_id = maybe_max(ID, AfterID)}.

rows_to_uniform_format(MessageRows, HostType, RoomJID) ->
    [row_to_uniform_format(Row, HostType, RoomJID) || Row <- MessageRows].

row_to_uniform_format(#{nick_name := BNick, message := Data, id := MessID},
                      HostType, RoomJID) ->
    SrcJID = jid:replace_resource(RoomJID, BNick),
    Packet = stored_binary_to_packet(HostType, Data),
    #{id => MessID, jid => SrcJID, packet => Packet}.

row_to_message_id(#{id := MsgID}) ->
    MsgID.

-spec get_mam_muc_gdpr_data(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ejabberd_gen_mam_archive:mam_muc_gdpr_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
get_mam_muc_gdpr_data(Acc, #{jid := Jid}, #{host_type := HostType}) ->
    BinJid = jid:to_binary(jid:to_lower(Jid)),
    PoolName = pool_name(HostType),
    FilterMap = #{from_jid  => BinJid},
    Rows = fetch_user_messages(PoolName, Jid, FilterMap),
    Messages = [{Id, exml:to_binary(stored_binary_to_packet(HostType, Data))}
                || #{message := Data, id:= Id} <- Rows],
    {ok, Messages ++ Acc}.

%% Offset is not supported
%% Each record is a tuple of form
%% `{<<"13663125233">>, <<"bob@localhost">>, <<"res1">>, <<binary>>}'.
%% Columns are `["id", "nick_name", "message"]'.
-spec extract_messages(PoolName, RoomJID, HostType, Filter, IMax, ReverseLimit) ->
                              [Row] when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid:jid(),
      HostType :: host_type(),
      Filter :: filter(),
      IMax :: pos_integer(),
      ReverseLimit :: boolean(),
      Row :: mongoose_cassandra:row().
extract_messages(_Worker, _RoomJID, _HostType, _Filter, 0, _) ->
    [];
extract_messages(PoolName, RoomJID, _HostType, Filter, IMax, false) ->
    QueryName = {extract_messages_query, select_filter(Filter)},
    Params = maps:put('[limit]', IMax, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName, Params),
    Rows;
extract_messages(PoolName, RoomJID, _HostType, Filter, IMax, true) ->
    QueryName = {extract_messages_r_query, select_filter(Filter)},
    Params = maps:put('[limit]', IMax, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName, Params),
    lists:reverse(Rows).

fetch_user_messages(PoolName, UserJID, FilterMap) ->
    QueryName = fetch_user_messages_query,
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, QueryName, FilterMap),
    lists:sort(Rows).


%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
-spec calc_index(PoolName, RoomJID, HostType, Filter, MessID) -> Count
                                                                 when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid:jid(),
      HostType :: host_type(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_index(PoolName, RoomJID, HostType, Filter, MessID) ->
    calc_count(PoolName, RoomJID, HostType, to_id(MessID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
-spec calc_before(PoolName, RoomJID, HostType, Filter, MessID) -> Count
                                                                  when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid:jid(),
      HostType :: host_type(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_before(PoolName, RoomJID, HostType, Filter, MessID) ->
    calc_count(PoolName, RoomJID, HostType, before_id(MessID, Filter)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE "
-spec calc_count(PoolName, RoomJID, HostType, Filter) -> Count
                                                         when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid:jid(),
      HostType :: host_type(),
      Filter :: filter(),
      Count :: non_neg_integer().
calc_count(PoolName, RoomJID, _HostType, Filter) ->
    QueryName = {calc_count_query, select_filter(Filter)},
    Params = eval_filter_params(Filter),
    {ok, [#{count := Count}]} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName,
                                              Params),
    Count.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
%% Uses previously calculated offsets to speed up queries
-spec offset_to_start_id(PoolName, RoomJID, Filter, Offset) -> Id when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid:jid(),
      Offset :: non_neg_integer(),
      Filter :: filter(),
      Id :: non_neg_integer() | undefined.
offset_to_start_id(PoolName, RoomJID, Filter, Offset) when is_integer(Offset), Offset >= 0,
                                                           Offset =< 100 ->
    calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset);
offset_to_start_id(PoolName, RoomJID, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    Params = maps:put(offset, Offset, eval_filter_params(Filter)),
    %% Try to find already calculated nearby offset to reduce query size
    case mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, prev_offset_query, Params) of
        {ok, []} -> %% No hints, just calculate offset sloooowly
            StartId = calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset),
            maybe_save_offset_hint(PoolName, RoomJID, Filter, 0, Offset, StartId);
        {ok, [#{offset := PrevOffset, id := PrevId}]} ->
            %% Offset hint found, use it to reduce query size
            case Offset of
                PrevOffset -> PrevId;
                _ ->
                    StartId = calc_offset_to_start_id(PoolName, RoomJID,
                                                      Filter#mam_muc_ca_filter{start_id = PrevId},
                                                      Offset - PrevOffset + 1),
                    maybe_save_offset_hint(PoolName, RoomJID, Filter, PrevOffset, Offset, StartId)
            end
    end.

%% @doc Saves offset hint for future use in order to speed up queries with similar offset
%% Hint is save only if previous offset hint was 50+ entires from current query
%% This function returns given StartId as passthrough for convenience
-spec maybe_save_offset_hint(PoolName :: mongoose_cassandra:pool_name(), RoomJID :: jid:jid(),
                             Filter :: filter(), HintOffset :: non_neg_integer(),
                             NewOffset :: non_neg_integer(),
                             StartId :: non_neg_integer() | undefined) ->
    StartId :: non_neg_integer() | undefined.
maybe_save_offset_hint(_PoolName, _UserJID, _Filter, _HintOffset, _NewOffset,
                       StartId = undefined) ->
    StartId;
maybe_save_offset_hint(PoolName, RoomJID, Filter, HintOffset, NewOffset, StartId) ->
    case abs(NewOffset - HintOffset) > 50 of
        true ->
            #mam_muc_ca_filter{room_jid = FRoomJID, with_nick = FWithNick} = Filter,
            Row = #{room_jid => FRoomJID, with_nick => FWithNick,
                    offset => NewOffset, id => StartId},
            mongoose_cassandra:cql_write(PoolName, RoomJID, ?MODULE,
                                         insert_offset_hint_query, [Row]);
        false ->
            skip
    end,
    StartId.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
-spec calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset) -> Id
                                                                        when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid:jid(),
      Offset :: non_neg_integer(),
      Filter :: filter(),
      Id :: non_neg_integer() | undefined.
calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    QueryName = {list_message_ids_query, select_filter(Filter)},
    Params = maps:put('[limit]', Offset + 1, eval_filter_params(Filter)),
    {ok, RowsIds} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName, Params),
    case RowsIds of
        [] -> undefined;
        [_ | _] ->
            maps:get(id, lists:last(RowsIds))
    end.

%% @doc Get closest offset -> message id 'hint' for specified offset
prev_offset_query_cql() ->
    "SELECT id, offset FROM mam_muc_message_offset WHERE room_jid = ? and with_nick = ?"
    " and offset <= ? LIMIT 1".

%% @doc Insert offset -> message id 'hint'
insert_offset_hint_query_cql() ->
    "INSERT INTO mam_muc_message_offset(room_jid, with_nick, id, offset) VALUES(?, ?, ?, ?)".

prepare_filter(RoomJID, Borders, Start, End, WithNick, MsgID) ->
    BRoomJID = mod_mam_utils:bare_jid(RoomJID),
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID = maybe_encode_compact_uuid(End, 255),
    %% In Cassandra, a column cannot be restricted by both an equality and an inequality relation.
    %% When MsgID is defined, it is used as both StartID2 and EndID2 to comply with this limitation.
    %% This means that the `ids` filter effectively overrides any "before" or "after" filters.
    {StartID2, EndID2} = case MsgID of
                            undefined ->
                                {apply_start_border(Borders, StartID),
                                 apply_end_border(Borders, EndID)};
                            ID ->
                                {ID, ID}
                        end,
    BWithNick = maybe_nick(WithNick),
    prepare_filter_params(BRoomJID, BWithNick, StartID2, EndID2).

prepare_filter_params(BRoomJID, BWithNick, StartID, EndID) ->
    #mam_muc_ca_filter{
       room_jid  = BRoomJID,
       with_nick = BWithNick,
       start_id  = StartID,
       end_id    = EndID
      }.

eval_filter_params(#mam_muc_ca_filter{
                      room_jid  = BRoomJID,
                      with_nick = BWithNick,
                      start_id  = StartID,
                      end_id    = EndID
                     }) ->
    Optional = maps:filter(fun(_K, V) -> V =/= undefined end,
                           #{start_id => StartID, end_id =>EndID}),
    maps:merge(#{room_jid => BRoomJID, with_nick => BWithNick}, Optional).

select_filter(#mam_muc_ca_filter{
                 start_id = StartID,
                 end_id   = EndID
                }) ->
    select_filter(StartID, EndID).


-spec select_filter(StartID, EndID) ->
                           all  | 'end'  | start  | start_end when
      StartID :: integer()  | undefined,
      EndID :: integer()  | undefined.
select_filter(undefined, undefined) ->
    all;
select_filter(undefined, _) ->
    'end';
select_filter(_, undefined) ->
    start;
select_filter(_, _) ->
    start_end.

prepare_filter_cql(StartID, EndID) ->
    case StartID of
        undefined -> "";
        _ -> " AND id >= :start_id"
    end ++
        case EndID of
            undefined -> "";
            _ -> " AND id <= :end_id"
        end.

filter_to_cql() ->
    [{select_filter(StartID, EndID),
      prepare_filter_cql(StartID, EndID)}
     || StartID <- [undefined, 0],
        EndID <- [undefined, 0]].

-spec calc_offset(PoolName, RoomJID, HostType, Filter, PageSize, TotalCount, RSM) -> Offset
                                                                                     when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid:jid(),
      HostType :: host_type(),
      Filter :: filter(),
      PageSize :: non_neg_integer(),
      TotalCount :: non_neg_integer(),
      RSM :: jlib:rsm_in() | undefined,
      Offset :: non_neg_integer().
%% Requesting the Last Page in a Result Set
calc_offset(_W, _RoomJID, _LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(PoolName, RoomJID, HostType, F, PS, _TC, #rsm_in{direction = before, id = ID})
  when is_integer(ID) ->
    max(0, calc_before(PoolName, RoomJID, HostType, F, ID) - PS);
calc_offset(PoolName, RoomJID, HostType, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
  when is_integer(ID) ->
    calc_index(PoolName, RoomJID, HostType, F, ID);
calc_offset(_W, _RoomJID, _LS, _F, _PS, _TC, _RSM) ->
    0.

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

maybe_nick(undefined) ->
    <<>>;
maybe_nick(WithNick) when is_binary(WithNick) ->
    WithNick.

%%====================================================================
%% Internal SQL part
%%====================================================================

extract_messages_queries() ->
    [{{extract_messages_query, FilterName}, extract_messages_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

extract_messages_r_queries() ->
    [{{extract_messages_r_query, FilterName}, extract_messages_r_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

calc_count_queries() ->
    [{{calc_count_query, FilterName}, calc_count_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

list_message_ids_queries() ->
    [{{list_message_ids_query, FilterName}, list_message_ids_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

extract_messages_cql(Filter) ->
    "SELECT id, nick_name, message FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++
        Filter ++ " ORDER BY id LIMIT ?".

extract_messages_r_cql(Filter) ->
    "SELECT id, nick_name, message FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++
        Filter ++ " ORDER BY id DESC LIMIT ?".

fetch_user_messages_cql() ->
    %% attempt to order results in the next error:
    %%    "ORDER BY with 2ndary indexes is not supported."
    "SELECT id, message FROM mam_muc_message "
    "WHERE from_jid = ?".

calc_count_cql(Filter) ->
    "SELECT COUNT(*) FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++ Filter.

list_message_ids_cql(Filter) ->
    "SELECT id FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++ Filter ++
        " ORDER BY id LIMIT ?".

%% ----------------------------------------------------------------------
%% Optimizations

packet_to_stored_binary(HostType, Packet) ->
    %% Module implementing mam_muc_message behaviour
    Module = db_message_format(HostType),
    mam_message:encode(Module, Packet).

stored_binary_to_packet(HostType, Bin) ->
    %% Module implementing mam_muc_message behaviour
    Module = db_message_format(HostType),
    mam_message:decode(Module, Bin).

%% ----------------------------------------------------------------------
%% Params getters

-spec db_message_format(HostType :: host_type()) -> module().
db_message_format(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, db_message_format).

-spec pool_name(HostType :: host_type()) -> default. %% returns mongoose_wpool:pool_name().
pool_name(_HostType) ->
    default.
