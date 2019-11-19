%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing messages from MUC rooms using RDBMS.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_rdbms_arch).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).

-callback encode(term()) -> binary().
-callback decode(binary()) -> term().

-export([archive_size/4,
         archive_message/9,
         lookup_messages/3,
         remove_archive/4]).

%% Called from mod_mam_rdbms_async_writer
-export([prepare_message/6, prepare_insert/2]).

%gdpr
-export([get_mam_muc_gdpr_data/2]).

%% ----------------------------------------------------------------------
%% Imports

%% UMessID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

%% Other
-import(mod_mam_utils,
        [apply_start_border/2,
         apply_end_border/2]).

-import(mongoose_rdbms,
        [escape_integer/1,
         use_escaped_string/1,
         use_escaped_integer/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_rsm.hrl").


%% ----------------------------------------------------------------------
%% Types

-type filter() :: iolist().
-type escaped_message_id() :: mongoose_rdbms:escaped_integer().
-type escaped_room_id() :: mongoose_rdbms:escaped_integer().
-type escaped_jid() :: mongoose_rdbms:escaped_string().
-type unix_timestamp() :: mod_mam:unix_timestamp().
-type packet() :: any().
-type raw_row() :: {binary(), binary(), binary()}.

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> 'ok'.
start(Host, Opts) ->
    prepare_insert(insert_mam_muc_message, 1),

    start_muc(Host, Opts).

-spec stop(jid:server()) -> 'ok'.
stop(Host) ->
    stop_muc(Host).

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

-spec start_muc(jid:server(), _) -> 'ok'.
start_muc(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(get_mam_muc_gdpr_data, Host, ?MODULE, get_mam_muc_gdpr_data, 50),
    ok.


-spec stop_muc(jid:server()) -> 'ok'.
stop_muc(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(get_mam_muc_gdpr_data, Host, ?MODULE, get_mam_muc_gdpr_data, 50),
    ok.


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec archive_size(integer(), jid:server(), integer(), jid:jid())
                  -> integer().
archive_size(Size, Host, RoomID, _RoomJID) when is_integer(Size) ->
    {selected, [{BSize}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM mam_muc_message ",
       "WHERE room_id = ", use_escaped_integer(escape_room_id(RoomID))]),
    mongoose_rdbms:result_to_integer(BSize).

-spec archive_message(_Result, jid:server(), MessID :: mod_mam:message_id(),
                      RoomID :: mod_mam:archive_id(), _LocJID :: jid:jid(),
                      SenderJID :: jid:jid(),
                      UserRoomJID :: jid:jid(), incoming, Packet :: packet()) -> ok.
archive_message(_Result, Host, MessID, RoomID, _LocJID = #jid{},
                SenderJID = #jid{}, UserRoomJID = #jid{}, incoming, Packet) ->
    try
        Row = prepare_message(Host, MessID, RoomID, SenderJID, UserRoomJID, Packet),
        {updated, 1} = mod_mam_utils:success_sql_execute(Host, insert_mam_muc_message, Row),
        ok
    catch _Type:Reason:StackTrace ->
            ?ERROR_MSG("event=archive_message_failed mess_id=~p room_id=~p "
                       "from_nick=~p reason='~p' stacktrace=~p",
                       [MessID, RoomID, UserRoomJID#jid.lresource, Reason, StackTrace]),
            {error, Reason}
    end.

-spec prepare_message(Host :: jid:server(), MessID :: mod_mam:message_id(),
                      RoomID :: mod_mam:archive_id(), SenderJID :: jid:jid(),
                      UserRoomJID :: jid:jid(), Packet :: packet()) -> [binary() | integer()].
prepare_message(Host, MessID, RoomID, SenderJID, #jid{ lresource = FromNick }, Packet) ->
    BareSenderJID = jid:to_bare(SenderJID),
    Data = packet_to_stored_binary(Host, Packet),
    TextBody = mod_mam_utils:packet_to_search_body(mod_mam_muc, Host, Packet),
    SenderID = mod_mam:archive_id_int(Host, BareSenderJID),
    [MessID, RoomID, SenderID, FromNick, Data, TextBody].

-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_muc_message,
    Fields = [id, room_id, sender_id, nick_name, message, search_body],
    Query = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields, Query),
    ok.


lookup_messages({error, _Reason}=Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, Host,
                #{archive_id := UserID, owner_jid := UserJID, rsm := RSM,
                  borders := Borders, start_ts := Start, end_ts := End, now := Now,
                  with_jid := WithJID, search_text := SearchText, page_size := PageSize,
                  is_simple := IsSimple}) ->
    try
        lookup_messages(Host,
                        UserID, UserJID, RSM, Borders,
                        Start, End, Now, WithJID,
                        mod_mam_utils:normalize_search_text(SearchText),
                        PageSize, IsSimple)
    catch _Type:Reason:S ->
        {error, {Reason, {stacktrace, S}}}
    end.

-spec lookup_messages(Host :: jid:server(),
                      ArchiveID :: mod_mam:archive_id(),
                      ArchiveJID :: jid:jid(),
                      RSM :: jlib:rsm_in()  | undefined,
                      Borders :: mod_mam:borders()  | undefined,
                      Start :: mod_mam:unix_timestamp()  | undefined,
                      End :: mod_mam:unix_timestamp()  | undefined,
                      Now :: mod_mam:unix_timestamp(),
                      WithJID :: jid:jid()  | undefined,
                      SearchText :: binary() | undefined,
                      PageSize :: integer(),
                      IsSimple :: boolean()  | opt_count) ->
                             {ok, mod_mam:lookup_result()}.
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID, SearchText,
                PageSize, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};
lookup_messages(Host, RoomID, RoomJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, Filter, 0, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};
%% Cannot be optimized:
%% - #rsm_in{direction = aft, id = ID}
%% - #rsm_in{direction = before, id = ID}
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = before, id = undefined}, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, opt_count) ->
    %% Last page
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, Filter, 0, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Host, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, opt_count) ->
    %% By offset
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;
lookup_messages(Host, RoomID, RoomJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, opt_count) ->
    %% First page
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, Filter, 0, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;
lookup_messages(Host, RoomID, RoomJID = #jid{},
                RSM = #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, _) when ID =/= undefined ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    TotalCount = calc_count(Host, Filter),
    Offset = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Host, from_id(ID, Filter), 0, PageSize + 1, false),
    Result = {TotalCount, Offset, rows_to_uniform_format(MessageRows, Host, RoomJID)},
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result);
lookup_messages(Host, RoomID, RoomJID = #jid{},
                RSM = #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID, SearchText,
                PageSize, _) when ID =/= undefined ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    TotalCount = calc_count(Host, Filter),
    Offset = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Host, to_id(ID, Filter), 0, PageSize + 1, true),
    Result = {TotalCount, Offset, rows_to_uniform_format(MessageRows, Host, RoomJID)},
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result);
lookup_messages(Host, RoomID, RoomJID = #jid{},
                RSM, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, _) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    TotalCount = calc_count(Host, Filter),
    Offset = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
    {ok, {TotalCount, Offset,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}}.

-spec get_mam_muc_gdpr_data(ejabberd_gen_mam_archive:mam_muc_gdpr_data(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(Acc, #jid{ user = User, server = Host }) ->
    case mod_mam:archive_id(Host, User) of
        undefined -> Acc;
        ArchiveID ->
            {selected, Rows} = extract_gdpr_messages(Host, ArchiveID),
            [{BMessID, gdpr_decode_packet(Host, SDataRaw)} || {BMessID,  SDataRaw} <- Rows] ++ Acc
    end.

-spec after_id(ID :: escaped_message_id(), Filter :: filter()) -> filter().
after_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id > ", use_escaped_integer(SID)].

-spec before_id(ID :: escaped_message_id() | undefined,
               Filter :: filter()) -> filter().
before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id < ", use_escaped_integer(SID)].

-spec from_id(ID :: escaped_message_id(), Filter :: filter()) -> filter().
from_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id >= ", use_escaped_integer(SID)].

-spec to_id(ID :: escaped_message_id(), Filter :: filter()) -> filter().
to_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id <= ", use_escaped_integer(SID)].


-spec rows_to_uniform_format([raw_row()], jid:server(), jid:jid()) ->
                                    [mod_mam_muc:row()].
rows_to_uniform_format(MessageRows, Host, RoomJID) ->
    [do_row_to_uniform_format(Host, Row, RoomJID) || Row <- MessageRows].


-spec do_row_to_uniform_format(jid:server(), raw_row(), jid:jid()) ->
                                   mod_mam_muc:row().
do_row_to_uniform_format(Host, {BMessID, BNick, SDataRaw}, RoomJID) ->
    MessID = mongoose_rdbms:result_to_integer(BMessID),
    SrcJID = jid:replace_resource(RoomJID, BNick),
    Data = mongoose_rdbms:unescape_binary(Host, SDataRaw),
    Packet = stored_binary_to_packet(Host, Data),
    {MessID, SrcJID, Packet}.


-spec row_to_message_id({binary(), _, _}) -> integer().
row_to_message_id({BMessID, _, _}) ->
    mongoose_rdbms:result_to_integer(BMessID).


-spec remove_archive(map(), jid:server(), mod_mam:archive_id(), jid:jid()) -> map().
remove_archive(Acc, Host, RoomID, _RoomJID) ->
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM mam_muc_message "
       "WHERE room_id = ", use_escaped_integer(escape_room_id(RoomID))]),
    Acc.

%% @doc Columns are `["id", "nick_name", "message"]'.
-spec extract_messages(Host :: jid:server(),
                       Filter :: filter(), IOffset :: non_neg_integer(), IMax :: pos_integer(),
                       ReverseLimit :: boolean()) -> [raw_row()].
extract_messages(_Host, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Host, Filter, IOffset, IMax, false) ->
    {selected, MessageRows} =
        do_extract_messages(Host, Filter, IOffset, IMax, " ORDER BY id "),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    MessageRows;
extract_messages(Host, Filter, IOffset, IMax, true) ->
    {selected, MessageRows} =
        do_extract_messages(Host, Filter, IOffset, IMax, " ORDER BY id DESC "),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    lists:reverse(MessageRows).

do_extract_messages(Host, Filter, 0, IMax, Order) ->
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits(IMax),
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT ", LimitMSSQL, " id, nick_name, message "
       "FROM mam_muc_message ",
       Filter,
       Order,
       " ", LimitSQL]);
do_extract_messages(Host, Filter, IOffset, IMax, Order) ->
    {LimitSQL, _LimitMSSQL} = rdbms_queries:get_db_specific_limits(IMax),
    Offset = rdbms_queries:get_db_specific_offset(IOffset, IMax),
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT id, nick_name, message "
       "FROM mam_muc_message ",
       Filter, Order, LimitSQL, Offset]).

extract_gdpr_messages(Host, ArchiveID) ->
    Filter = ["WHERE sender_id = ", use_escaped_integer(escape_integer(ArchiveID))],
    mod_mam_utils:success_sql_query(
        Host,
        ["SELECT id, message "
         "FROM mam_muc_message ",
         Filter, " ORDER BY id"]).

%% @doc Zero-based index of the row with UMessID in the result test.
%% If the element does not exists, the MessID of the next element will
%% be returned instead.
%%
%% ```
%% "SELECT COUNT(*) as "index" FROM mam_muc_message WHERE id <= '",  UMessID
%% '''
-spec calc_index(Host :: jid:server(),
                 Filter :: iodata(), SUMessID :: escaped_message_id()) -> non_neg_integer().
calc_index(Host, Filter, SUMessID) ->
    {selected, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM mam_muc_message ",
       Filter, " AND id <= ", use_escaped_integer(SUMessID)]),
    mongoose_rdbms:result_to_integer(BIndex).


%% @doc Count of elements in RSet before the passed element.
%% The element with the passed UMessID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE id < '",  UMessID
-spec calc_before(Host :: jid:server(),
                  Filter :: iodata(), SUMessID :: escaped_message_id()) -> non_neg_integer().
calc_before(Host, Filter, SUMessID) ->
    {selected, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM mam_muc_message ",
       Filter, " AND id < ", use_escaped_integer(SUMessID)]),
    mongoose_rdbms:result_to_integer(BIndex).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE "
-spec calc_count(Host :: jid:server(),
                 Filter :: filter()) -> non_neg_integer().
calc_count(Host, Filter) ->
    {selected, [{BCount}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) ",
       "FROM mam_muc_message ", Filter]),
    mongoose_rdbms:result_to_integer(BCount).


%% @doc prepare_filter/5
-spec prepare_filter(RoomID :: mod_mam:archive_id(), Borders :: mod_mam:borders() | undefined,
                     Start :: unix_timestamp() | undefined, End :: unix_timestamp() | undefined,
                     WithJID :: jid:jid() | undefined, SearchText :: binary() | undefined) -> filter().
prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText) ->
    SWithNick = maybe_jid_to_escaped_resource(WithJID),
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    make_filter(RoomID, StartID2, EndID2, SWithNick, SearchText).


-spec make_filter(RoomID  :: non_neg_integer(),
                  StartID :: mod_mam:message_id() | undefined,
                  EndID :: mod_mam:message_id() | undefined,
                  SWithNick :: escaped_jid() | undefined,
                  SearchText :: binary() | undefined) -> filter().
make_filter(RoomID, StartID, EndID, SWithNick, SearchText) ->
   ["WHERE room_id=", use_escaped_integer(escape_room_id(RoomID)),
     case StartID of
         undefined -> "";
         _         -> [" AND id >= ", use_escaped_integer(escape_integer(StartID))]
     end,
     case EndID of
         undefined -> "";
         _         -> [" AND id <= ", use_escaped_integer(escape_integer(EndID))]
     end,
     case SWithNick of
         undefined -> "";
         _         -> [" AND nick_name = ", use_escaped_string(SWithNick)]
     end,
     case SearchText of
         undefined -> "";
         _         -> prepare_search_filters(SearchText)
     end
    ].

%% Constructs a separate LIKE filter for each word.
%% SearchText example is "word1%word2%word3".
prepare_search_filters(SearchText) ->
    Words = binary:split(SearchText, <<"%">>, [global]),
    [prepare_search_filter(Word) || Word <- Words].

-spec prepare_search_filter(binary()) -> filter().
prepare_search_filter(Word) ->
    [" AND search_body like ",
     %% Search for "%Word%"
     mongoose_rdbms:use_escaped_like(mongoose_rdbms:escape_like(Word))].

%% @doc #rsm_in{
%%    max = non_neg_integer() | undefined,
%%    direction = before | aft | undefined,
%%    id = binary() | undefined,
%%    index = non_neg_integer() | undefined}
-spec calc_offset(Host :: jid:server(),
                  Filter :: filter(), PageSize :: non_neg_integer(),
                  TotalCount :: non_neg_integer(), RSM :: jlib:rsm_in() | undefined)
                 -> non_neg_integer().
calc_offset(_LS, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
  when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Host, F, PS, _TC, #rsm_in{direction = before, id = MessID})
  when is_integer(MessID) ->
    SMessID = escape_message_id(MessID),
    max(0, calc_before(Host, F, SMessID) - PS);
calc_offset(Host, F, _PS, _TC, #rsm_in{direction = aft, id = MessID})
  when is_integer(MessID) ->
    SMessID = escape_message_id(MessID),
    calc_index(Host, F, SMessID);
calc_offset(_LS, _F, _PS, _TC, _RSM) ->
    0.


-spec escape_message_id(mod_mam:message_id()) -> escaped_message_id().
escape_message_id(MessID) when is_integer(MessID) ->
    escape_integer(MessID).


-spec escape_room_id(mod_mam:archive_id()) -> escaped_room_id().
escape_room_id(RoomID) when is_integer(RoomID) ->
    escape_integer(RoomID).


-spec maybe_jid_to_escaped_resource('undefined' | jid:jid())
                                   -> 'undefined' | mongoose_rdbms:escaped_string().
maybe_jid_to_escaped_resource(undefined) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = <<>>}) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = WithLResource}) ->
    mongoose_rdbms:escape_string(WithLResource).


-spec maybe_encode_compact_uuid('undefined' | integer(), 0 | 255)
                               -> 'undefined' | integer().
maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).


%% ----------------------------------------------------------------------
%% Optimizations

packet_to_stored_binary(Host, Packet) ->
    Module = db_message_codec(Host),
    mam_message:encode(Module, Packet).

stored_binary_to_packet(Host, Bin) ->
    Module = db_message_codec(Host),
    mam_message:decode(Module, Bin).

-spec db_message_codec(Host :: jid:server()) -> module().
db_message_codec(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, db_message_format, mam_message_compressed_eterm).


gdpr_decode_packet(Host, SDataRaw) ->
    Codec = mod_mam_meta:get_mam_module_opt(Host, ?MODULE, db_message_format,
                                            mam_message_compressed_eterm),
    Data = mongoose_rdbms:unescape_binary(Host, SDataRaw),
    Message = mam_message:decode(Codec, Data),
    exml:to_binary(Message).
