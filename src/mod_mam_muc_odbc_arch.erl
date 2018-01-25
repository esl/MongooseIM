%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing messages from MUC rooms using ODBC.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_odbc_arch).

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
         remove_archive/4,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Called from mod_mam_odbc_async_writer
-export([prepare_message/8, prepare_insert/2]).


%% ----------------------------------------------------------------------
%% Imports

%% UMessID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

%% Other
-import(mod_mam_utils,
        [apply_start_border/2,
         apply_end_border/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_rsm.hrl").


%% ----------------------------------------------------------------------
%% Types

-type filter() :: iolist().
-type escaped_message_id() :: string().
-type escaped_jid() :: binary().
-type unix_timestamp() :: mod_mam:unix_timestamp().
-type packet() :: any().
-type raw_row() :: {binary(), binary(), binary()}.

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> 'ok'.
start(Host, Opts) ->
    case lists:keyfind(hand_made_partitions, 1, Opts) of
        false -> ok;
        _ ->
            ?ERROR_MSG("hand_made_partitions option for mod_mam_muc_odbc_arch "
                       "is no longer supported", []),
            error(hand_made_partitions_not_supported)
    end,

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
    ejabberd_hooks:add(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
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
    ejabberd_hooks:delete(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE,
                          purge_multiple_messages, 50),
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
       "FROM ", "mam_muc_message", " ",
       "WHERE room_id = '", escape_room_id(RoomID), "'"]),
    mongoose_rdbms:result_to_integer(BSize).

-spec archive_message(_Result, jid:server(), MessID :: mod_mam:message_id(),
                      RoomID :: mod_mam:archive_id(), _LocJID :: jid:jid(),
                      _RemJID :: jid:jid(),
                      _SrcJID :: jid:jid(), incoming, Packet :: packet()) -> ok.
archive_message(_Result, Host, MessID, RoomID,
                _LocJID=#jid{},
                _RemJID=#jid{},
                _SrcJID=#jid{lresource=FromNick}, incoming, Packet) ->
    try
        archive_message_unsafe(Host, MessID, RoomID, FromNick, Packet)
    catch _Type:Reason ->
            {error, Reason}
    end.

-spec archive_message_unsafe(jid:server(), mod_mam:message_id(), mod_mam:archive_id(),
                             FromNick :: jid:user(), packet()) -> ok.
archive_message_unsafe(Host, MessID, RoomID, FromNick, Packet) ->
    SRoomID = integer_to_list(RoomID),
    SFromNick = mongoose_rdbms:escape(FromNick),
    Data = packet_to_stored_binary(Host, Packet),
    EscFormat = mongoose_rdbms:escape_format(Host),
    SData = mongoose_rdbms:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    TextBody = mod_mam_utils:packet_to_search_body(mod_mam_muc, Host, Packet),
    STextBody = mongoose_rdbms:escape(TextBody),
    write_message(Host, SMessID, SRoomID, SFromNick, SData, STextBody).


-spec write_message(jid:server(), string(),
                    SRoomId :: string(), SFromNick :: jid:user(), SData :: binary(),
                    STextBody :: binary() | undefined) -> 'ok'.
write_message(Host, SMessID, SRoomID, SFromNick, SData, STextBody) ->
    {updated, 1} =
    mod_mam_utils:success_sql_query(
      Host,
      ["INSERT INTO ", "mam_muc_message", " ",
              "(id, room_id, nick_name, message, search_body) "
       "VALUES ('", SMessID, "', '", SRoomID, "', "
               "'", SFromNick, "', '", SData, "', '", STextBody, "')"]),
    ok.


-spec prepare_message(jid:server(), mod_mam:message_id(), mod_mam:archive_id(),
                      _LocJID :: jid:jid(), _RemJID :: jid:jid(),
                      _SrcJID :: jid:jid(), incoming, packet()) -> list().
prepare_message(Host, MessID, RoomID,
                _LocJID=#jid{},
                _RemJID=#jid{},
                _SrcJID=#jid{lresource=FromNick}, incoming, Packet) ->
    Data = packet_to_stored_binary(Host, Packet),
    TextBody = mod_mam_utils:packet_to_search_body(mod_mam_muc, Host, Packet),
    [MessID, RoomID, FromNick, Data, TextBody].


-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_muc_message,
    Fields = [id, room_id, nick_name, message, search_body],
    Query = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields, Query),
    ok.


lookup_messages({error, _Reason}=Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, Host,
                #{archive_id := UserID, owner_jid := UserJID, rsm := RSM,
                  borders := Borders, start_ts := Start, end_ts := End, now := Now,
                  with_jid := WithJID, search_text := SearchText, page_size := PageSize,
                  limit_passed := LimitPassed, max_result_limit := MaxResultLimit,
                  is_simple := IsSimple}) ->
    try
        lookup_messages(Host,
                        UserID, UserJID, RSM, Borders,
                        Start, End, Now, WithJID,
                        mod_mam_utils:normalize_search_text(SearchText),
                        PageSize, LimitPassed, MaxResultLimit,
                        IsSimple)
    catch _Type:Reason ->
        S = erlang:get_stacktrace(),
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
                      PageSize :: integer(), LimitPassed :: boolean(),
                      MaxResultLimit :: integer(),
                      IsSimple :: boolean()  | opt_count) ->
                             {ok, mod_mam:lookup_result()}
                                 | {error, 'policy-violation'}.
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID, SearchText,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};
lookup_messages(Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};
lookup_messages(Host, RoomID, RoomJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
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
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
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
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
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
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
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
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    TotalCount = calc_count(Host, Filter),
    Offset     = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, after_id(ID, Filter), 0, PageSize, false),
            {ok, {TotalCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;
lookup_messages(Host, RoomID, RoomJID = #jid{},
                RSM = #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID, SearchText,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    TotalCount = calc_count(Host, Filter),
    Offset     = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, before_id(ID, Filter), 0, PageSize, true),
            {ok, {TotalCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;
lookup_messages(Host, RoomID, RoomJID = #jid{},
                RSM, Borders,
                Start, End, _Now, WithJID, SearchText,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, SearchText),
    TotalCount = calc_count(Host, Filter),
    Offset     = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
            {ok, {TotalCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end.


-spec after_id(integer(), [[binary()], ...]) -> [[binary()], ...].
after_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id > '", SID, "'"].


-spec before_id('undefined' | integer(), [[binary()], ...]) -> [[binary()], ...].
before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id < '", SID, "'"].


-spec rows_to_uniform_format([raw_row()], jid:server(), jid:jid()) ->
                                    [mod_mam_muc:row()].
rows_to_uniform_format(MessageRows, Host, RoomJID) ->
    EscFormat = mongoose_rdbms:escape_format(Host),
    DbEngine = mongoose_rdbms:db_engine(Host),
    [row_to_uniform_format(Host, DbEngine, EscFormat, Row, RoomJID) || Row <- MessageRows].


-spec row_to_uniform_format(jid:server(), atom(), atom(),
                            raw_row(), jid:jid()) -> mod_mam_muc:row().
row_to_uniform_format(Host, DbEngine, EscFormat, {BMessID, BNick, SDataRaw}, RoomJID) ->
    MessID = mongoose_rdbms:result_to_integer(BMessID),
    SrcJID = jid:replace_resource(RoomJID, BNick),
    SData = mongoose_rdbms:unescape_odbc_binary(DbEngine, SDataRaw),
    Data = mongoose_rdbms:unescape_binary(EscFormat, SData),
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
      ["DELETE FROM ", "mam_muc_message", " "
       "WHERE room_id = '", escape_room_id(RoomID), "'"]),
    Acc.

-spec purge_single_message(_Result, Host :: jid:server(),
                           MessID :: mod_mam:message_id(),
                           RoomID :: mod_mam:archive_id(),
                           RoomJID :: jid:jid(),
                           Now :: unix_timestamp()) ->
                                  ok  | {error, 'not-allowed'  | 'not-found'}.
purge_single_message(_Result, Host, MessID, RoomID, _RoomJID, _Now) ->
    Result =
        mod_mam_utils:success_sql_query(
          Host,
          ["DELETE FROM mam_muc_message "
           "WHERE room_id = '", escape_room_id(RoomID), "' "
           "AND id = '", escape_message_id(MessID), "'"]),
    case Result of
        {updated, 0} -> {error, 'not-found'};
        {updated, 1} -> ok
    end.


-spec purge_multiple_messages(_Result, Host :: jid:server(),
                              RoomID :: mod_mam:archive_id(),
                              RoomJID :: jid:jid(),
                              Borders :: mod_mam:borders()  | undefined,
                              Start :: unix_timestamp()  | undefined,
                              End :: unix_timestamp()  | undefined,
                              Now :: unix_timestamp(),
                              WithJID :: jid:jid()  | undefined) ->
                                     ok  | {error, 'not-allowed'}.
purge_multiple_messages(_Result, Host, RoomID, _RoomJID, Borders,
                        Start, End, _Now, WithJID) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID, undefined),
    {updated, _} =
        mod_mam_utils:success_sql_query(
          Host,
          ["DELETE FROM mam_muc_message ", Filter]),
    ok.


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

%% @doc Zero-based index of the row with UMessID in the result test.
%% If the element does not exists, the MessID of the next element will
%% be returned instead.
%% "SELECT COUNT(*) as "index" FROM mam_muc_message WHERE id <= '",  UMessID
-spec calc_index(Host :: jid:server(),
                 Filter :: iodata(), SUMessID :: escaped_message_id()) -> non_neg_integer().
calc_index(Host, Filter, SUMessID) ->
    {selected, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", "mam_muc_message", " ",
       Filter, " AND id <= '", SUMessID, "'"]),
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
       Filter, " AND id < '", SUMessID, "'"]),
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
                     WithJID :: jid:jid() | undefined, SearchText :: string() | undefined) -> filter().
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
   ["WHERE room_id='", escape_room_id(RoomID), "'",
     case StartID of
         undefined -> "";
         _         -> [" AND id >= ", integer_to_list(StartID)]
     end,
     case EndID of
         undefined -> "";
         _         -> [" AND id <= ", integer_to_list(EndID)]
     end,
     case SWithNick of
         undefined -> "";
         _         -> [" AND nick_name = '", SWithNick, "'"]
     end,
     case SearchText of
         undefined -> "";
         _         -> [" AND search_body like '%", SearchText, "%'"]
     end
    ].


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


-spec escape_message_id(mod_mam:message_id()) -> string().
escape_message_id(MessID) when is_integer(MessID) ->
    integer_to_list(MessID).


-spec escape_room_id(mod_mam:archive_id()) -> string().
escape_room_id(RoomID) when is_integer(RoomID) ->
    integer_to_list(RoomID).


-spec maybe_jid_to_escaped_resource('undefined' | jid:jid())
                                   -> 'undefined' | binary() | string().
maybe_jid_to_escaped_resource(undefined) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = <<>>}) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = WithLResource}) ->
    mongoose_rdbms:escape(WithLResource).


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

