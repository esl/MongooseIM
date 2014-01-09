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
-export([archive_size/4,
         archive_message/9,
         lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Called from mod_mam_odbc_async_writer
-export([prepare_message/8,
         archive_messages/2,
         archive_messages/3]).


%% ----------------------------------------------------------------------
%% Imports

%% UMessID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

%% Other
-import(mod_mam_utils,
        [apply_start_border/2,
         apply_end_border/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").


%% ----------------------------------------------------------------------
%% Types

-type filter() :: iolist().
-type room_id() :: non_neg_integer().
-type message_id() :: non_neg_integer().
-type escaped_message_id() :: binary().
-type escaped_jid() :: binary().
-type server_host() :: binary().
-type unix_timestamp() :: non_neg_integer().



-ifdef(HAND_MADE_PARTITIONS).
select_table(N) ->
    io_lib:format("mam_muc_message_~2..0B", [N rem partition_count()]).

partition_count() ->
    16.
-else.
select_table(_) ->
    "mam_muc_message".
-endif.


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    start_muc(Host, Opts).

stop(Host) ->
    stop_muc(Host).


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

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
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

archive_size(Size, Host, RoomID, _RoomJID) when is_integer(Size) ->
    {selected, _ColumnNames, [{BSize}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", select_table(RoomID), " ",
       "WHERE room_id = '", escape_room_id(RoomID), "'"]),
    list_to_integer(binary_to_list(BSize)).

archive_message(_Result, Host, MessID, RoomID,
                _LocJID=#jid{},
                _RemJID=#jid{},
                _SrcJID=#jid{lresource=FromNick}, incoming, Packet) ->
    archive_message_1(Host, MessID, RoomID, FromNick, Packet).

archive_message_1(Host, MessID, RoomID, FromNick, Packet) ->
    SRoomID = integer_to_list(RoomID),
    SFromNick = ejabberd_odbc:escape(FromNick),
    Data = term_to_binary(Packet, [compressed]),
    EscFormat = ejabberd_odbc:escape_format(Host),
    SData = ejabberd_odbc:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    write_message(Host, SMessID, RoomID, SRoomID, SFromNick, SData).

write_message(Host, SMessID, RoomID, SRoomID, SFromNick, SData) ->
    {updated, 1} =
    mod_mam_utils:success_sql_query(
      Host,
      ["INSERT INTO ", select_table(RoomID), " ",
              "(id, room_id, nick_name, message) "
       "VALUES ('", SMessID, "', '", SRoomID, "', "
               "'", SFromNick, "', '", SData, "')"]),
    ok.

prepare_message(Host, MessID, RoomID,
                _LocJID=#jid{luser=_RoomName},
                _RemJID=#jid{},
                _SrcJID=#jid{lresource=FromNick}, incoming, Packet) ->
    prepare_message_1(Host, MessID, RoomID, FromNick, Packet).

prepare_message_1(Host, MessID, RoomID, FromNick, Packet) ->
    SRoomID = integer_to_list(RoomID),
    SFromNick = ejabberd_odbc:escape(FromNick),
    Data = term_to_binary(Packet, [compressed]),
    EscFormat = ejabberd_odbc:escape_format(Host),
    SData = ejabberd_odbc:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    [SMessID, SRoomID, SFromNick, SData].

archive_messages(LServer, Acc) ->
    mod_mam_utils:success_sql_query(
      LServer,
      ["INSERT INTO mam_muc_message(id, room_id, nick_name, message) "
       "VALUES ", tuples(Acc)]).

archive_messages(LServer, Acc, N) ->
    mod_mam_utils:success_sql_query(
      LServer,
      ["INSERT INTO ", select_table(N), " ",
           "(id, room_id, nick_name, message) "
       "VALUES ", tuples(Acc)]).

-spec lookup_messages(Result, Host,
                      RoomID, RoomJID, RSM, Borders,
                      Start, End, Now, WithJID,
                      PageSize, LimitPassed, MaxResultLimit,
                      IsSimple) -> Result when
    Host    :: server_host(),
    RoomJID :: #jid{},
    RoomID  :: room_id(),
    RSM     :: #rsm_in{} | undefined,
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    PageSize :: non_neg_integer(),
    WithJID :: #jid{} | undefined,
    LimitPassed :: boolean(),
    MaxResultLimit :: non_neg_integer(),
    IsSimple :: boolean() | opt_count,
    Result :: {ok, {TotalCount, Offset, MessageRows}} | {error, 'policy-violation'},
    TotalCount :: non_neg_integer(),
    Offset  :: non_neg_integer(),
    MessageRows :: list(tuple()).

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, RoomID, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, RoomID, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, RoomID, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, RoomID, Filter, 0, PageSize, false),
    {ok, {undefined, undefined,
          rows_to_uniform_format(MessageRows, Host, RoomJID)}};


%% Cannot be optimized:
%% - #rsm_in{direction = aft, id = ID} 
%% - #rsm_in{direction = before, id = ID} 

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = before, id = undefined}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% Last page
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, RoomID, Filter, 0, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0, 
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Host, RoomID, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% By offset
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, RoomID, Filter, Offset, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset, 
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, RoomID, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% First page
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, RoomID, Filter, 0, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, RoomID, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;


lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                RSM = #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    TotalCount = calc_count(Host, RoomID, Filter),
    Offset     = calc_offset(Host, RoomID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, RoomID, after_id(ID, Filter), 0, PageSize, false),
            {ok, {TotalCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                RSM = #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    TotalCount = calc_count(Host, RoomID, Filter),
    Offset     = calc_offset(Host, RoomID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, RoomID, before_id(ID, Filter), 0, PageSize, true),
            {ok, {TotalCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end;

lookup_messages(_Result, Host, RoomID, RoomJID = #jid{},
                RSM, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    TotalCount = calc_count(Host, RoomID, Filter),
    Offset     = calc_offset(Host, RoomID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, RoomID, Filter, Offset, PageSize, false),
            {ok, {TotalCount, Offset,
                  rows_to_uniform_format(MessageRows, Host, RoomJID)}}
    end.

after_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id > '", SID, "'"].

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id < '", SID, "'"].

rows_to_uniform_format(MessageRows, Host, RoomJID) ->
    [row_to_uniform_format(Row, Host, RoomJID) || Row <- MessageRows].

row_to_uniform_format({BMessID,BNick,SData}, Host, RoomJID) ->
    MessID = list_to_integer(binary_to_list(BMessID)),
    SrcJID = jlib:jid_replace_resource(RoomJID, BNick),
    EscFormat = ejabberd_odbc:escape_format(Host),
    Data = ejabberd_odbc:unescape_binary(EscFormat, SData),
    Packet = binary_to_term(Data),
    {MessID, SrcJID, Packet}.

row_to_message_id({BMessID,_,_}) ->
    list_to_integer(binary_to_list(BMessID)).

remove_archive(Host, RoomID, _RoomJID) ->
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(RoomID), " "
       "WHERE room_id = '", escape_room_id(RoomID), "'"]),
    ok.

-spec purge_single_message(_Result, Host, MessID, RoomID, RoomJID, Now) ->
    ok | {error, 'not-allowed' | 'not-found'} when
    Host    :: server_host(),
    MessID  :: message_id(),
    RoomID  :: room_id(),
    RoomJID :: #jid{},
    Now     :: unix_timestamp().
purge_single_message(_Result, Host, MessID, RoomID, _RoomJID, _Now) ->
    Result =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(RoomID), " "
       "WHERE room_id = '", escape_room_id(RoomID), "' "
       "AND id = '", escape_message_id(MessID), "'"]),
    case Result of
        {updated, 0} -> {error, 'not-found'};
        {updated, 1} -> ok
    end.

-spec purge_multiple_messages(_Result, Host,
                              RoomID, RoomJID, Borders,
                              Start, End, Now, WithJID) ->
    ok | {error, 'not-allowed'} when
    Host    :: server_host(),
    RoomID  :: room_id(),
    RoomJID :: #jid{},
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: #jid{} | undefined.
purge_multiple_messages(_Result, Host, RoomID, _RoomJID, Borders,
                        Start, End, _Now, WithJID) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(RoomID), " ", Filter]),
    ok.

%% Columns are `["id","nick_name","message"]'.
-spec extract_messages(Host, RoomID, Filter, IOffset, IMax, ReverseLimit) ->
    [Record] when
    Host    :: server_host(),
    RoomID  :: room_id(),
    Filter  :: filter(),
    IOffset :: non_neg_integer(),
    IMax    :: pos_integer(),
    ReverseLimit :: boolean(),
    Record  :: tuple().
extract_messages(_Host, _RoomID, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Host, RoomID, Filter, IOffset, IMax, false) ->
    {selected, _ColumnNames, MessageRows} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT id, nick_name, message "
       "FROM ", select_table(RoomID), " ",
        Filter,
       " ORDER BY id"
       " LIMIT ", integer_to_list(IMax),
         case IOffset of
             0 -> "";
             _ -> [" OFFSET ", integer_to_list(IOffset)]
         end]),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    MessageRows;
extract_messages(Host, RoomID, Filter, IOffset, IMax, true) ->
    {selected, _ColumnNames, MessageRows} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT id, nick_name, message "
       "FROM ", select_table(RoomID), " ",
        Filter,
       " ORDER BY id DESC"
       " LIMIT ", integer_to_list(IMax),
         case IOffset of
             0 -> "";
             _ -> [" OFFSET ", integer_to_list(IOffset)]
         end]),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    lists:reverse(MessageRows).


%% Zero-based index of the row with UMessID in the result test.
%% If the element does not exists, the MessID of the next element will
%% be returned instead.
%% "SELECT COUNT(*) as "index" FROM mam_muc_message WHERE id <= '",  UMessID
-spec calc_index(Host, RoomID, Filter, SUMessID) -> Count
    when
    Host     :: server_host(),
    RoomID   :: room_id(),
    Filter   :: filter(),
    SUMessID :: escaped_message_id(),
    Count    :: non_neg_integer().
calc_index(Host, RoomID, Filter, SUMessID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", select_table(RoomID), " ",
       Filter, " AND id <= '", SUMessID, "'"]),
    list_to_integer(binary_to_list(BIndex)).

%% @doc Count of elements in RSet before the passed element.
%% The element with the passed UMessID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE id < '",  UMessID
-spec calc_before(Host, RoomID, Filter, SUMessID) -> Count
    when
    Host     :: server_host(),
    RoomID   :: room_id(),
    Filter   :: filter(),
    SUMessID :: escaped_message_id(),
    Count    :: non_neg_integer().
calc_before(Host, RoomID, Filter, SUMessID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", select_table(RoomID), " ",
       Filter, " AND id < '", SUMessID, "'"]),
    list_to_integer(binary_to_list(BIndex)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE "
-spec calc_count(Host, RoomID, Filter) -> Count
    when
    Host     :: server_host(),
    RoomID   :: room_id(),
    Filter   :: filter(),
    Count    :: non_neg_integer().
calc_count(Host, RoomID, Filter) ->
    {selected, _ColumnNames, [{BCount}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) ",
       "FROM ", select_table(RoomID), " ", Filter]),
    list_to_integer(binary_to_list(BCount)).


%% prepare_filter/5
-spec prepare_filter(RoomID, Borders, Start, End, WithJID) -> filter()
    when
    RoomID  :: room_id(),
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    WithJID :: #jid{} | undefined.
prepare_filter(RoomID, Borders, Start, End, WithJID) ->
    SWithNick = maybe_jid_to_escaped_resource(WithJID),
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filter_1(RoomID, StartID2, EndID2, SWithNick).

-spec prepare_filter_1(RoomID, StartID, EndID, SWithNick) -> filter()
    when
    RoomID  :: non_neg_integer(),
    StartID :: message_id() | undefined,
    EndID   :: message_id() | undefined,
    SWithNick :: escaped_jid() | undefined.
prepare_filter_1(RoomID, StartID, EndID, SWithNick) ->
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
     end].



%% #rsm_in{
%%    max = non_neg_integer() | undefined,
%%    direction = before | aft | undefined,
%%    id = binary() | undefined,
%%    index = non_neg_integer() | undefined}
-spec calc_offset(Host, RoomID, Filter, PageSize, TotalCount, RSM) -> Offset
    when
    Host     :: server_host(),
    RoomID   :: room_id(),
    Filter   :: filter(),
    PageSize :: non_neg_integer(),
    TotalCount :: non_neg_integer(),
    RSM      :: #rsm_in{} | undefined,
    Offset   :: non_neg_integer().
calc_offset(_LS, _RoomID, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
    when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_LS, _RoomID, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Host, RoomID, F, PS, _TC, #rsm_in{direction = before, id = MessID})
    when is_integer(MessID) ->
    SMessID = escape_message_id(MessID),
    max(0, calc_before(Host, RoomID, F, SMessID) - PS);
calc_offset(Host, RoomID, F, _PS, _TC, #rsm_in{direction = aft, id = MessID})
    when is_integer(MessID) ->
    SMessID = escape_message_id(MessID),
    calc_index(Host, RoomID, F, SMessID);
calc_offset(_LS, _RoomID, _F, _PS, _TC, _RSM) ->
    0.

escape_message_id(MessID) when is_integer(MessID) ->
    integer_to_list(MessID).

escape_room_id(RoomID) when is_integer(RoomID) ->
    integer_to_list(RoomID).

maybe_jid_to_escaped_resource(undefined) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = <<>>}) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = WithLResource}) ->
    ejabberd_odbc:escape(WithLResource).

join([H|T]) ->
    [H, [", " ++ X || X <- T]].

tuples(Rows) ->
    join([tuple(Row) || Row <- Rows]).

tuple([H|T]) ->
    ["('", H, "'", [[", '", X, "'"] || X <- T], ")"].

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).
