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
-type escaped_message_id() :: string().
-type escaped_jid() :: binary().
-type unix_timestamp() :: mod_mam:unix_timestamp().
-type packet() :: any().
-type raw_row() :: {binary(), binary(), binary()}.

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(ejabberd:server(),_) -> 'ok'.
start(Host, Opts) ->
    compile_params_module(Opts),
    start_muc(Host, Opts).


-spec stop(ejabberd:server()) -> 'ok'.
stop(Host) ->
    stop_muc(Host).


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

-spec start_muc(ejabberd:server(),_) -> 'ok'.
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


-spec stop_muc(ejabberd:server()) -> 'ok'.
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

-spec archive_size(integer(), ejabberd:server(), integer(), ejabberd:jid())
            -> integer().
archive_size(Size, Host, RoomID, _RoomJID) when is_integer(Size) ->
    {selected, _ColumnNames, [{BSize}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", select_table(RoomID), " ",
       "WHERE room_id = '", escape_room_id(RoomID), "'"]),
    ejabberd_odbc:result_to_integer(BSize).

-spec archive_message(_Result, ejabberd:server(), MessID :: mod_mam:message_id(),
        RoomID :: mod_mam:archive_id(), _LocJID :: ejabberd:jid(), _RemJID :: ejabberd:jid(),
        _SrcJID :: ejabberd:jid(), incoming, Packet :: packet()) -> ok.
archive_message(Result, Host, MessID, UserID,
                     LocJID, RemJID, SrcJID, Dir, Packet) ->
    try
        archive_message_2(Result, Host, MessID, UserID,
                          LocJID, RemJID, SrcJID, Dir, Packet)
    catch _Type:Reason ->
        {error, Reason}
    end.

archive_message_2(_Result, Host, MessID, RoomID,
                  _LocJID=#jid{},
                  _RemJID=#jid{},
                  _SrcJID=#jid{lresource=FromNick}, incoming, Packet) ->
    archive_message_1(Host, MessID, RoomID, FromNick, Packet).


-spec archive_message_1(ejabberd:server(), mod_mam:message_id(), mod_mam:archive_id(),
        FromNick :: ejabberd:user(), packet()) -> ok.
archive_message_1(Host, MessID, RoomID, FromNick, Packet) ->
    SRoomID = integer_to_list(RoomID),
    SFromNick = ejabberd_odbc:escape(FromNick),
    Data = packet_to_stored_binary(Packet),
    EscFormat = ejabberd_odbc:escape_format(Host),
    SData = ejabberd_odbc:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    write_message(Host, SMessID, RoomID, SRoomID, SFromNick, SData).


-spec write_message(ejabberd:server(), string(), RoomId :: mod_mam:archive_id(),
        SRoomId :: string(), SFromNick :: ejabberd:user(), SData :: binary()) -> 'ok'.
write_message(Host, SMessID, RoomID, SRoomID, SFromNick, SData) ->
    {updated, 1} =
    mod_mam_utils:success_sql_query(
      Host,
      ["INSERT INTO ", select_table(RoomID), " ",
              "(id, room_id, nick_name, message) "
       "VALUES ('", SMessID, "', '", SRoomID, "', "
               "'", SFromNick, "', '", SData, "')"]),
    ok.


-spec prepare_message(ejabberd:server(), mod_mam:message_id(), mod_mam:archive_id(),
        _LocJID :: ejabberd:jid(), _RemJID :: ejabberd:jid(),
        _SrcJID :: ejabberd:jid(), incoming, packet()) -> list().
prepare_message(Host, MessID, RoomID,
                _LocJID=#jid{luser=_RoomName},
                _RemJID=#jid{},
                _SrcJID=#jid{lresource=FromNick}, incoming, Packet) ->
    prepare_message_1(Host, MessID, RoomID, FromNick, Packet).


prepare_message_1(Host, MessID, RoomID, FromNick, Packet) ->
    SRoomID = integer_to_list(RoomID),
    SFromNick = ejabberd_odbc:escape(FromNick),
    Data = packet_to_stored_binary(Packet),
    EscFormat = ejabberd_odbc:escape_format(Host),
    SData = ejabberd_odbc:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    [SMessID, SRoomID, SFromNick, SData].


-spec archive_messages(ejabberd:lserver(), Acc :: [[any(),...]]) -> any().
archive_messages(LServer, Acc) ->
    mod_mam_utils:success_sql_query(
      LServer,
      ["INSERT INTO mam_muc_message(id, room_id, nick_name, message) "
       "VALUES ", tuples(Acc)]).


-spec archive_messages(ejabberd:lserver(), Acc :: [[any(),...]],
                       N :: any()) -> any().
archive_messages(LServer, Acc, N) ->
    mod_mam_utils:success_sql_query(
      LServer,
      ["INSERT INTO ", select_table(N), " ",
           "(id, room_id, nick_name, message) "
       "VALUES ", tuples(Acc)]).

-spec lookup_messages(Result :: any(), Host :: ejabberd:server(),
                      ArchiveID :: mod_mam:archive_id(),
                      ArchiveJID :: ejabberd:jid(),
                      RSM :: jlib:rsm_in()  | undefined,
                      Borders :: mod_mam:borders()  | undefined,
                      Start :: mod_mam:unix_timestamp()  | undefined,
                      End :: mod_mam:unix_timestamp()  | undefined,
                      Now :: mod_mam:unix_timestamp(),
                      WithJID :: ejabberd:jid()  | undefined,
                      PageSize :: non_neg_integer(), LimitPassed :: boolean(),
                      MaxResultLimit :: non_neg_integer(),
                      IsSimple :: boolean()  | opt_count) ->
    {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.
lookup_messages({error, _Reason}=Result, _Host,
                     _RoomID, _RoomJID, _RSM, _Borders,
                     _Start, _End, _Now, _WithJID,
                     _PageSize, _LimitPassed, _MaxResultLimit,
                     _IsSimple) ->
    Result;
lookup_messages(_Result, Host,
                RoomID, RoomJID, RSM, Borders,
                Start, End, Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit,
                IsSimple) ->
    try
        lookup_messages_2(Host,
                          RoomID, RoomJID, RSM, Borders,
                          Start, End, Now, WithJID,
                          PageSize, LimitPassed, MaxResultLimit,
                          IsSimple)
    catch _Type:Reason ->
        S = erlang:get_stacktrace(),
        {error, {Reason, S}}
    end.


lookup_messages_2(Host,
                  RoomID, RoomJID = #jid{}, RSM, Borders,
                  Start, End, Now, WithJID,
                  PageSize, LimitPassed, MaxResultLimit,
                  IsSimple) ->
    lookup_messages_3(Host,
                      RoomID, RoomJID, RSM, Borders,
                      Start, End, Now, WithJID,
                      PageSize, LimitPassed, MaxResultLimit,
                      IsSimple, is_opt_count_supported_for(RSM)).

%% Not supported:
%% - #rsm_in{direction = aft, id = ID}
%% - #rsm_in{direction = before, id = ID}
is_opt_count_supported_for(#rsm_in{direction = before, id = undefined}) ->
    true; %% last page is supported
is_opt_count_supported_for(#rsm_in{direction = undefined}) ->
    true; %% offset
is_opt_count_supported_for(undefined) ->
    true; %% no RSM
is_opt_count_supported_for(_) ->
    false.

lookup_messages_3(Host, RoomID, RoomJID,
                  RSM, Borders,
                  Start, End, _Now, WithJID,
                  PageSize, _LimitPassed, _MaxResultLimit, true, _) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    lookup_messages_simple(Host, RoomID, RoomJID, RSM, PageSize, Filter);
lookup_messages_3(Host, RoomID, RoomJID,
                  RSM, Borders,
                  Start, End, _Now, WithJID,
                  PageSize, _LimitPassed, _MaxResultLimit, opt_count, true) ->
    %% Extract messages first than calculate offset and total count
    %% Useful for small result sets (less than one page, than one query is enough)
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    lookup_messages_opt_count(Host, RoomID, RoomJID, RSM, PageSize, Filter);
lookup_messages_3(Host, RoomID, RoomJID,
                  RSM, Borders,
                  Start, End, _Now, WithJID,
                  PageSize, LimitPassed, MaxResultLimit, _, _) ->
    %% Unsupported opt_count or just a regular query
    %% Calculate offset and total count first than extract messages
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    lookup_messages_regular(Host, RoomID, RoomJID, RSM, PageSize, Filter,
                            LimitPassed, MaxResultLimit).

lookup_messages_simple(Host, RoomID, RoomJID,
                #rsm_in{direction = aft, id = ID},
                PageSize, Filter) ->
    %% Get last rows from result set
    MessageRows = extract_messages(Host, RoomID, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, RoomJID, MessageRows)}};
lookup_messages_simple(Host, RoomID, RoomJID,
                       #rsm_in{direction = before, id = ID},
                       PageSize, Filter) ->
    MessageRows = extract_messages(Host, RoomID, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, RoomJID, MessageRows)}};
lookup_messages_simple(Host, RoomID, RoomJID,
                       #rsm_in{direction = undefined, index = Offset},
                       PageSize, Filter) ->
    %% Apply offset
    MessageRows = extract_messages(Host, RoomID, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, RoomJID, MessageRows)}};
lookup_messages_simple(Host, RoomID, RoomJID,
                       undefined,
                       PageSize, Filter) ->
    MessageRows = extract_messages(Host, RoomID, Filter, 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, RoomJID, MessageRows)}}.

%% Cannot be optimized:
%% - #rsm_in{direction = aft, id = ID}
%% - #rsm_in{direction = before, id = ID}
lookup_messages_opt_count(Host, RoomID, RoomJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(Host, RoomID, Filter, 0, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, RoomJID, MessageRows)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Host, RoomID, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, RoomJID, MessageRows)}}
    end;
lookup_messages_opt_count(Host, RoomID, RoomJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) ->
    %% By offset
    MessageRows = extract_messages(Host, RoomID, Filter, Offset, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, RoomJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, RoomID, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(Host, RoomJID, MessageRows)}}
    end;
lookup_messages_opt_count(Host, RoomID, RoomJID,
                          undefined,
                          PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(Host, RoomID, Filter, 0, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, RoomJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, RoomID, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(Host, RoomJID, MessageRows)}}
    end.

lookup_messages_regular(Host, RoomID, RoomJID,
                        RSM = #rsm_in{direction = aft, id = ID},
                        PageSize, Filter, LimitPassed, MaxResultLimit) ->
    TotalCount = calc_count(Host, RoomID, Filter),
    Offset     = calc_offset(Host, RoomID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, RoomID, after_id(ID, Filter), 0, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, RoomJID, MessageRows)}}
    end;
lookup_messages_regular(Host, RoomID, RoomJID,
                        RSM = #rsm_in{direction = before, id = ID},
                        PageSize, Filter, LimitPassed, MaxResultLimit) ->
    TotalCount = calc_count(Host, RoomID, Filter),
    Offset     = calc_offset(Host, RoomID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, RoomID, before_id(ID, Filter), 0, PageSize, true),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, RoomJID, MessageRows)}}
    end;
lookup_messages_regular(Host, RoomID, RoomJID, RSM,
                        PageSize, Filter, LimitPassed, MaxResultLimit) ->
    TotalCount = calc_count(Host, RoomID, Filter),
    Offset     = calc_offset(Host, RoomID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, RoomID, Filter, Offset, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, RoomJID, MessageRows)}}
    end.

is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) ->
    TotalCount - Offset > MaxResultLimit andalso not LimitPassed.

-spec after_id(integer(), [[binary()],...]) -> [[binary()],...].
after_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id > '", SID, "'"].


-spec before_id('undefined' | integer(), [[binary()],...]) -> [[binary()],...].
before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id < '", SID, "'"].


-spec rows_to_uniform_format(ejabberd:server(), ejabberd:jid(), [raw_row()]) ->
    [mod_mam_muc:row()].
rows_to_uniform_format(Host, RoomJID, MessageRows) ->
    EscFormat = ejabberd_odbc:escape_format(Host),
    DbEngine = ejabberd_odbc:db_engine(Host),
    [row_to_uniform_format(DbEngine, EscFormat, Row, RoomJID) || Row <- MessageRows].


-spec row_to_uniform_format(atom(), atom(), raw_row(), ejabberd:jid()) -> mod_mam_muc:row().
row_to_uniform_format(DbEngine, EscFormat, {BMessID,BNick,SDataRaw}, RoomJID) ->
    MessID = list_to_integer(binary_to_list(BMessID)),
    SrcJID = jid:replace_resource(RoomJID, BNick),
    SData = ejabberd_odbc:unescape_odbc_binary(DbEngine, SDataRaw),
    Data = ejabberd_odbc:unescape_binary(EscFormat, SData),
    Packet = stored_binary_to_packet(Data),
    {MessID, SrcJID, Packet}.


-spec row_to_message_id({binary(),_,_}) -> integer().
row_to_message_id({BMessID,_,_}) ->
    list_to_integer(binary_to_list(BMessID)).


-spec remove_archive(ejabberd:server(), mod_mam:archive_id(), ejabberd:jid()) -> 'ok'.
remove_archive(Host, RoomID, _RoomJID) ->
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(RoomID), " "
       "WHERE room_id = '", escape_room_id(RoomID), "'"]),
    ok.


-spec purge_single_message(_Result, Host :: ejabberd:server(),
                           MessID :: mod_mam:message_id(),
                           RoomID :: mod_mam:archive_id(),
                           RoomJID :: ejabberd:jid(),
                           Now :: unix_timestamp()) ->
                              ok  | {error, 'not-allowed'  | 'not-found'}.
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


-spec purge_multiple_messages(_Result, Host :: ejabberd:server(),
                              RoomID :: mod_mam:archive_id(),
                              RoomJID :: ejabberd:jid(),
                              Borders :: mod_mam:borders()  | undefined,
                              Start :: unix_timestamp()  | undefined,
                              End :: unix_timestamp()  | undefined,
                              Now :: unix_timestamp(),
                              WithJID :: ejabberd:jid()  | undefined) ->
                                 ok  | {error, 'not-allowed'}.
purge_multiple_messages(_Result, Host, RoomID, _RoomJID, Borders,
                        Start, End, _Now, WithJID) ->
    Filter = prepare_filter(RoomID, Borders, Start, End, WithJID),
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(RoomID), " ", Filter]),
    ok.


%% @doc Columns are `["id","nick_name","message"]'.
-spec extract_messages(Host :: ejabberd:server(), RoomID :: mod_mam:archive_id(),
        Filter :: filter(), IOffset :: non_neg_integer(), IMax :: pos_integer(),
        ReverseLimit :: boolean()) -> [raw_row()].
extract_messages(_Host, _RoomID, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Host, RoomID, Filter, IOffset, IMax, false) ->
    {selected, _ColumnNames, MessageRows} =
        do_extract_messages(Host, RoomID, Filter, IOffset, IMax, " ORDER BY id "),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    MessageRows;
extract_messages(Host, RoomID, Filter, IOffset, IMax, true) ->
    {selected, _ColumnNames, MessageRows} =
        do_extract_messages(Host, RoomID, Filter, IOffset, IMax, " ORDER BY id DESC "),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    lists:reverse(MessageRows).

do_extract_messages(Host, RoomID, Filter, 0, IMax, Order) ->
    {LimitSQL, LimitMSSQL} = odbc_queries:get_db_specific_limits(IMax),
    mod_mam_utils:success_sql_query(
        Host,
        ["SELECT ", LimitMSSQL, " id, nick_name, message "
        "FROM ", select_table(RoomID), " ",
            Filter,
            Order,
            " ", LimitSQL]);
do_extract_messages(Host, RoomID, Filter, IOffset, IMax, Order) ->
    {LimitSQL, _LimitMSSQL} = odbc_queries:get_db_specific_limits(IMax),
    Offset = odbc_queries:get_db_specific_offset(IOffset, IMax),
    mod_mam_utils:success_sql_query(
        Host,
        ["SELECT id, nick_name, message "
         "FROM ", select_table(RoomID), " ",
         Filter, Order, LimitSQL, Offset]).


%% @doc Zero-based index of the row with UMessID in the result test.
%% If the element does not exists, the MessID of the next element will
%% be returned instead.
%% "SELECT COUNT(*) as "index" FROM mam_muc_message WHERE id <= '",  UMessID
-spec calc_index(Host :: ejabberd:server(), RoomID :: mod_mam:archive_id(),
    Filter :: iodata(), SUMessID :: escaped_message_id()) -> non_neg_integer().
calc_index(Host, RoomID, Filter, SUMessID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", select_table(RoomID), " ",
       Filter, " AND id <= '", SUMessID, "'"]),
    ejabberd_odbc:result_to_integer(BIndex).


%% @doc Count of elements in RSet before the passed element.
%% The element with the passed UMessID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE id < '",  UMessID
-spec calc_before(Host :: ejabberd:server(), RoomID :: mod_mam:archive_id(),
    Filter :: iodata(), SUMessID :: escaped_message_id()) -> non_neg_integer().
calc_before(Host, RoomID, Filter, SUMessID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", select_table(RoomID), " ",
       Filter, " AND id < '", SUMessID, "'"]),
    ejabberd_odbc:result_to_integer(BIndex).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE "
-spec calc_count(Host :: ejabberd:server(), RoomID :: mod_mam:archive_id(),
                 Filter :: filter()) -> non_neg_integer().
calc_count(Host, RoomID, Filter) ->
    {selected, _ColumnNames, [{BCount}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) ",
       "FROM ", select_table(RoomID), " ", Filter]),
    ejabberd_odbc:result_to_integer(BCount).


%% @doc prepare_filter/5
-spec prepare_filter(RoomID :: mod_mam:archive_id(), Borders :: #mam_borders{} | undefined,
        Start :: unix_timestamp() | undefined, End :: unix_timestamp() | undefined,
        WithJID :: #jid{} | undefined) -> filter().
prepare_filter(RoomID, Borders, Start, End, WithJID) ->
    SWithNick = maybe_jid_to_escaped_resource(WithJID),
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filter_1(RoomID, StartID2, EndID2, SWithNick).


-spec prepare_filter_1(RoomID  :: non_neg_integer(),
        StartID :: mod_mam:message_id() | undefined,
        EndID :: mod_mam:message_id() | undefined,
        SWithNick :: escaped_jid() | undefined) -> filter().
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


%% @doc #rsm_in{
%%    max = non_neg_integer() | undefined,
%%    direction = before | aft | undefined,
%%    id = binary() | undefined,
%%    index = non_neg_integer() | undefined}
-spec calc_offset(Host :: ejabberd:server(), RoomID :: mod_mam:archive_id(),
        Filter :: filter(), PageSize :: non_neg_integer(),
        TotalCount :: non_neg_integer(), RSM :: jlib:rsm_in() | undefined)
            -> non_neg_integer().
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


-spec escape_message_id(mod_mam:message_id()) -> string().
escape_message_id(MessID) when is_integer(MessID) ->
    integer_to_list(MessID).


-spec escape_room_id(mod_mam:archive_id()) -> string().
escape_room_id(RoomID) when is_integer(RoomID) ->
    integer_to_list(RoomID).


-spec maybe_jid_to_escaped_resource('undefined' | ejabberd:jid())
                    -> 'undefined' | binary() | string().
maybe_jid_to_escaped_resource(undefined) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = <<>>}) ->
    undefined;
maybe_jid_to_escaped_resource(#jid{lresource = WithLResource}) ->
    ejabberd_odbc:escape(WithLResource).


-spec join([[any(),...],...]) -> [[any()],...].
join([H|T]) ->
    [H, [", " ++ X || X <- T]].


-spec tuples([[any(),...]]) -> [[any()],...].
tuples(Rows) ->
    join([tuple(Row) || Row <- Rows]).


-spec tuple([any(),...]) -> [any(),...].
tuple([H|T]) ->
    ["('", H, "'", [[", '", X, "'"] || X <- T], ")"].


-spec maybe_encode_compact_uuid('undefined' | integer(), 0 | 255)
                            -> 'undefined' | integer().
maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).


%% ----------------------------------------------------------------------
%% Optimizations

packet_to_stored_binary(Packet) ->
    %% Module implementing mam_message behaviour
    Module = db_message_format(),
    Module:encode(Packet).

stored_binary_to_packet(Bin) ->
    %% Module implementing mam_message behaviour
    Module = db_message_format(),
    Module:decode(Bin).

select_table(N) ->
    case hand_made_partitions() of
        true ->
            io_lib:format("mam_muc_message_~2..0B", [N rem partition_count()]);
        false ->
            "mam_muc_message"
    end.

partition_count() ->
    16.

%% ----------------------------------------------------------------------
%% Dynamic params module

%% compile([
%%      {db_message_format, module()},
%%      {hand_made_partitions, boolean()},
%%      ])
compile_params_module(Params) ->
    CodeStr = params_helper(expand_simple_param(Params)),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_mam_muc_odbc_arch_params.erl", Code).

expand_simple_param(Params) ->
    lists:flatmap(fun(simple) -> simple_params();
                     ({simple,true}) -> simple_params();
                     (Param) -> [Param]
                  end, Params).

simple_params() ->
    [{db_message_format, mam_message_xml}].

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
        "-module(mod_mam_muc_odbc_arch_params).~n"
        "-compile(export_all).~n"
        "db_message_format() -> ~p.~n"
        "hand_made_partitions() -> ~p.~n",
        [proplists:get_value(db_message_format, Params, mam_message_compressed_eterm),
         proplists:get_bool(hand_made_partitions, Params)]))).

-spec db_message_format() -> compressed_term|term|xml.
db_message_format() ->
    mod_mam_muc_odbc_arch_params:db_message_format().

-spec hand_made_partitions() -> boolean().
hand_made_partitions() ->
    mod_mam_muc_odbc_arch_params:hand_made_partitions().
