%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc RDBMS backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_rdbms_arch).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-callback encode(term()) -> binary().
-callback decode(binary()) -> term().

-export([archive_size/4,
         archive_message/3,
         lookup_messages/3,
         remove_archive/4]).

-export([get_mam_pm_gdpr_data/2]).

%% Called from mod_mam_rdbms_async_writer
-export([prepare_message/2, retract_message/2, prepare_insert/2]).

%% ----------------------------------------------------------------------
%% Imports

%% UID
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

-type filter_field()      :: tuple().
-type filter()            :: [filter_field()].

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> 'ok'.
start(Host, Opts) ->
    start_pm(Host, Opts),

    prepare_insert(insert_mam_message, 1),
    mongoose_rdbms:prepare(mam_archive_size, mam_message, [user_id],
                           [<<"SELECT COUNT(*) FROM mam_message ">>,
                            index_hint_sql(Host),
                            <<"WHERE user_id = ?">>]),
    mongoose_rdbms:prepare(mam_archive_remove, mam_message, [user_id],
                           [<<"DELETE FROM mam_message "
                              "WHERE user_id = ?">>]),
    mongoose_rdbms:prepare(mam_make_tombstone, mam_message, [message, user_id, id],
                           [<<"UPDATE mam_message SET message = ?, search_body = '' "
                              "WHERE user_id = ? AND id = ?">>]),

    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(1),
    mongoose_rdbms:prepare(mam_select_messages_to_retract, mam_message,
                           [user_id, remote_bare_jid, origin_id, direction],
                           [<<"SELECT ", LimitMSSQL/binary,
                              " id, message FROM mam_message"
                              " WHERE user_id = ? AND remote_bare_jid = ? "
                              " AND origin_id = ? AND direction = ?"
                              " ORDER BY id DESC ", LimitSQL/binary>>]),

    mongoose_rdbms:prepare(mam_extract_gdpr_messages, mam_message,
                           [user_id],
                           [<<"SELECT id, from_jid, message "
                              " FROM mam_message "
                              " WHERE user_id=? "
                              " ORDER BY id">>]),
    ok.


-spec stop(jid:server()) -> ok.
stop(Host) ->
    stop_pm(Host).

-spec get_mam_pm_gdpr_data(ejabberd_gen_mam_archive:mam_pm_gdpr_data(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(Acc, #jid{ user = User, server = Server, lserver = LServer } = UserJid) ->
    case mod_mam:archive_id(Server, User) of
        undefined -> [];
        ArchiveID ->
            {selected, Rows} = extract_gdpr_messages(LServer, ArchiveID),
            [{BMessID, gdpr_decode_jid(LServer, UserJid, FromJID),
              gdpr_decode_packet(LServer, SDataRaw)} || {BMessID, FromJID, SDataRaw} <- Rows] ++ Acc
    end.

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

-spec start_pm(jid:server(), _) -> 'ok'.
start_pm(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(get_mam_pm_gdpr_data, Host, ?MODULE, get_mam_pm_gdpr_data, 50),
    ok.


-spec stop_pm(jid:server()) -> ok.
stop_pm(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(get_mam_pm_gdpr_data, Host, ?MODULE, get_mam_pm_gdpr_data, 50),
    ok.

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

encode_direction(incoming) -> <<"I">>;
encode_direction(outgoing) -> <<"O">>.


-spec archive_size(Size :: integer(), Host :: jid:server(),
                   ArcId :: mod_mam:archive_id(), ArcJID :: jid:jid()) -> integer().
archive_size(Size, Host, UserID, _UserJID) when is_integer(Size) ->
    {selected, [{BSize}]} = mod_mam_utils:success_sql_execute(Host, mam_archive_size, [UserID]),
    mongoose_rdbms:result_to_integer(BSize).


-spec index_hint_sql(jid:server()) -> string().
index_hint_sql(Host) ->
    case mongoose_rdbms:db_engine(Host) of
        mysql ->
            "USE INDEX(PRIMARY, i_mam_message_rem) ";
        _ ->
            ""
    end.


-spec archive_message(_Result, jid:server(), mod_mam:archive_message_params()) -> ok.
archive_message(_Result, Host, Params) ->
    try
        do_archive_message(Host, Params)
    catch Class:Reason:StackTrace ->
              ?LOG_ERROR(#{what => archive_message_failed,
                           host => Host, mam_params => Params,
                           class => Class, reason => Reason, stacktrace => StackTrace}),
            {error, Reason}
    end.

do_archive_message(Host, Params) ->
    Row = prepare_message(Host, Params),
    {updated, 1} = mod_mam_utils:success_sql_execute(Host, insert_mam_message, Row),
    retract_message(Host, Params).

-spec retract_message(jid:server(), mod_mam:archive_message_params()) -> ok.
retract_message(Host, #{archive_id := UserID,
                        local_jid := LocJID,
                        remote_jid := RemJID,
                        direction := Dir,
                        packet := Packet}) ->
    case mod_mam_utils:get_retract_id(mod_mam, Host, Packet) of
        none ->
            ok;
        OriginIDToRetract ->
            retract_message(Host, UserID, LocJID, RemJID, OriginIDToRetract, Dir)
    end.

retract_message(Host, UserID, LocJID, RemJID, OriginID, Dir) ->
    MinBareRemJID = minify_bare_jid(Host, LocJID, RemJID),
    BinDir = encode_direction(Dir),
    {selected, Rows} = execute_select_messages_to_retract(
                         Host, UserID, MinBareRemJID, OriginID, BinDir),
    make_tombstone(Host, UserID, OriginID, Rows),
    ok.

make_tombstone(_Host, UserID, OriginID, []) ->
    ?LOG_INFO(#{what => make_tombstone_failed,
                text => <<"Message to retract was not found by origin id">>,
                user_id => UserID, origin_id => OriginID});
make_tombstone(Host, UserID, OriginID, [{ResMessID, ResData}]) ->
    Data = mongoose_rdbms:unescape_binary(Host, ResData),
    Packet = stored_binary_to_packet(Host, Data),
    MessID = mongoose_rdbms:result_to_integer(ResMessID),
    Tombstone = mod_mam_utils:tombstone(Packet, OriginID),
    TombstoneData = packet_to_stored_binary(Host, Tombstone),
    execute_make_tombstone(Host, TombstoneData, UserID, MessID).

execute_select_messages_to_retract(Host, UserID, BareRemJID, OriginID, Dir) ->
    mod_mam_utils:success_sql_execute(Host, mam_select_messages_to_retract,
                                      [UserID, BareRemJID, OriginID, Dir]).

execute_make_tombstone(Host, TombstoneData, UserID, MessID) ->
    {updated, _} =
        mod_mam_utils:success_sql_execute(Host, mam_make_tombstone,
                                          [TombstoneData, UserID, MessID]).

-spec prepare_message(jid:server(), mod_mam:archive_message_params()) -> list().
prepare_message(Host, #{message_id := MessID,
                        archive_id := UserID,
                        local_jid := LocJID,
                        remote_jid := RemJID = #jid{lresource = RemLResource},
                        source_jid := SrcJID,
                        origin_id := OriginID,
                        direction := Dir,
                        packet := Packet}) ->
    SBareRemJID = jid_to_stored_binary(Host, LocJID, jid:to_bare(RemJID)),
    SSrcJID = jid_to_stored_binary(Host, LocJID, SrcJID),
    SDir = encode_direction(Dir),
    SOriginID = case OriginID of
                    none -> null;
                    _ -> OriginID
                end,
    Data = packet_to_stored_binary(Host, Packet),
    TextBody = mod_mam_utils:packet_to_search_body(mod_mam, Host, Packet),
    [MessID, UserID, SBareRemJID, RemLResource, SDir, SSrcJID, SOriginID, Data, TextBody].

-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_message,
    Fields = [id, user_id, remote_bare_jid, remote_resource,
              direction, from_jid, origin_id, message, search_body],
    {Query, Fields2} = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields2, Query),
    ok.

-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
                             {ok, mod_mam:lookup_result()}.
lookup_messages({error, _Reason}=Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, Host, Params) ->
    try
        RSM = maps:get(rsm, Params),
        SearchText = maps:get(search_text, Params),
        Params2 = Params#{
                    norm_search_text => mod_mam_utils:normalize_search_text(SearchText),
                    opt_count_supported => is_opt_count_supported_for(RSM)},
        Filter = prepare_filter(Host, Params2),
        do_lookup_messages(Host, Filter, Params2)
    catch _Type:Reason:S ->
        {error, {Reason, {stacktrace, S}}}
    end.

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

%% There are several strategies how to extract messages:
%% - we can use regular query that requires counting;
%% - we can reduce number of queries if we skip counting for small data sets;
%% - sometimes we want not to count at all
%%   (for example, our client side counts ones and keep the information)
do_lookup_messages(Host, Filter, Params = #{owner_jid := UserJID, rsm := RSM,
                                            page_size := PageSize}) ->
    case Params of
        #{is_simple := true} ->
            %% Simple query without calculating offset and total count
            lookup_messages_simple(Host, UserJID, RSM, PageSize, Filter);
        #{is_simple := opt_count, opt_count_supported := true} ->
            %% Extract messages first than calculate offset and total count
            %% Useful for small result sets (less than one page, than one query is enough)
            lookup_messages_opt_count(Host, UserJID, RSM, PageSize, Filter);
        _ ->
            %% Unsupported opt_count or just a regular query
            %% Calculate offset and total count first than extract messages
            lookup_messages_regular(Host, UserJID, RSM, PageSize, Filter)
    end.

lookup_messages_simple(Host, UserJID, RSM, PageSize, Filter) ->
    {Filter2, Offset, Order} = rsm_to_filter(RSM, Filter),
    MessageRows = extract_messages(Host, Filter2, Offset, PageSize, Order),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}}.

rsm_to_filter(RSM, Filter) ->
    case RSM of
        %% Get last rows from result set
        #rsm_in{direction = aft, id = ID} ->
            {after_id(ID, Filter), 0, asc};
        #rsm_in{direction = before, id = ID} ->
            {before_id(ID, Filter), 0, desc};
        #rsm_in{direction = undefined, index = Index} ->
            {Filter, Index, asc};
        undefined ->
            {Filter, 0, asc}
    end.

%% Cases that cannot be optimized and used with this function:
%% - #rsm_in{direction = aft, id = ID}
%% - #rsm_in{direction = before, id = ID}
lookup_messages_opt_count(Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(Host, Filter, 0, PageSize, desc),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Host, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;
lookup_messages_opt_count(Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) ->
    %% By offset
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, asc),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;
lookup_messages_opt_count(Host, UserJID,
                          undefined,
                          PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(Host, Filter, 0, PageSize, asc),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end.

lookup_messages_regular(Host, UserJID, RSM, PageSize, Filter) ->
    TotalCount = calc_count(Host, Filter),
    Offset = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    MessageRows =
        case RSM of
            #rsm_in{direction = aft, id = ID} ->
                extract_messages(Host, from_id(ID, Filter), 0, PageSize + 1, asc);
            #rsm_in{direction = before, id = ID} when ID =/= undefined ->
                extract_messages(Host, to_id(ID, Filter), 0, PageSize + 1, desc);
            _ ->
                extract_messages(Host, Filter, Offset, PageSize, asc)
        end,
    Result = {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)},
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result).

-spec after_id(ID :: mod_mam:message_id(), Filter :: filter()) -> filter().
after_id(ID, Filter) ->
    [{greater, id, ID}|Filter].

-spec before_id(ID :: mod_mam:message_id() | undefined,
               Filter :: filter()) -> filter().
before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter) ->
    [{lower, id, ID}|Filter].

-spec from_id(ID :: mod_mam:message_id(), Filter :: filter()) -> filter().
from_id(ID, Filter) ->
    [{ge, id, ID}|Filter].

-spec to_id(ID :: mod_mam:message_id(), Filter :: filter()) -> filter().
to_id(ID, Filter) ->
    [{le, id, ID}|Filter].

rows_to_uniform_format(Host, UserJID, MessageRows) ->
    [do_row_to_uniform_format(Host, UserJID, Row) || Row <- MessageRows].

do_row_to_uniform_format(Host, UserJID, {BMessID, BSrcJID, SDataRaw}) ->
    MessID = mongoose_rdbms:result_to_integer(BMessID),
    SrcJID = stored_binary_to_jid(Host, UserJID, BSrcJID),
    Data = mongoose_rdbms:unescape_binary(Host, SDataRaw),
    Packet = stored_binary_to_packet(Host, Data),
    {MessID, SrcJID, Packet}.

row_to_message_id({BMessID, _, _}) ->
    mongoose_rdbms:result_to_integer(BMessID).


%% Removals

-spec remove_archive(Acc :: mongoose_acc:t(), Host :: jid:server(),
                     ArchiveID :: mod_mam:archive_id(),
                     RoomJID :: jid:jid()) -> mongoose_acc:t().
remove_archive(Acc, Host, UserID, _UserJID) ->
    remove_archive(Host, UserID),
    Acc.

remove_archive(Host, UserID) ->
    {updated, _} = mod_mam_utils:success_sql_execute(Host, mam_archive_remove, [UserID]).

%% @doc Each record is a tuple of form
%% `{<<"13663125233">>, <<"bob@localhost">>, <<binary>>}'.
%% Columns are `["id", "from_jid", "message"]'.
-type msg() :: {binary(), jid:literal_jid(), binary()}.
-spec extract_messages(Host :: jid:server(),
                       Filter :: filter(), IOffset :: non_neg_integer(), IMax :: pos_integer(),
                       Order :: asc | desc) -> [msg()].
extract_messages(_Host, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Host, Filter, IOffset, IMax, Order) ->
    {selected, MessageRows} =
        do_extract_messages(Host, Filter, IOffset, IMax, Order),
    ?LOG_DEBUG(#{what => mam_extract_messages,
                 mam_filter => Filter, offset => IOffset, max => IMax,
                 host => Host, message_rows => MessageRows}),
    maybe_reserve(Order, MessageRows).

do_extract_messages(Host, Filters, IOffset, IMax, Order) ->
    Filters2 = Filters ++ rdbms_queries:limit_offset_filters(IMax, IOffset),
    do_lookup_query(lookup, Host, Filters2, Order).

do_lookup_query(QueryType, Host, Filters, Order) ->
    StmtName = filters_to_statement_name(QueryType, Filters, Order),
    case mongoose_rdbms:prepared(StmtName) of
        false ->
            %% Create a new type of a query
            SQL = lookup_sql_binary(QueryType, Filters, Order),
            Columns = filters_to_columns(Filters),
            mongoose_rdbms:prepare(StmtName, mam_message, Columns, SQL);
        true ->
            ok
    end,
    Args = filters_to_args(Filters),
    try mongoose_rdbms:execute(Host, StmtName, Args) of
        {selected, Rs} when is_list(Rs) ->
            {selected, Rs};
        Error ->
            What = #{what => mam_lookup_failed, statement => StmtName,
                     sql_query => lookup_sql_binary(QueryType, Filters, Order),
                     reason => Error, host => Host},
            ?LOG_ERROR(What),
            error(What)
    catch Class:Error:Stacktrace ->
            What = #{what => mam_lookup_failed, statement => StmtName,
                     sql_query => lookup_sql_binary(QueryType, Filters, Order),
                     class => Class, stacktrace => Stacktrace,
                     reason => Error, host => Host},
            ?LOG_ERROR(What),
            error(What)
    end.

%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
%% "SELECT COUNT(*) as "index" FROM mam_message WHERE id <= '",  UID
-spec calc_index(Host :: jid:server(),
                 Filter :: filter(),
                 ID :: mod_mam:message_id()) -> non_neg_integer().
calc_index(Host, Filter, ID) ->
    calc_count(Host, [{le, id, ID}|Filter]).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE id < '",  UID
-spec calc_before(Host :: jid:server(),
                  Filter :: filter(),
                  ID :: mod_mam:message_id()) -> non_neg_integer().
calc_before(Host, Filter, ID) ->
    calc_count(Host, [{lower, id, ID}|Filter]).

%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(Host :: jid:server(),
                 Filter :: filter()) -> non_neg_integer().
calc_count(Host, Filter) ->
    {selected, [{BCount}]} =
        do_lookup_query(count, Host, Filter, unordered),
    mongoose_rdbms:result_to_integer(BCount).

%% @doc #rsm_in{
%%    max = non_neg_integer() | undefined,
%%    direction = before | aft | undefined,
%%    id = binary() | undefined,
%%    index = non_neg_integer() | undefined}
-spec calc_offset(Host :: jid:server(),
                  Filter :: filter(), PageSize :: non_neg_integer(),
                  TotalCount :: non_neg_integer(), RSM :: jlib:rsm_in()) -> non_neg_integer().
calc_offset(_LS, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
  when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Host, F, PS, _TC, #rsm_in{direction = before, id = ID})
  when is_integer(ID) ->
    max(0, calc_before(Host, F, ID) - PS);
calc_offset(Host, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
  when is_integer(ID) ->
    calc_index(Host, F, ID);
calc_offset(_LS, _F, _PS, _TC, _RSM) ->
    0.

maybe_reserve(asc, List) ->
    List;
maybe_reserve(desc, List) ->
    lists:reverse(List).

extract_gdpr_messages(Host, ArchiveID) ->
    mod_mam_utils:success_sql_execute(Host, mam_extract_gdpr_messages, [ArchiveID]).

minify_bare_jid(Host, LocJID, JID) ->
    jid_to_stored_binary(Host, LocJID, jid:to_bare(JID)).

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

%% ----------------------------------------------------------------------
%% Optimizations

jid_to_stored_binary(Host, UserJID, JID) ->
    Module = db_jid_codec(Host),
    mam_jid:encode(Module, UserJID, JID).

stored_binary_to_jid(Host, UserJID, BSrcJID) ->
    Module = db_jid_codec(Host),
    mam_jid:decode(Module, UserJID, BSrcJID).

packet_to_stored_binary(Host, Packet) ->
    Module = db_message_codec(Host),
    mam_message:encode(Module, Packet).

stored_binary_to_packet(Host, Bin) ->
    Module = db_message_codec(Host),
    mam_message:decode(Module, Bin).

-spec db_jid_codec(jid:server()) -> module().
db_jid_codec(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, db_jid_format, mam_jid_mini).

-spec db_message_codec(jid:server()) -> module().
db_message_codec(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, db_message_format, mam_message_compressed_eterm).

%gdpr helpers
gdpr_decode_jid(Host, UserJID, BSrcJID) ->
    Codec = mod_mam_meta:get_mam_module_opt(Host, ?MODULE, db_jid_format, mam_jid_mini),
    JID = mam_jid:decode(Codec, UserJID, BSrcJID),
    jid:to_binary(JID).

gdpr_decode_packet(Host, SDataRaw) ->
    Codec = mod_mam_meta:get_mam_module_opt(Host, ?MODULE, db_message_format,
                                            mam_message_compressed_eterm),
    Data = mongoose_rdbms:unescape_binary(Host, SDataRaw),
    Message = mam_message:decode(Codec, Data),
    exml:to_binary(Message).

%% ----------------------------------------------------------------------
%% Prepared queries helpers

lookup_sql_binary(QueryType, Filters, Order) ->
    iolist_to_binary(lookup_sql(QueryType, Filters, Order)).

lookup_sql(QueryType, Filters, Order) ->
    LimitSQL = limit_sql(QueryType),
    OrderSQL = order_to_sql(Order),
    FilterSQL = filters_to_sql(Filters),
    ["SELECT ", columns_sql(QueryType), " "
     "FROM mam_message ",
     FilterSQL, OrderSQL, LimitSQL].

columns_sql(lookup) -> "id, from_jid, message";
columns_sql(count) -> "COUNT(*)".

limit_sql(lookup) -> rdbms_queries:limit_offset_sql();
limit_sql(count) -> "".

filters_to_columns(Filters) ->
   [Column || {_Op, Column, _Value} <- Filters].

filters_to_args(Filters) ->
   [Value || {_Op, _Column, Value} <- Filters].

filters_to_statement_name(QueryType, Filters, Order) ->
    QueryId = query_type_to_id(QueryType),
    Ids = [type_to_id(Type) ++ column_to_id(Col) || {Type, Col, _Val} <- Filters],
    OrderId = order_type_to_id(Order),
    list_to_atom("mam_" ++ QueryId ++ "_" ++ OrderId ++ "_" ++ lists:append(Ids)).

query_type_to_id(lookup) -> "lookup";
query_type_to_id(count) -> "count".

order_type_to_id(desc) -> "d";
order_type_to_id(asc) -> "a";
order_type_to_id(unordered) -> "u".

order_to_sql(asc) -> " ORDER BY id ";
order_to_sql(desc) -> " ORDER BY id DESC ";
order_to_sql(unordered) -> " ".

type_to_id(le)      -> "le"; %% lower or equal
type_to_id(ge)      -> "ge"; %% greater or equal
type_to_id(equal)   -> "eq";
type_to_id(lower)   -> "lt"; %% lower than
type_to_id(greater) -> "gt"; %% greater than
type_to_id(like)    -> "lk";
type_to_id(limit)   -> "li";
type_to_id(offset)  -> "of".

column_to_id(id) -> "i";
column_to_id(user_id) -> "u";
column_to_id(remote_bare_jid) -> "b";
column_to_id(remote_resource) -> "r";
column_to_id(search_body) -> "s";
%% fictional columns
column_to_id(limit) -> "l";
column_to_id(offset) -> "o".

filters_to_sql(Filters) ->
    SQLs = [filter_to_sql(Filter) || Filter <- Filters],
    case skip_undefined(SQLs) of
        [] ->
            "";
        Defined ->
            [" WHERE ", rdbms_queries:join(Defined, " AND ")]
    end.

skip_undefined(List) ->
    [X || X <- List, X =/= undefined].

filter_to_sql({Op, Column, _Value}) ->
    filter_to_sql(Op, atom_to_list(Column)).

filter_to_sql(limit, _) ->
    undefined;
filter_to_sql(offset, _) ->
    undefined;
filter_to_sql(lower, Column) ->
    Column ++ " < ?";
filter_to_sql(greater, Column) ->
    Column ++ " > ?";
filter_to_sql(le, Column) ->
    Column ++ " <= ?";
filter_to_sql(ge, Column) ->
    Column ++ " >= ?";
filter_to_sql(equal, Column) ->
    Column ++ " = ?";
filter_to_sql(like, Column) ->
    Column ++ " LIKE ?".

-spec prepare_filter(Host :: jid:server(), Params :: map()) -> filter().
prepare_filter(Host, #{
                       archive_id := UserID, owner_jid := UserJID,
                       borders := Borders, start_ts := Start, end_ts := End,
                       with_jid := WithJID, norm_search_text := SearchText
                      }) ->
    {MinWithJID, WithResource} =
        case WithJID of
            undefined -> {undefined, undefined};
            #jid{lresource = <<>>} ->
                {minify_bare_jid(Host, UserJID, WithJID), undefined};
            #jid{lresource = WithLResource} ->
                {minify_bare_jid(Host, UserJID, WithJID),
                 WithLResource}
        end,
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filters(UserID, StartID2, EndID2, MinWithJID, WithResource, SearchText).

-spec prepare_filters(UserID :: non_neg_integer(),
                      StartID :: mod_mam:message_id() | undefined,
                      EndID :: mod_mam:message_id() | undefined,
                      WithJID :: binary() | undefined,
                      WithResource :: binary() | undefined,
                      SearchText :: binary() | undefined) -> filter().
prepare_filters(UserID, StartID, EndID, WithJID, WithResource, SearchText) ->
    [{equal, user_id, UserID}] ++
    case StartID of
        undefined -> [];
        _         -> [{ge, id, StartID}]
    end ++
    case EndID of
        undefined -> [];
        _         -> [{le, id, EndID}]
    end ++
    case WithJID of
        undefined -> [];
        _         -> [{equal, remote_bare_jid, WithJID}]
    end ++
    case WithResource of
        undefined -> [];
        _         -> [{equal, remote_resource, WithResource}]
    end ++
    case SearchText of
        undefined -> [];
        _         -> prepare_search_filters(SearchText)
    end.

%% Constructs a separate LIKE filter for each word.
%% SearchText example is "word1%word2%word3".
%% Order of words does not matter (they can go in any order).
prepare_search_filters(SearchText) ->
    Words = binary:split(SearchText, <<"%">>, [global]),
    [prepare_search_filter(Word) || Word <- Words].

-spec prepare_search_filter(binary()) -> filter().
prepare_search_filter(Word) ->
     %% Search for "%Word%"
    {like, search_body, <<"%", Word/binary, "%">>}.
