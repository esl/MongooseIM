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
         archive_message/9,
         lookup_messages/3,
         remove_archive/4]).

-export([get_mam_pm_gdpr_data/2]).

%% Called from mod_mam_rdbms_async_writer
-export([prepare_message/8, prepare_insert/2]).

%% ----------------------------------------------------------------------
%% Imports

%% UID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

%% Other
-import(mod_mam_utils,
        [apply_start_border/2,
         apply_end_border/2]).

-import(mongoose_rdbms,
        [escape_string/1,
         escape_integer/1,
         use_escaped_string/1,
         use_escaped_integer/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_rsm.hrl").

%% ----------------------------------------------------------------------
%% Types

-type filter()            :: mongoose_rdbms:sql_query_part().
-type escaped_message_id() :: mongoose_rdbms:escape_string().
-type escaped_jid()       :: mongoose_rdbms:escaped_string().
-type escaped_resource()  :: mongoose_rdbms:escaped_string().


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


-spec archive_message(_Result, Host :: jid:server(),
                      MessID :: mod_mam:message_id(), UserID :: mod_mam:archive_id(),
                      LocJID :: jid:jid(), RemJID :: jid:jid(),
                      SrcJID :: jid:jid(), Dir :: atom(), Packet :: any()) -> ok.
archive_message(Result, Host, MessID, UserID,
                LocJID, RemJID, SrcJID, Dir, Packet) when is_integer(UserID) ->
    try
        do_archive_message(Result, Host, MessID, UserID,
                           LocJID, RemJID, SrcJID, Dir, Packet)
    catch _Type:Reason:StackTrace ->
            ?ERROR_MSG("event=archive_message_failed mess_id=~p user_id=~p "
                       "loc_jid=~p rem_jid=~p src_jid=~p dir=~p reason='~p' stacktrace=~p",
                       [MessID, UserID, LocJID, RemJID, SrcJID, Dir,
                        Reason, StackTrace]),
            {error, Reason}
    end.

do_archive_message(_Result, Host, MessID, UserID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    Row = prepare_message(Host, MessID, UserID, LocJID, RemJID, SrcJID, Dir, Packet),
    {updated, 1} = mod_mam_utils:success_sql_execute(Host, insert_mam_message, Row),
    ok.

prepare_message(Host, MessID, UserID, LocJID = #jid{}, RemJID = #jid{lresource = RemLResource},
                SrcJID, Dir, Packet) ->
    SBareRemJID = jid_to_stored_binary(Host, LocJID, jid:to_bare(RemJID)),
    SSrcJID = jid_to_stored_binary(Host, LocJID, SrcJID),
    SDir = encode_direction(Dir),
    Data = packet_to_stored_binary(Host, Packet),
    TextBody = mod_mam_utils:packet_to_search_body(mod_mam, Host, Packet),
    [MessID, UserID, SBareRemJID, RemLResource, SDir, SSrcJID, Data, TextBody].

-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_message,
    Fields = [id, user_id, remote_bare_jid, remote_resource,
              direction, from_jid, message, search_body],
    Query = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields, Query),
    ok.

-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
                             {ok, mod_mam:lookup_result()}.
lookup_messages({error, _Reason}=Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, Host, Params) ->
    try
        UserID = maps:get(archive_id, Params),
        UserJID = maps:get(owner_jid, Params),
        RSM = maps:get(rsm, Params),
        Borders = maps:get(borders, Params),
        Start = maps:get(start_ts, Params),
        End = maps:get(end_ts, Params),
        Now = maps:get(now, Params),
        WithJID = maps:get(with_jid, Params),
        SearchText = maps:get(search_text, Params),
        PageSize = maps:get(page_size, Params),
        IsSimple = maps:get(is_simple, Params),

        do_lookup_messages(Host,
                           UserID, UserJID, RSM, Borders,
                           Start, End, Now, WithJID,
                           mod_mam_utils:normalize_search_text(SearchText),
                           PageSize,
                           IsSimple, is_opt_count_supported_for(RSM))
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
do_lookup_messages(Host, UserID, UserJID,
                   RSM, Borders,
                   Start, End, _Now, WithJID, SearchText,
                   PageSize, true, _) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(Host, UserID, UserJID, Borders, Start, End, WithJID, SearchText),
    lookup_messages_simple(Host, UserJID, RSM, PageSize, Filter);
do_lookup_messages(Host, UserID, UserJID,
                   RSM, Borders,
                   Start, End, _Now, WithJID, SearchText,
                   PageSize, opt_count, true) ->
    %% Extract messages first than calculate offset and total count
    %% Useful for small result sets (less than one page, than one query is enough)
    Filter = prepare_filter(Host, UserID, UserJID, Borders, Start, End, WithJID, SearchText),
    lookup_messages_opt_count(Host, UserJID, RSM, PageSize, Filter);
do_lookup_messages(Host, UserID, UserJID,
                   RSM, Borders,
                   Start, End, _Now, WithJID, SearchText,
                   PageSize, _, _) ->
    %% Unsupported opt_count or just a regular query
    %% Calculate offset and total count first than extract messages
    Filter = prepare_filter(Host, UserID, UserJID, Borders, Start, End, WithJID, SearchText),
    lookup_messages_regular(Host, UserJID, RSM, PageSize, Filter).

lookup_messages_simple(Host, UserJID,
                       #rsm_in{direction = aft, id = ID},
                       PageSize, Filter) ->
    %% Get last rows from result set
    MessageRows = extract_messages(Host, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};
lookup_messages_simple(Host, UserJID,
                       #rsm_in{direction = before, id = ID},
                       PageSize, Filter) ->
    MessageRows = extract_messages(Host, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};
lookup_messages_simple(Host, UserJID,
                       #rsm_in{direction = undefined, index = Offset},
                       PageSize, Filter) ->
    %% Apply offset
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};
lookup_messages_simple(Host, UserJID, undefined, PageSize, Filter) ->
    MessageRows = extract_messages(Host, Filter, 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}}.

%% Cases that cannot be optimized and used with this function:
%% - #rsm_in{direction = aft, id = ID}
%% - #rsm_in{direction = before, id = ID}
lookup_messages_opt_count(Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(Host, Filter, 0, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            IndexHintSQL = index_hint_sql(Host),
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Host, before_id(FirstID, Filter), IndexHintSQL),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;
lookup_messages_opt_count(Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) ->
    %% By offset
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            IndexHintSQL = index_hint_sql(Host),
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, after_id(LastID, Filter), IndexHintSQL),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;
lookup_messages_opt_count(Host, UserJID,
                          undefined,
                          PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(Host, Filter, 0, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            IndexHintSQL = index_hint_sql(Host),
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, after_id(LastID, Filter), IndexHintSQL),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end.

lookup_messages_regular(Host, UserJID,
                        RSM = #rsm_in{direction = aft, id = ID},
                        PageSize, Filter) when ID =/= undefined ->
    IndexHintSQL = index_hint_sql(Host),
    TotalCount = calc_count(Host, Filter, IndexHintSQL),
    Offset = calc_offset(Host, Filter, IndexHintSQL, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Host, from_id(ID, Filter), 0, PageSize + 1, false),
    Result = {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)},
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result);
lookup_messages_regular(Host, UserJID,
                        RSM = #rsm_in{direction = before, id = ID},
                        PageSize, Filter) when ID =/= undefined ->
    IndexHintSQL = index_hint_sql(Host),
    TotalCount = calc_count(Host, Filter, IndexHintSQL),
    Offset = calc_offset(Host, Filter, IndexHintSQL, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Host, to_id(ID, Filter), 0, PageSize + 1, true),
    Result = {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)},
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result);
lookup_messages_regular(Host, UserJID, RSM,
                        PageSize, Filter) ->
    IndexHintSQL = index_hint_sql(Host),
    TotalCount = calc_count(Host, Filter, IndexHintSQL),
    Offset     = calc_offset(Host, Filter, IndexHintSQL, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Host, Filter, Offset, PageSize, false),
    {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}.

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
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM mam_message "
       "WHERE user_id = ", use_escaped_integer(escape_user_id(UserID))]).

%% @doc Each record is a tuple of form
%% `{<<"13663125233">>, <<"bob@localhost">>, <<binary>>}'.
%% Columns are `["id", "from_jid", "message"]'.
-type msg() :: {binary(), jid:literal_jid(), binary()}.
-spec extract_messages(Host :: jid:server(),
                       Filter :: filter(), IOffset :: non_neg_integer(), IMax :: pos_integer(),
                       ReverseLimit :: boolean()) -> [msg()].
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
        ["SELECT ", LimitMSSQL, " id, from_jid, message "
        "FROM mam_message ",
            Filter,
            Order,
            " ", LimitSQL]);
do_extract_messages(Host, Filter, IOffset, IMax, Order) ->
    {LimitSQL, _LimitMSSQL} = rdbms_queries:get_db_specific_limits(IMax),
    Offset = rdbms_queries:get_db_specific_offset(IOffset, IMax),
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT id, from_jid, message "
       "FROM mam_message ",
       Filter, Order, LimitSQL, Offset]).

extract_gdpr_messages(Host, ArchiveID) ->
    Filter = ["WHERE user_id=", use_escaped_integer(escape_user_id(ArchiveID))],
    mod_mam_utils:success_sql_query(
        Host,
        ["SELECT id, from_jid, message "
         "FROM mam_message ",
         Filter, " ORDER BY id"]).

%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
%% "SELECT COUNT(*) as "index" FROM mam_message WHERE id <= '",  UID
-spec calc_index(Host :: jid:server(),
                 Filter :: filter(), IndexHintSQL :: string(),
                 SUID :: escaped_message_id()) -> non_neg_integer().
calc_index(Host, Filter, IndexHintSQL, SUID) ->
    {selected, [{BIndex}]} =
        mod_mam_utils:success_sql_query(
          Host,
          ["SELECT COUNT(*) FROM mam_message ",
           IndexHintSQL, Filter,
           " AND id <= ", use_escaped_integer(SUID)]),
    mongoose_rdbms:result_to_integer(BIndex).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE id < '",  UID
-spec calc_before(Host :: jid:server(),
                  Filter :: filter(), IndexHintSQL :: string(), SUID :: escaped_message_id()) ->
                      non_neg_integer().
calc_before(Host, Filter, IndexHintSQL, SUID) ->
    {selected, [{BIndex}]} =
        mod_mam_utils:success_sql_query(
          Host,
          ["SELECT COUNT(*) FROM mam_message ",
           IndexHintSQL, Filter,
           " AND id < ", use_escaped_integer(SUID)]),
    mongoose_rdbms:result_to_integer(BIndex).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(Host :: jid:server(),
                 Filter :: filter(), IndexHintSQL :: string()) -> non_neg_integer().
calc_count(Host, Filter, IndexHintSQL) ->
    {selected, [{BCount}]} =
        mod_mam_utils:success_sql_query(
          Host,
          ["SELECT COUNT(*) FROM mam_message ",
           IndexHintSQL, Filter]),
    mongoose_rdbms:result_to_integer(BCount).


-spec prepare_filter(Host :: jid:server(), UserID :: mod_mam:archive_id(),
                     UserJID :: jid:jid(), Borders :: mod_mam:borders(),
                     Start :: mod_mam:unix_timestamp() | undefined,
                     End :: mod_mam:unix_timestamp() | undefined, WithJID :: jid:jid(),
                     SearchText :: binary() | undefined)
                    -> filter().
prepare_filter(Host, UserID, UserJID, Borders, Start, End, WithJID, SearchText) ->
    {SWithJID, SWithResource} =
        case WithJID of
            undefined -> {undefined, undefined};
            #jid{lresource = <<>>} ->
                {minify_and_escape_bare_jid(Host, UserJID, WithJID), undefined};
            #jid{lresource = WithLResource} ->
                {minify_and_escape_bare_jid(Host, UserJID, WithJID),
                 mongoose_rdbms:escape_string(WithLResource)}
        end,
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filter_sql(UserID, StartID2, EndID2, SWithJID, SWithResource, SearchText).


-spec prepare_filter_sql(UserID :: non_neg_integer(),
                         StartID :: mod_mam:message_id() | undefined,
                         EndID :: mod_mam:message_id() | undefined,
                         SWithJID :: escaped_jid() | undefined,
                         SWithResource :: escaped_resource() | undefined,
                         SearchText :: binary() | undefined) -> filter().
prepare_filter_sql(UserID, StartID, EndID, SWithJID, SWithResource, SearchText) ->
    ["WHERE user_id=", use_escaped_integer(escape_user_id(UserID)),
     case StartID of
         undefined -> "";
         _         -> [" AND id >= ", use_escaped_integer(escape_integer(StartID))]
     end,
     case EndID of
         undefined -> "";
         _         -> [" AND id <= ", use_escaped_integer(escape_integer(EndID))]
     end,
     case SWithJID of
         undefined -> "";
         _         -> [" AND remote_bare_jid = ", use_escaped_string(SWithJID)]
     end,
     case SWithResource of
         undefined -> "";
         _         -> [" AND remote_resource = ", use_escaped_string(SWithResource)]
     end,
     case SearchText of
         undefined -> "";
         _         -> prepare_search_filters(SearchText)
     end
    ].

%% Constructs a separate LIKE filter for each word.
%% SearchText example is "word1%word2%word3".
%% Order of words does not matter (they can go in any order).
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
                  Filter :: filter(), IndexHintSQL :: string(), PageSize :: non_neg_integer(),
                  TotalCount :: non_neg_integer(), RSM :: jlib:rsm_in()) -> non_neg_integer().
calc_offset(_LS, _F, _IH, _PS, _TC, #rsm_in{direction = undefined, index = Index})
  when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_LS, _F, _IH, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Host, F, IH, PS, _TC, #rsm_in{direction = before, id = ID})
  when is_integer(ID) ->
    SID = escape_message_id(ID),
    max(0, calc_before(Host, F, IH, SID) - PS);
calc_offset(Host, F, IH, _PS, _TC, #rsm_in{direction = aft, id = ID})
  when is_integer(ID) ->
    SID = escape_message_id(ID),
    calc_index(Host, F, IH, SID);
calc_offset(_LS, _F, _IH, _PS, _TC, _RSM) ->
    0.

escape_message_id(MessID) when is_integer(MessID) ->
    escape_integer(MessID).

escape_user_id(UserID) when is_integer(UserID) ->
    escape_integer(UserID).

%% @doc Strip resource, minify and escape JID.
minify_and_escape_bare_jid(Host, LocJID, JID) ->
    escape_string(jid_to_stored_binary(Host, LocJID, jid:to_bare(JID))).

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
