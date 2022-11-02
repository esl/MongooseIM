%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 16:59
%%%-------------------------------------------------------------------
-module(mod_inbox_rdbms).

-include("mod_inbox.hrl").
-include("mongoose_logger.hrl").

-define(DEFAULT_ORDER, <<"DESC">>).
-define(EXPRESSION_MAYBE_UNREAD_COUNT,
    "unread_count = CASE unread_count WHEN 0 THEN 1 ELSE unread_count END").
% -define(EXPRESSION_KEEP_ARCHIVE_BOX,
%         {expression, <<"box">>, <<"CASE WHEN inbox.box='archive' THEN inbox.box ELSE ? END">>}).
% -define(EXPRESSION_INCR_COUNT,
%         {expression, <<"unread_count">>, <<"inbox.unread_count+?">>}).
% -define(EXPRESSION_SET_COUNT,
%         {expression, <<"unread_count">>, <<"?">>}).

-type value() :: binary() | integer() | boolean().
% -type query_op() :: binary() | {assignment | expression, binary(), binary()}.
-record(update_prop, {name :: atom(),
                      fields = [] :: [atom()],
                      query = <<>> :: binary(),
                      values = [] :: [value()]}).
-type update_prop() :: #update_prop{}.

-record(lookup_inbox, {name :: atom(),
                       fields = [] :: [atom()],
                       from = <<"inbox">> :: binary(),
                       order = ?DEFAULT_ORDER :: binary(),
                       limit = {<<>>, <<>>} :: {iodata(), iodata()},
                       conditions = [<<"box <> 'bin">>] :: iodata(),
                       values = [] :: [value()]}).
-type lookup_inbox() :: #lookup_inbox{}.

-behaviour(mod_inbox_backend).

%% API
-export([get_inbox/4,
         init/2,
         set_inbox/6,
         set_inbox_incr_unread/5,
         reset_unread/4,
         empty_user_bin/4,
         empty_domain_bin/3,
         empty_global_bin/2,
         remove_inbox_row/2,
         remove_domain/2,
         clear_inbox/3,
         get_inbox_unread/2,
         get_full_entry/2,
         get_entry_properties/2,
         set_entry_properties/3]).
-export([check_result/1]).

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

%% TODO pools aren't multitenancy-ready yet
init(HostType, _Options) ->
    RowCond = <<"WHERE luser = ? AND lserver = ? AND remote_bare_jid = ?">>,
    mongoose_rdbms:prepare(inbox_select_entry, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"SELECT", (query_row())/binary, "FROM inbox ", RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_select_unread_count, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"SELECT unread_count FROM inbox ", RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_select_properties, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"SELECT box, muted_until, unread_count FROM inbox ", RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_reset_unread, inbox,
                           [luser, lserver, remote_bare_jid, timestamp],
                           <<"UPDATE inbox SET unread_count = 0 ",
                             RowCond/binary, " AND timestamp <= ?">>),
    mongoose_rdbms:prepare(inbox_reset_unread_msg, inbox,
                           [luser, lserver, remote_bare_jid, msg_id, timestamp],
                           <<"UPDATE inbox SET unread_count = 0 ", RowCond/binary,
                             " AND msg_id = ? AND timestamp <= ?">>),
    % removals
    mongoose_rdbms:prepare(inbox_clean_global_bin, inbox, [timestamp],
                           <<"DELETE FROM inbox WHERE box='bin' AND timestamp < ?">>),
    mongoose_rdbms:prepare(inbox_clean_domain_bin, inbox, [lserver, timestamp],
                           <<"DELETE FROM inbox WHERE",
                             " lserver = ? AND box='bin' AND timestamp < ?">>),
    mongoose_rdbms:prepare(inbox_clean_user_bin, inbox, [lserver, luser, timestamp],
                           <<"DELETE FROM inbox WHERE",
                             " lserver = ? AND luser = ? AND box='bin' AND timestamp < ?">>),
    mongoose_rdbms:prepare(inbox_delete_row, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"DELETE FROM inbox ", RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_delete, inbox,
                           [luser, lserver],
                           <<"DELETE FROM inbox WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(inbox_delete_domain, inbox,
                           [lserver], <<"DELETE FROM inbox WHERE lserver = ?">>),
    % upserts
    BoxQuery = <<"CASE WHEN ?='bin' THEN 'bin'",
                     " WHEN inbox.box='archive' THEN 'inbox'",
                     " ELSE inbox.box END">>,
    UniqueKeyFields = inbox_unique_keys(),
    InsertFields = inbox_insert_keys(),
    rdbms_queries:prepare_upsert(HostType, inbox_upsert, inbox,
                                 InsertFields,
                                 [<<"msg_id">>,
                                  {expression, <<"box">>, BoxQuery},
                                  <<"content">>,
                                  <<"unread_count">>,
                                  <<"timestamp">>],
                                 UniqueKeyFields, <<"timestamp">>),
    rdbms_queries:prepare_upsert(HostType, inbox_upsert_incr_unread, inbox,
                                 InsertFields,
                                 [<<"msg_id">>,
                                  {expression, <<"box">>, BoxQuery},
                                  <<"content">>,
                                  {expression, <<"unread_count">>, <<"inbox.unread_count + ?">>},
                                  <<"timestamp">>],
                                 UniqueKeyFields, <<"timestamp">>),
    % prepare_getters(),
    prepare_setters(),
    ok.

prepare_getters() ->
    [ prepare_get_inbox(
        choose_getter(#{order => Order, limit => Limit,
                        start => Start, 'end' => End,
                        hidden_read => Hidden, box => Box}))
      || Order <- [asc, desc],
         Limit <- [false, 0],
         Start <- [false, 0],
         End <- [false, 0],
         Hidden <- [false, true],
         Box <- [false, <<"all">>, <<"box">>] ].

-spec prepare_get_inbox(lookup_inbox()) -> {ok, atom()} | {error, already_exists}.
prepare_get_inbox(#lookup_inbox{name = QueryName, fields = Fields,
                                order = Order, limit = {MSLimit, Limit},
                                conditions = Conditions}) ->
    case mongoose_rdbms:prepared(QueryName) of
        false ->
            FinalQuery = ["SELECT ", MSLimit, query_row(),
                          " FROM inbox WHERE luser = ? AND lserver = ? ", Conditions,
                          " ORDER BY timestamp ", Order, " ", Limit],
            mongoose_rdbms:prepare(QueryName, inbox, Fields, FinalQuery);
        true ->
            {ok, QueryName}
    end.

prepare_setters() ->
    [ prepare_update_property(
        choose_setter(#{unread_count => Count, muted_until => Muted, box => Box}))
      || Count <- [false, 0, 1],
         Muted <- [false, 0],
         Box <- [false, <<"box">>] ].

-spec prepare_update_property(update_prop()) -> {ok, atom()} | {error, already_exists}.
prepare_update_property(#update_prop{name = QueryName, fields = Fields, query = Query}) ->
    case mongoose_rdbms:prepared(QueryName) of
        false ->
            RowCond = <<" WHERE luser = ? AND lserver = ? AND remote_bare_jid = ?">>,
            FinalQuery = [<<"UPDATE inbox SET ">>, Query, RowCond],
            mongoose_rdbms:prepare(QueryName, inbox, Fields, FinalQuery);
        true ->
            {ok, QueryName}
    end.

-spec inbox_insert_keys() -> [binary()].
inbox_insert_keys() ->
    [<<"luser">>, <<"lserver">>, <<"remote_bare_jid">>,
     <<"msg_id">>, <<"box">>, <<"content">>, <<"unread_count">>, <<"timestamp">>].

-spec inbox_unique_keys() -> [binary()].
inbox_unique_keys() ->
    [<<"luser">>, <<"lserver">>, <<"remote_bare_jid">>].

-spec get_inbox(HostType :: mongooseim:host_type(),
                LUser :: jid:luser(),
                LServer :: jid:lserver(),
                Params :: mod_inbox:get_inbox_params()) -> [mod_inbox:inbox_res()].
get_inbox(HostType, LUser, LServer, Params) ->
    case get_inbox_rdbms(HostType, LUser, LServer, Params) of
        {selected, []} ->
            [];
        {selected, Res} ->
            [decode_row(HostType, R) || R <- Res]
    end.

-spec get_inbox_unread(mongooseim:host_type(), mod_inbox:entry_key()) ->
    {ok, integer()}.
get_inbox_unread(HostType, {LUser, LServer, RemBareJID}) ->
    Res = execute_select_unread_count(HostType, LUser, LServer, RemBareJID),
    {ok, Val} = check_result(Res),
    %% We read unread_count value when the message is sent and is not yet in receiver inbox
    %% so we have to add +1
    {ok, Val + 1}.

-spec set_inbox(HostType, InboxEntryKey, Packet, Count, MsgId, Timestamp) ->
    mod_inbox:write_res() when
      HostType :: mongooseim:host_type(),
      InboxEntryKey :: mod_inbox:entry_key(),
      Packet :: exml:element(),
      Count :: integer(),
      MsgId :: binary(),
      Timestamp :: integer().
set_inbox(HostType, {LUser, LServer, LToBareJid}, Packet, Count, MsgId, Timestamp) ->
    Content = exml:to_binary(Packet),
    Unique = [LUser, LServer, LToBareJid],
    Update = [MsgId, <<"inbox">>, Content, Count, Timestamp],
    Insert = [LUser, LServer, LToBareJid, MsgId, <<"inbox">>, Content, Count, Timestamp],
    Res = rdbms_queries:execute_upsert(HostType, inbox_upsert, Insert, Update, Unique),
    %% MySQL returns 1 when an upsert is an insert
    %% and 2, when an upsert acts as update
    check_result_is_expected(Res, [1, 2]).

-spec empty_user_bin(HostType :: mongooseim:host_type(),
                     LServer :: jid:lserver(),
                     LUser :: jid:luser(),
                     TS :: integer()) -> non_neg_integer().
empty_user_bin(HostType, LServer, LUser, TS) ->
    {updated, BinN} = mongoose_rdbms:execute_successfully(
                        HostType, inbox_clean_user_bin, [LServer, LUser, TS]),
    mongoose_rdbms:result_to_integer(BinN).

-spec empty_domain_bin(HostType :: mongooseim:host_type(),
                       LServer :: jid:lserver(),
                       TS :: integer()) -> non_neg_integer().
empty_domain_bin(HostType, LServer, TS) ->
    {updated, BinN} = mongoose_rdbms:execute_successfully(
                        HostType, inbox_clean_domain_bin, [LServer, TS]),
    mongoose_rdbms:result_to_integer(BinN).

-spec empty_global_bin(HostType :: mongooseim:host_type(),
                       TS :: integer()) -> non_neg_integer().
empty_global_bin(HostType, TS) ->
    case mongoose_rdbms:execute(HostType, inbox_clean_global_bin, [TS]) of
        {updated, BinN} ->
            mongoose_rdbms:result_to_integer(BinN);
        {Error, Reason} ->
            ?LOG_WARNING(#{what => inbox_clean_global_bin_failed,
                           error => Error, reason => Reason}),
            0
    end.

-spec remove_inbox_row(HostType :: mongooseim:host_type(),
                       InboxEntryKey :: mod_inbox:entry_key()) -> mod_inbox:write_res().
remove_inbox_row(HostType, {LUser, LServer, LToBareJid}) ->
    Res = execute_delete(HostType, LUser, LServer, LToBareJid),
    check_result(Res).

-spec remove_domain(HostType :: mongooseim:host_type(),
                    LServer :: jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    execute_delete_domain(HostType, LServer),
    ok.

-spec set_inbox_incr_unread(
        mongooseim:host_type(), mod_inbox:entry_key(), exml:element(), binary(), integer()) ->
    mod_inbox:count_res().
set_inbox_incr_unread(HostType, Entry, Packet, MsgId, Timestamp) ->
    set_inbox_incr_unread(HostType, Entry, Packet, MsgId, Timestamp, 1).

-spec set_inbox_incr_unread(HostType :: mongooseim:host_type(),
                            InboxEntryKey :: mod_inbox:entry_key(),
                            Packet :: exml:element(),
                            MsgId :: binary(),
                            Timestamp :: integer(),
                            Incrs :: pos_integer()) -> mod_inbox:count_res().
set_inbox_incr_unread(HostType, {LUser, LServer, LToBareJid}, Packet, MsgId, Timestamp, Incrs) ->
    Content = exml:to_binary(Packet),
    Unique = [LUser, LServer, LToBareJid],
    Update = [MsgId, <<"inbox">>, Content, Incrs, Timestamp],
    Insert = [LUser, LServer, LToBareJid, MsgId, <<"inbox">>, Content, Incrs, Timestamp],
    Res = rdbms_queries:execute_upsert(HostType, inbox_upsert_incr_unread, Insert, Update, Unique),
    check_result(Res).

-spec reset_unread(HostType :: mongooseim:host_type(),
                   InboxEntryKey :: mod_inbox:entry_key(),
                   MsgId :: binary() | undefined,
                   TS :: integer()) -> mod_inbox:write_res().
reset_unread(HostType, {LUser, LServer, LToBareJid}, MsgId, TS) ->
    Res = execute_reset_unread(HostType, LUser, LServer, LToBareJid, MsgId, TS),
    check_result(Res).

-spec clear_inbox(HostType :: mongooseim:host_type(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver()) -> mod_inbox:write_res().
clear_inbox(HostType, LUser, LServer) ->
    Res = execute_delete(HostType, LUser, LServer),
    check_result(Res).

-spec get_full_entry(HostType :: mongooseim:host_type(),
                     InboxEntryKey :: mod_inbox:entry_key()) ->
    inbox_res() | nil().
get_full_entry(HostType, {LUser, LServer, RemBareJID}) ->
    case execute_select_full_entry(HostType, LUser, LServer, RemBareJID) of
        {selected, []} ->
            [];
        {selected, [Selected]} ->
            decode_row(HostType, Selected)
    end.

-spec get_entry_properties(HostType :: mongooseim:host_type(),
                           InboxEntryKey :: mod_inbox:entry_key()) ->
    entry_properties() | nil().
get_entry_properties(HostType, {LUser, LServer, RemBareJID}) ->
    case execute_select_properties(HostType, LUser, LServer, RemBareJID) of
        {selected, []} ->
            [];
        {selected, [Selected]} ->
            decode_properties(Selected)
    end.

-spec set_entry_properties(HostType :: mongooseim:host_type(),
                           InboxEntryKey :: mod_inbox:entry_key(),
                           entry_properties()) ->
    entry_properties() | {error, binary()}.
set_entry_properties(HostType, {LUser, LServer, RemBareJID}, Properties) ->
    case set_entry_properties_rdbms(HostType, LUser, LServer, RemBareJID, Properties) of
        {error, Msg} when is_list(Msg) ->
            {error, list_to_binary(Msg)};
        {error, Msg} ->
            {error, Msg};
        {updated, 0} ->
            {error, <<"item-not-found">>};
        {selected, [Result]} ->
            decode_properties(Result)
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec get_inbox_rdbms(HostType :: mongooseim:host_type(),
                      LUser :: jid:luser(),
                      LServer :: jid:lserver(),
                      Params :: mod_inbox:get_inbox_params()) ->
    mongoose_rdbms:query_result().
get_inbox_rdbms(HostType, LUser, LServer, Params) ->
    QueryName = lookup_query_name(Params),
    case mongoose_rdbms:prepared(QueryName) of
        false ->
            SQL = lookup_query(Params),
            Columns = lookup_query_columns(Params),
            mongoose_rdbms:prepare(QueryName, inbox, Columns, SQL);
        true ->
            ok
    end,
    Args = lookup_query_args(LServer, LUser, Params),
    mongoose_rdbms:execute_successfully(HostType, QueryName, Args).

set_entry_properties_rdbms(HostType, LUser, LServer, RemBareJID, Properties) ->
    #update_prop{name = QueryName, values = Args} = choose_setter(Properties),
    {atomic, TransactionResult} =
        mongoose_rdbms:sql_transaction(
          LServer,
          fun() -> set_entry_properties_t(HostType, QueryName, LUser, LServer, RemBareJID, Args) end),
    TransactionResult.

-spec set_entry_properties_t(
        mongooseim:host_type(), atom(), jid:luser(), jid:lserver(), jid:literal_jid(), [binary()]) ->
    mongoose_rdbms:query_result().
set_entry_properties_t(HostType, QueryName, LUser, LServer, RemBareJID, Args) ->
    RowCond = [LUser, LServer, RemBareJID],
    case mongoose_rdbms:execute_successfully(HostType, QueryName, Args ++ RowCond) of
        {updated, 1} ->
            execute_select_properties(HostType, LUser, LServer, RemBareJID);
        Other ->
            Other
    end.

%% Inbox lookup

-spec lookup_query(mod_inbox:get_inbox_params()) -> iolist().
lookup_query(#{order := Order} = Params) ->
    OrderSQL = order_to_sql(Order),
    {LimitSQL, MSLimitSQL}  = sql_and_where_limit(maps:get(limit, Params, undefined)),
    Conditions = [lookup_sql_condition(Key, maps:get(Key, Params, undefined))
                  || Key <- [start, 'end', hidden_read, box]],
    ["SELECT ", MSLimitSQL, query_row(),
     " FROM inbox WHERE luser = ? AND lserver = ?", Conditions,
     " ORDER BY timestamp ", OrderSQL, " ", LimitSQL].

-spec lookup_query_args(jid:lserver(), jid:luser(), mod_inbox:get_inbox_params()) -> list().
lookup_query_args(LServer, LUser, Params) ->
    Args = [LUser, LServer | [maps:get(Key, Params) || Key <- lookup_arg_keys(Params)]],
    case maps:get(limit, Params, undefined) of
        undefined -> Args;
        Limit -> rdbms_queries:add_limit_arg(Limit, Args)
    end.

-spec lookup_query_columns(mod_inbox:get_inbox_params()) -> [atom()].
lookup_query_columns(Params) ->
    Columns = [luser, lserver | lists:map(fun param_to_column/1, lookup_arg_keys(Params))],
    case maps:get(limit, Params, undefined) of
        undefined -> Columns;
        _ -> rdbms_queries:add_limit_arg(limit, Columns)
    end.

-spec lookup_arg_keys(mod_inbox:get_inbox_params()) -> [atom()].
lookup_arg_keys(Params) ->
    lists:filter(
      fun(box) -> maps:is_key(box, Params) andalso maps:get(box, Params, undefined) =/= <<"all">>;
         (Key) -> maps:is_key(Key, Params)
      end, [start, 'end', box]).

-spec lookup_query_name(mod_inbox:get_inbox_params()) -> atom().
lookup_query_name(Params) ->
    IDString = lists:flatmap(fun(Param) ->
                                     param_id(Param, maps:get(Param, Params, undefined))
                             end, lookup_param_keys()),
    list_to_atom("inbox_lookup" ++ IDString).

-spec lookup_param_keys() -> [atom()].
lookup_param_keys() ->
    [order, limit, start, 'end', hidden_read, box].

-spec param_to_column(atom()) -> atom().
param_to_column(start) -> timestamp;
param_to_column('end') -> timestamp;
param_to_column(box) -> box.

-spec param_id(Key :: atom(), Value :: any()) -> string().
param_id(box, undefined) -> "_no_bin";
param_id(box, <<"all">>) -> "";
param_id(box, _) -> "_box";
param_id(_, undefined) -> "";
param_id(order, desc) -> "_desc";
param_id(order, asc) -> "_asc";
param_id(limit, _) -> "_lim";
param_id(start, _) -> "_start";
param_id('end', _) -> "_end";
param_id(hidden_read, true) -> "_hr";
param_id(hidden_read, false) -> "".

-spec order_to_sql(Order :: asc | desc) -> binary().
order_to_sql(asc) -> <<"ASC">>;
order_to_sql(desc) -> <<"DESC">>.

-spec sql_and_where_limit(non_neg_integer() | undefined) -> {iolist(), iolist()}.
sql_and_where_limit(undefined) ->
    {"", ""};
sql_and_where_limit(_) ->
    rdbms_queries:get_db_specific_limits().

-spec lookup_sql_condition(Key :: atom(), Value :: any()) -> string().
lookup_sql_condition(start, Timestamp) when is_integer(Timestamp) ->
    " AND timestamp >= ?";
lookup_sql_condition('end', Timestamp) when is_integer(Timestamp) ->
    " AND timestamp <= ?";
lookup_sql_condition(hidden_read, true) ->
    " AND unread_count > 0";
lookup_sql_condition(box, undefined) ->
    " AND box <> 'bin'";
lookup_sql_condition(box, <<"all">>) ->
    "";
lookup_sql_condition(box, Val) when is_binary(Val) ->
    " AND box = ?";
lookup_sql_condition(_, _) ->
    "".

order_desc(#{hidden_read := false, box := false, limit := false, start := false, 'end' := false}) ->
    #lookup_inbox{name = inbox_lookup};

order_desc(#{hidden_read := true, box := false, limit := false, start := false, 'end' := false}) ->
    L0 = #lookup_inbox{name = inbox_lookup_hidden_read},
    L0#lookup_inbox{conditions = [<<"unread_count > 0 AND ">> | L0#lookup_inbox.conditions]};

order_desc(#{hidden_read := false, box := false, limit := false, start := false, 'end' := false}) ->
    L0 = #lookup_inbox{name = inbox_lookup_hidden_read},
    % L0#lookup_inbox{conditions = [<<"unread_count > 0 AND ">> | L0#lookup_inbox.conditions]};

    #lookup_inbox{name = inbox_lookup_hidden_read,
                  fields = [],
                  from = <<"(SELECT timestamp ORDER BY timestamp DESC WHERE remote_bare_jid = ?)">>,
                  order = <<"DESC">>,
                  limit = {<<>>, <<>>},
                  conditions = <<"unread_count > 0">>,
                  values = []}.


%% NOTE: the question marks in the `query' field need to match in the same order and number
%% with the function in the `value' field, which describe how to generate the corresponding data.
%% This is not checked statically or dynamically, but the proximity of the description should
%% make the definition easy to get right.
-spec choose_getter(map()) -> update_prop().
%% order desc
choose_getter(#{order := desc} = Params) ->
    order_desc(Params);

choose_getter(#{order := Order, hidden_read := Hidden, box := Box,
                limit := Limit, start := Start, 'end' := End}) ->
    #lookup_inbox{name = inbox_update_properties_noop}.


%% NOTE: the question marks in the `query' field need to match in the same order and number
%% with the function in the `value' field, which describe how to generate the corresponding data.
%% This is not checked statically or dynamically, but the proximity of the description should
%% make the definition easy to get right.
-spec choose_setter(mod_inbox_entries:properties()) -> update_prop().
choose_setter(#{unread_count := false} = Properties) ->
    unread_count_false(Properties);
choose_setter(#{unread_count := 0} = Properties) ->
    unread_count_0(Properties);
choose_setter(#{unread_count := 1} = Properties) ->
    unread_count_1(Properties).

unread_count_false(#{muted_until := false, box := false}) ->
    #update_prop{name = inbox_update_properties_noop};
unread_count_false(#{muted_until := MutedUntil, box := false})
  when is_integer(MutedUntil) ->
    #update_prop{name = inbox_update_properties_muted_until,
                 fields = [muted_until],
                 query = <<"muted_until = ?">>,
                 values = [MutedUntil]};
unread_count_false(#{muted_until := false, box := Box})
  when is_binary(Box) ->
    #update_prop{name = inbox_update_properties_box,
                 fields = [box],
                 query = <<"box = ?">>,
                 values = [Box]};
unread_count_false(#{muted_until := MutedUntil, box := Box})
  when is_integer(MutedUntil), is_binary(Box) ->
    #update_prop{name = inbox_update_properties_muted_until_box,
                 fields = [muted_until, box],
                 query = <<"muted_until = ?, box = ?">>,
                 values = [MutedUntil, Box]}.

unread_count_0(#{muted_until := false, box := false}) ->
    #update_prop{name = inbox_update_properties_reset_unread_count,
                 query = <<"unread_count = 0">>};
unread_count_0(#{muted_until := MutedUntil, box := false})
  when is_integer(MutedUntil) ->
    #update_prop{name = inbox_update_properties_reset_unread_count_muted_until,
                 fields = [muted_until],
                 query = <<"unread_count = 0, muted_until = ?">>,
                 values = [MutedUntil]};
unread_count_0(#{muted_until := false, box := Box})
  when is_binary(Box) ->
    #update_prop{name = inbox_update_properties_reset_unread_count_box,
                 fields = [box],
                 query = <<"unread_count = 0, box = ?">>,
                 values = [Box]};
unread_count_0(#{muted_until := MutedUntil, box := Box})
  when is_integer(MutedUntil), is_binary(Box) ->
    #update_prop{name = inbox_update_properties_reset_unread_count_muted_until_box,
                 fields = [muted_until, box],
                 query = <<"unread_count = 0, muted_until = ?, box = ?">>,
                 values = [MutedUntil, Box]}.

unread_count_1(#{muted_until := false, box := false}) ->
    #update_prop{name = inbox_update_properties_incr_unread_count,
                 query = <<?EXPRESSION_MAYBE_UNREAD_COUNT>>};
unread_count_1(#{muted_until := MutedUntil, box := false})
  when is_integer(MutedUntil) ->
    #update_prop{name = inbox_update_properties_incr_unread_count_muted_until,
                 fields = [muted_until],
                 query = <<?EXPRESSION_MAYBE_UNREAD_COUNT, ", muted_until = ?">>,
                 values = [MutedUntil]};
unread_count_1(#{muted_until := false, box := Box})
  when is_binary(Box) ->
    #update_prop{name = inbox_update_properties_incr_unread_count_box,
                 fields = [box],
                 query = <<?EXPRESSION_MAYBE_UNREAD_COUNT, ", box = ?">>,
                 values = [Box]};
unread_count_1(#{muted_until := MutedUntil, box := Box})
  when is_integer(MutedUntil), is_binary(Box) ->
    #update_prop{name = inbox_update_properties_incr_unread_count_muted_until_box,
                 fields = [muted_until, box],
                 query = <<?EXPRESSION_MAYBE_UNREAD_COUNT, ", muted_until = ?, box = ?">>,
                 values = [MutedUntil, Box]}.

%% Query execution

-spec execute_select_unread_count(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_select_unread_count(HostType, LUser, LServer, RemBareJID) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_select_unread_count,
                                        [LUser, LServer, RemBareJID]).

-spec execute_select_full_entry(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_select_full_entry(HostType, LUser, LServer, RemBareJID) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_select_entry,
                                        [LUser, LServer, RemBareJID]).

-spec execute_select_properties(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_select_properties(HostType, LUser, LServer, RemBareJID) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_select_properties,
                                        [LUser, LServer, RemBareJID]).

-spec execute_reset_unread(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid(), binary() | undefined, integer()) ->
          mongoose_rdbms:query_result().
execute_reset_unread(HostType, LUser, LServer, RemBareJID, undefined, TS) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_reset_unread,
                                        [LUser, LServer, RemBareJID, TS]);
execute_reset_unread(HostType, LUser, LServer, RemBareJID, MsgId, TS) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_reset_unread_msg,
                                        [LUser, LServer, RemBareJID, MsgId, TS]).

-spec execute_delete(mongooseim:host_type(),
                     jid:luser(), jid:lserver(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_delete(HostType, LUser, LServer, RemBareJID) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_delete_row, [LUser, LServer, RemBareJID]).

-spec execute_delete(mongooseim:host_type(), jid:lserver(), jid:luser()) -> mongoose_rdbms:query_result().
execute_delete(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_delete, [LUser, LServer]).

-spec execute_delete_domain(HostType :: mongooseim:host_type(),
                            LServer :: jid:lserver()) ->
    mongoose_rdbms:query_result().
execute_delete_domain(HostType, LServer) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_delete_domain, [LServer]).

%% DB result processing
-type db_return() :: {RemBareJID :: jid:luser(),
                      MsgId :: id(),
                      MsgContents :: binary(),
                      Timestamp :: non_neg_integer() | binary(),
                      Count :: non_neg_integer() | binary(),
                      MutedUntil :: binary(),
                      Box :: binary()}.

query_row() ->
    <<" remote_bare_jid, msg_id, box, content, timestamp, muted_until, unread_count ">>.

-spec decode_row(mongooseim:host_type(), db_return()) -> inbox_res().
decode_row(HostType, {RemBareJID, MsgId, Box, Content, Timestamp, MutedUntil, Count}) ->
    {ok, Parsed} = exml:parse(mongoose_rdbms:unescape_binary(HostType, Content)),
    BCount = mongoose_rdbms:result_to_integer(Count),
    NumericTimestamp = mongoose_rdbms:result_to_integer(Timestamp),
    NumericMutedUntil = mongoose_rdbms:result_to_integer(MutedUntil),
    #{remote_jid => RemBareJID,
      msg_id => MsgId,
      box => Box,
      msg => Parsed,
      timestamp => NumericTimestamp,
      muted_until => NumericMutedUntil,
      unread_count => BCount}.

-spec decode_properties({_, _, _}) -> entry_properties().
decode_properties({Box, BMutedUntil, BCount}) ->
    Count = mongoose_rdbms:result_to_integer(BCount),
    MutedUntil = mongoose_rdbms:result_to_integer(BMutedUntil),
    #{box => Box,
      unread_count => Count,
      muted_until => MutedUntil}.

-spec check_result_is_expected(_, list()) -> mod_inbox:write_res().
check_result_is_expected({updated, Val}, ValList) when is_list(ValList) ->
    case lists:member(Val, ValList) of
        true -> ok;
        _ -> {error, {expected_does_not_match, Val, ValList}}
    end;
check_result_is_expected(Result, _) ->
    {error, {bad_result, Result}}.

-spec check_result(_) -> mod_inbox:count_res().
check_result({selected, []}) ->
    {ok, 0};
check_result({selected, [{Val}]}) ->
    parse_result(Val);
check_result({updated, _, [{Val}]}) ->
    parse_result(Val);
check_result({updated, _}) ->
    ok;
check_result(Result) ->
    {error, {bad_result, Result}}.

parse_result(Value) when is_integer(Value) ->
    {ok, Value};
parse_result(Value) when is_binary(Value) ->
    {ok, binary_to_integer(Value)};
parse_result(null) ->
    {ok, 0};
parse_result(Value) ->
    {error, {unknown_result_value_type, Value}}.
