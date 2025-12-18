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
init(HostType, Opts) ->
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
    prepare_remove_domain(Opts),
    % upserts
    BoxQuery = <<"CASE WHEN ?='bin' THEN 'bin'",
                     " WHEN inbox.box='archive' THEN 'inbox'",
                     " ELSE inbox.box END">>,
    UniqueKeyFields = [<<"luser">>, <<"lserver">>, <<"remote_bare_jid">>],
    InsertFields = UniqueKeyFields ++ [<<"msg_id">>, <<"box">>, <<"content">>, <<"unread_count">>, <<"timestamp">>],
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
    ok.

prepare_remove_domain(#{delete_domain_limit := infinity}) ->
    mongoose_rdbms:prepare(
      inbox_delete_domain, inbox, [lserver], <<"DELETE FROM inbox WHERE lserver = ?">>);
prepare_remove_domain(#{delete_domain_limit := Limit}) ->
    LimitSQL = rdbms_queries:limit(Limit),
    ServerTable = <<"(SELECT * FROM ",
                        "(SELECT lserver, luser, remote_bare_jid FROM inbox",
                        " WHERE lserver = ? ", LimitSQL/binary, ") AS T)">>,
    mongoose_rdbms:prepare(
      inbox_incr_delete_domain, inbox, [lserver],
      <<"DELETE FROM inbox WHERE (lserver, luser, remote_bare_jid) IN ", ServerTable/binary>>).

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
    Update = [MsgId, <<"inbox">>, Content, Count, Timestamp],
    Insert = [LUser, LServer, LToBareJid, MsgId, <<"inbox">>, Content, Count, Timestamp],
    Res = rdbms_queries:execute_upsert(HostType, inbox_upsert, Insert, Update),
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
                    LServer :: jid:lserver()) -> term().
remove_domain(HostType, LServer) ->
    DeleteDomainLimit = gen_mod:get_module_opt(HostType, mod_inbox, delete_domain_limit),
    execute_delete_domain(HostType, LServer, DeleteDomainLimit).

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
    Update = [MsgId, <<"inbox">>, Content, Incrs, Timestamp],
    Insert = [LUser, LServer, LToBareJid, MsgId, <<"inbox">>, Content, Incrs, Timestamp],
    Res = rdbms_queries:execute_upsert(HostType, inbox_upsert_incr_unread, Insert, Update),
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
    QueryName = update_query_name(Properties),
    case mongoose_rdbms:prepared(QueryName) of
        false ->
            SQL = update_properties_query(Properties),
            Columns = update_query_columns(Properties),
            mongoose_rdbms:prepare(QueryName, inbox, Columns, SQL);
        true ->
            ok
    end,
    {atomic, TransactionResult} =
        mongoose_rdbms:sql_transaction(
          LServer,
          fun() -> set_entry_properties_t(HostType, QueryName, LUser, LServer, RemBareJID, Properties) end),
    TransactionResult.

-spec set_entry_properties_t(mongooseim:host_type(), atom(), jid:luser(), jid:lserver(), jid:literal_jid(),
                             entry_properties()) ->
          mongoose_rdbms:query_result().
set_entry_properties_t(HostType, QueryName, LUser, LServer, RemBareJID, Properties) ->
    Args = update_query_args(LUser, LServer, RemBareJID, Properties),
    case mongoose_rdbms:execute_successfully(HostType, QueryName, Args) of
        {updated, 1} ->
            execute_select_properties(HostType, LUser, LServer, RemBareJID);
        Other ->
            Other
    end.

%% Inbox lookup

-spec lookup_query(mod_inbox:get_inbox_params()) -> iolist().
lookup_query(#{order := Order} = Params) ->
    OrderSQL = order_to_sql(Order),
    LimitSQL = sql_and_where_limit(maps:get(limit, Params, undefined)),
    Conditions = [lookup_sql_condition(Key, maps:get(Key, Params, undefined))
                  || Key <- [start, 'end', hidden_read, box]],
    ["SELECT ", query_row(),
     " FROM inbox WHERE luser = ? AND lserver = ?", Conditions,
     " ORDER BY timestamp ", OrderSQL, LimitSQL].

-spec lookup_query_args(jid:lserver(), jid:luser(), mod_inbox:get_inbox_params()) -> list().
lookup_query_args(LServer, LUser, Params) ->
    Args = [LUser, LServer | [maps:get(Key, Params) || Key <- lookup_arg_keys(Params)]],
    case maps:get(limit, Params, undefined) of
        undefined -> Args;
        Limit -> Args ++ [Limit]
    end.

-spec lookup_query_columns(mod_inbox:get_inbox_params()) -> [atom()].
lookup_query_columns(Params) ->
    Columns = [luser, lserver | lists:map(fun param_to_column/1, lookup_arg_keys(Params))],
    case maps:get(limit, Params, undefined) of
        undefined -> Columns;
        _ -> Columns ++ [limit]
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

-spec sql_and_where_limit(non_neg_integer() | undefined) -> binary().
sql_and_where_limit(undefined) ->
    <<>>;
sql_and_where_limit(_) ->
    rdbms_queries:limit().

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

%% Property update

update_properties_query(Properties) ->
    KVs = [{Key, maps:get(Key, Properties, undefined)} || Key <- property_keys()],
    Parts = [update_sql_part(Key, Value) || {Key, Value} <- KVs, Value =/= undefined],
    ["UPDATE inbox SET ", string:join(Parts, ", "),
     " WHERE luser = ? AND lserver = ? AND remote_bare_jid = ?"].

update_query_args(LUser, LServer, RemBareJID, Properties) ->
    [maps:get(Key, Properties) || Key <- update_arg_keys(Properties)] ++
        [LUser, LServer, RemBareJID].

update_query_columns(Properties) ->
    update_arg_keys(Properties) ++ [luser, lserver, remote_bare_jid].

update_arg_keys(Properties) ->
    lists:filter(fun(Key) -> maps:is_key(Key, Properties) end, [box, muted_until]).

update_query_name(Properties) ->
    IDString = lists:flatmap(fun(Prop) ->
                                     property_id(Prop, maps:get(Prop, Properties, undefined))
                             end, property_keys()),
    list_to_atom("inbox_update_properties" ++ IDString).

property_keys() ->
    [unread_count, box, muted_until].

-spec property_id(Key :: atom(), Value :: any()) -> string().
property_id(_, undefined) -> "";
property_id(unread_count, 0) -> "_read";
property_id(unread_count, 1) -> "_unread";
property_id(box, _) -> "_box";
property_id(muted_until, _) -> "_muted".

-spec update_sql_part(Key :: atom(), Value :: any()) -> string().
update_sql_part(unread_count, 0) ->
    "unread_count = 0";
update_sql_part(unread_count, 1) ->
    "unread_count = CASE unread_count WHEN 0 THEN 1 ELSE unread_count END";
update_sql_part(box, Val) when is_binary(Val) ->
    "box = ?";
update_sql_part(muted_until, Val) when is_integer(Val) ->
    "muted_until = ?".

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

-spec execute_delete_domain(mongooseim:host_type(), jid:lserver(), infinity | non_neg_integer()) ->
    mongoose_rdbms:query_result().
execute_delete_domain(HostType, LServer, infinity) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_delete_domain, [LServer]);
execute_delete_domain(HostType, LServer, Limit) ->
    mod_mam_utils:incremental_delete_domain(HostType, LServer, Limit, [inbox_incr_delete_domain], 0).

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
      unread_count => BCount,
      extra => []}.

-spec decode_properties({_, _, _}) -> entry_properties().
decode_properties({Box, BMutedUntil, BCount}) ->
    Count = mongoose_rdbms:result_to_integer(BCount),
    MutedUntil = mongoose_rdbms:result_to_integer(BMutedUntil),
    #{box => Box,
      unread_count => Count,
      muted_until => MutedUntil,
      extra => []}.

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
