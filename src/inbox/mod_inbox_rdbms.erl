%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 16:59
%%%-------------------------------------------------------------------
-module(mod_inbox_rdbms).

-include("mod_inbox.hrl").

-behaviour(mod_inbox_backend).

%% API
-export([get_inbox/4,
         init/2,
         set_inbox/6,
         set_inbox_incr_unread/5,
         reset_unread/3,
         remove_inbox_row/2,
         remove_domain/2,
         clear_inbox/3,
         get_inbox_unread/2,
         get_entry_properties/2,
         set_entry_properties/3]).

-type archived() :: binary().
-type muted_until() :: binary().
-type msg_content() :: binary().
-type db_return() :: {jid:luser(),
                      msg_content(),
                      count_bin(),
                      non_neg_integer() | binary(),
                      archived(),
                      muted_until()}.

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

%% TODO pools aren't multitenancy-ready yet
init(HostType, _Options) ->
    RowCond = <<"WHERE luser = ? AND lserver = ? AND remote_bare_jid = ?">>,
    mongoose_rdbms:prepare(inbox_select_unread_count, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"SELECT unread_count FROM inbox ",
                             RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_select_properties, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"SELECT archive, unread_count, muted_until FROM inbox ",
                             RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_reset_unread, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"UPDATE inbox SET unread_count = 0 ", RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_reset_unread_msg, inbox,
                           [luser, lserver, remote_bare_jid, msg_id],
                           <<"UPDATE inbox SET unread_count = 0 ", RowCond/binary,
                             " AND msg_id = ?">>),
    mongoose_rdbms:prepare(inbox_delete_row, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"DELETE FROM inbox ", RowCond/binary>>),
    mongoose_rdbms:prepare(inbox_delete, inbox,
                           [luser, lserver],
                           <<"DELETE FROM inbox WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(inbox_delete_domain, inbox,
                           [lserver], <<"DELETE FROM inbox WHERE lserver = ?">>),
    UniqueKeyFields = [<<"luser">>, <<"lserver">>, <<"remote_bare_jid">>],
    InsertFields =
        UniqueKeyFields ++ [<<"content">>, <<"unread_count">>, <<"msg_id">>, <<"timestamp">>],
    rdbms_queries:prepare_upsert(HostType, inbox_upsert, inbox,
                                 InsertFields,
                                 [<<"content">>, <<"unread_count">>,
                                  <<"msg_id">>, <<"timestamp">>, <<"archive">>],
                                 UniqueKeyFields),
    rdbms_queries:prepare_upsert(HostType, inbox_upsert_incr_unread, inbox,
                                 InsertFields,
                                 [<<"content">>, <<"msg_id">>, <<"timestamp">>, <<"archive">>,
                                  {<<"unread_count">>, <<"unread_count = inbox.unread_count + ?">>}],
                                 UniqueKeyFields),
    ok.

-spec get_inbox(HostType :: mongooseim:host_type(),
                LUser :: jid:luser(),
                LServer :: jid:lserver(),
                Params :: mod_inbox:get_inbox_params()) -> get_inbox_res().
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

-spec set_inbox(HostType, InboxEntryKey, Content, Count, MsgId, Timestamp) ->
    mod_inbox:write_res() when
      HostType :: mongooseim:host_type(),
      InboxEntryKey :: mod_inbox:entry_key(),
      Content :: binary(),
      Count :: integer(),
      MsgId :: binary(),
      Timestamp :: integer().
set_inbox(HostType, {LUser, LServer, ToBareJid}, Content, Count, MsgId, Timestamp) ->
    LToBareJid = jid:nameprep(ToBareJid),
    InsertParams = [LUser, LServer, LToBareJid,
                    Content, Count, MsgId, Timestamp],
    UpdateParams = [Content, Count, MsgId, Timestamp, false],
    UniqueKeyValues  = [LUser, LServer, LToBareJid],
    Res = rdbms_queries:execute_upsert(HostType, inbox_upsert,
                                       InsertParams, UpdateParams, UniqueKeyValues),
    %% MySQL returns 1 when an upsert is an insert
    %% and 2, when an upsert acts as update
    check_result_is_expected(Res, [1, 2]).

-spec remove_inbox_row(HostType :: mongooseim:host_type(),
                       InboxEntryKey :: mod_inbox:entry_key()) -> mod_inbox:write_res().
remove_inbox_row(HostType, {LUser, LServer, ToBareJid}) ->
    LToBareJid = jid:nameprep(ToBareJid),
    Res = execute_delete(HostType, LUser, LServer, LToBareJid),
    check_result(Res).

-spec remove_domain(HostType :: mongooseim:host_type(),
                    LServer :: jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    execute_delete_domain(HostType, LServer),
    ok.

-spec set_inbox_incr_unread(
        mongooseim:host_type(), mod_inbox:entry_key(), binary(), binary(), integer()) ->
    mod_inbox:count_res().
set_inbox_incr_unread(HostType, Entry, Content, MsgId, Timestamp) ->
    set_inbox_incr_unread(HostType, Entry, Content, MsgId, Timestamp, 1).

-spec set_inbox_incr_unread(HostType :: mongooseim:host_type(),
                            InboxEntryKey :: mod_inbox:entry_key(),
                            Content :: binary(),
                            MsgId :: binary(),
                            Timestamp :: integer(),
                            Incrs :: pos_integer()) -> mod_inbox:count_res().
set_inbox_incr_unread(HostType, {LUser, LServer, ToBareJid}, Content, MsgId, Timestamp, Incrs) ->
    LToBareJid = jid:nameprep(ToBareJid),
    InsertParams = [LUser, LServer, LToBareJid, Content, Incrs, MsgId, Timestamp],
    UpdateParams = [Content, MsgId, Timestamp, false, Incrs],
    UniqueKeyValues  = [LUser, LServer, LToBareJid],
    Res = rdbms_queries:execute_upsert(HostType, inbox_upsert_incr_unread,
                                       InsertParams, UpdateParams, UniqueKeyValues),
    check_result(Res).

-spec reset_unread(HosType :: mongooseim:host_type(),
                   InboxEntryKey :: mod_inbox:entry_key(),
                   MsgId :: binary() | undefined) -> mod_inbox:write_res().
reset_unread(HostType, {LUser, LServer, ToBareJid}, MsgId) ->
    LToBareJid = jid:nameprep(ToBareJid),
    Res = execute_reset_unread(HostType, LUser, LServer, LToBareJid, MsgId),
    check_result(Res).

-spec clear_inbox(HostType :: mongooseim:host_type(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver()) -> mod_inbox:write_res().
clear_inbox(HostType, LUser, LServer) ->
    Res = execute_delete(HostType, LUser, LServer),
    check_result(Res).

-spec get_entry_properties(HosType :: mongooseim:host_type(),
                           InboxEntryKey :: mod_inbox:entry_key()) ->
    entry_properties() | nil().
get_entry_properties(HostType, {LUser, LServer, RemBareJID}) ->
    case execute_select_properties(HostType, LUser, LServer, RemBareJID) of
        {selected, []} ->
            [];
        {selected, [Selected]} ->
            decode_entries(Selected)
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
            decode_entries(Result)
    end.

decode_entries({BArchive, BCount, BMutedUntil}) ->
    Archive = mongoose_rdbms:to_bool(BArchive),
    Count = mongoose_rdbms:result_to_integer(BCount),
    MutedUntil = mongoose_rdbms:result_to_integer(BMutedUntil),
    #{archive => Archive,
      unread_count => Count,
      muted_until => MutedUntil}.

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
    {LimitSQL, MSLimitSQL}  = sql_and_where_limit(maps:get(limit, Params, undefined)),
    Conditions = [lookup_sql_condition(Key, maps:get(Key, Params, undefined)) ||
                     Key <- [start, 'end', hidden_read, archive]],
    ["SELECT ", MSLimitSQL,
     " remote_bare_jid, content, unread_count, timestamp, archive, muted_until "
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
    lists:filter(fun(Key) -> maps:is_key(Key, Params) end, [start, 'end', archive]).

-spec lookup_query_name(mod_inbox:get_inbox_params()) -> atom().
lookup_query_name(Params) ->
    IDString = lists:flatmap(fun(Param) ->
                                     param_id(Param, maps:get(Param, Params, undefined))
                             end, lookup_param_keys()),
    list_to_atom("inbox_lookup" ++ IDString).

-spec lookup_param_keys() -> [atom()].
lookup_param_keys() ->
    [order, limit, start, 'end', hidden_read, archive].

-spec param_to_column(atom()) -> atom().
param_to_column(start) -> timestamp;
param_to_column('end') -> timestamp;
param_to_column(archive) -> archive.

-spec param_id(Key :: atom(), Value :: any()) -> string().
param_id(_, undefined) -> "";
param_id(order, desc) -> "_desc";
param_id(order, asc) -> "_asc";
param_id(limit, _) -> "_lim";
param_id(start, _) -> "_start";
param_id('end', _) -> "_end";
param_id(hidden_read, true) -> "_hr";
param_id(hidden_read, false) -> "";
param_id(archive, _) -> "_arch".

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
lookup_sql_condition(archive, Val) when is_boolean(Val) ->
    " AND archive = ?";
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
    lists:filter(fun(Key) -> maps:is_key(Key, Properties) end, [archive, muted_until]).

update_query_name(Properties) ->
    IDString = lists:flatmap(fun(Prop) ->
                                     property_id(Prop, maps:get(Prop, Properties, undefined))
                             end, property_keys()),
    list_to_atom("inbox_update_properties" ++ IDString).

property_keys() ->
    [unread_count, archive, muted_until].

-spec property_id(Key :: atom(), Value :: any()) -> string().
property_id(_, undefined) -> "";
property_id(unread_count, 0) -> "_read";
property_id(unread_count, 1) -> "_unread";
property_id(archive, _) -> "_arch";
property_id(muted_until, _) -> "_muted".

-spec update_sql_part(Key :: atom(), Value :: any()) -> string().
update_sql_part(unread_count, 0) ->
    "unread_count = 0";
update_sql_part(unread_count, 1) ->
    "unread_count = CASE unread_count WHEN 0 THEN 1 ELSE unread_count END";
update_sql_part(archive, Val) when is_boolean(Val) ->
    "archive = ?";
update_sql_part(muted_until, Val) when is_integer(Val) ->
    "muted_until = ?".

%% Query execution

-spec execute_select_unread_count(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_select_unread_count(HostType, LUser, LServer, RemBareJID) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_select_unread_count,
                                        [LUser, LServer, RemBareJID]).

-spec execute_select_properties(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_select_properties(HostType, LUser, LServer, RemBareJID) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_select_properties,
                                        [LUser, LServer, RemBareJID]).

-spec execute_reset_unread(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid(), binary() | undefined) ->
          mongoose_rdbms:query_result().
execute_reset_unread(HostType, LUser, LServer, RemBareJID, undefined) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_reset_unread,
                                        [LUser, LServer, RemBareJID]);
execute_reset_unread(HostType, LUser, LServer, RemBareJID, MsgId) ->
    mongoose_rdbms:execute_successfully(HostType, inbox_reset_unread_msg,
                                        [LUser, LServer, RemBareJID, MsgId]).

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

%% Result processing

-spec decode_row(mongooseim:host_type(), db_return()) -> inbox_res().
decode_row(HostType, {Username, Content, Count, Timestamp, Archive, MutedUntil}) ->
    Data = mongoose_rdbms:unescape_binary(HostType, Content),
    BCount = mongoose_rdbms:result_to_integer(Count),
    NumericTimestamp = mongoose_rdbms:result_to_integer(Timestamp),
    BoolArchive = mongoose_rdbms:to_bool(Archive),
    NumericMutedUntil = mongoose_rdbms:result_to_integer(MutedUntil),
    #{remote_jid => Username,
      msg => Data,
      unread_count => BCount,
      timestamp => NumericTimestamp,
      archive => BoolArchive,
      muted_until => NumericMutedUntil}.

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
