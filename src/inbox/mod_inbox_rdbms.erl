%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 16:59
%%%-------------------------------------------------------------------
-module(mod_inbox_rdbms).
-author("ludwikbukowski").
-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_inbox.hrl").
-include("mongoose_logger.hrl").

-behaviour(mod_inbox).

%% API
-export([get_inbox/3,
         init/2,
         set_inbox/7,
         set_inbox_incr_unread/6,
         reset_unread/4,
         remove_inbox_row/3,
         clear_inbox/1,
         clear_inbox/2,
         get_inbox_unread/3,
         get_entry_properties/3,
         set_entry_properties/4]).

%% For specific backends
-export([esc_string/1, esc_int/1]).

-type archived() :: binary().
-type muted_until() :: binary().
-type msg_content() :: binary().
-type db_return() :: {username(),
                      msg_content(),
                      count_bin(),
                      non_neg_integer() | binary(),
                      archived(),
                      muted_until()}.

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

init(VHost, _Options) ->
    UniqueKeyFields = [<<"luser">>, <<"lserver">>, <<"remote_bare_jid">>],
    InsertFields =
        UniqueKeyFields ++ [<<"content">>, <<"unread_count">>, <<"msg_id">>, <<"timestamp">>],
    rdbms_queries:prepare_upsert(VHost, inbox_upsert, inbox,
                                 InsertFields,
                                 [<<"content">>, <<"unread_count">>,
                                  <<"msg_id">>, <<"timestamp">>, <<"archive">>],
                                 UniqueKeyFields),
    rdbms_queries:prepare_upsert(VHost, inbox_upsert_incr_unread, inbox,
                                 InsertFields,
                                 [<<"content">>, <<"unread_count = inbox.unread_count + 1">>,
                                  <<"msg_id">>, <<"timestamp">>, <<"archive">>],
                                 UniqueKeyFields),
    ok.

-spec get_inbox(LUsername :: jid:luser(),
                LServer :: jid:lserver(),
                Params :: mod_inbox:get_inbox_params()) -> get_inbox_res().
get_inbox(LUsername, LServer, Params) ->
    case get_inbox_rdbms(LUsername, LServer, Params) of
        {selected, []} ->
            [];
        {selected, Res} ->
            [decode_row(LServer, R) || R <- Res]
    end.


-spec get_inbox_rdbms(LUser :: jid:luser(),
                      LServer :: jid:lserver(),
                      Params :: mod_inbox:get_inbox_params()) ->
    mongoose_rdbms:query_result().
get_inbox_rdbms(LUser, LServer, Params) ->
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
    mongoose_rdbms:execute_successfully(LServer, QueryName, Args).

-spec get_inbox_unread(jid:luser(), jid:lserver(), jid:literal_jid()) -> {ok, integer()}.
get_inbox_unread(LUSername, LServer, RemBareJIDBin) ->
    Query = ["SELECT unread_count FROM inbox "
             "WHERE luser=", esc_string(LUSername),
                 "AND lserver=", esc_string(LServer),
                 "AND remote_bare_jid=", esc_string(RemBareJIDBin),
             ";"],
    Res = sql_query(LServer, Query),
    {ok, Val} = check_result(Res),
    %% We read unread_count value when the message is sent and is not yet in receiver inbox
    %% so we have to add +1
    {ok, Val + 1}.

-spec set_inbox(LUsername, LServer, ToBareJid, Content, Count, MsgId, Timestamp) ->
    inbox_write_res() when
      LUsername :: jid:luser(),
      LServer :: jid:lserver(),
      ToBareJid :: jid:literal_jid(),
      Content :: binary(),
      Count :: integer(),
      MsgId :: binary(),
      Timestamp :: integer().
set_inbox(LUsername, LServer, ToBareJid, Content, Count, MsgId, Timestamp) ->
    LToBareJid = jid:nameprep(ToBareJid),
    InsertParams = [LUsername, LServer, LToBareJid,
                    Content, Count, MsgId, Timestamp],
    UpdateParams = [Content, Count, MsgId, Timestamp, false],
    UniqueKeyValues  = [LUsername, LServer, LToBareJid],
    Res = rdbms_queries:execute_upsert(LServer, inbox_upsert,
                                       InsertParams, UpdateParams, UniqueKeyValues),
    %% MySQL returns 1 when an upsert is an insert
    %% and 2, when an upsert acts as update
    ok = check_result(Res, [1, 2]).

-spec remove_inbox_row(LUsername :: jid:luser(),
                       LServer :: jid:lserver(),
                       ToBareJid :: jid:literal_jid()) -> ok.
remove_inbox_row(LUsername, LServer, ToBareJid) ->
    LToBareJid = jid:nameprep(ToBareJid),
    Res = remove_inbox_rdbms(LUsername, LServer, LToBareJid),
    check_result(Res).

-spec remove_inbox_rdbms(Username :: jid:luser(),
                         Server :: jid:lserver(),
                         ToBareJid :: jid:literal_jid()) -> mongoose_rdbms:query_result().
remove_inbox_rdbms(Username, Server, ToBareJid) ->
    sql_query(Server, ["delete from inbox where luser=",
        esc_string(Username), " and lserver=", esc_string(Server),
        " and remote_bare_jid=",
        esc_string(ToBareJid), ";"]).

%% This function was not refatorected to use the generic upsert helper
%% becase this helper doesn't support parametrized queries for incremental change
-spec set_inbox_incr_unread(LUsername :: jid:luser(),
                            LServer :: jid:lserver(),
                            ToBareJid :: jid:literal_jid(),
                            Content :: binary(),
                            MsgId :: binary(),
                            Timestamp :: integer()) -> ok | {ok, integer()}.
set_inbox_incr_unread(LUsername, LServer, ToBareJid, Content, MsgId, Timestamp) ->
    LToBareJid = jid:nameprep(ToBareJid),
    InsertParams = [LUsername, LServer, LToBareJid, Content, 1, MsgId, Timestamp],
    UpdateParams = [Content, MsgId, Timestamp, false],
    UniqueKeyValues  = [LUsername, LServer, LToBareJid],
    Res = rdbms_queries:execute_upsert(LServer, inbox_upsert_incr_unread,
                                       InsertParams, UpdateParams, UniqueKeyValues),
    check_result(Res).

-spec reset_unread(LUsername :: jid:luser(),
                   LServer :: jid:lserver(),
                   BareJid :: jid:literal_jid(),
                   MsgId :: binary() | undefined) -> ok.
reset_unread(LUsername, LServer, ToBareJid, MsgId) ->
    LToBareJid = jid:nameprep(ToBareJid),
    Res = reset_inbox_unread_rdbms(LUsername, LServer, LToBareJid, MsgId),
    check_result(Res).

-spec reset_inbox_unread_rdbms(Username :: jid:luser(),
                               Server :: jid:lserver(),
                               ToBareJid :: jid:literal_jid(),
                               MsgId :: binary() | undefined) -> mongoose_rdbms:query_result().
reset_inbox_unread_rdbms(Username, Server, ToBareJid, undefined) ->
    sql_query(Server, ["update inbox set unread_count=0",
        " where luser=", esc_string(Username),
        " and lserver=", esc_string(Server),
        " and remote_bare_jid=", esc_string(ToBareJid), ";"]);
reset_inbox_unread_rdbms(Username, Server, ToBareJid, MsgId) ->
    sql_query(Server, ["update inbox set unread_count=0 where luser=",
        esc_string(Username), " and lserver=", esc_string(Server), " and remote_bare_jid=",
        esc_string(ToBareJid), " and msg_id=", esc_string(MsgId), ";"]).

-spec clear_inbox(LUsername :: jid:luser(), LServer :: jid:lserver()) -> inbox_write_res().
clear_inbox(LUsername, LServer) ->
    Res = clear_inbox_rdbms(LUsername, LServer),
    check_result(Res).

-spec clear_inbox(LServer :: jid:lserver()) -> inbox_write_res().
clear_inbox(LServer) ->
    Res = clear_inbox_rdbms(LServer),
    check_result(Res).


-spec get_entry_properties(jid:luser(), jid:lserver(), jid:literal_jid()) ->
    entry_properties().
get_entry_properties(LUser, LServer, BinEntryJID) ->
    Query = ["SELECT archive, unread_count, muted_until ",
             "FROM inbox "
             "WHERE luser = ", esc_string(LUser), " AND "
                   "lserver = ", esc_string(LServer), " AND "
                   "remote_bare_jid = ", esc_string(BinEntryJID)],
    case sql_query(LServer, Query) of
        {selected, []} ->
            [];
        {selected, [Selected]} ->
            decode_entries(Selected)
    end.

-spec set_entry_properties(jid:luser(), jid:lserver(), jid:literal_jid(), entry_properties()) ->
    entry_properties() | {error, binary()}.
set_entry_properties(LUser, LServer, BinEntryJID, Params) ->
    UnreadCount = sql_set_unread_count(maps:get(unread_count, Params, undefined)),
    MutedUntil = sql_set_muted_until(maps:get(muted_until, Params, undefined)),
    Archive = sql_set_archive(maps:get(archive, Params, undefined)),
    {Output, Ending} = returning_properties(mongoose_rdbms:db_engine(LServer), LUser, LServer, BinEntryJID),
    Query = ["UPDATE inbox ",
             "SET ", lists:droplast(lists:append([UnreadCount, MutedUntil, Archive])),
             Output,
             " WHERE "
                 "luser=", esc_string(LUser), " AND "
                 "lserver=", esc_string(LServer), " AND "
                 "remote_bare_jid=", esc_string(BinEntryJID),
             Ending],
    case sql_query(LServer, Query) of
        {error, Msg} when is_list(Msg) ->
            {error, list_to_binary(Msg)};
        {error, Msg} ->
            {error, Msg};
        {updated, 0, []} ->
            {error, <<"item-not-found">>};
        {updated, 1, [Result]} ->
            decode_entries(Result);
        {selected, []} ->
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

returning_properties(pgsql, _, _, _) ->
    {"", [" RETURNING archive, unread_count, muted_until;"]};
returning_properties(mysql, LUser, LServer, BinEntryJID) ->
    {"", ["; SELECT archive, unread_count, muted_until"
        " FROM inbox"
        " WHERE "
            "luser=", esc_string(LUser), " AND "
            "lserver=", esc_string(LServer), " AND "
            "remote_bare_jid=", esc_string(BinEntryJID), ";"]};
returning_properties(odbc, _, _, _) ->
    {" OUTPUT inserted.archive, inserted.unread_count, inserted.muted_until ", ""}.

sql_set_archive(undefined) ->
    [];
sql_set_archive(Val) when is_boolean(Val) ->
    ["archive=", encode_bool(Val), ","].

sql_set_unread_count(undefined) ->
    [];
sql_set_unread_count(0) ->
    ["unread_count=0", ","];
sql_set_unread_count(1) ->
    ["unread_count = CASE unread_count WHEN 0 THEN 1 ELSE unread_count END", ","].

sql_set_muted_until(undefined) ->
    [];
sql_set_muted_until(0) ->
    ["muted_until=0", ","];
sql_set_muted_until(Int) ->
    ["muted_until=", esc_int(Int), ","].

-spec esc_string(binary() | string()) -> mongoose_rdbms:sql_query_part().
esc_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

-spec esc_int(integer()) -> mongoose_rdbms:sql_query_part().
esc_int(Integer) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Integer)).


%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec lookup_query(mod_inbox:get_inbox_params()) -> iolist().
lookup_query(#{order := Order} = Params) ->
    OrderSQL = order_to_sql(Order),
    {LimitSQL, MSLimitSQL}  = sql_and_where_limit(maps:get(limit, Params, undefined)),
    BeginSQL = sql_and_where_timestamp(">=", maps:get(start, Params, undefined)),
    EndSQL = sql_and_where_timestamp("<=", maps:get('end', Params, undefined)),
    HiddenSQL = sql_and_where_unread_count(maps:get(hidden_read, Params, false)),
    Archive = sql_and_where_archive(maps:get(archive, Params, undefined)),
    ["SELECT ", MSLimitSQL,
     " remote_bare_jid, content, unread_count, timestamp, archive, muted_until "
     " FROM inbox WHERE luser = ? AND lserver = ?", BeginSQL, EndSQL, HiddenSQL, Archive,
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

-spec sql_and_where_timestamp(Operator :: string(), Timestamp :: integer()) -> iolist().
sql_and_where_timestamp(_Operator, undefined) ->
    [];
sql_and_where_timestamp(Operator, _NumericTimestamp) ->
    [" AND timestamp ", Operator, " ?"].

-spec sql_and_where_unread_count(HiddenRead :: boolean()) -> iolist().
sql_and_where_unread_count(true) ->
    [" AND unread_count > 0"];
sql_and_where_unread_count(_) ->
    [].

-spec sql_and_where_archive(ArchiveBox :: boolean() | undefined) -> iolist().
sql_and_where_archive(Val) when is_boolean(Val) ->
    [" AND archive = ?"];
sql_and_where_archive(undefined) ->
    [].

encode_bool(Val) ->
    encode_bool(Val, mongoose_rdbms_type:get()).

encode_bool(true, mssql) -> "1";
encode_bool(false, mssql) -> "0";
encode_bool(true, _) -> "true";
encode_bool(false, _) -> "false".

-spec clear_inbox_rdbms(Username :: jid:luser(), Server :: jid:lserver()) -> mongoose_rdbms:query_result().
clear_inbox_rdbms(Username, Server) ->
    sql_query(Server, ["delete from inbox where luser=",
        esc_string(Username), " and lserver=", esc_string(Server), ";"]).

-spec clear_inbox_rdbms(Server :: jid:lserver()) -> mongoose_rdbms:query_result().
clear_inbox_rdbms(Server) ->
    sql_query(Server, ["delete from inbox;"]).

-spec decode_row(host(), db_return()) -> inbox_res().
decode_row(LServer, {Username, Content, Count, Timestamp, Archive, MutedUntil}) ->
    Data = mongoose_rdbms:unescape_binary(LServer, Content),
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

check_result({updated, Val}, ValList) when is_list(ValList) ->
    case lists:member(Val, ValList) of
        true ->
            ok;
        _ ->
            {error, {expected_does_not_match, Val, ValList}}
    end;
check_result(Result, _) ->
    {error, {bad_result, Result}}.

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

sql_query(Pool, Query) ->
    Result = mongoose_rdbms:sql_query(Pool, Query),
    log_result(Result, Query),
    Result.

log_result({selected, _}, _Query) ->
    ok;
log_result({updated, _}, _Query) ->
    ok;
log_result({updated, _, _}, _Query) ->
    ok;
log_result(Result, Query) ->
    ?LOG_ERROR(#{what => inbox_rdbms_failed,
                 sql_query => iolist_to_binary(Query),
                 result => Result}).
