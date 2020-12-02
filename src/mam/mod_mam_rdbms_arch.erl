%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc RDBMS backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_rdbms_arch).

-define(SEARCH_WORDS_LIMIT, 10).

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

-type column() :: atom().
-type filter_field() :: {like, column(), binary()}
    | {le, column(), integer()}
    | {ge, column(), integer()}
    | {equal, column(), integer() | binary()}
    | {lower, column(), integer()}
    | {greater, column(), integer()}
    | {limit, limit, integer()}
    | {offset, offset, integer()}.

-type filter() :: [filter_field()].

-type env_vars() :: map().

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> 'ok'.
start(Host, Opts) ->
    start_pm(Host, Opts),

    prepare_insert(insert_mam_message, 1),
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
    ok.


-spec stop(jid:server()) -> ok.
stop(Host) ->
    stop_pm(Host).

-spec get_mam_pm_gdpr_data(ejabberd_gen_mam_archive:mam_pm_gdpr_data(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(Acc, #jid{luser = User, lserver = Host} = ArcJID) ->
    case mod_mam:archive_id(Host, User) of
        undefined -> [];
        ArchiveID ->
            Env = env_vars(Host, ArcJID),
            {selected, Rows} = extract_gdpr_messages(Env, ArchiveID),
            [uniform_to_gdpr(row_to_uniform_format(Row, Env)) || Row <- Rows] ++ Acc
    end.

uniform_to_gdpr({MessID, RemoteJID, Packet}) ->
    {integer_to_binary(MessID), jid:to_binary(RemoteJID), exml:to_binary(Packet)}.

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

-spec archive_size(Size :: integer(), Host :: jid:server(),
                   ArcId :: mod_mam:archive_id(), ArcJID :: jid:jid()) -> integer().
archive_size(Size, Host, UserID, ArcJID) when is_integer(Size) ->
    Filter = [{equal, user_id, UserID}],
    Env = env_vars(Host, ArcJID),
    Result = lookup_query(count, Env, Filter, unordered),
    mongoose_rdbms:selected_to_integer(Result).

-spec archive_message(_Result, jid:server(), mod_mam:archive_message_params()) -> ok.
archive_message(_Result, Host, Params = #{local_jid := ArcJID}) ->
    try
        Env = env_vars(Host, ArcJID),
        do_archive_message(Host, Params, Env),
        retract_message(Host, Params, Env)
    catch Class:Reason:StackTrace ->
              ?LOG_ERROR(#{what => archive_message_failed,
                           host => Host, mam_params => Params,
                           class => Class, reason => Reason, stacktrace => StackTrace}),
            {error, Reason}
    end.

do_archive_message(Host, Params, Env) ->
    Row = prepare_message_with_env(Params, Env),
    {updated, 1} = mod_mam_utils:success_sql_execute(Host, insert_mam_message, Row).

%% Retraction logic
%% Called after inserting a new message
-spec retract_message(jid:server(), mod_mam:archive_message_params()) -> ok.
retract_message(Host, #{local_jid := ArcJID} = Params)  ->
    Env = env_vars(Host, ArcJID),
    retract_message(Host, Params, Env).

-spec retract_message(jid:server(), mod_mam:archive_message_params(), env_vars()) -> ok.
retract_message(Host, #{archive_id := UserID,
                        remote_jid := RemJID,
                        direction := Dir,
                        packet := Packet}, Env) ->
    case get_retract_id(Packet, Env) of
        none -> ok;
        OriginIDToRetract -> retract_message(Host, UserID, RemJID, OriginIDToRetract, Dir, Env)
    end.

retract_message(Host, UserID, RemJID, OriginID, Dir, Env) ->
    MinBareRemJID = encode_jid(jid:to_bare(RemJID), Env),
    BinDir = encode_direction(Dir),
    {selected, Rows} = execute_select_messages_to_retract(
                         Host, UserID, MinBareRemJID, OriginID, BinDir),
    make_tombstone(Host, UserID, OriginID, Rows, Env),
    ok.

make_tombstone(_Host, UserID, OriginID, [], _Env) ->
    ?LOG_INFO(#{what => make_tombstone_failed,
                text => <<"Message to retract was not found by origin id">>,
                user_id => UserID, origin_id => OriginID});
make_tombstone(Host, UserID, OriginID, [{ResMessID, ResData}], Env) ->
    Data = unescape_binary(ResData, Env),
    Packet = decode_packet(Data, Env),
    MessID = mongoose_rdbms:result_to_integer(ResMessID),
    Tombstone = mod_mam_utils:tombstone(Packet, OriginID),
    TombstoneData = encode_packet(Tombstone, Env),
    execute_make_tombstone(Host, TombstoneData, UserID, MessID).

execute_select_messages_to_retract(Host, UserID, BareRemJID, OriginID, Dir) ->
    mod_mam_utils:success_sql_execute(Host, mam_select_messages_to_retract,
                                      [UserID, BareRemJID, OriginID, Dir]).

execute_make_tombstone(Host, TombstoneData, UserID, MessID) ->
    {updated, _} =
        mod_mam_utils:success_sql_execute(Host, mam_make_tombstone,
                                          [TombstoneData, UserID, MessID]).

%% Insert logic
-record(db_mapping, {column, param, format}).

db_mappings() ->
    [#db_mapping{column = id, param = message_id, format = int},
     #db_mapping{column = user_id, param = archive_id, format = int},
     #db_mapping{column = remote_bare_jid, param = remote_jid, format = bare_jid},
     #db_mapping{column = remote_resource, param = remote_jid, format = jid_resource},
     #db_mapping{column = direction, param = direction, format = direction},
     #db_mapping{column = from_jid, param = source_jid, format = jid},
     #db_mapping{column = origin_id, param = origin_id, format = maybe_binary},
     #db_mapping{column = message, param = packet, format = xml},
     #db_mapping{column = search_body, param = packet, format = search}].

-spec prepare_message(jid:server(), mod_mam:archive_message_params()) -> list().
prepare_message(Host, Params = #{local_jid := ArcJID}) ->
    Env = env_vars(Host, ArcJID),
    prepare_message_with_env(Params, Env).

prepare_message_with_env(Params, Env) ->
    [prepare_value(Params, Env, Mapping) || Mapping <- db_mappings()].

prepare_value(Params, Env, #db_mapping{param = Param, format = Format}) ->
    Value = maps:get(Param, Params),
    encode_value(Format, Value, Env).

encode_value(int, Value, _Env) when is_integer(Value) ->
    Value;
encode_value(maybe_binary, none, _Env) ->
    null;
encode_value(maybe_binary, Value, _Env) when is_binary(Value) ->
    Value;
encode_value(direction, Value, _Env) ->
    encode_direction(Value);
encode_value(bare_jid, Value, Env) ->
    encode_jid(jid:to_bare(Value), Env);
encode_value(jid, Value, Env) ->
    encode_jid(Value, Env);
encode_value(jid_resource, #jid{lresource = Res}, _Env) ->
    Res;
encode_value(xml, Value, Env) ->
    encode_packet(Value, Env);
encode_value(search, Value, #{full_text_search := SearchEnabled}) ->
    mod_mam_utils:packet_to_search_body(SearchEnabled, Value).

column_names() ->
     [Column || #db_mapping{column = Column} <- db_mappings()].

-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_message,
    Fields = column_names(),
    {Query, Fields2} = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields2, Query),
    ok.


%% Removal logic
-spec remove_archive(Acc :: mongoose_acc:t(), Host :: jid:server(),
                     ArchiveID :: mod_mam:archive_id(),
                     RoomJID :: jid:jid()) -> mongoose_acc:t().
remove_archive(Acc, Host, UserID, _ArcJID) ->
    remove_archive(Host, UserID),
    Acc.

remove_archive(Host, UserID) ->
    {updated, _} = mod_mam_utils:success_sql_execute(Host, mam_archive_remove, [UserID]).

%% GDPR logic
extract_gdpr_messages(Env, ArchiveID) ->
    Filters = [{equal, user_id, ArchiveID}],
    lookup_query(lookup, Env, Filters, asc).

%% Lookup logic
-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
                             {ok, mod_mam:lookup_result()}.
lookup_messages({error, _Reason}=Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, Host, Params = #{owner_jid := ArcJID}) ->
    try
        Env = env_vars(Host, ArcJID),
        ExtParams = extend_params(Params, Env),
        Filter = prepare_filter(ExtParams),
        mam_lookup:lookup(Env, Filter, ExtParams)
    catch _Type:Reason:S ->
        {error, {Reason, {stacktrace, S}}}
    end.

row_to_uniform_format({BMessID, BSrcJID, SDataRaw}, Env) ->
    MessID = mongoose_rdbms:result_to_integer(BMessID),
    SrcJID = decode_jid(BSrcJID, Env),
    Data = unescape_binary(SDataRaw, Env),
    Packet = decode_packet(Data, Env),
    {MessID, SrcJID, Packet}.

lookup_query(QueryType, #{host := Host} = _Env, Filters, Order) ->
    StmtName = filters_to_statement_name(QueryType, Filters, Order),
    case mongoose_rdbms:prepared(StmtName) of
        false ->
            %% Create a new type of a query
            SQL = lookup_sql_binary(QueryType, Filters, Order, index_hint_sql(Host)),
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
                     sql_query => lookup_sql_binary(QueryType, Filters, Order, index_hint_sql(Host)),
                     reason => Error, host => Host},
            ?LOG_ERROR(What),
            error(What)
    catch Class:Error:Stacktrace ->
            What = #{what => mam_lookup_failed, statement => StmtName,
                     sql_query => lookup_sql_binary(QueryType, Filters, Order, index_hint_sql(Host)),
                     class => Class, stacktrace => Stacktrace,
                     reason => Error, host => Host},
            ?LOG_ERROR(What),
            error(What)
    end.


%% ----------------------------------------------------------------------
%% Optimizations and extensible code

encode_direction(incoming) -> <<"I">>;
encode_direction(outgoing) -> <<"O">>.

make_start_id(Start, Borders) ->
    StartID = maybe_encode_compact_uuid(Start, 0),
    apply_start_border(Borders, StartID).

make_end_id(End, Borders) ->
    EndID = maybe_encode_compact_uuid(End, 255),
    apply_end_border(Borders, EndID).

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

maybe_encode_bare_jid(undefined, _Env) ->
    undefined;
maybe_encode_bare_jid(JID, Env) ->
    encode_jid(jid:to_bare(JID), Env).

-spec encode_jid(jid:jid(), env_vars()) -> binary().
encode_jid(JID, #{db_jid_codec := Codec, archive_jid := ArcJID}) ->
    mam_jid:encode(Codec, ArcJID, JID).

-spec decode_jid(binary(), env_vars()) -> jid:jid().
decode_jid(EncodedJID, #{db_jid_codec := Codec, archive_jid := ArcJID}) ->
    mam_jid:decode(Codec, ArcJID, EncodedJID).

-spec encode_packet(exml:element(), env_vars()) -> binary().
encode_packet(Packet, #{db_message_codec := Codec}) ->
    mam_message:encode(Codec, Packet).

-spec decode_packet(binary(), env_vars()) -> exml:element().
decode_packet(Bin, #{db_message_codec := Codec}) ->
    mam_message:decode(Codec, Bin).

-spec unescape_binary(binary(), env_vars()) -> binary().
unescape_binary(Bin, #{host := Host}) ->
    %% Funny, rdbms ignores this Host variable
    mongoose_rdbms:unescape_binary(Host, Bin).

-spec get_retract_id(exml:element(), env_vars()) -> none | binary().
get_retract_id(Packet, #{has_message_retraction := Enabled}) ->
    mod_mam_utils:get_retract_id(Enabled, Packet).

env_vars(Host, ArcJID) ->
    %% Please, minimize usage of the host field.
    %% It's only for passing into RDBMS.
    #{host => Host,
      archive_jid => ArcJID,
      lookup_fun => fun lookup_query/4,
      row_to_uniform_format_fun => fun row_to_uniform_format/2,
      has_message_retraction => mod_mam_utils:has_message_retraction(mod_mam, Host),
      full_text_search => mod_mam_utils:has_full_text_search(mod_mam, Host),
      db_jid_codec => db_jid_codec(Host, ?MODULE),
      db_message_codec => db_message_codec(Host, ?MODULE)}.

-spec db_jid_codec(jid:server(), module()) -> module().
db_jid_codec(Host, Module) ->
    gen_mod:get_module_opt(Host, Module, db_jid_format, mam_jid_mini).

-spec db_message_codec(jid:server(), module()) -> module().
db_message_codec(Host, Module) ->
    gen_mod:get_module_opt(Host, Module, db_message_format, mam_message_compressed_eterm).

%% ----------------------------------------------------------------------
%% Prepared queries helpers

lookup_sql_binary(QueryType, Filters, Order, IndexHintSQL) ->
    iolist_to_binary(lookup_sql(QueryType, Filters, Order, IndexHintSQL)).

lookup_sql(QueryType, Filters, Order, IndexHintSQL) ->
    LimitSQL = limit_sql(Filters),
    OrderSQL = order_to_sql(Order),
    FilterSQL = filters_to_sql(Filters),
    ["SELECT ", columns_sql(QueryType), " "
     "FROM mam_message ",
     IndexHintSQL, FilterSQL, OrderSQL, LimitSQL].

columns_sql(lookup) -> "id, from_jid, message";
columns_sql(count) -> "COUNT(*)".

%% Caller should provide both limit and offset fields in the correct order.
%% See limit_offset_filters.
%% No limits option is fine too (it is used with count and GDPR).
limit_sql(Filters) ->
    case lists:keymember(limit, 1, Filters) of %% and offset
        true -> rdbms_queries:limit_offset_sql();
        false -> ""
    end.

-spec index_hint_sql(jid:server()) -> string().
index_hint_sql(Host) ->
    case mongoose_rdbms:db_engine(Host) of
        mysql ->
            "USE INDEX(PRIMARY, i_mam_message_rem) ";
        _ ->
            ""
    end.

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

extend_params(#{start_ts := Start, end_ts := End, with_jid := WithJID,
                borders := Borders, search_text := SearchText} = Params, Env) ->
    Params#{norm_search_text => mod_mam_utils:normalize_search_text(SearchText),
            start_id => make_start_id(Start, Borders),
            end_id => make_end_id(End, Borders),
            remote_bare_jid => maybe_encode_bare_jid(WithJID, Env),
            remote_resource => jid_to_non_empty_resource(WithJID)}.

jid_to_non_empty_resource(#jid{lresource = Res}) when byte_size(Res) > 0 ->
    Res;
jid_to_non_empty_resource(_) ->
    undefined.

-record(lookup_field, {op, column, param, required, value_maker}).

lookup_fields() ->
    [#lookup_field{op = equal, column = user_id, param = archive_id, required = true},
     #lookup_field{op = ge, column = id, param = start_id},
     #lookup_field{op = le, column = id, param = end_id},
     #lookup_field{op = equal, column = remote_bare_jid, param = remote_bare_jid},
     #lookup_field{op = equal, column = remote_resource, param = remote_resource},
     #lookup_field{op = like, column = search_body, param = norm_search_text, value_maker = search_words}].

prepare_filter(Params) ->
    [new_filter(Field, Value)
     || Field <- lookup_fields(),
        Value <- field_to_values(Field, Params)].

field_to_values(#lookup_field{param = Param, value_maker = ValueMaker, required = Required} = Field, Params) ->
    case maps:find(Param, Params) of
        {ok, Value} when Value =/= undefined ->
            make_value(ValueMaker, Value);
        Other when Required ->
            error(#{reason => missing_required_field, field => Field, params => Params, result => Other});
        _ ->
            []
    end.

make_value(search_words, Value) ->
    search_words(Value);
make_value(undefined, Value) ->
    [Value].

new_filter(#lookup_field{op = Op, column = Column}, Value) ->
    {Op, Column, Value}.

%% Constructs a separate LIKE filter for each word.
%% SearchText example is "word1%word2%word3".
%% Order of words does not matter (they can go in any order).
search_words(SearchText) ->
    Words = binary:split(SearchText, <<"%">>, [global]),
    [<<"%", Word/binary, "%">> || Word <- lists:sublist(Words, ?SEARCH_WORDS_LIMIT)].
