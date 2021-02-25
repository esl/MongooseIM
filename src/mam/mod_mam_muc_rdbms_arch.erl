%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc RDBMS backend for MUC Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_rdbms_arch).

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

-export([get_mam_muc_gdpr_data/2]).

%% Called from mod_mam_rdbms_async_writer
-export([prepare_message/2, retract_message/2, prepare_insert/2]).
-export([extend_params_with_sender_id/2]).

%% ----------------------------------------------------------------------
%% Imports

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_rsm.hrl").
-include("mongoose_mam.hrl").

%% ----------------------------------------------------------------------
%% Types

-type env_vars() :: mod_mam_rdbms_arch:env_vars().

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> ok.
start(Host, Opts) ->
    start_hooks(Host, Opts),
    register_prepared_queries(),
    ok.

-spec stop(jid:server()) -> ok.
stop(Host) ->
    stop_hooks(Host).

-spec get_mam_muc_gdpr_data(ejabberd_gen_mam_archive:mam_pm_gdpr_data(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(Acc, #jid{luser = User, lserver = Host} = _UserJID) ->
    case mod_mam:archive_id(Host, User) of
        undefined ->
            Acc;
        SenderID ->
            %% We don't know the real room JID here, use FakeEnv
            FakeEnv = env_vars(Host, jid:make(<<>>, <<>>, <<>>)),
            {selected, Rows} = extract_gdpr_messages(Host, SenderID),
            [mam_decoder:decode_muc_gdpr_row(Row, FakeEnv) || Row <- Rows] ++ Acc
    end.

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

-spec start_hooks(jid:server(), _) -> ok.
start_hooks(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(get_mam_muc_gdpr_data, Host, ?MODULE, get_mam_muc_gdpr_data, 50),
    ok.


-spec stop_hooks(jid:server()) -> ok.
stop_hooks(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(get_mam_muc_gdpr_data, Host, ?MODULE, get_mam_muc_gdpr_data, 50),
    ok.

%% ----------------------------------------------------------------------
%% SQL queries

register_prepared_queries() ->
    prepare_insert(insert_mam_muc_message, 1),
    mongoose_rdbms:prepare(mam_muc_archive_remove, mam_muc_message, [room_id],
                           <<"DELETE FROM mam_muc_message "
                             "WHERE room_id = ?">>),
    mongoose_rdbms:prepare(mam_muc_make_tombstone, mam_muc_message, [message, room_id, id],
                           <<"UPDATE mam_muc_message SET message = ?, search_body = '' "
                             "WHERE room_id = ? AND id = ?">>),
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(1),
    mongoose_rdbms:prepare(mam_muc_select_messages_to_retract, mam_muc_message,
                           [room_id, sender_id, origin_id],
                           <<"SELECT ", LimitMSSQL/binary,
                             " id, message FROM mam_muc_message"
                             " WHERE room_id = ? AND sender_id = ? "
                             " AND origin_id = ?"
                             " ORDER BY id DESC ", LimitSQL/binary>>),
    mongoose_rdbms:prepare(mam_muc_extract_gdpr_messages, mam_muc_message, [sender_id],
                           <<"SELECT id, message FROM mam_muc_message "
                             " WHERE sender_id = ? ORDER BY id">>).

%% ----------------------------------------------------------------------
%% Declarative logic

db_mappings() ->
    [#db_mapping{column = id, param = message_id, format = int},
     #db_mapping{column = room_id, param = archive_id, format = int},
     #db_mapping{column = sender_id, param = sender_id, format = int},
     #db_mapping{column = nick_name, param = source_jid, format = jid_resource},
     #db_mapping{column = origin_id, param = origin_id, format = maybe_string},
     #db_mapping{column = message, param = packet, format = xml},
     #db_mapping{column = search_body, param = packet, format = search}].

lookup_fields() ->
    [#lookup_field{op = equal, column = room_id, param = archive_id, required = true},
     #lookup_field{op = ge, column = id, param = start_id},
     #lookup_field{op = le, column = id, param = end_id},
     #lookup_field{op = equal, column = nick_name, param = remote_resource},
     #lookup_field{op = like, column = search_body, param = norm_search_text, value_maker = search_words}].

env_vars(Host, ArcJID) ->
    %% Please, minimize the usage of the host field.
    %% It's only for passing into RDBMS.
    #{host => Host,
      archive_jid => ArcJID,
      table => mam_muc_message,
      index_hint_fn => fun index_hint_sql/1,
      columns_sql_fn => fun columns_sql/1,
      column_to_id_fn => fun column_to_id/1,
      lookup_fn => fun lookup_query/5,
      decode_row_fn => fun row_to_uniform_format/2,
      has_message_retraction => mod_mam_utils:has_message_retraction(mod_mam_muc, Host),
      has_full_text_search => mod_mam_utils:has_full_text_search(mod_mam_muc, Host),
      db_jid_codec => db_jid_codec(Host, ?MODULE),
      db_message_codec => db_message_codec(Host, ?MODULE)}.

row_to_uniform_format(Row, Env) ->
    mam_decoder:decode_muc_row(Row, Env).

-spec index_hint_sql(env_vars()) -> string().
index_hint_sql(_) -> "".

columns_sql(lookup) -> "id, nick_name, message";
columns_sql(count) -> "COUNT(*)".

column_to_id(id) -> "i";
column_to_id(room_id) -> "u";
column_to_id(nick_name) -> "n";
column_to_id(search_body) -> "s".

column_names(Mappings) ->
     [Column || #db_mapping{column = Column} <- Mappings].

%% ----------------------------------------------------------------------
%% Options

-spec db_jid_codec(jid:server(), module()) -> module().
db_jid_codec(Host, Module) ->
    gen_mod:get_module_opt(Host, Module, db_jid_format, mam_jid_rfc).

-spec db_message_codec(jid:server(), module()) -> module().
db_message_codec(Host, Module) ->
    gen_mod:get_module_opt(Host, Module, db_message_format, mam_message_compressed_eterm).

-spec get_retract_id(exml:element(), env_vars()) -> none | binary().
get_retract_id(Packet, #{has_message_retraction := Enabled}) ->
    mod_mam_utils:get_retract_id(Enabled, Packet).

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec archive_size(Size :: integer(), Host :: jid:server(),
                   ArcId :: mod_mam:archive_id(), ArcJID :: jid:jid()) -> integer().
archive_size(Size, Host, ArcID, ArcJID) when is_integer(Size) ->
    Filter = [{equal, room_id, ArcID}],
    Env = env_vars(Host, ArcJID),
    Result = lookup_query(count, Env, Filter, unordered, all),
    mongoose_rdbms:selected_to_integer(Result).

extend_params_with_sender_id(Host, Params = #{remote_jid := SenderJID}) ->
    BareSenderJID = jid:to_bare(SenderJID),
    SenderID = mod_mam:archive_id_int(Host, BareSenderJID),
    Params#{sender_id => SenderID}.

-spec archive_message(_Result, jid:server(), mod_mam:archive_message_params()) -> ok.
archive_message(_Result, Host, Params0 = #{local_jid := ArcJID}) ->
    try
        Params = extend_params_with_sender_id(Host, Params0),
        Env = env_vars(Host, ArcJID),
        do_archive_message(Host, Params, Env),
        retract_message(Host, Params, Env),
        ok
    catch error:Reason:StackTrace ->
              ?LOG_ERROR(#{what => archive_message_failed,
                           host => Host, mam_params => Params0,
                           reason => Reason, stacktrace => StackTrace}),
              erlang:raise(error, Reason, StackTrace)
    end.

do_archive_message(Host, Params, Env) ->
    Row = mam_encoder:encode_message(Params, Env, db_mappings()),
    {updated, 1} = mongoose_rdbms:execute_successfully(Host, insert_mam_muc_message, Row).

%% Retraction logic
%% Called after inserting a new message
-spec retract_message(jid:server(), mod_mam:archive_message_params()) -> ok.
retract_message(Host, #{local_jid := ArcJID} = Params)  ->
    Env = env_vars(Host, ArcJID),
    retract_message(Host, Params, Env).

-spec retract_message(jid:server(), mod_mam:archive_message_params(), env_vars()) -> ok.
retract_message(Host, #{archive_id := ArcID, sender_id := SenderID,
                        packet := Packet}, Env) ->
    case get_retract_id(Packet, Env) of
        none -> ok;
        OriginIDToRetract ->
            Info = get_retraction_info(Host, ArcID, SenderID, OriginIDToRetract, Env),
            make_tombstone(Host, ArcID, OriginIDToRetract, Info, Env)
    end.

get_retraction_info(Host, ArcID, SenderID, OriginID, Env) ->
    {selected, Rows} =
        execute_select_messages_to_retract(Host, ArcID, SenderID, OriginID),
    mam_decoder:decode_retraction_info(Env, Rows).

make_tombstone(_Host, ArcID, OriginID, skip, _Env) ->
    ?LOG_INFO(#{what => make_tombstone_failed,
                text => <<"Message to retract was not found by origin id">>,
                user_id => ArcID, origin_id => OriginID});
make_tombstone(Host, ArcID, OriginID, #{packet := Packet, message_id := MessID}, Env) ->
    Tombstone = mod_mam_utils:tombstone(Packet, OriginID),
    TombstoneData = mam_encoder:encode_packet(Tombstone, Env),
    execute_make_tombstone(Host, TombstoneData, ArcID, MessID).

execute_select_messages_to_retract(Host, ArcID, SenderID, OriginID) ->
    mongoose_rdbms:execute_successfully(Host, mam_muc_select_messages_to_retract,
                                      [ArcID, SenderID, OriginID]).

execute_make_tombstone(Host, TombstoneData, ArcID, MessID) ->
    mongoose_rdbms:execute_successfully(Host, mam_muc_make_tombstone,
                                      [TombstoneData, ArcID, MessID]).

%% Insert logic
-spec prepare_message(jid:server(), mod_mam:archive_message_params()) -> list().
prepare_message(Host, Params = #{local_jid := ArcJID}) ->
    Env = env_vars(Host, ArcJID),
    mam_encoder:encode_message(Params, Env, db_mappings()).

-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_muc_message,
    Fields = column_names(db_mappings()),
    {Query, Fields2} = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields2, Query),
    ok.

%% Removal logic
-spec remove_archive(Acc :: mongoose_acc:t(), Host :: jid:server(),
                     ArcID :: mod_mam:archive_id(),
                     ArcJID :: jid:jid()) -> mongoose_acc:t().
remove_archive(Acc, Host, ArcID, _ArcJID) ->
    mongoose_rdbms:execute_successfully(Host, mam_muc_archive_remove, [ArcID]),
    Acc.

%% GDPR logic
extract_gdpr_messages(Host, SenderID) ->
    mongoose_rdbms:execute_successfully(Host, mam_muc_extract_gdpr_messages, [SenderID]).

%% Lookup logic
-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
                             {ok, mod_mam:lookup_result()}.
lookup_messages({error, _Reason} = Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, Host, Params = #{owner_jid := ArcJID}) ->
    Env = env_vars(Host, ArcJID),
    ExdParams = mam_encoder:extend_lookup_params(Params, Env),
    Filter = mam_filter:produce_filter(ExdParams, lookup_fields()),
    mam_lookup:lookup(Env, Filter, ExdParams).

lookup_query(QueryType, Env, Filters, Order, OffsetLimit) ->
    mam_lookup_sql:lookup_query(QueryType, Env, Filters, Order, OffsetLimit).
