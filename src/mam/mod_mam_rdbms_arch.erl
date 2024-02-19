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
-export([start/2, stop/1, hooks/1, supported_features/0]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-callback encode(term()) -> binary().
-callback decode(binary()) -> term().

-export([archive_size/3,
         archive_message/3,
         lookup_messages/3,
         remove_archive/3,
         remove_domain/3,
         get_mam_pm_gdpr_data/3]).

%% Called from mod_mam_rdbms_async_pool_writer
-export([prepare_message/2, retract_message/2, prepare_insert/2]).

-type host_type() :: mongooseim:host_type().

%% ----------------------------------------------------------------------
%% Imports

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_mam.hrl").

%% ----------------------------------------------------------------------
%% Types

-type env_vars() :: #{
        host_type := host_type(),
        archive_jid := jid:jid(),
        table := atom(),
        index_hint_fn := fun((env_vars()) -> mam_lookup_sql:sql_part()),
        columns_sql_fn := fun((mam_lookup_sql:query_type()) -> mam_lookup_sql:sql_part()),
        column_to_id_fn := fun((mam_lookup_sql:column()) -> string()),
        lookup_fn := mam_lookup_sql:lookup_query_fn(),
        decode_row_fn := fun((Row :: tuple(), env_vars()) -> Decoded :: tuple()),
        has_message_retraction := boolean(),
        has_full_text_search := boolean(),
        db_jid_codec := module(),
        db_message_codec := module()
       }.

-export_type([env_vars/0]).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, Opts) ->
    register_prepared_queries(Opts),
    ok.

-spec stop(host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec get_mam_pm_gdpr_data(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ejabberd_gen_mam_archive:mam_pm_gdpr_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
get_mam_pm_gdpr_data(Acc,
                     #{jid := #jid{luser = LUser, lserver = LServer} = ArcJID},
                     #{host_type := HostType}) ->
    case mod_mam_pm:archive_id(LServer, LUser) of
        undefined ->
            {ok, Acc};
        ArcID ->
            Env = env_vars(HostType, ArcJID),
            {selected, Rows} = extract_gdpr_messages(Env, ArcID),
            {ok, [uniform_to_gdpr(row_to_uniform_format(Row, Env)) || Row <- Rows] ++ Acc}
    end.

-spec uniform_to_gdpr(mod_mam:message_row()) -> tuple().
uniform_to_gdpr(#{id := MessID, jid := RemoteJID, packet := Packet}) ->
    {integer_to_binary(MessID), jid:to_binary(RemoteJID), exml:to_binary(Packet)}.

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_pm

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, no_writer) of
        true ->
            [];
        false ->
            [{mam_archive_message, HostType, fun ?MODULE:archive_message/3, #{}, 50}]
    end ++
    [{mam_archive_size, HostType, fun ?MODULE:archive_size/3, #{}, 50},
     {mam_lookup_messages, HostType, fun ?MODULE:lookup_messages/3, #{}, 50},
     {mam_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 50},
     {get_mam_pm_gdpr_data, HostType, fun ?MODULE:get_mam_pm_gdpr_data/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50}].

%% ----------------------------------------------------------------------
%% SQL queries

register_prepared_queries(Opts) ->
    prepare_insert(insert_mam_message, 1),
    mongoose_rdbms:prepare(mam_archive_remove, mam_message, [user_id],
                           <<"DELETE FROM mam_message "
                             "WHERE user_id = ?">>),

    %% Domain Removal
    prepare_remove_domain(Opts),

    mongoose_rdbms:prepare(mam_make_tombstone, mam_message, [message, user_id, id],
                           <<"UPDATE mam_message SET message = ?, search_body = '' "
                             "WHERE user_id = ? AND id = ?">>),
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(1),
    mongoose_rdbms:prepare(mam_select_messages_to_retract_on_origin_id, mam_message,
                           [user_id, remote_bare_jid, origin_id, direction],
                           <<"SELECT ", LimitMSSQL/binary,
                             " id, message FROM mam_message"
                             " WHERE user_id = ? AND remote_bare_jid = ? "
                             " AND origin_id = ? AND direction = ?"
                             " ORDER BY id DESC ", LimitSQL/binary>>),
    mongoose_rdbms:prepare(mam_select_messages_to_retract_on_stanza_id, mam_message,
                           [user_id, remote_bare_jid, id, direction],
                           <<"SELECT ", LimitMSSQL/binary,
                             " origin_id, message FROM mam_message"
                             " WHERE user_id = ? AND remote_bare_jid = ? "
                             " AND id = ? AND direction = ?"
                             " ORDER BY id DESC ", LimitSQL/binary>>).

prepare_remove_domain(#{delete_domain_limit := infinity}) ->
    mongoose_rdbms:prepare(mam_remove_domain, mam_message, ['mam_server_user.server'],
                           <<"DELETE FROM mam_message "
                             "WHERE user_id IN "
                             "(SELECT id from mam_server_user WHERE server = ?)">>),
    mongoose_rdbms:prepare(mam_remove_domain_prefs, mam_config, ['mam_server_user.server'],
                           <<"DELETE FROM mam_config "
                             "WHERE user_id IN "
                             "(SELECT id from mam_server_user WHERE server = ?)">>),
    mongoose_rdbms:prepare(mam_remove_domain_users, mam_server_user, [server],
                           <<"DELETE FROM mam_server_user WHERE server = ?">>);
prepare_remove_domain(#{delete_domain_limit := Limit}) ->
    LimitSQL = case mongoose_rdbms:db_type() of
                        mssql -> throw(delete_domain_limit_not_supported_for_mssql);
                        _ -> {MaybeLimitSQL, _} = rdbms_queries:get_db_specific_limits_binaries(Limit),
                             MaybeLimitSQL
                    end,
    IdTable = <<"(SELECT * FROM ",
                    "(SELECT msg.user_id, msg.id FROM mam_message msg",
                    " INNER JOIN mam_server_user msu ON msu.id=msg.user_id",
                    " WHERE msu.server = ? ", LimitSQL/binary, ") AS T)">>,
    mongoose_rdbms:prepare(mam_incr_remove_domain, mam_message, ['mam_server_user.server'],
                           <<"DELETE FROM mam_message WHERE (user_id, id) IN ", IdTable/binary>>),
    CfgTable = <<"(SELECT * FROM ",
                     "(SELECT cfg.user_id, cfg.remote_jid FROM mam_config cfg",
                     " INNER JOIN mam_server_user msu ON msu.id=cfg.user_id",
                     " WHERE msu.server = ? ", LimitSQL/binary, ") AS T)">>,
    mongoose_rdbms:prepare(mam_incr_remove_domain_prefs, mam_config, ['mam_server_user.server'],
                           <<"DELETE FROM mam_config "
                             "WHERE (user_id, remote_jid) IN ", CfgTable/binary>>),
    ServerTable = <<"(SELECT * FROM ",
                    "(SELECT id FROM mam_server_user WHERE server = ? ", LimitSQL/binary, ") as t)">>,
    mongoose_rdbms:prepare(mam_incr_remove_domain_users, mam_server_user, [server],
                           <<"DELETE FROM mam_server_user WHERE id IN ", ServerTable/binary>>).


%% ----------------------------------------------------------------------
%% Declarative logic

db_mappings() ->
    %% One entry per the database field
    [#db_mapping{column = id, param = message_id, format = int},
     #db_mapping{column = user_id, param = archive_id, format = int},
     #db_mapping{column = remote_bare_jid, param = remote_jid, format = bare_jid},
     #db_mapping{column = remote_resource, param = remote_jid, format = jid_resource},
     #db_mapping{column = direction, param = direction, format = direction},
     #db_mapping{column = from_jid, param = source_jid, format = jid},
     #db_mapping{column = origin_id, param = origin_id, format = maybe_string},
     #db_mapping{column = message, param = packet, format = xml},
     #db_mapping{column = search_body, param = packet, format = search},
     #db_mapping{column = is_groupchat, param = is_groupchat, format = bool}].

lookup_fields() ->
    %% Describe each possible filtering option
    [#lookup_field{op = equal, column = user_id, param = archive_id, required = true},
     #lookup_field{op = ge, column = id, param = start_id},
     #lookup_field{op = le, column = id, param = end_id},
     #lookup_field{op = equal, column = remote_bare_jid, param = remote_bare_jid},
     #lookup_field{op = equal, column = remote_resource, param = remote_resource},
     #lookup_field{op = like, column = search_body, param = norm_search_text, value_maker = search_words},
     #lookup_field{op = equal, column = is_groupchat, param = include_groupchat},
     #lookup_field{op = equal, column = id, param = message_id}].

-spec env_vars(host_type(), jid:jid()) -> env_vars().
env_vars(HostType, ArcJID) ->
    %% Please, minimize the usage of the host_type field.
    %% It's only for passing into RDBMS.
    #{host_type => HostType,
      archive_jid => ArcJID,
      table => mam_message,
      index_hint_fn => fun index_hint_sql/1,
      columns_sql_fn => fun columns_sql/1,
      column_to_id_fn => fun column_to_id/1,
      lookup_fn => fun lookup_query/5,
      decode_row_fn => fun row_to_uniform_format/2,
      has_message_retraction => mod_mam_utils:has_message_retraction(mod_mam_pm, HostType),
      has_full_text_search => mod_mam_utils:has_full_text_search(mod_mam_pm, HostType),
      db_jid_codec => mod_mam_utils:db_jid_codec(HostType, ?MODULE),
      db_message_codec => mod_mam_utils:db_message_codec(HostType, ?MODULE)}.

row_to_uniform_format(Row, Env) ->
    mam_decoder:decode_row(Row, Env).

-spec index_hint_sql(env_vars()) -> string().
index_hint_sql(#{host_type := HostType}) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql -> "USE INDEX(PRIMARY, i_mam_message_rem) ";
        _ -> ""
    end.

columns_sql(lookup) -> "id, from_jid, message";
columns_sql(count) -> "COUNT(*)".

%% For each unique column in lookup_fields()
column_to_id(id) -> "i";
column_to_id(user_id) -> "u";
column_to_id(remote_bare_jid) -> "b";
column_to_id(remote_resource) -> "r";
column_to_id(search_body) -> "s";
column_to_id(is_groupchat) -> "g".

column_names(Mappings) ->
     [Column || #db_mapping{column = Column} <- Mappings].

%% ----------------------------------------------------------------------
%% Options

-spec get_retract_id(exml:element(), env_vars()) -> none | mod_mam_utils:retraction_id().
get_retract_id(Packet, #{has_message_retraction := Enabled}) ->
    mod_mam_utils:get_retract_id(Enabled, Packet).

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec archive_size(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: integer(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, owner := jid:jid()},
    Extra :: gen_hook:extra().
archive_size(Size, #{archive_id := ArcID, owner := ArcJID}, #{host_type := HostType}) when is_integer(Size) ->
    Filter = [{equal, user_id, ArcID}],
    Env = env_vars(HostType, ArcJID),
    Result = lookup_query(count, Env, Filter, unordered, all),
    {ok, mongoose_rdbms:selected_to_integer(Result)}.

-spec archive_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: mod_mam:archive_message_params(),
    Extra :: gen_hook:extra().
archive_message(_Result, #{local_jid := ArcJID} = Params, #{host_type := HostType}) ->
    try
        assert_archive_id_provided(Params),
        Env = env_vars(HostType, ArcJID),
        do_archive_message(HostType, Params, Env),
        retract_message(HostType, Params, Env),
        {ok, ok}
    catch error:Reason:StackTrace ->
        ?LOG_ERROR(#{what => archive_message_failed,
                    host_type => HostType, mam_params => Params,
                    reason => Reason, stacktrace => StackTrace}),
        erlang:raise(error, Reason, StackTrace)
    end.

do_archive_message(HostType, Params, Env) ->
    Row = mam_encoder:encode_message(Params, Env, db_mappings()),
    {updated, 1} = mongoose_rdbms:execute_successfully(HostType, insert_mam_message, Row).

%% Retraction logic
%% Called after inserting a new message
-spec retract_message(host_type(), mod_mam:archive_message_params()) -> ok.
retract_message(HostType, #{local_jid := ArcJID} = Params)  ->
    Env = env_vars(HostType, ArcJID),
    retract_message(HostType, Params, Env).

-spec retract_message(host_type(), mod_mam:archive_message_params(), env_vars()) -> ok.
retract_message(HostType, #{archive_id := ArcID, remote_jid := RemJID,
                            direction := Dir, packet := Packet} = Params, Env) ->
    case get_retract_id(Packet, Env) of
        none -> ok;
        RetractionId ->
            Info = get_retraction_info(HostType, ArcID, RemJID, RetractionId, Dir, Env),
            make_tombstone(HostType, ArcID, RetractionId, Info, Params, Env)
    end.

get_retraction_info(HostType, ArcID, RemJID, RetractionId, Dir, Env) ->
    %% Code style notice:
    %% - Add Ext prefix for all externally encoded data
    %% (in cases, when we usually add Bin, B, S Esc prefixes)
    ExtBareRemJID = mam_encoder:encode_jid(jid:to_bare(RemJID), Env),
    ExtDir = mam_encoder:encode_direction(Dir),
    {selected, Rows} = execute_select_messages_to_retract(
                         HostType, ArcID, ExtBareRemJID, RetractionId, ExtDir),
    mam_decoder:decode_retraction_info(Env, Rows, RetractionId).

make_tombstone(_HostType, ArcID, RetractionId, skip, _Params, _Env) ->
    ?LOG_INFO(#{what => make_tombstone_failed,
                text => <<"Message to retract was not found">>,
                user_id => ArcID, retraction_context => RetractionId});
make_tombstone(HostType, ArcID, _RetractionId,
               RetractionInfo = #{message_id := MessID}, Params,
               #{archive_jid := ArcJID} = Env) ->
    RetractionInfo1 = mongoose_hooks:mam_retraction(HostType, RetractionInfo, Params),
    Tombstone = mod_mam_utils:tombstone(RetractionInfo1, ArcJID),
    TombstoneData = mam_encoder:encode_packet(Tombstone, Env),
    execute_make_tombstone(HostType, TombstoneData, ArcID, MessID).

execute_select_messages_to_retract(HostType, ArcID, BareRemJID, {origin_id, OriginID}, Dir) ->
    mongoose_rdbms:execute_successfully(HostType, mam_select_messages_to_retract_on_origin_id,
                                      [ArcID, BareRemJID, OriginID, Dir]);
execute_select_messages_to_retract(HostType, ArcID, BareRemJID, {stanza_id, BinStanzaId}, Dir) ->
    StanzaId = mod_mam_utils:external_binary_to_mess_id(BinStanzaId),
    mongoose_rdbms:execute_successfully(HostType, mam_select_messages_to_retract_on_stanza_id,
                                      [ArcID, BareRemJID, StanzaId, Dir]).

execute_make_tombstone(HostType, TombstoneData, ArcID, MessID) ->
    mongoose_rdbms:execute_successfully(HostType, mam_make_tombstone,
                                        [TombstoneData, ArcID, MessID]).

%% Insert logic
-spec prepare_message(host_type(), mod_mam:archive_message_params()) -> list().
prepare_message(HostType, Params = #{local_jid := ArcJID}) ->
    Env = env_vars(HostType, ArcJID),
    mam_encoder:encode_message(Params, Env, db_mappings()).

-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_message,
    Fields = column_names(db_mappings()),
    {Query, Fields2} = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields2, Query),
    ok.

%% Removal logic
-spec remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, owner := jid:jid()},
    Extra :: gen_hook:extra().
remove_archive(Acc, #{archive_id := ArcID}, #{host_type := HostType}) ->
    mongoose_rdbms:execute_successfully(HostType, mam_archive_remove, [ArcID]),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_domain_api:remove_domain_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    F = fun() ->
            case gen_mod:get_module_opt(HostType, ?MODULE, delete_domain_limit) of
                infinity -> remove_domain_all(HostType, Domain);
                Limit -> remove_domain_batch(HostType, Domain, Limit)
            end,
            Acc
        end,
    mongoose_domain_api:remove_domain_wrapper(Acc, F, ?MODULE).

-spec remove_domain_all(host_type(), jid:lserver()) -> any().
remove_domain_all(HostType, Domain) ->
    {atomic, _} = mongoose_rdbms:sql_transaction(HostType, fun() ->
        mongoose_rdbms:execute_successfully(HostType, mam_remove_domain, [Domain]),
        mongoose_rdbms:execute_successfully(HostType, mam_remove_domain_prefs, [Domain]),
        mongoose_rdbms:execute_successfully(HostType, mam_remove_domain_users, [Domain])
    end).

-spec remove_domain_batch(host_type(), jid:lserver(), non_neg_integer()) -> any().
remove_domain_batch(HostType, Domain, Limit) ->
    DeleteQueries = [mam_incr_remove_domain, mam_incr_remove_domain_prefs, mam_incr_remove_domain_users],
    TotalDeleted = mod_mam_utils:incremental_delete_domain(HostType, Domain, Limit, DeleteQueries, 0),
    ?LOG_INFO(#{what => mam_domain_removal_completed, total_records_deleted => TotalDeleted,
                domain => Domain, host_type => HostType}).

%% GDPR logic
extract_gdpr_messages(Env, ArcID) ->
    Filters = [{equal, user_id, ArcID}],
    lookup_query(lookup, Env, Filters, asc, all).

%% Lookup logic
-spec lookup_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {ok, mod_mam:lookup_result()},
    Params :: mam_iq:lookup_params(),
    Extra :: gen_hook:extra().
lookup_messages(_Result, #{owner_jid := ArcJID} = Params, #{host_type := HostType}) ->
    Env = env_vars(HostType, ArcJID),
    ExdParams = mam_encoder:extend_lookup_params(Params, Env),
    Filter = mam_filter:produce_filter(ExdParams, lookup_fields()),
    {ok, mam_lookup:lookup(Env, Filter, ExdParams)}.

lookup_query(QueryType, Env, Filters, Order, OffsetLimit) ->
    mam_lookup_sql:lookup_query(QueryType, Env, Filters, Order, OffsetLimit).

assert_archive_id_provided(#{archive_id := ArcID}) when is_integer(ArcID) ->
    ok.
