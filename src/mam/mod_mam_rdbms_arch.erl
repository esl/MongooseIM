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
-export([start/2, stop/1, supported_features/0]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-callback encode(term()) -> binary().
-callback decode(binary()) -> term().

-export([archive_size/4,
         archive_message/3,
         lookup_messages/3,
         remove_archive/4,
         remove_domain/3]).

-export([get_mam_pm_gdpr_data/3]).

%% Called from mod_mam_rdbms_async_pool_writer
-export([prepare_message/2, retract_message/2, prepare_insert/2]).

-ignore_xref([behaviour_info/1, remove_archive/4, remove_domain/3]).

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
start(HostType, _Opts) ->
    start_hooks(HostType),
    register_prepared_queries(),
    ok.

-spec stop(host_type()) -> ok.
stop(HostType) ->
    stop_hooks(HostType).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec get_mam_pm_gdpr_data(ejabberd_gen_mam_archive:mam_pm_gdpr_data(),
                           host_type(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(Acc, HostType,
                     #jid{luser = LUser, lserver = LServer} = ArcJID) ->
    case mod_mam:archive_id(LServer, LUser) of
        undefined ->
            Acc;
        ArcID ->
            Env = env_vars(HostType, ArcJID),
            {selected, Rows} = extract_gdpr_messages(Env, ArcID),
            [uniform_to_gdpr(row_to_uniform_format(Row, Env)) || Row <- Rows] ++ Acc
    end.

-spec uniform_to_gdpr(mod_mam:message_row()) -> tuple().
uniform_to_gdpr(#{id := MessID, jid := RemoteJID, packet := Packet}) ->
    {integer_to_binary(MessID), jid:to_binary(RemoteJID), exml:to_binary(Packet)}.

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

-spec start_hooks(host_type()) -> ok.
start_hooks(HostType) ->
    ejabberd_hooks:add(hooks(HostType)).

-spec stop_hooks(host_type()) -> ok.
stop_hooks(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)).

hooks(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, no_writer) of
        true ->
            [];
        false ->
            [{mam_archive_message, HostType, ?MODULE, archive_message, 50}]
    end ++
    [{mam_archive_size, HostType, ?MODULE, archive_size, 50},
     {mam_lookup_messages, HostType, ?MODULE, lookup_messages, 50},
     {mam_remove_archive, HostType, ?MODULE, remove_archive, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50},
     {get_mam_pm_gdpr_data, HostType, ?MODULE, get_mam_pm_gdpr_data, 50}].

%% ----------------------------------------------------------------------
%% SQL queries

register_prepared_queries() ->
    prepare_insert(insert_mam_message, 1),
    mongoose_rdbms:prepare(mam_archive_remove, mam_message, [user_id],
                           <<"DELETE FROM mam_message "
                             "WHERE user_id = ?">>),
    mongoose_rdbms:prepare(mam_remove_domain, mam_message, ['mam_server_user.server'],
                           <<"DELETE FROM mam_message "
                             "WHERE user_id IN "
                             "(SELECT id from mam_server_user WHERE server = ?)">>),
    mongoose_rdbms:prepare(mam_remove_domain_prefs, mam_config, ['mam_server_user.server'],
                           <<"DELETE FROM mam_config "
                             "WHERE user_id IN "
                             "(SELECT id from mam_server_user WHERE server = ?)">>),
    mongoose_rdbms:prepare(mam_remove_domain_users, mam_server_user, [server],
                           <<"DELETE FROM mam_server_user WHERE server = ?">>),
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
     #db_mapping{column = search_body, param = packet, format = search}].

lookup_fields() ->
    %% Describe each possible filtering option
    [#lookup_field{op = equal, column = user_id, param = archive_id, required = true},
     #lookup_field{op = ge, column = id, param = start_id},
     #lookup_field{op = le, column = id, param = end_id},
     #lookup_field{op = equal, column = remote_bare_jid, param = remote_bare_jid},
     #lookup_field{op = equal, column = remote_resource, param = remote_resource},
     #lookup_field{op = like, column = search_body, param = norm_search_text, value_maker = search_words}].

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
      has_message_retraction => mod_mam_utils:has_message_retraction(mod_mam, HostType),
      has_full_text_search => mod_mam_utils:has_full_text_search(mod_mam, HostType),
      db_jid_codec => db_jid_codec(HostType, ?MODULE),
      db_message_codec => db_message_codec(HostType, ?MODULE)}.

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
column_to_id(search_body) -> "s".

column_names(Mappings) ->
     [Column || #db_mapping{column = Column} <- Mappings].

%% ----------------------------------------------------------------------
%% Options

-spec db_jid_codec(host_type(), module()) -> module().
db_jid_codec(HostType, Module) ->
    gen_mod:get_module_opt(HostType, Module, db_jid_format).

-spec db_message_codec(host_type(), module()) -> module().
db_message_codec(HostType, Module) ->
    gen_mod:get_module_opt(HostType, Module, db_message_format).

-spec get_retract_id(exml:element(), env_vars()) -> none | mod_mam_utils:retraction_id().
get_retract_id(Packet, #{has_message_retraction := Enabled}) ->
    mod_mam_utils:get_retract_id(Enabled, Packet).

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec archive_size(Size :: integer(), HostType :: host_type(),
                   ArcId :: mod_mam:archive_id(), ArcJID :: jid:jid()) -> integer().
archive_size(Size, HostType, ArcID, ArcJID) when is_integer(Size) ->
    Filter = [{equal, user_id, ArcID}],
    Env = env_vars(HostType, ArcJID),
    Result = lookup_query(count, Env, Filter, unordered, all),
    mongoose_rdbms:selected_to_integer(Result).

-spec archive_message(_Result, host_type(), mod_mam:archive_message_params()) -> ok.
archive_message(_Result, HostType, Params = #{local_jid := ArcJID}) ->
    try
        assert_archive_id_provided(Params),
        Env = env_vars(HostType, ArcJID),
        do_archive_message(HostType, Params, Env),
        retract_message(HostType, Params, Env),
        ok
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
-spec remove_archive(Acc :: mongoose_acc:t(), HostType :: host_type(),
                     ArcID :: mod_mam:archive_id(),
                     RoomJID :: jid:jid()) -> mongoose_acc:t().
remove_archive(Acc, HostType, ArcID, _ArcJID) ->
    mongoose_rdbms:execute_successfully(HostType, mam_archive_remove, [ArcID]),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(), host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    {atomic, _} = mongoose_rdbms:sql_transaction(HostType, fun() ->
            mongoose_rdbms:execute_successfully(HostType, mam_remove_domain, [Domain]),
            mongoose_rdbms:execute_successfully(HostType, mam_remove_domain_prefs, [Domain]),
            mongoose_rdbms:execute_successfully(HostType, mam_remove_domain_users, [Domain])
        end),
    Acc.

%% GDPR logic
extract_gdpr_messages(Env, ArcID) ->
    Filters = [{equal, user_id, ArcID}],
    lookup_query(lookup, Env, Filters, asc, all).

%% Lookup logic
-spec lookup_messages(Result :: any(), HostType :: host_type(), Params :: map()) ->
                             {ok, mod_mam:lookup_result()}.
lookup_messages({error, _Reason}=Result, _HostType, _Params) ->
    Result;
lookup_messages(_Result, HostType, Params = #{owner_jid := ArcJID}) ->
    Env = env_vars(HostType, ArcJID),
    ExdParams = mam_encoder:extend_lookup_params(Params, Env),
    Filter = mam_filter:produce_filter(ExdParams, lookup_fields()),
    mam_lookup:lookup(Env, Filter, ExdParams).

lookup_query(QueryType, Env, Filters, Order, OffsetLimit) ->
    mam_lookup_sql:lookup_query(QueryType, Env, Filters, Order, OffsetLimit).

assert_archive_id_provided(#{archive_id := ArcID}) when is_integer(ArcID) ->
    ok.
