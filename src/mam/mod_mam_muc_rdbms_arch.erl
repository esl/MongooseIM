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

-export([get_mam_muc_gdpr_data/3]).

%% Called from mod_mam_muc_rdbms_async_pool_writer
-export([prepare_message/2, retract_message/2, prepare_insert/2]).
-export([extend_params_with_sender_id/2]).

-ignore_xref([behaviour_info/1, remove_archive/4, remove_domain/3]).

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
-type host_type() :: mongooseim:host_type().

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(host_type(), _) -> ok.
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

-spec get_mam_muc_gdpr_data(ejabberd_gen_mam_archive:mam_pm_gdpr_data(),
                            host_type(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(Acc, HostType, #jid{luser = LUser, lserver = LServer} = _UserJID) ->
    case mod_mam:archive_id(LServer, LUser) of
        undefined ->
            Acc;
        SenderID ->
            %% We don't know the real room JID here, use FakeEnv
            FakeEnv = env_vars(HostType, jid:make(<<>>, <<>>, <<>>)),
            {selected, Rows} = extract_gdpr_messages(HostType, SenderID),
            [mam_decoder:decode_muc_gdpr_row(Row, FakeEnv) || Row <- Rows] ++ Acc
    end.

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

-spec start_hooks(host_type()) -> ok.
start_hooks(HostType) ->
    ejabberd_hooks:add(hooks(HostType)).

-spec stop_hooks(host_type()) -> ok.
stop_hooks(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)).

hooks(HostType) ->
    NoWriter = gen_mod:get_module_opt(HostType, ?MODULE, no_writer, false),
    case NoWriter of
        true ->
            [];
        false ->
            [{mam_muc_archive_message, HostType, ?MODULE, archive_message, 50}]
    end ++
    [{mam_muc_archive_size, HostType, ?MODULE, archive_size, 50},
     {mam_muc_lookup_messages, HostType, ?MODULE, lookup_messages, 50},
     {mam_muc_remove_archive, HostType, ?MODULE, remove_archive, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50},
     {get_mam_muc_gdpr_data, HostType, ?MODULE, get_mam_muc_gdpr_data, 50}].

%% ----------------------------------------------------------------------
%% SQL queries

register_prepared_queries() ->
    prepare_insert(insert_mam_muc_message, 1),
    mongoose_rdbms:prepare(mam_muc_archive_remove, mam_muc_message, [room_id],
                           <<"DELETE FROM mam_muc_message "
                             "WHERE room_id = ?">>),
    mongoose_rdbms:prepare(mam_muc_remove_domain, mam_muc_message, ['mam_server_user.server'],
                           <<"DELETE FROM mam_muc_message "
                             "WHERE room_id IN (SELECT id FROM mam_server_user where server = ?)">>),
    mongoose_rdbms:prepare(mam_muc_remove_domain_users, mam_server_user, [server],
                           <<"DELETE FROM mam_server_user WHERE server = ?">>),
    mongoose_rdbms:prepare(mam_muc_make_tombstone, mam_muc_message, [message, room_id, id],
                           <<"UPDATE mam_muc_message SET message = ?, search_body = '' "
                             "WHERE room_id = ? AND id = ?">>),
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(1),
    mongoose_rdbms:prepare(mam_muc_select_messages_to_retract_on_origin_id, mam_muc_message,
                           [room_id, sender_id, origin_id],
                           <<"SELECT ", LimitMSSQL/binary,
                             " id, message FROM mam_muc_message"
                             " WHERE room_id = ? AND sender_id = ? "
                             " AND origin_id = ?"
                             " ORDER BY id DESC ", LimitSQL/binary>>),
    mongoose_rdbms:prepare(mam_muc_select_messages_to_retract_on_stanza_id, mam_muc_message,
                           [room_id, sender_id, id],
                           <<"SELECT ", LimitMSSQL/binary,
                             " origin_id, message FROM mam_muc_message"
                             " WHERE room_id = ? AND sender_id = ? "
                             " AND id = ?"
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

-spec env_vars(host_type(), jid:jid()) -> env_vars().
env_vars(HostType, ArcJID) ->
    %% Please, minimize the usage of the host field.
    %% It's only for passing into RDBMS.
    #{host_type => HostType,
      archive_jid => ArcJID,
      table => mam_muc_message,
      index_hint_fn => fun index_hint_sql/1,
      columns_sql_fn => fun columns_sql/1,
      column_to_id_fn => fun column_to_id/1,
      lookup_fn => fun lookup_query/5,
      decode_row_fn => fun row_to_uniform_format/2,
      has_message_retraction => mod_mam_utils:has_message_retraction(mod_mam_muc, HostType),
      has_full_text_search => mod_mam_utils:has_full_text_search(mod_mam_muc, HostType),
      db_jid_codec => db_jid_codec(HostType, ?MODULE),
      db_message_codec => db_message_codec(HostType, ?MODULE)}.

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

-spec db_jid_codec(host_type(), module()) -> module().
db_jid_codec(HostType, Module) ->
    gen_mod:get_module_opt(HostType, Module, db_jid_format, mam_jid_rfc).

-spec db_message_codec(host_type(), module()) -> module().
db_message_codec(HostType, Module) ->
    gen_mod:get_module_opt(HostType, Module, db_message_format, mam_message_compressed_eterm).

-spec get_retract_id(exml:element(), env_vars()) -> none | mod_mam_utils:retraction_id().
get_retract_id(Packet, #{has_message_retraction := Enabled}) ->
    mod_mam_utils:get_retract_id(Enabled, Packet).

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec archive_size(Size :: integer(), HostType :: mongooseim:host_type(),
                   ArcId :: mod_mam:archive_id(), ArcJID :: jid:jid()) -> integer().
archive_size(Size, HostType, ArcID, ArcJID) when is_integer(Size) ->
    Filter = [{equal, room_id, ArcID}],
    Env = env_vars(HostType, ArcJID),
    Result = lookup_query(count, Env, Filter, unordered, all),
    mongoose_rdbms:selected_to_integer(Result).

extend_params_with_sender_id(HostType, Params = #{remote_jid := SenderJID}) ->
    BareSenderJID = jid:to_bare(SenderJID),
    SenderID = mod_mam:archive_id_int(HostType, BareSenderJID),
    Params#{sender_id => SenderID}.

-spec archive_message(_Result, HostType :: mongooseim:host_type(),
                      mod_mam:archive_message_params()) -> ok.
archive_message(_Result, HostType, Params0 = #{local_jid := ArcJID}) ->
    try
        Params = extend_params_with_sender_id(HostType, Params0),
        Env = env_vars(HostType, ArcJID),
        do_archive_message(HostType, Params, Env),
        retract_message(HostType, Params, Env),
        ok
    catch error:Reason:StackTrace ->
              ?LOG_ERROR(#{what => archive_message_failed,
                           host_type => HostType, mam_params => Params0,
                           reason => Reason, stacktrace => StackTrace}),
              erlang:raise(error, Reason, StackTrace)
    end.

do_archive_message(HostType, Params, Env) ->
    Row = mam_encoder:encode_message(Params, Env, db_mappings()),
    {updated, 1} = mongoose_rdbms:execute_successfully(HostType, insert_mam_muc_message, Row).

%% Retraction logic
%% Called after inserting a new message
-spec retract_message(mongooseim:host_type(), mod_mam:archive_message_params()) -> ok.
retract_message(HostType, #{local_jid := ArcJID} = Params)  ->
    Env = env_vars(HostType, ArcJID),
    retract_message(HostType, Params, Env).

-spec retract_message(mongooseim:host_type(), mod_mam:archive_message_params(), env_vars()) -> ok.
retract_message(HostType, #{archive_id := ArcID, sender_id := SenderID,
                            packet := Packet}, Env) ->
    case get_retract_id(Packet, Env) of
        none -> ok;
        RetractionId ->
            Info = get_retraction_info(HostType, ArcID, SenderID, RetractionId, Env),
            make_tombstone(HostType, ArcID, RetractionId, Info, Env)
    end.

get_retraction_info(HostType, ArcID, SenderID, RetractionId, Env) ->
    {selected, Rows} =
        execute_select_messages_to_retract(HostType, ArcID, SenderID, RetractionId),
    mam_decoder:decode_retraction_info(Env, Rows, RetractionId).

make_tombstone(_HostType, ArcID, RetractionId, skip, _Env) ->
    ?LOG_INFO(#{what => make_tombstone_failed,
                text => <<"Message to retract was not found">>,
                user_id => ArcID, retraction_context => RetractionId});
make_tombstone(HostType, ArcID, _RetractionId,
               RetractionInfo = #{message_id := MessID},
               #{archive_jid := ArcJID} = Env) ->
    Tombstone = mod_mam_utils:tombstone(RetractionInfo, ArcJID),
    TombstoneData = mam_encoder:encode_packet(Tombstone, Env),
    execute_make_tombstone(HostType, TombstoneData, ArcID, MessID).

execute_select_messages_to_retract(HostType, ArcID, SenderID, {origin_id, OriginID}) ->
    mongoose_rdbms:execute_successfully(HostType, mam_muc_select_messages_to_retract_on_origin_id,
                                      [ArcID, SenderID, OriginID]);
execute_select_messages_to_retract(HostType, ArcID, SenderID, {stanza_id, BinStanzaId}) ->
    StanzaId = mod_mam_utils:external_binary_to_mess_id(BinStanzaId),
    mongoose_rdbms:execute_successfully(HostType, mam_muc_select_messages_to_retract_on_stanza_id,
                                      [ArcID, SenderID, StanzaId]).

execute_make_tombstone(HostType, TombstoneData, ArcID, MessID) ->
    mongoose_rdbms:execute_successfully(HostType, mam_muc_make_tombstone,
                                      [TombstoneData, ArcID, MessID]).

%% Insert logic
-spec prepare_message(mongooseim:host_type(), mod_mam:archive_message_params()) -> list().
prepare_message(HostType, Params = #{local_jid := ArcJID}) ->
    Env = env_vars(HostType, ArcJID),
    mam_encoder:encode_message(Params, Env, db_mappings()).

-spec prepare_insert(Name :: atom(), NumRows :: pos_integer()) -> ok.
prepare_insert(Name, NumRows) ->
    Table = mam_muc_message,
    Fields = column_names(db_mappings()),
    {Query, Fields2} = rdbms_queries:create_bulk_insert_query(Table, Fields, NumRows),
    mongoose_rdbms:prepare(Name, Table, Fields2, Query),
    ok.

%% Removal logic
-spec remove_archive(Acc :: mongoose_acc:t(), HostType :: mongooseim:host_type(),
                     ArcID :: mod_mam:archive_id(),
                     ArcJID :: jid:jid()) -> mongoose_acc:t().
remove_archive(Acc, HostType, ArcID, _ArcJID) ->
    mongoose_rdbms:execute_successfully(HostType, mam_muc_archive_remove, [ArcID]),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    SubHosts = get_subhosts(HostType, Domain),
    {atomic, _} = mongoose_rdbms:sql_transaction(HostType, fun() ->
            [remove_domain_trans(HostType, SubHost) || SubHost <- SubHosts]
        end),
    Acc.

remove_domain_trans(HostType, MucHost) ->
    mongoose_rdbms:execute_successfully(HostType, mam_muc_remove_domain, [MucHost]),
    mongoose_rdbms:execute_successfully(HostType, mam_muc_remove_domain_users, [MucHost]).

get_subhosts(HostType, Domain) ->
    MucHostPattern = gen_mod:get_module_opt(HostType, mod_muc, host,
                                            mod_muc:default_host()),
    LightHostPattern = gen_mod:get_module_opt(HostType, mod_muc_light, host,
                                              mod_muc_light:default_host()),
    MucHost = mongoose_subdomain_utils:get_fqdn(MucHostPattern, Domain),
    LightHost = mongoose_subdomain_utils:get_fqdn(LightHostPattern, Domain),
    lists:usort([MucHost, LightHost]).

%% GDPR logic
extract_gdpr_messages(HostType, SenderID) ->
    mongoose_rdbms:execute_successfully(HostType, mam_muc_extract_gdpr_messages, [SenderID]).

%% Lookup logic
-spec lookup_messages(Result :: any(), HostType :: mongooseim:host_type(), Params :: map()) ->
                             {ok, mod_mam:lookup_result()}.
lookup_messages({error, _Reason} = Result, _HostType, _Params) ->
    Result;
lookup_messages(_Result, HostType, Params = #{owner_jid := ArcJID}) ->
    Env = env_vars(HostType, ArcJID),
    ExdParams = mam_encoder:extend_lookup_params(Params, Env),
    Filter = mam_filter:produce_filter(ExdParams, lookup_fields()),
    mam_lookup:lookup(Env, Filter, ExdParams).

lookup_query(QueryType, Env, Filters, Order, OffsetLimit) ->
    mam_lookup_sql:lookup_query(QueryType, Env, Filters, Order, OffsetLimit).
