%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_mam).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
-xep([{xep, 313}, {version, "1.1.0"}, {legacy_versions, ["0.5"]}]).
-xep([{xep, 424}, {version, "0.3.0"}]).

-include("mod_mam.hrl").
-include("mongoose_config_spec.hrl").

-type module_opts() :: gen_mod:module_opts().
-type module_map() :: gen_mod_deps:module_map().

%% ----------------------------------------------------------------------
%% Datetime types
%% Microseconds from 01.01.1970
-type unix_timestamp() :: non_neg_integer().

%% ----------------------------------------------------------------------
%% Other types
-type archive_behaviour()   :: roster | always | never.
-type message_id()          :: non_neg_integer().

-type archive_id()          :: non_neg_integer().

-type borders()             :: #mam_borders{}.

-type message_row() :: #{id := message_id(), jid := jid:jid(), packet := exml:element()}.
-type lookup_result() :: {TotalCount :: non_neg_integer() | undefined,
                          Offset :: non_neg_integer() | undefined,
                          MessageRows :: [message_row()]}.

-type lookup_result_map() :: #{total_count := TotalCount :: non_neg_integer() | undefined,
                               offset := Offset :: non_neg_integer() | undefined,
                               messages := MessageRows :: [message_row()],
                               is_complete => boolean()}.

%% Internal types
-type iterator_fun() :: fun(() -> {'ok', {_, _}}).
-type rewriter_fun() :: fun((JID :: jid:literal_jid())
                            -> jid:literal_jid()).
-type restore_option() :: {rewrite_jids, rewriter_fun() | [{binary(), binary()}]}
                        | new_message_ids.

-type preference() :: {DefaultMode :: archive_behaviour(),
                       AlwaysJIDs  :: [jid:literal_jid()],
                       NeverJIDs   :: [jid:literal_jid()]}.

-type archive_message_params() :: #{message_id := message_id(),
                                    archive_id := archive_id(),
                                    local_jid := jid:jid(),
                                    remote_jid := jid:jid(),
                                    source_jid := jid:jid(),
                                    origin_id := binary() | none,
                                    direction := atom(),
                                    packet := exml:element(),
                                    %% Only in mod_mam_pm
                                    is_groupchat => boolean(),
                                    %% Only in mod_mam_muc_rdbms_arch:retract_message/2
                                    sender_id => archive_id()}.

-export_type([rewriter_fun/0,
              borders/0,
              preference/0,
              archive_behaviour/0,
              iterator_fun/0,
              unix_timestamp/0,
              archive_id/0,
              lookup_result/0,
              lookup_result_map/0,
              message_row/0,
              message_id/0,
              restore_option/0,
              archive_message_params/0,
              mam_type/0
             ]).

-export([start/2, stop/1, config_spec/0, supported_features/0, deps/2, config_metrics/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec start(mongooseim:host_type(), module_opts()) -> any().
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> any().
stop(_HostType) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Items = maps:merge(common_config_items(), root_config_items()),
    #section{
       items = Items#{<<"pm">> => pm_config_spec(),
                      <<"muc">> => muc_config_spec()},
       defaults = #{<<"backend">> => rdbms,
                    <<"no_stanzaid_element">> => false,
                    <<"is_archivable_message">> => mod_mam_utils,
                    <<"send_message">> => mod_mam_utils,
                    <<"archive_chat_markers">> => false,
                    <<"message_retraction">> => true,
                    <<"full_text_search">> => true,
                    <<"cache_users">> => true,
                    <<"default_result_limit">> => 50,
                    <<"max_result_limit">> => 50,
                    <<"enforce_simple_queries">> => false}
      }.

pm_config_spec() ->
    #section{items = maps:merge(common_config_items(), pm_config_items()),
             defaults = #{<<"archive_groupchats">> => false,
                          <<"same_mam_id_for_peers">> => false}}.

muc_config_spec() ->
    #section{items = maps:merge(common_config_items(), muc_config_items()),
             defaults = #{<<"host">> => mod_muc:default_host()}}.

root_config_items() ->
    Cache = mongoose_user_cache:config_spec(),
    AsyncWriter = async_config_spec(),
    #{<<"cache">> => Cache#section{include = always},
      <<"async_writer">> => AsyncWriter#section{include = always}}.

common_config_items() ->
    #{%% General options
      <<"backend">> => #option{type = atom,
                               validate = {enum, [rdbms, cassandra, elasticsearch]}},
      <<"no_stanzaid_element">> => #option{type = boolean},
      <<"is_archivable_message">> => #option{type = atom,
                                             validate = module},
      <<"send_message">> => #option{type = atom,
                                    validate = module},
      <<"archive_chat_markers">> => #option{type = boolean},
      <<"message_retraction">> => #option{type = boolean},

      %% Common backend options
      <<"user_prefs_store">> => #option{type = atom,
                                        validate = {enum, [rdbms, cassandra, mnesia]}},
      <<"full_text_search">> => #option{type = boolean},

      %% RDBMS-specific options
      <<"cache_users">> => #option{type = boolean},

      %% Low-level options
      <<"default_result_limit">> => #option{type = integer,
                                            validate = non_negative},
      <<"enforce_simple_queries">> => #option{type = boolean},
      <<"delete_domain_limit">> => #option{type = int_or_infinity,
                                           validate = positive},
      <<"max_result_limit">> => #option{type = integer,
                                        validate = non_negative},
      <<"db_jid_format">> => #option{type = atom,
                                     validate = module},
      <<"db_message_format">> => #option{type = atom,
                                         validate = module},
      <<"extra_fin_element">> => #option{type = atom,
                                         validate = module},
      <<"extra_lookup_params">> => #option{type = atom,
                                           validate = module}
     }.

pm_config_items() ->
    #{<<"async_writer">> => async_config_spec(),
      <<"archive_groupchats">> => #option{type = boolean},
      <<"same_mam_id_for_peers">> => #option{type = boolean}}.

muc_config_items() ->
    #{<<"async_writer">> => async_config_spec(),
      <<"host">> => #option{type = string,
                            validate = subdomain_template,
                            process = fun mongoose_subdomain_utils:make_subdomain_pattern/1}}.

async_config_spec() ->
    #section{
       items = #{<<"enabled">> => #option{type = boolean},
                 <<"flush_interval">> => #option{type = integer, validate = non_negative},
                 <<"batch_size">> => #option{type = integer, validate = non_negative},
                 <<"pool_size">> => #option{type = integer, validate = non_negative}},
       defaults = #{<<"enabled">> => true,
                    <<"flush_interval">> => 2000,
                    <<"batch_size">> => 30,
                    <<"pool_size">> => 4 * erlang:system_info(schedulers_online)}
      }.

-spec deps(mongooseim:host_type(), module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    DepsWithPm = handle_nested_opts(pm, Opts, #{}),
    DepsWithPmAndMuc = handle_nested_opts(muc, Opts, DepsWithPm),

    [{DepMod, DepOpts, hard} || {DepMod, DepOpts} <- maps:to_list(DepsWithPmAndMuc)].

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-type mam_type() :: pm | muc.
-type mam_backend() :: rdbms | cassandra | elasticsearch.

-spec handle_nested_opts(mam_type(), module_opts(), module_map()) -> module_map().
handle_nested_opts(Key, RootOpts, Deps) ->
    case maps:find(Key, RootOpts) of
        error -> Deps;
        {ok, Opts} ->
            FullOpts = maps:merge(maps:without([pm, muc], RootOpts), Opts),
            parse_opts(Key, FullOpts, Deps)
    end.

-spec parse_opts(mam_type(), module_opts(), module_map()) -> module_map().
parse_opts(Type, Opts, Deps) ->
    %% Opts are merged root options with options inside pm or muc section
    CoreMod = mam_type_to_core_mod(Type),
    CoreModOpts = maps:with(valid_core_mod_opts(CoreMod), Opts),
    WithCoreDeps = add_dep(CoreMod, CoreModOpts, Deps),
    {Backend, BackendOpts} = maps:take(backend, Opts),
    WithPrefs = add_prefs_store_module(Backend, Type, Opts, WithCoreDeps),
    parse_backend_opts(Backend, Type, BackendOpts, WithPrefs).

-spec mam_type_to_core_mod(mam_type()) -> module().
mam_type_to_core_mod(pm) -> mod_mam_pm;
mam_type_to_core_mod(muc) -> mod_mam_muc.

%% Get a list of options to pass into the two modules.
%% They don't have to be defined in pm or muc sections, the root section is enough.
-spec valid_core_mod_opts(module()) -> [atom()].
valid_core_mod_opts(mod_mam_pm) ->
    [archive_groupchats, same_mam_id_for_peers] ++ common_opts();
valid_core_mod_opts(mod_mam_muc) ->
    [host] ++ common_opts().

common_opts() ->
    [async_writer,
     is_archivable_message,
     send_message,
     archive_chat_markers,
     extra_fin_element,
     extra_lookup_params,
     full_text_search,
     message_retraction,
     default_result_limit,
     max_result_limit,
     enforce_simple_queries,
     no_stanzaid_element].

-spec add_prefs_store_module(mam_backend(), mam_type(), module_opts(), module_map()) -> module_map().
add_prefs_store_module(Backend, Type, #{user_prefs_store := Store}, Deps) ->
    PrefsModule = prefs_module(Backend, Store),
    add_dep(PrefsModule, #{Type => true}, Deps);
add_prefs_store_module(_Backend, _Type, _Opts, Deps) ->
    Deps.

-spec parse_backend_opts(mam_backend(), mam_type(), module_opts(), module_map()) -> module_map().
parse_backend_opts(cassandra, Type, Opts, Deps) ->
    Opts1 = maps:with([db_message_format], Opts),
    add_dep(cassandra_arch_module(Type), maps:merge(arch_defaults(), Opts1), Deps);
parse_backend_opts(rdbms, Type, Opts, Deps) ->
    lists:foldl(fun(OptionGroup, DepsIn) -> add_rdbms_deps(OptionGroup, Type, Opts, DepsIn) end,
                Deps, [basic, user_cache, async_writer]);
parse_backend_opts(elasticsearch, Type, _Opts, Deps0) ->
    add_dep(elasticsearch_arch_module(Type), Deps0).

-spec add_rdbms_deps(basic | user_cache | async_writer,
                     mam_type(), module_opts(), module_map()) -> module_map().
add_rdbms_deps(basic, Type, Opts, Deps) ->
    Opts1 = maps:with([db_message_format, db_jid_format, delete_domain_limit], Opts),
    Deps1 = add_dep(rdbms_arch_module(Type), maps:merge(rdbms_arch_defaults(Type), Opts1), Deps),
    add_dep(mod_mam_rdbms_user, user_db_types(Type), Deps1);
add_rdbms_deps(user_cache, Type, #{cache_users := true, cache := CacheOpts}, Deps) ->
    Deps1 = case gen_mod:get_opt(module, CacheOpts, internal) of
                internal -> Deps;
                mod_cache_users -> add_dep(mod_cache_users, Deps)
            end,
    add_dep(mod_mam_cache_user, CacheOpts#{Type => true}, Deps1);
add_rdbms_deps(async_writer, Type, #{async_writer := AsyncOpts = #{enabled := true}}, Deps) ->
    Deps1 = add_dep(rdbms_arch_module(Type), #{no_writer => true}, Deps),
    add_dep(rdbms_async_arch_module(Type), AsyncOpts, Deps1);
add_rdbms_deps(_, _Type, _Opts, Deps) ->
    Deps.

% muc backend requires both pm and muc user DB to populate sender_id column
-spec user_db_types(mam_type()) -> module_opts().
user_db_types(pm) -> #{pm => true};
user_db_types(muc) -> #{pm => true, muc => true}.

cassandra_arch_module(pm) -> mod_mam_cassandra_arch;
cassandra_arch_module(muc) -> mod_mam_muc_cassandra_arch.

arch_defaults() -> #{db_message_format => mam_message_xml}.

rdbms_arch_defaults(pm) ->
    maps:merge(rdbms_arch_defaults(), #{db_jid_format => mam_jid_mini});
rdbms_arch_defaults(muc) ->
    maps:merge(rdbms_arch_defaults(), #{db_jid_format => mam_jid_rfc}).

rdbms_arch_defaults() ->
    #{db_message_format => mam_message_compressed_eterm,
      no_writer => false, delete_domain_limit => infinity}.

rdbms_arch_module(pm) -> mod_mam_rdbms_arch;
rdbms_arch_module(muc) -> mod_mam_muc_rdbms_arch.

rdbms_async_arch_module(pm) -> mod_mam_rdbms_arch_async;
rdbms_async_arch_module(muc) -> mod_mam_muc_rdbms_arch_async.

elasticsearch_arch_module(pm) -> mod_mam_elasticsearch_arch;
elasticsearch_arch_module(muc) -> mod_mam_muc_elasticsearch_arch.

prefs_module(rdbms, rdbms) -> mod_mam_rdbms_prefs;
prefs_module(cassandra, cassandra) -> mod_mam_cassandra_prefs;
prefs_module(_, mnesia) -> mod_mam_mnesia_prefs;
prefs_module(Backend, PrefsStore) ->
    error(#{what => invalid_mam_user_prefs_store,
            backend => Backend,
            user_prefs_store => PrefsStore}).

-spec add_dep(module(), module_map()) -> module_map().
add_dep(Dep, Deps) ->
    add_dep(Dep, #{}, Deps).

-spec add_dep(module(), module_opts(), module_map()) -> module_map().
add_dep(Dep, Opts, Deps) ->
    PrevOpts = maps:get(Dep, Deps, #{}),
    NewOpts = maps:merge(PrevOpts, Opts),
    maps:put(Dep, NewOpts, Deps).

config_metrics(Host) ->
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, [backend]).
