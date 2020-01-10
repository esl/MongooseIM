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

-module(mod_mam_meta).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-type deps() :: #{module() => proplists:proplist()}.

-export([start/2, stop/1, deps/2, get_mam_module_configuration/3, get_mam_module_opt/4]).

-export([config_metrics/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(_Host, _Opts) ->
    ok.


-spec stop(Host :: jid:server()) -> any().
stop(_Host) ->
    ok.


-spec deps(_Host :: jid:server(), Opts :: proplists:proplist()) ->
                  gen_mod:deps_list().
deps(_Host, Opts0) ->
    Opts = normalize(Opts0),

    DepsWithPm = handle_nested_opts(pm, Opts, [], #{}),
    DepsWithPmAndMuc = handle_nested_opts(muc, Opts, false, DepsWithPm),

    [{Dep, Args, hard} || {Dep, Args} <- maps:to_list(DepsWithPmAndMuc)].

get_mam_module_configuration(Host, MamModule, DefaultValue) ->
    %% Modules' configuration is stored in 2 different places:
    %%
    %%   * ejabberd_modules ETS table - managed by the gen_mod module.
    %%     initialised on module startup but can be changed runtime via
    %%     gen_mod interfaces. removed when module is stopped.
    %%
    %%   * local_config mnesia table  - managed by ejabberd_config, but
    %%     it's only gen_mod changing stored configuration of the modules.
    %%     changes are done in the next way: configuration is stored when
    %%     module is started, removed - when stopped, updated on module
    %%     restart.
    %%
    %% None of the MAM modules changes its configuration dynamically via
    %% gen_mod interfaces and also (theoretically) modules can be stopped
    %% using gen_mod:stop_module_keep_config/2 interface, so local_config
    %% mnesia table is more preferable source of the configuration.
    Modules = ejabberd_config:get_local_option(modules, Host),
    case proplists:get_value(MamModule, Modules) of
        undefined ->
            case proplists:get_value(?MODULE, Modules) of
                undefined -> DefaultValue;
                MamMetaParams ->
                    Deps = deps(Host, MamMetaParams),
                    case lists:keyfind(MamModule, 1, Deps) of
                        {MamModule, Params, _} -> Params;
                        _ -> DefaultValue
                    end
            end;
        Params -> Params
    end.

get_mam_module_opt(Host, MamModule, Opt, DefaultValue) ->
    case get_mam_module_configuration(Host, MamModule, undefined) of
        undefined -> DefaultValue;
        Configuration -> proplists:get_value(Opt, Configuration, DefaultValue)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec handle_nested_opts(Key :: atom(), RootOpts :: proplists:proplist(),
                         Default :: term(), deps()) -> deps().
handle_nested_opts(Key, RootOpts, Default, Deps) ->
    case proplists:get_value(Key, RootOpts, Default) of
        false -> Deps;
        Opts0 ->
            Opts = normalize(Opts0),
            FullOpts = lists:ukeymerge(1, Opts, RootOpts),
            parse_opts(Key, FullOpts, Deps)
    end.


-spec parse_opts(Type :: pm | muc, Opts :: proplists:proplist(), deps()) -> deps().
parse_opts(Type, Opts, Deps) ->
    CoreMod = mam_type_to_core_mod(Type),
    CoreModOpts = filter_opts(Opts, valid_core_mod_opts(CoreMod)),
    WithCoreDeps = add_dep(CoreMod, CoreModOpts, Deps),
    Backend = proplists:get_value(backend, Opts, rdbms),
    parse_backend_opts(Backend, Type, Opts, WithCoreDeps).

-spec mam_type_to_core_mod(atom()) -> module().
mam_type_to_core_mod(pm) -> mod_mam;
mam_type_to_core_mod(muc) -> mod_mam_muc.

filter_opts(Opts, ValidOpts) ->
    lists:filtermap(
        fun(Key) ->
            case proplists:lookup(Key, Opts) of
                none -> false;
                Opt -> {true, Opt}
            end
        end, ValidOpts).

-spec valid_core_mod_opts(module()) -> [atom()].
valid_core_mod_opts(mod_mam) ->
    [
     no_stanzaid_element,
     is_archivable_message,
     archive_chat_markers,
     extra_lookup_params,
     full_text_search,
     archive_groupchats,
     default_result_limit,
     max_result_limit
    ];
valid_core_mod_opts(mod_mam_muc) ->
    [
     is_archivable_message,
     archive_chat_markers,
     host,
     extra_lookup_params,
     full_text_search,
     default_result_limit,
     max_result_limit
    ].

-spec parse_backend_opts(rdbms | cassandra | riak | elasticsearch, Type :: pm | muc,
                         Opts :: proplists:proplist(), deps()) -> deps().
parse_backend_opts(cassandra, Type, Opts, Deps0) ->
    ModArch =
        case Type of
            pm -> mod_mam_cassandra_arch;
            muc -> mod_mam_muc_cassandra_arch
        end,

    Opts1 = filter_opts(Opts, [db_message_format, pool_name, simple]),
    Deps = add_dep(ModArch, Opts1, Deps0),

    case proplists:get_value(user_prefs_store, Opts, false) of
        cassandra -> add_dep(mod_mam_cassandra_prefs, [Type], Deps);
        mnesia -> add_dep(mod_mam_mnesia_prefs, [Type], Deps);
        _ -> Deps
    end;

parse_backend_opts(riak, Type, Opts, Deps0) ->
    Opts1 = filter_opts(Opts, [db_message_format, search_index, bucket_type]),
    Deps = add_dep(mod_mam_riak_timed_arch_yz, [Type | Opts1], Deps0),

    case proplists:get_value(user_prefs_store, Opts, false) of
        mnesia -> add_dep(mod_mam_mnesia_prefs, [Type], Deps);
        _ -> Deps
    end;

parse_backend_opts(rdbms, Type, Opts0, Deps0) ->
    Opts = add_default_rdbms_opts(Opts0),

    {ModRDBMSArch, ModAsyncWriter} =
        case Type of
            pm -> {mod_mam_rdbms_arch, mod_mam_rdbms_async_pool_writer};
            muc -> {mod_mam_muc_rdbms_arch, mod_mam_muc_rdbms_async_pool_writer}
        end,

    Deps1 = add_dep(ModRDBMSArch, [Type], Deps0),
    Deps = add_dep(mod_mam_rdbms_user, user_db_types(Type), Deps1),

    lists:foldl(
      pa:bind(fun parse_rdbms_opt/5, Type, ModRDBMSArch, ModAsyncWriter),
      Deps, Opts);

parse_backend_opts(elasticsearch, Type, Opts, Deps0) ->
    ModArch =
        case Type of
            pm -> mod_mam_elasticsearch_arch;
            muc -> mod_mam_muc_elasticsearch_arch
        end,

    Deps = add_dep(ModArch, Deps0),

    case proplists:get_value(user_prefs_store, Opts, false) of
        mnesia -> add_dep(mod_mam_mnesia_prefs, [Type], Deps);
        _ -> Deps
    end.

% muc backend requires both pm and muc user DB to populate sender_id column
-spec user_db_types(pm | muc) -> [pm | muc].
user_db_types(pm) -> [pm];
user_db_types(muc) -> [pm, muc].

-spec normalize(proplists:proplist()) -> [{atom(), term()}].
normalize(Opts) ->
    lists:ukeysort(1, proplists:unfold(Opts)).


-spec add_dep(Dep :: module(), deps()) -> deps().
add_dep(Dep, Deps) ->
    add_dep(Dep, [], Deps).


-spec add_dep(Dep :: module(), Args :: proplists:proplist(), deps()) -> deps().
add_dep(Dep, Args, Deps) ->
    PrevArgs = maps:get(Dep, Deps, []),
    NewArgs = lists:usort(Args ++ PrevArgs),
    maps:put(Dep, NewArgs, Deps).


-spec add_default_rdbms_opts(Opts :: proplists:proplist()) -> proplists:proplist().
add_default_rdbms_opts(Opts) ->
    lists:foldl(
      fun({Key, _} = DefaultOpt, Acc) ->
              case proplists:lookup(Key, Opts) of
                  none -> [DefaultOpt | Acc];
                  _ -> Acc
              end
      end,
      Opts,
      [{cache_users, true}, {async_writer, true}]).


-spec parse_rdbms_opt(Type :: pm | muc, module(), module(),
                        Option :: {module(), term()}, deps()) -> deps().
parse_rdbms_opt(Type, ModRDBMSArch, ModAsyncWriter, Option, Deps) ->
    case Option of
        {cache_users, true} ->
            add_dep(mod_mam_cache_user, [Type], Deps);
        {user_prefs_store, rdbms} ->
            add_dep(mod_mam_rdbms_prefs, [Type], Deps);
        {user_prefs_store, mnesia} ->
            add_dep(mod_mam_mnesia_prefs, [Type], Deps);
        {rdbms_message_format, simple} ->
            add_dep(ModRDBMSArch, rdbms_simple_opts(), Deps);
        {async_writer, true} ->
            DepsWithNoWriter = add_dep(ModRDBMSArch, [no_writer], Deps),
            add_dep(ModAsyncWriter, [Type], DepsWithNoWriter);
        {async_writer_rdbms_pool, PoolName} ->
            add_dep(ModAsyncWriter, [{rdbms_pool, PoolName}], Deps);
        _ -> Deps
    end.

-spec rdbms_simple_opts() -> list().
rdbms_simple_opts() -> [{db_jid_format, mam_jid_rfc}, {db_message_format, mam_message_xml}].

config_metrics(Host) ->
    OptsToReport = [{backend, rdbms}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
