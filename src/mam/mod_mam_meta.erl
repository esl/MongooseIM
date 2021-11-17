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

-export([start/2, stop/1, config_spec/0, supported_features/0, deps/2]).

-export([config_metrics/1]).

-include("mongoose_config_spec.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(_Host, _Opts) ->
    ok.


-spec stop(Host :: jid:server()) -> any().
stop(_Host) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Items = config_items(),
    #section{
       items = Items#{<<"pm">> => #section{items = maps:merge(Items, pm_config_items())},
                      <<"muc">> => #section{items = maps:merge(Items, muc_config_items())},
                      <<"riak">> => riak_config_spec()}
      }.

config_items() ->
    #{%% General options
      <<"backend">> => #option{type = atom,
                               validate = {enum, [rdbms, riak, cassandra, elasticsearch]}},
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
      <<"cache">> => mongoose_user_cache:config_spec(),
      <<"rdbms_message_format">> => #option{type = atom,
                                            validate = {enum, [simple, internal]}},
      <<"async_writer">> => #option{type = boolean},
      <<"flush_interval">> => #option{type = integer,
                                      validate = non_negative},
      <<"max_batch_size">> => #option{type = integer,
                                      validate = non_negative},

      %% Low-level options
      <<"default_result_limit">> => #option{type = integer,
                                            validate = non_negative},
      <<"max_result_limit">> => #option{type = integer,
                                        validate = non_negative},
      <<"async_writer_rdbms_pool">> => #option{type = atom,
                                               validate = pool_name},
      <<"db_jid_format">> => #option{type = atom,
                                     validate = module},
      <<"db_message_format">> => #option{type = atom,
                                         validate = module},
      <<"simple">> => #option{type = boolean},
      <<"extra_fin_element">> => #option{type = atom,
                                         validate = module},
      <<"extra_lookup_params">> => #option{type = atom,
                                           validate = module}
     }.

pm_config_items() ->
    #{<<"archive_groupchats">> => #option{type = boolean},
      <<"same_mam_id_for_peers">> => #option{type = boolean}}.

muc_config_items() ->
    #{<<"host">> => #option{type = string,
                            validate = subdomain_template,
                            process = fun mongoose_subdomain_utils:make_subdomain_pattern/1}}.

riak_config_spec() ->
    #section{
       items = #{<<"search_index">> => #option{type = binary,
                                               validate = non_empty},
                 <<"bucket_type">> => #option{type = binary,
                                              validate = non_empty}},
       format = none
      }.

-spec deps(_Host :: jid:server(), Opts :: proplists:proplist()) ->
                  gen_mod:deps_list().
deps(_Host, Opts0) ->
    Opts = normalize(Opts0),

    DepsWithPm = handle_nested_opts(pm, Opts, false, #{}),
    DepsWithPmAndMuc = handle_nested_opts(muc, Opts, false, DepsWithPm),

    [{Dep, Args, hard} || {Dep, Args} <- maps:to_list(DepsWithPmAndMuc)].

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
    %% Opts are merged root options with options inside pm or muc section
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

%% Get a list of options to pass into the two modules.
%% They don't required to be defined in pm or muc sections,
%% the root section is enough.
-spec valid_core_mod_opts(module()) -> [atom()].
valid_core_mod_opts(mod_mam) ->
    [no_stanzaid_element, archive_groupchats, same_mam_id_for_peers] ++ common_opts();
valid_core_mod_opts(mod_mam_muc) ->
    [host] ++ common_opts().

common_opts() ->
    [is_archivable_message,
     send_message,
     archive_chat_markers,
     extra_fin_element,
     extra_lookup_params,
     full_text_search,
     message_retraction,
     default_result_limit,
     max_result_limit].

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
    Opts1 = add_default_rdbms_opts(Opts0),
    Opts = add_rdbms_cache_opts(Opts1),

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
      end, Opts, [{async_writer, true}]).

add_rdbms_cache_opts(Opts) ->
    case {lists:keyfind(cache_users, 1, Opts), lists:keyfind(cache, 1, Opts)} of
        {{cache_users, false}, _} ->
            lists:keydelete(cache, 1, Opts);
        {{cache_users, true}, false} ->
            [{cache, []} | Opts];
        {false, false} ->
            [{cache, []} | Opts];
        {false, {cache, _}} ->
            Opts
    end.

-spec parse_rdbms_opt(Type :: pm | muc, module(), module(),
                      Option :: {module(), term()}, deps()) -> deps().
parse_rdbms_opt(Type, ModRDBMSArch, ModAsyncWriter, Option, Deps) ->
    case Option of
        {cache, CacheOpts} ->
            Deps1 = case gen_mod:get_opt(module, CacheOpts, internal) of
                        internal -> Deps;
                        mod_cache_users -> add_dep(mod_cache_users, Deps)
                    end,
            add_dep(mod_mam_cache_user, [Type | CacheOpts], Deps1);
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
