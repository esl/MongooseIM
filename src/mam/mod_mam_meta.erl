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

-type module_map() :: gen_mod_deps:module_map().

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
       items = Items#{<<"pm">> => #section{items = maps:merge(Items, pm_config_items()),
                                           format_items = map},
                      <<"muc">> => #section{items = maps:merge(Items, muc_config_items()),
                                            format_items = map},
                      <<"riak">> => riak_config_spec()},
       defaults = #{<<"backend">> => rdbms,
                    <<"cache_users">> => true,
                    <<"async_wrtier">> => []},
       format_items = map
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
      <<"async_writer">> => mod_mam_rdbms_arch_async:config_spec(),

      %% Low-level options
      <<"default_result_limit">> => #option{type = integer,
                                            validate = non_negative},
      <<"max_result_limit">> => #option{type = integer,
                                        validate = non_negative},
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
       format_items = map
      }.

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    DepsWithPm = handle_nested_opts(pm, Opts, #{}),
    DepsWithPmAndMuc = handle_nested_opts(muc, Opts, DepsWithPm),

    [{DepMod, DepOpts, hard} || {DepMod, DepOpts} <- maps:to_list(DepsWithPmAndMuc)].

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-type mam_type() :: pm | muc.
-type mam_backend() :: rdbms | riak | cassandra | elasticsearch.

-spec handle_nested_opts(mam_type(), gen_mod:module_opts(), module_map()) -> module_map().
handle_nested_opts(Key, RootOpts, Deps) ->
    case maps:find(Key, RootOpts) of
        error -> Deps;
        {ok, Opts} ->
            FullOpts = maps:merge(maps:without([pm, muc], RootOpts), Opts),
            parse_opts(Key, FullOpts, Deps)
    end.

-spec parse_opts(mam_type(), gen_mod:module_opts(), module_map()) -> module_map().
parse_opts(Type, Opts, Deps) ->
    %% Opts are merged root options with options inside pm or muc section
    CoreMod = mam_type_to_core_mod(Type),
    CoreModOpts = maps:with(valid_core_mod_opts(CoreMod), Opts),
    WithCoreDeps = add_dep(CoreMod, CoreModOpts, Deps),
    {Backend, BackendOpts} = maps:take(backend, Opts),
    WithPrefs = add_prefs_store_module(Backend, Type, Opts, WithCoreDeps),
    parse_backend_opts(Backend, Type, BackendOpts, WithPrefs).

-spec mam_type_to_core_mod(mam_type()) -> module().
mam_type_to_core_mod(pm) -> mod_mam;
mam_type_to_core_mod(muc) -> mod_mam_muc.

%% Get a list of options to pass into the two modules.
%% They don't have to be defined in pm or muc sections, the root section is enough.
-spec valid_core_mod_opts(module()) -> [atom()].
valid_core_mod_opts(mod_mam) ->
    [no_stanzaid_element, archive_groupchats, same_mam_id_for_peers] ++ common_opts();
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
     max_result_limit].

add_prefs_store_module(Backend, Type, #{user_prefs_store := Store}, Deps) ->
    PrefsModule = prefs_module(Backend, Store),
    add_dep(PrefsModule, #{Type => true}, Deps);
add_prefs_store_module(_Backend, _Type, _Opts, Deps) ->
    Deps.

-spec parse_backend_opts(mam_backend(), mam_type(), gen_mod:module_opts(), module_map()) ->
          module_map().
parse_backend_opts(cassandra, Type, Opts, Deps) ->
    Opts1 = maps:with([db_message_format, pool_name, simple], Opts),
    add_dep(cassandra_arch_module(Type), Opts1, Deps);
parse_backend_opts(riak, Type, Opts, Deps) ->
    Opts1 = maps:with([db_message_format, search_index, bucket_type], Opts),
    add_dep(mod_mam_riak_timed_arch_yz, Opts1#{Type => true}, Deps);
parse_backend_opts(rdbms, Type, Opts, Deps) ->
    lists:foldl(fun(OptionGroup, DepsIn) -> add_rdbms_deps(OptionGroup, Type, Opts, DepsIn) end,
                Deps, [basic, cache, simple, async]);
parse_backend_opts(elasticsearch, Type, _Opts, Deps0) ->
    add_dep(elasticsearch_arch_module(Type), Deps0).

% muc backend requires both pm and muc user DB to populate sender_id column
-spec user_db_types(mam_type()) -> [pm | muc].
user_db_types(pm) -> [pm];
user_db_types(muc) -> [pm, muc].

add_rdbms_deps(basic, Type, #{}, Deps) ->
    Deps1 = add_dep(rdbms_arch_module(Type), Deps),
    add_dep(mod_mam_rdbms_user, #{types => user_db_types(Type)}, Deps1);
add_rdbms_deps(cache, Type, #{cache_users := true, cache := CacheOpts}, Deps) ->
    Deps1 = case gen_mod:get_opt(module, CacheOpts, internal) of
                internal -> Deps;
                mod_cache_users -> add_dep(mod_cache_users, Deps)
            end,
    add_dep(mod_mam_cache_user, CacheOpts#{type => Type}, Deps1);
add_rdbms_deps(simple, Type, #{rdbms_message_format := simple}, Deps) ->
    add_dep(rdbms_arch_module(Type), rdbms_simple_opts(), Deps);
add_rdbms_deps(async, Type, #{async_writer := AsyncOpts = #{enabled := true}}, Deps) ->
    Deps1 = add_dep(rdbms_arch_module(Type), #{no_writer => true}, Deps),
    add_dep(mod_mam_rdbms_arch_async, #{Type => AsyncOpts}, Deps1);
add_rdbms_deps(_, _Type, _Opts, Deps) ->
    Deps.

cassandra_arch_module(pm) -> mod_mam_cassandra_arch;
cassandra_arch_module(muc) -> mod_mam_muc_cassandra_arch.

rdbms_arch_module(pm) -> mod_mam_rdbms_arch;
rdbms_arch_module(muc) -> mod_mam_muc_rdbms_arch.

elasticsearch_arch_module(pm) -> mod_mam_elasticsearch_arch;
elasticsearch_arch_module(muc) -> mod_mam_muc_elasticsearch_arch.

prefs_module(rdbms, rdbms) -> mod_mam_rdbms_prefs;
prefs_module(cassandra, cassandra) -> mod_mam_cassandra_prefs;
prefs_module(_, mnesia) -> mod_mam_mnesia_prefs;
prefs_module(Backend, PrefsStore) ->
    error(#{what => invalid_mam_user_prefs_store,
            backend => Backend,
            user_prefs_store => PrefsStore}).

-spec rdbms_simple_opts() -> list().
rdbms_simple_opts() -> [{db_jid_format, mam_jid_rfc}, {db_message_format, mam_message_xml}].

-spec add_dep(module(), module_map()) -> module_map().
add_dep(Dep, Deps) ->
    add_dep(Dep, #{}, Deps).

-spec add_dep(module(), gen_mod:module_opts(), module_map()) -> module_map().
add_dep(Dep, Opts, Deps) ->
    PrevOpts = maps:get(Dep, Deps, #{}),
    NewOpts = maps:merge(PrevOpts, Opts),
    maps:put(Dep, NewOpts, Deps).

config_metrics(Host) ->
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, [backend]).
