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

-type deps() :: #{module() => proplists:proplist()}.

-export([start/2, stop/1, deps/2]).

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

    CoreModOpts =
        lists:filtermap(
          fun(Key) ->
                  case proplists:lookup(Key, Opts) of
                      none -> false;
                      Opt -> {true, Opt}
                  end
          end, valid_core_mod_opts(CoreMod)),

    WithCoreDeps = add_dep(CoreMod, CoreModOpts, Deps),
    Backend = proplists:get_value(backend, Opts, odbc),

    parse_backend_opts(Backend, Type, Opts, WithCoreDeps).

-spec mam_type_to_core_mod(atom()) -> module().
mam_type_to_core_mod(pm) -> mod_mam;
mam_type_to_core_mod(muc) -> mod_mam_muc.

-spec valid_core_mod_opts(module()) -> [atom()].
valid_core_mod_opts(mod_mam) ->
    [add_archived_element,
     no_stanzaid_element,
     is_archivable_message,
     archive_chat_markers,
     extra_lookup_params,
     full_text_search,
     archive_groupchats];
valid_core_mod_opts(mod_mam_muc) ->
    [add_archived_element, is_archivable_message, host, extra_lookup_params, full_text_search].

-spec parse_backend_opts(odbc | cassandra | riak, Type :: pm | muc,
                         Opts :: proplists:proplist(), deps()) -> deps().
parse_backend_opts(cassandra, Type, Opts, Deps0) ->
    ModArch =
        case Type of
            pm -> mod_mam_cassandra_arch;
            muc -> mod_mam_muc_cassandra_arch
        end,

    Deps = add_dep(ModArch, Deps0),

    case proplists:get_value(user_prefs_store, Opts, false) of
        cassandra -> add_dep(mod_mam_cassandra_prefs, [Type], Deps);
        mnesia -> add_dep(mod_mam_mnesia_prefs, [Type], Deps);
        _ -> Deps
    end;

parse_backend_opts(riak, Type, Opts, Deps0) ->
    Deps = add_dep(mod_mam_riak_timed_arch_yz, [Type], Deps0),

    case proplists:get_value(user_prefs_store, Opts, false) of
        mnesia -> add_dep(mod_mam_mnesia_prefs, [Type], Deps);
        _ -> Deps
    end;

parse_backend_opts(odbc, Type, Opts0, Deps0) ->
    Opts = add_default_odbc_opts(Opts0),

    {ModODBCArch, ModAsyncWriter} =
        case Type of
            pm -> {mod_mam_odbc_arch, mod_mam_odbc_async_pool_writer};
            muc -> {mod_mam_muc_odbc_arch, mod_mam_muc_odbc_async_pool_writer}
        end,

    Deps1 = add_dep(ModODBCArch, [Type], Deps0),
    Deps = add_dep(mod_mam_odbc_user, [Type], Deps1),

    lists:foldl(
      pa:bind(fun parse_backend_opt/5, Type, ModODBCArch, ModAsyncWriter),
      Deps, Opts).


-spec normalize(proplists:proplist()) -> [{atom(), term()}].
normalize(Opts) ->
    lists:ukeysort(1, proplists:unfold(Opts)).


-spec add_dep(Dep :: module(), deps()) -> deps().
add_dep(Dep, Deps) ->
    add_dep(Dep, [], Deps).


-spec add_dep(Dep :: module(), Args :: proplists:proplist(), deps()) -> deps().
add_dep(Dep, Args, Deps) ->
    PrevArgs = maps:get(Dep, Deps, []),
    NewArgs = Args ++ PrevArgs,
    maps:put(Dep, NewArgs, Deps).


-spec add_default_odbc_opts(Opts :: proplists:proplist()) -> proplists:proplist().
add_default_odbc_opts(Opts) ->
    lists:foldl(
      fun({Key, _} = DefaultOpt, Acc) ->
              case proplists:lookup(Key, Opts) of
                  none -> [DefaultOpt | Acc];
                  _ -> Acc
              end
      end,
      Opts,
      [{cache_users, true}, {async_writer, true}]).


-spec parse_backend_opt(Type :: pm | muc, module(), module(),
                        Option :: {module(), term()}, deps()) -> deps().
parse_backend_opt(Type, ModODBCArch, ModAsyncWriter, Option, Deps) ->
    case Option of
        {cache_users, true} ->
            add_dep(mod_mam_cache_user, [Type], Deps);
        {user_prefs_store, odbc} ->
            add_dep(mod_mam_odbc_prefs, [Type], Deps);
        {user_prefs_store, mnesia} ->
            add_dep(mod_mam_mnesia_prefs, [Type], Deps);
        {odbc_message_format, simple} ->
            add_dep(ModODBCArch, odbc_simple_opts(), Deps);
        {async_writer, true} ->
            DepsWithNoWriter = add_dep(ModODBCArch, [no_writer], Deps),
            add_dep(ModAsyncWriter, [Type], DepsWithNoWriter);
        {async_writer_odbc_pool, PoolName} ->
            add_dep(ModAsyncWriter, [{odbc_pool, PoolName}], Deps);
        _ -> Deps
    end.

-spec odbc_simple_opts() -> list().
odbc_simple_opts() -> [{db_jid_format, mam_jid_rfc}, {db_message_format, mam_message_xml}].
