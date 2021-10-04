%%%----------------------------------------------------------------------
%%% File    : ejabberd_config.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Load config file
%%% Created : 14 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_config).
-author('alexey@process-one.net').

-export([start/0,
         load_file/1,
         add_global_option/2,
         get_global_option/1,
         get_global_option_or_default/2,
         add_local_option/2,
         get_local_option/1,
         get_local_option/2,
         del_local_option/1,
         get_local_option_or_default/2]).

-export([get_local_config/0,
         get_host_local_config/0,
         get_config_path/0]).

%% Helper function to get all options in the shell
-export([get_categorized_options/0]).

%% Introspection
-export([config_state/0]).
-export([config_states/0]).

-export([other_cluster_nodes/0]).

-ignore_xref([config_state/0, config_states/0, get_categorized_options/0,
              get_host_local_config/0, get_local_config/0]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-type host() :: any(). % TODO: specify this
-type state() :: mongoose_config_parser:state().
-type key() :: mongoose_config_parser:key().
-type value() :: mongoose_config_parser:value().

-type categorized_options() :: #{global_config => list(),
                                 local_config => list(),
                                 host_config => list()}.

-spec start() -> ok.
start() ->
    mnesia:create_table(config,
                        [{ram_copies, [node()]},
                         {storage_properties,
                          [{ets, [{read_concurrency, true}]}]},
                         {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    mnesia:create_table(local_config,
                        [{ram_copies, [node()]},
                         {storage_properties,
                          [{ets, [{read_concurrency, true}]}]},
                         {local_content, true},
                         {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
    Config = get_config_path(),
    ejabberd_config:load_file(Config),
    %% This start time is used by mod_last:
    add_local_option(node_start, {node_start, erlang:system_time(second)}),
    ok.


%% @doc Get the filename of the ejabberd configuration file.
%% The filename can be specified with: erl -config "/path/to/mongooseim.toml".
%% It can also be specified with the environment variable EJABBERD_CONFIG_PATH.
%% If not specified, the default value 'mongooseim.toml' is assumed.
-spec get_config_path() -> string().
get_config_path() ->
    DefaultPath = case os:getenv("EJABBERD_CONFIG_PATH") of
                      false ->
                          ?CONFIG_PATH;
                      Path ->
                          Path
                  end,

    application:get_env(mongooseim, config, DefaultPath).

%% @doc Load the ejabberd configuration file.
%% It also includes additional configuration files and replaces macros.
%% This function will crash if finds some error in the configuration file.
-spec load_file(File :: string()) -> ok.
load_file(File) ->
    State = mongoose_config_parser:parse_file(File),
    set_opts(State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process terms

-spec set_opts(state()) -> 'ok' | none().
set_opts(State) ->
    case mnesia:transaction(fun() -> do_set_opts(State) end) of
        {atomic, _} -> ok;
        {aborted, {no_exists, Table}} ->
            handle_table_does_not_exist_error(Table)
    end.

-spec do_set_opts(state()) -> 'ok' | none().
do_set_opts(State) ->
    Opts = mongoose_config_parser:state_to_opts(State),
    maybe_clean_global_opts(State),
    maybe_clean_local_opts(State),
    maybe_clean_acls_opts(State),
    lists:foreach(fun(R) -> mnesia:write(R) end, Opts).

maybe_clean_global_opts(State) ->
    case mongoose_config_parser:can_override(global, State) of
        true ->
            clean_global_opts();
        _ ->
            ok
    end.

maybe_clean_local_opts(State) ->
    case mongoose_config_parser:can_override(local, State) of
        true ->
            clean_local_opts();
        _ ->
            ok
    end.

maybe_clean_acls_opts(State) ->
    case mongoose_config_parser:can_override(acls, State) of
        true ->
            clean_acls_opts();
        _ ->
            ok
    end.

clean_global_opts() ->
    Ksg = mnesia:all_keys(config),
    lists:foreach(fun(K) -> mnesia:delete({config, K}) end, Ksg).

clean_local_opts() ->
    Ksl = mnesia:all_keys(local_config),
    Ksl2 = lists:delete(node_start, Ksl),
    lists:foreach(fun(K) -> mnesia:delete({local_config, K}) end, Ksl2).

clean_acls_opts() ->
    Ksa = mnesia:all_keys(acl),
    lists:foreach(fun(K) -> mnesia:delete({acl, K}) end, Ksa).

-spec add_global_option(Opt :: key(), Val :: value()) -> {atomic|aborted, _}.
add_global_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                           mnesia:write(#config{key   = Opt,
                                                value = Val})
                       end).


-spec add_local_option(Opt :: key(), Val :: value()) -> {atomic|aborted, _}.
add_local_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                           mnesia:write(#local_config{key   = Opt,
                                                      value = Val})
                       end).

-spec del_local_option(Opt :: key()) -> {atomic | aborted, _}.
del_local_option(Opt) ->
    mnesia:transaction(fun mnesia:delete/1, [{local_config, Opt}]).

-spec get_global_option(key()) -> value() | undefined.
get_global_option(Opt) ->
    case ets:lookup(config, Opt) of
        [#config{value = Val}] ->
            Val;
        _ ->
            undefined
    end.

-spec get_global_option_or_default(key(), value()) -> value().
get_global_option_or_default(Opt, DefaultValue) ->
    case ets:lookup(config, Opt) of
        [#config{value = Val}] ->
            Val;
        _ ->
            DefaultValue
    end.

-spec get_local_option(key()) -> value() | undefined.
get_local_option(Opt) ->
    case ets:lookup(local_config, Opt) of
        [#local_config{value = Val}] ->
            Val;
        _ ->
            undefined
    end.

-spec get_local_option(key(), host()) -> value() | undefined.
get_local_option(Opt, Host) ->
    case get_local_option({Opt, Host}) of
        undefined -> get_global_option(Opt);
        Val -> Val
    end.

-spec get_local_option_or_default(key(), value()) -> value().
get_local_option_or_default(Opt, Default) ->
    case get_local_option(Opt) of
        undefined ->
            Default;
        Value ->
            Value
    end.

handle_table_does_not_exist_error(Table) ->
    MnesiaDirectory = mnesia:system_info(directory),
    Msg = <<"Error reading Mnesia database spool files:~n"
            "The Mnesia database couldn't read the spool file for the table.~n"
            "ejabberd needs read and write access in the directory.~n"
            "Maybe the problem is a change in the computer hostname,~n"
            "or a change in the Erlang node name.~n"
            "Check the ejabberd guide for details about changing the~n"
            "computer hostname or Erlang node name.~n">>,
    ?LOG_ERROR(#{what => error_reading_mnesia_db, text => Msg,
                 table => Table, directory => MnesiaDirectory, node => node()}),
    exit("Error reading Mnesia database").

%% match all hosts
-spec get_host_local_config() -> [{local_config, {term(), jid:server()}, term()}].
get_host_local_config() ->
    mnesia:dirty_match_object({local_config, {'_', '_'}, '_'}).

-spec get_local_config() -> [{local_config, term(), term()}].
get_local_config() ->
    Keys = lists:filter(fun is_not_host_specific/1, mnesia:dirty_all_keys(local_config)),
    lists:flatten(lists:map(fun(Key) ->
                                mnesia:dirty_read(local_config, Key)
                            end,
                            Keys)).

-spec is_not_host_specific(atom()
                           | {atom(), jid:server()}
                           | {atom(), atom(), atom()}) -> boolean().
is_not_host_specific(Key) when is_atom(Key) ->
    true;
is_not_host_specific({Key, Host}) when is_atom(Key), is_binary(Host) ->
    false;
is_not_host_specific({Key, PoolType, PoolName})
  when is_atom(Key), is_atom(PoolType), is_atom(PoolName) ->
    true.

-spec get_global_config() -> [{config, term(), term()}].
get_global_config() ->
    mnesia:dirty_match_object(config, {config, '_', '_'}).

%% @doc Returns current all options in memory, grouped by category.
-spec get_categorized_options() -> categorized_options().
get_categorized_options() ->
    #{global_config => get_global_config(),
      local_config => get_local_config(),
      host_config => get_host_local_config()}.

%% @doc Returns configs on disc and in memory for this node for inspection.
config_state() ->
    ConfigFile = get_config_path(),
    State = mongoose_config_parser:parse_file(ConfigFile),
    #{mongoose_node => node(),
      config_file => ConfigFile,
      loaded_categorized_options => get_categorized_options(),
      ondisc_config_state => State}.

config_states() ->
    config_states(all_cluster_nodes()).

%% @doc Returns config states from all nodes in cluster
%% State from the local node comes as head of a list
config_states(Nodes) ->
    {S, F} = rpc:multicall(Nodes, ?MODULE, config_state, [], 30000),
    case F of
        [] ->
            S;
        [_|_] ->
            erlang:error(#{issue => config_state_failed,
                           cluster_nodes => Nodes,
                           failed_nodes => F})
    end.

all_cluster_nodes() ->
    [node()|other_cluster_nodes()].

-spec other_cluster_nodes() -> [node()].
other_cluster_nodes() ->
    lists:filter(fun is_mongooseim_node/1, nodes()).

-spec is_mongooseim_node(node()) -> boolean().
is_mongooseim_node(Node) ->
    Apps = rpc:call(Node, application, which_applications, []),
    lists:keymember(mongooseim, 1, Apps).
