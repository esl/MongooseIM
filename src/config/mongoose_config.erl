-module(mongoose_config).

%% API
-export([start/0,
         stop/0,
         get_config_path/0,
         lookup_opt/1,
         get_opt/2,
         get_opt/1]).

%% Test API, do not use outside of test suites, options set here are not cleaned up by stop/0
-export([set_opt/2,
         unset_opt/1]).

%% Shell utilities intended for debugging and system inspection
-export([config_state/0,
         config_states/0]).

-ignore_xref([set_opt/2, unset_opt/1, config_state/0, config_states/0]).

-include("mongoose.hrl").

-type key() :: atom() | host_type_key().
-type host_type_key() :: {atom(), mongooseim:host_type_or_global()}.

%% Top-level key() followed by inner_key() for each of the nested maps
-type key_path() :: [key() | inner_key()].
-type inner_key() :: atom() | binary() | integer() | string() | tuple().

-type value() :: atom() | binary() | integer() | string() | [value()] | tuple() | map().

-export_type([host_type_key/0, key/0, key_path/0, value/0]).

-spec start() -> ok.
start() ->
    Path = get_config_path(),
    State = mongoose_config_parser:parse_file(Path),
    persistent_term:put(mongoose_config_state, State),
    set_opts(State).

-spec stop() -> ok | {error, not_started}.
stop() ->
    try persistent_term:get(mongoose_config_state) of
        State ->
            unset_opts(State),
            persistent_term:erase(mongoose_config_state),
            ok
    catch
        _:_ ->
            {error, not_started}
    end.

%% @doc Get the filename of the ejabberd configuration file.
%% The filename can be specified with: erl -config "/path/to/mongooseim.toml".
%% It can also be specified with the environment variable EJABBERD_CONFIG_PATH.
%% If not specified, the default value 'mongooseim.toml' is assumed.
-spec get_config_path() -> string().
get_config_path() ->
    DefaultPath = case os:getenv("EJABBERD_CONFIG_PATH") of
                      false -> ?CONFIG_PATH;
                      Path -> Path
                  end,
    application:get_env(mongooseim, config, DefaultPath).

-spec set_opts(mongoose_config_parser:state()) -> ok.
set_opts(State) ->
    Opts = mongoose_config_parser:get_opts(State),
    lists:foreach(fun({Key, Value}) -> set_opt(Key, Value) end, Opts).

-spec unset_opts(mongoose_config_parser:state()) -> ok.
unset_opts(State) ->
    Opts = mongoose_config_parser:get_opts(State),
    lists:foreach(fun unset_opt/1, proplists:get_keys(Opts)).

-spec set_opt(key() | key_path(), value()) -> ok.
set_opt([Key], Value) ->
    set_opt(Key, Value);
set_opt([Key | Rest], Value) ->
    set_opt(Key, set_nested_opt(get_opt(Key), Rest, Value));
set_opt(Key, Value) ->
    persistent_term:put({?MODULE, Key}, Value).

-spec unset_opt(key() | key_path()) -> ok.
unset_opt([Key]) ->
    unset_opt(Key);
unset_opt([Key | Rest]) ->
    set_opt(Key, unset_nested_opt(get_opt(Key), Rest));
unset_opt(Key) ->
    persistent_term:erase({?MODULE, Key}),
    ok.

%% @doc Use instead of get_opt(Key, undefined)
-spec lookup_opt(key() | key_path()) -> {ok, value()} | {error, not_found}.
lookup_opt(Key) ->
    try get_opt(Key) of
        Value -> {ok, Value}
    catch
        error:badarg -> {error, not_found}; % missing persistent term
        error:{badkey, _} -> {error, not_found} % missing map key
    end.

% @doc Returns Default if the option does not exist
-spec get_opt(key() | key_path(), value()) -> value().
get_opt(Key, Default) ->
    try
        get_opt(Key)
    catch
        error:badarg -> Default; % missing persistent term
        error:{badkey, _} -> Default % missing map key
    end.

%% @doc Fails if the option does not exist
-spec get_opt(key() | key_path()) -> value().
get_opt([Key | Rest]) ->
    Config = persistent_term:get({?MODULE, Key}),
    lists:foldl(fun maps:get/2, Config, Rest);
get_opt(Key) ->
    persistent_term:get({?MODULE, Key}).

-spec config_state() -> mongoose_config_parser:state().
config_state() ->
    persistent_term:get(mongoose_config_state).

-spec config_states() -> [mongoose_config_parser:state()].
config_states() ->
    config_states(mongoose_cluster:all_cluster_nodes()).

-spec config_states([node()]) -> [mongoose_config_parser:state()].
%% @doc Returns config states from all nodes in cluster
%% State from the local node comes as head of a list
config_states(Nodes) ->
    {States, FailedNodes} = rpc:multicall(Nodes, ?MODULE, config_state, [], 30000),
    case FailedNodes of
        [] ->
            States;
        [_|_] ->
            erlang:error(#{issue => config_state_failed,
                           cluster_nodes => Nodes,
                           failed_nodes => FailedNodes})
    end.

%% Internal functions

set_nested_opt(M, [Key], Value) ->
    M#{Key => Value};
set_nested_opt(M, [Key | Path], Value) ->
    M#{Key => set_nested_opt(maps:get(Key, M), Path, Value)}.

unset_nested_opt(M, [Key]) ->
    maps:remove(Key, M);
unset_nested_opt(M, [Key | Path]) ->
    M#{Key := unset_nested_opt(maps:get(Key, M), Path)}.
