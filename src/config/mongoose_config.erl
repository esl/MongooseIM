-module(mongoose_config).

%% API
-export([start/0,
         stop/0,
         get_config_path/0,
         lookup_opt/1,
         get_opt/1,
         get_opt/2]).

%% Test API, do not use outside of test suites, options set here are not cleaned up by stop/0
-export([set_opt/2,
         unset_opt/1]).

%% Shell utilities intended for debugging and system inspection
-export([config_state/0,
         config_states/0]).

-ignore_xref([set_opt/2, unset_opt/1, config_state/0, config_states/0]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-type key() :: atom() | host_type_key() | host_type_or_global_key().
-type s2s_domain_key() :: {atom(), jid:lserver()}.
-type host_type_key() :: {atom() | s2s_domain_key(), mongooseim:host_type()}.
-type host_type_or_global_key() :: {shaper | access | acl, atom(), mongooseim:host_type() | global}.

-type value() :: atom()
               | binary()
               | integer()
               | string()
               | [value()]
               | tuple().

-export_type([key/0, value/0]).

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
    Opts = mongoose_config_parser:state_to_opts(State),
    [set_opt(Key, Value) || #local_config{key = Key, value = Value} <- Opts],
    ok.

-spec unset_opts(mongoose_config_parser:state()) -> ok.
unset_opts(State) ->
    Opts = mongoose_config_parser:state_to_opts(State),
    [unset_opt(Key) || #local_config{key = Key} <- Opts],
    ok.

-spec set_opt(key(), value()) -> ok.
set_opt(Key, Value) ->
    persistent_term:put({?MODULE, Key}, Value).

-spec unset_opt(key()) -> boolean().
unset_opt(Key) ->
    persistent_term:erase({?MODULE, Key}).

%% @doc Use instead of get_opt(Key, undefined)
-spec lookup_opt(key()) -> {ok, value()} | {error, not_found}.
lookup_opt(Key) ->
    try persistent_term:get({?MODULE, Key}) of
        Value -> {ok, Value}
    catch
        error:_ -> {error, not_found}
    end.

%% @doc Fails if the option does not exist
-spec get_opt(key()) -> value().
get_opt(Key) ->
    persistent_term:get({?MODULE, Key}).

-spec get_opt(key(), value()) -> value().
get_opt(Key, Default) ->
    persistent_term:get({?MODULE, Key}, Default).

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
