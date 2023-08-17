-module(mongoose_config).

%% API
-export([start/0,
         stop/0,
         get_config_path/0,
         lookup_opt/1,
         get_opt/2,
         get_opt/1]).

%% Test API, do not use outside of test suites
-export([set_opts/1,
         get_opts/0,
         erase_opts/0,
         set_opt/2,
         unset_opt/1]).

-ignore_xref([set_opts/1, get_opts/0, erase_opts/0, set_opt/2, unset_opt/1]).

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
    Opts = mongoose_config_parser:parse_file(Path),
    set_opts(maps:from_list(Opts)).

-spec stop() -> ok | {error, not_started}.
stop() ->
    case erase_opts() of
        true ->
            ok;
        false ->
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

-spec set_opts(#{key() => value()}) -> ok.
set_opts(Opts) ->
    persistent_term:put(?MODULE, Opts).

-spec get_opts() -> #{key() => value()}.
get_opts() ->
    persistent_term:get(?MODULE).

-spec erase_opts() -> boolean().
erase_opts() ->
    persistent_term:erase(?MODULE).

-spec set_opt(key() | key_path(), value()) -> ok.
set_opt(KeyPath, Value) when is_list(KeyPath) ->
    Opts = persistent_term:get(?MODULE),
    NewOpts = set_nested_opt(Opts, KeyPath, Value),
    persistent_term:put(?MODULE, NewOpts);
set_opt(Key, Value) ->
    set_opt([Key], Value).

-spec unset_opt(key() | key_path()) -> ok.
unset_opt(KeyPath) when is_list(KeyPath) ->
    Opts = persistent_term:get(?MODULE),
    NewOpts = unset_nested_opt(Opts, KeyPath),
    persistent_term:put(?MODULE, NewOpts);
unset_opt(Key) ->
    unset_opt([Key]).

%% @doc Use instead of get_opt(Key, undefined)
-spec lookup_opt(key() | key_path()) -> {ok, value()} | {error, not_found}.
lookup_opt(Key) ->
    try get_opt(Key) of
        Value -> {ok, Value}
    catch
        error:{badkey, _} -> {error, not_found} % missing map key
    end.

% @doc Returns Default if the option does not exist
-spec get_opt(key() | key_path(), value()) -> value().
get_opt(Key, Default) ->
    try
        get_opt(Key)
    catch
        error:{badkey, _} -> Default % missing map key
    end.

%% @doc Fails if the option does not exist
-spec get_opt(key() | key_path()) -> value().
get_opt(KeyPath) when is_list(KeyPath) ->
    Opts = persistent_term:get(?MODULE),
    lists:foldl(fun maps:get/2, Opts, KeyPath);
get_opt(Key) ->
    get_opt([Key]).

%% Internal functions

set_nested_opt(M, [Key], Value) ->
    M#{Key => Value};
set_nested_opt(M, [Key | Path], Value) ->
    M#{Key => set_nested_opt(maps:get(Key, M), Path, Value)}.

unset_nested_opt(M, [Key]) ->
    maps:remove(Key, M);
unset_nested_opt(M, [Key | Path]) ->
    M#{Key := unset_nested_opt(maps:get(Key, M), Path)}.
