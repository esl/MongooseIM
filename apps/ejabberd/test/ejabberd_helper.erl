-module(ejabberd_helper).

-export([start_ejabberd/1,
         stop_ejabberd/0,
         use_config_file/2,
         start_ejabberd_with_config/2]).


-spec start_ejabberd(any()) -> 'ok' | {error, any()}.
start_ejabberd(_Config) ->
    {ok, _} = ejabberd:start().

-spec stop_ejabberd() -> 'ok' | {error, any()}.
stop_ejabberd() ->
    application:stop(ejabberd).

-spec use_config_file(any(), file:name_all()) -> ok.
use_config_file(Config, ConfigFile) ->
    application:load(ejabberd),
    DataDir = proplists:get_value(data_dir, Config),
    ConfigPath = filename:join([DataDir, ConfigFile]),
    application:set_env(ejabberd, config, ConfigPath).

-spec start_ejabberd_with_config(any(), file:name_all()) -> ok.
start_ejabberd_with_config(Config, ConfigFile) ->
    use_config_file(Config, ConfigFile),
    {ok, _} = start_ejabberd(Config).

