-module(ejabberd_helper).

-export([start_ejabberd/1,
         stop_ejabberd/0,
         use_config_file/2,
         start_ejabberd_with_config/2,
         copy/2,
         data/2,
         priv/2,
         suite_priv/2]).


-spec start_ejabberd(any()) -> 'ok' | {error, any()}.
start_ejabberd(_Config) ->
    {ok, _} = ejabberd:start().

-spec stop_ejabberd() -> 'ok' | {error, any()}.
stop_ejabberd() ->
    application:stop(mongooseim).

-spec use_config_file(any(), file:name_all()) -> ok.
use_config_file(Config, ConfigFile) ->
    %% To avoid "NIF library already loaded (reload disallowed since OTP 20)."
    %% error on fast_tls restart, always keep it running
    application:ensure_all_started(fast_tls),
    application:load(mongooseim),
    DataDir = proplists:get_value(data_dir, Config),
    ConfigPath = filename:join([DataDir, ConfigFile]),
    application:set_env(mongooseim, config, ConfigPath).

-spec start_ejabberd_with_config(any(), file:name_all()) -> ok.
start_ejabberd_with_config(Config, ConfigFile) ->
    use_config_file(Config, ConfigFile),
    {ok, _} = start_ejabberd(Config).

copy(Src, Dst) ->
    {ok, _} = file:copy(Src, Dst).

data(Config, Path) ->
    rel(Config, data_dir, Path).

%% This is the private config of a particular testcase and run, i.e.
%% .../mongooseim/apps/ejabberd/logs/ct_run.test@x4.local.2014-10-08_16.33.48/apps.ejabberd.ejabberd_config_SUITE.split_config.logs/run.2014-10-08_16.33.48/log_private/
priv(Config, Path) ->
    rel(Config, priv_dir, Path).

rel(Config, To, Path) ->
    Dir = proplists:get_value(To, Config),
    filename:join([Dir, Path]).


%% This is the suite-level private config for a test run.
%% .../mongooseim/apps/ejabberd/logs/ct_run.test@x4.local.2014-10-08_16.33.48/log_private/
suite_priv(Config, PathSuffix) ->
    Dir = proplists:get_value(priv_dir, Config),
    FourDirsUp = lists:foldl(fun (_, Path) -> filename:dirname(Path) end,
        Dir, lists:seq(1,4)),
    %% filename:join/1 strips trailing slashes... cool, ain't it?
    preserve_trailing_slash(lists:last(PathSuffix),
        filename:join([FourDirsUp, "log_private",
            PathSuffix])).


preserve_trailing_slash($/, Path) -> Path ++ [$/];
preserve_trailing_slash(__, Path) -> Path.

