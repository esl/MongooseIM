-module(ejabberd_helper).

-export([start_ejabberd/1,
         stop_ejabberd/0,
         use_config_file/2]).

-export([ensure_all_started/1]).

-spec start_ejabberd(any()) -> 'ok' | {error, any()}.
start_ejabberd(_Config) ->
    {ok, Deps} = application:get_key(ejabberd, applications),
    start_deps(Deps),
    ok = application:start(ejabberd).

start_deps([]) -> ok;
start_deps([Dep | Deps]) ->
    case application:start(Dep) of
        {error, {already_started, Dep}} -> start_deps(Deps);
        {error, {not_started, NewDep}} -> start_deps([NewDep, Dep | Deps]);
        ok -> start_deps(Deps)
    end.

ensure_all_started(App) ->
    do_ensure_all_started(App, []).

do_ensure_all_started(App, AccDeps) ->
    case application:start(App) of
        ok ->
            {ok, [App | AccDeps]};
        {error, {already_started, _Dep}} ->
            {ok, AccDeps};
        {error, {not_started, Dep}} ->
            {ok, Acc2} = do_ensure_all_started(Dep, AccDeps),
            do_ensure_all_started(App, Acc2)
    end.

-spec stop_ejabberd() -> 'ok' | {error, any()}.
stop_ejabberd() ->
    application:stop(ejabberd).

-spec use_config_file(any(), file:name_all()) -> ok.
use_config_file(Config, ConfigFile) ->
    application:load(ejabberd),
    DataDir = proplists:get_value(data_dir, Config),
    ConfigPath = filename:join([DataDir, ConfigFile]),
    application:set_env(ejabberd, config, ConfigPath).
