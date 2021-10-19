-module(mongoose_config_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_helper, [start_ejabberd/1,
                          stop_ejabberd/0,
                          use_config_file/2,
                          copy/2,
                          data/2]).

all() ->
    [smoke,
     {group, cluster}].

groups() ->
    [
     {cluster, [], [cluster_smoke]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

init_per_group(cluster, Config) ->
    start_slave_node(Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(cluster, Config) ->
    stop_slave_node(Config),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

%%
%% Tests
%%

smoke(Config) ->
    % when
    start_ejabberd_with_config(Config, "mongooseim.minimal.toml"),
    % then
    ?assert(lists:keymember(mongooseim, 1, application:which_applications())),
    % cleanup
    ok = stop_ejabberd().

cluster_smoke(C) ->
    SlaveNode = slave_node(C),
    copy(data(C, "mongooseim.minimal.toml"), data(C, "mongooseim.toml")),
    {ok, _} = start_ejabberd_with_config(C, "mongooseim.toml"),
    {ok, _} = start_remote_ejabberd_with_config(SlaveNode, C, "mongooseim.toml"),
    maybe_join_cluster(SlaveNode),
    [State, State] = mongoose_config:config_states(),
    % cleanup
    ok = stop_ejabberd(),
    stop_remote_ejabberd(SlaveNode),
    ok.

%%
%% Helpers
%%

start_ejabberd_with_config(Config, ConfigFile) ->
    use_config_file(Config, ConfigFile),
    {ok, _} = start_ejabberd(Config).

start_slave_node(Config) ->
    SlaveNode = do_start_slave_node(),
    [{slave_node, SlaveNode}|Config].

do_start_slave_node() ->
    Opts = [{monitor_master, true},
            {boot_timeout, 15}, %% in seconds
            {init_timeout, 10}, %% in seconds
            {startup_timeout, 10}], %% in seconds
    {ok, SlaveNode} = ct_slave:start(slave_name(), Opts),
    {ok, CWD} = file:get_cwd(),
    ok = rpc:call(SlaveNode, file, set_cwd, [CWD]),
    %% Tell the remote node where to find the SUITE code
    %% Be aware, that P1 likes to put there stuff into
    %% /usr/lib/erlang/lib/
    %% So add_paths is NOT enough here
    ok = rpc:call(SlaveNode, code, add_pathsa, [lists:reverse(code_paths())]),
    check_that_p1_tls_is_correct(SlaveNode),
    SlaveNode.

check_that_p1_tls_is_correct(SlaveNode) ->
    ?assertEqual(fast_tls:module_info(md5),
                 rpc:call(SlaveNode, fast_tls, module_info, [md5])).

stop_slave_node(Config) ->
    ct_slave:stop(slave_node(Config)),
    ok.

slave_node(Config) ->
    get_required_config(slave_node, Config).

get_required_config(Key, Config) ->
    case proplists:get_value(Key, Config) of
        undefined ->
            ct:fail({get_required_config_failed, Key});
        Value ->
            Value
    end.

slave_name() ->
    'mim_slave'.

start_remote_ejabberd_with_config(RemoteNode, C, ConfigFile) ->
    rpc:call(RemoteNode, ?MODULE, start_ejabberd_with_config, [C, ConfigFile]).

stop_remote_ejabberd(SlaveNode) ->
    rpc:call(SlaveNode, ejabberd_helper, stop_ejabberd, []).

code_paths() ->
    [filename:absname(Path) || Path <- code:get_path()].

maybe_join_cluster(SlaveNode) ->
    Result = rpc:call(SlaveNode, ejabberd_admin, join_cluster,
                      [atom_to_list(node())]),
    case Result of
        {ok, _} ->
            ok;
        {already_joined, _} ->
            ok
    end.
