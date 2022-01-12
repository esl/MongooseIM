-module(mongoose_config_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_helper, [start_ejabberd/1,
                          start_ejabberd_with_config/2,
                          stop_ejabberd/0,
                          use_config_file/2,
                          copy/2,
                          data/2]).

all() ->
    [get_opt,
     lookup_opt,
     load_from_file,
     {group, cluster}].

groups() ->
    [
     {cluster, [], [cluster_load_from_file]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    [persistent_term:erase(Key) || {Key = {mongoose_config, _}, _Value} <- persistent_term:get()],
    persistent_term:erase(mongoose_config_state),
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

get_opt(_Config) ->
    ?assertError(badarg, mongoose_config:get_opt(get_me)),
    ?assertEqual(default_value, mongoose_config:get_opt(get_me, default_value)),
    mongoose_config:set_opt(get_me, you_got_me),
    ?assertEqual(you_got_me, mongoose_config:get_opt(get_me)),
    mongoose_config:set_opt(get_me, you_got_me_again),
    ?assertEqual(you_got_me_again, mongoose_config:get_opt(get_me)),
    ?assertEqual(you_got_me_again, mongoose_config:get_opt(get_me, default_value)),
    mongoose_config:unset_opt(get_me),
    ?assertError(badarg, mongoose_config:get_opt(get_me)),
    ?assertEqual(default_value, mongoose_config:get_opt(get_me, default_value)).

lookup_opt(_Config) ->
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt(look_me_up)),
    mongoose_config:set_opt(look_me_up, here_i_am),
    ?assertEqual({ok, here_i_am}, mongoose_config:lookup_opt(look_me_up)),
    mongoose_config:unset_opt(look_me_up),
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt(look_me_up)).

load_from_file(Config) ->
    use_config_file(Config, "mongooseim.minimal.toml"),
    ok = mongoose_config:start(),
    State = mongoose_config:config_state(),
    check_loaded_config(State),

    ok = mongoose_config:stop(),
    check_removed_config(),

    %% Try to stop it again
    {error, not_started} = mongoose_config:stop().

cluster_load_from_file(Config) ->
    SlaveNode = slave_node(Config),
    copy(data(Config, "mongooseim.minimal.toml"), data(Config, "mongooseim.toml")),

    %% Start clustered MongooseIM and check the loaded config
    {ok, _} = start_ejabberd_with_config(Config, "mongooseim.toml"),
    {ok, _} = start_remote_ejabberd_with_config(SlaveNode, Config, "mongooseim.toml"),
    maybe_join_cluster(SlaveNode),
    [State, State] = mongoose_config:config_states(),
    check_loaded_config(State),

    ok = stop_ejabberd(),
    stop_remote_ejabberd(SlaveNode),
    check_removed_config().

%%
%% Helpers
%%

check_loaded_config(State) ->
    Opts = lists:sort(mongoose_config_parser:state_to_opts(State)),
    ExpectedOpts = lists:sort(minimal_config_opts()),
    ?assertEqual(ExpectedOpts, Opts),
    [?assertEqual(Val, mongoose_config:get_opt(Key)) || {Key, Val} <- ExpectedOpts].

check_removed_config() ->
    Opts = minimal_config_opts(),
    ?assertError(badarg, mongoose_config:config_state()),
    [?assertError(badarg, mongoose_config:get_opt(Key)) || {Key, _} <- Opts].

minimal_config_opts() ->
    [{all_metrics_are_global, false},
     {default_server_domain, <<"localhost">>},
     {hide_service_name, false},
     {host_types, []},
     {hosts, [<<"localhost">>]},
     {language, <<"en">>},
     {listen, []},
     {loglevel, warning},
     {mongooseimctl_access_commands, []},
     {rdbms_server_type, generic},
     {registration_timeout, 600},
     {routing_modules, ejabberd_router:default_routing_modules()},
     {sm_backend, {mnesia, []}},
     {{auth, <<"localhost">>}, config_parser_helper:default_auth()},
     {{modules, <<"localhost">>}, #{}},
     {{replaced_wait_timeout, <<"localhost">>}, 2000}].

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
    rpc:call(RemoteNode, ejabberd_helper, start_ejabberd_with_config, [C, ConfigFile]).

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

