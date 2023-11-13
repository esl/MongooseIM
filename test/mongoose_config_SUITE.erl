-module(mongoose_config_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_helper, [start_ejabberd_with_config/2,
                          use_config_file/2,
                          copy/2,
                          data/2]).

all() ->
    [{group, opts},
     {group, cluster}].

groups() ->
    [
     {opts, [get_opt,
             lookup_opt,
             get_path,
             lookup_path,
             set_short_path,
             set_long_path,
             unset_path,
             load_from_file]},
     {cluster, [], [cluster_load_from_file]}
    ].

init_per_suite(Config) ->
    mnesia:start(), %% TODO Remove this call when possible (We still need it for s2s)
    {ok, _} = application:ensure_all_started(jid),
    mongoose_config:set_opts(#{}),
    Config.

end_per_suite(_Config) ->
    mongoose_config:erase_opts(),
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
    ?assertError({badkey, get_me}, mongoose_config:get_opt(get_me)),
    ?assertEqual(default_value, mongoose_config:get_opt(get_me, default_value)),
    mongoose_config:set_opt(get_me, you_got_me),
    ?assertEqual(you_got_me, mongoose_config:get_opt(get_me)),
    mongoose_config:set_opt(get_me, you_got_me_again),
    ?assertEqual(you_got_me_again, mongoose_config:get_opt(get_me)),
    ?assertEqual(you_got_me_again, mongoose_config:get_opt(get_me, default_value)),
    mongoose_config:unset_opt(get_me),
    ?assertError({badkey, get_me}, mongoose_config:get_opt(get_me)),
    ?assertEqual(default_value, mongoose_config:get_opt(get_me, default_value)).

lookup_opt(_Config) ->
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt(look_me_up)),
    mongoose_config:set_opt(look_me_up, here_i_am),
    ?assertEqual({ok, here_i_am}, mongoose_config:lookup_opt(look_me_up)),
    mongoose_config:unset_opt(look_me_up),
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt(look_me_up)).

get_path(_Config) ->
    ?assertError({badkey, root}, mongoose_config:get_opt([root])),
    ?assertError({badkey, root}, mongoose_config:get_opt([root, branch])),
    mongoose_config:set_opt(root, #{branch => leaf}),
    ?assertEqual(#{branch => leaf}, mongoose_config:get_opt([root])),
    ?assertEqual(leaf, mongoose_config:get_opt([root, branch])),
    ?assertError({badmap, leaf}, mongoose_config:get_opt([root, branch, leaf])),
    mongoose_config:unset_opt(root),
    ?assertError({badkey, root}, mongoose_config:get_opt([root])).

lookup_path(_Config) ->
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt([basement])),
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt([basement, floor])),
    mongoose_config:set_opt(basement, #{floor => roof}),
    ?assertEqual({ok, #{floor => roof}}, mongoose_config:lookup_opt([basement])),
    ?assertEqual({ok, roof}, mongoose_config:lookup_opt([basement, floor])),
    ?assertError({badmap, roof}, mongoose_config:lookup_opt([basement, floor, roof])),
    mongoose_config:unset_opt(basement),
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt([basement])).

set_short_path(_Config) ->
    mongoose_config:set_opt([a], 1),
    ?assertEqual(1, mongoose_config:get_opt(a)),
    mongoose_config:set_opt([a], 2),
    ?assertEqual(2, mongoose_config:get_opt(a)),
    ?assertError({badmap, 2}, mongoose_config:set_opt([a, b], c)),
    ?assertEqual(2, mongoose_config:get_opt(a)).

set_long_path(_Config) ->
    ?assertError({badkey, one}, mongoose_config:set_opt([one, two, three], 4)),
    mongoose_config:set_opt([one], #{}),
    ?assertError({badkey, two}, mongoose_config:set_opt([one, two, three], 4)),
    mongoose_config:set_opt([one, two], #{}),
    mongoose_config:set_opt([one, two, three], 4),
    ?assertEqual(#{two => #{three => 4}}, mongoose_config:get_opt(one)),
    mongoose_config:set_opt([one, two], 3),
    ?assertEqual(#{two => 3}, mongoose_config:get_opt(one)).

unset_path(_Config) ->
    mongoose_config:set_opt(foo, #{bar => #{baz => boom}}),
    ?assertEqual(#{bar => #{baz => boom}}, mongoose_config:get_opt(foo)),
    ?assertError({badmap, boom}, mongoose_config:unset_opt([foo, bar, baz, boom])),
    mongoose_config:unset_opt([foo, bar, baz]),
    ?assertEqual(#{bar => #{}}, mongoose_config:get_opt(foo)), % empty map is not removed
    mongoose_config:unset_opt([foo, bar, baz]), % no error for a non-existing key
    ?assertEqual(#{bar => #{}}, mongoose_config:get_opt(foo)),
    mongoose_config:unset_opt([foo]),
    ?assertError({badkey, foo}, mongoose_config:get_opt(foo)),
    ?assertError({badkey, foo}, mongoose_config:unset_opt([foo, bar])),
    mongoose_config:unset_opt([foo]). % no error for a non-existing key

load_from_file(Config) ->
    use_config_file(Config, "mongooseim.minimal.toml"),
    ok = mongoose_config:start(),
    Opts = mongoose_config:get_opts(),
    check_loaded_config(Opts),

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
    check_loaded_config(mongoose_config:get_opts()),
    check_loaded_config(rpc:call(SlaveNode, mongoose_config, get_opts, [])),

    ok = mongooseim:stop(),
    stop_remote_ejabberd(SlaveNode),
    check_removed_config().

%%
%% Helpers
%%

check_loaded_config(Opts) ->
    ?assertEqual(minimal_config_opts(), Opts).

check_removed_config() ->
    ?assertError(badarg, mongoose_config:get_opts()).

minimal_config_opts() ->
    #{all_metrics_are_global => false,
      default_server_domain => <<"localhost">>,
      hide_service_name => false,
      host_types => [],
      hosts => [<<"localhost">>],
      internal_databases => #{mnesia => #{}},
      language => <<"en">>,
      listen => [],
      loglevel => warning,
      outgoing_pools => [],
      rdbms_server_type => generic,
      registration_timeout => 600,
      routing_modules => mongoose_router:default_routing_modules(),
      services => #{},
      sm_backend => mnesia,
      component_backend => mnesia,
      s2s_backend => mnesia,
      {auth, <<"localhost">>} => config_parser_helper:default_auth(),
      {modules, <<"localhost">>} => #{},
      {replaced_wait_timeout, <<"localhost">>} => 2000,
      {s2s, <<"localhost">>} => config_parser_helper:default_s2s()}.

start_slave_node(Config) ->
    SlaveNode = do_start_slave_node(),
    [{slave_node, SlaveNode}|Config].

do_start_slave_node() ->
    Opts = [{monitor_master, true},
            {boot_timeout, 15}, %% in seconds
            {init_timeout, 10}, %% in seconds
            {startup_timeout, 10}], %% in seconds
    {ok, SlaveNode} = ct_slave:start(slave_name(), Opts),
    rpc:call(SlaveNode, mnesia, start, []), %% TODO remove this call when possible
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
    rpc:call(SlaveNode, mongooseim, stop, []).

code_paths() ->
    [filename:absname(Path) || Path <- code:get_path()].

maybe_join_cluster(SlaveNode) ->
    Result = rpc:call(SlaveNode, mongoose_server_api, join_cluster,
                      [atom_to_list(node())]),
    case Result of
        {ok, _} ->
            ok;
        {already_joined, _} ->
            ok
    end.

