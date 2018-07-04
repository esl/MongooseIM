-module(ejabberd_config_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(ne(A, B), ?assertNot(A == B)).

-import(ejabberd_helper, [start_ejabberd/1,
                          stop_ejabberd/0,
                          use_config_file/2,
                          copy/2,
                          data/2,
                          suite_priv/2]).

all() ->
    [smoke,
     {group, reload_local},
     {group, reload_cluster},
     {group, odbc_pools},
     split_config].

groups() ->
    [{reload_local, [], [coalesce_multiple_local_config_options,
                         add_a_module,
                         delete_a_module,
                         reload_a_module]},
     {reload_cluster, [], [cluster_smoke,
                           change_module_option_with_node_param_opts,
                           change_module_option_with_node_specific_mods,
                           module_deps_work_correctly_with_reload_cluster]},
     {odbc_pools, [], [odbc_server_no_pools,
                          odbc_server_pools]}
    ].

init_per_suite(Config) ->
    stringprep:start(),
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(module_deps_work_correctly_with_reload_cluster, _Config) ->
    % cleanup
    stop_ejabberd(),
    meck:unload();
end_per_testcase(_TestCase, _Config) ->
    ok.

init_per_group(reload_cluster, Config) ->
    start_slave_node(Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(reload_cluster, Config) ->
    stop_slave_node(Config),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

%%
%% Tests
%%

odbc_server_no_pools(Config) ->
    FileName = get_ejabberd_cfg(Config, "ejabberd.odbc_no_pools.cfg"),
    try ejabberd_config:load_file(FileName) of
        _ -> ct:fail(success_without_pools)
    catch
        exit:Reason ->
            ?eq(Reason, no_odbc_pools)
    end.

odbc_server_pools(Config) ->
    FileName = get_ejabberd_cfg(Config, "ejabberd.odbc_pools.cfg"),
    try ejabberd_config:load_file(FileName) of
        _ -> ok
    catch
        _:_ ->
            ct:fail(failed_with_pools)
    end.

smoke(Config) ->
    % when
    start_ejabberd_with_config(Config, "ejabberd.default.cfg"),
    % then
    ?assert(lists:keymember(mongooseim, 1, application:which_applications())),
    % cleanup
    ok = stop_ejabberd().

coalesce_multiple_local_config_options(_Config) ->
    F = fun mongoose_config_parser:group_host_changes/1,
    ?eq(coalesced_modules_section(), F(multiple_modules_sections())).

add_a_module(C) ->
    % given a running server with a specific module off
    copy(data(C, "ejabberd.default.cfg"), data(C, "ejabberd.cfg")),
    start_ejabberd_with_config(C, "ejabberd.cfg"),
    ?eq(false, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
    % when adding the module to the configuration
    copy(data(C, "ejabberd.with_mod_offline.cfg"), data(C, "ejabberd.cfg")),
    ejabberd_config:reload_local(),
    % then the new module gets started
    ?eq(true, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
    % cleanup
    ok = stop_ejabberd().

delete_a_module(C) ->
    % given a running server with a specific module on
    copy(data(C, "ejabberd.with_mod_offline.cfg"), data(C, "ejabberd.cfg")),
    start_ejabberd_with_config(C, "ejabberd.cfg"),
    ?eq(true, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
    % when deleting the module from the configuration
    copy(data(C, "ejabberd.default.cfg"), data(C, "ejabberd.cfg")),
    ejabberd_config:reload_local(),
    % then the module is stopped
    ?eq(false, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
    % cleanup
    ok = stop_ejabberd().

reload_a_module(C) ->
    % given a running server with a specific module on
    copy(data(C, "ejabberd.with_mod_offline.cfg"), data(C, "ejabberd.cfg")),
    start_ejabberd_with_config(C, "ejabberd.cfg"),
    ?eq(true, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
    OfflineProcBefore = get_module_pid(mod_offline_localhost),
    % when changing the module configuration
    copy(data(C, "ejabberd.with_mod_offline.different_opts.cfg"),
         data(C, "ejabberd.cfg")),
    ejabberd_config:reload_local(),
    % then the module is reloaded
    ?eq(true, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
    OfflineProcAfter = get_module_pid(mod_offline_localhost),
    ?ne(OfflineProcBefore, OfflineProcAfter),
    % cleanup
    ok = stop_ejabberd().

split_config(Config) ->
    % given
    given_vhost_config_split_into_multiple_files(Config),
    % when
    application:load(mongooseim),
    application:set_env(mongooseim, config, suite_priv(Config, "etc/ejabberd.cfg")),
    {ok, _} = start_ejabberd(Config),
    % then
    then_vhost_config_works(Config),
    % cleanup
    ok = stop_ejabberd().

given_vhost_config_split_into_multiple_files(C) ->
    [ ok = filelib:ensure_dir(D) || D <- [suite_priv(C, "etc/"),
                                          suite_priv(C, "etc/fake.domain.one/"),
                                          suite_priv(C, "etc/fake.domain.two/")] ],
    copy(data(C, "ejabberd.split.cfg"), suite_priv(C, "etc/ejabberd.cfg")),
    copy(data(C, "ejabberd.hosts.cfg"), suite_priv(C, "etc/ejabberd.hosts.cfg")),
    copy(data(C, "fake.domain.one/host.cfg"),
         suite_priv(C, "etc/fake.domain.one/host.cfg")),
    copy(data(C, "fake.domain.one/host.cfg"),
         suite_priv(C, "etc/fake.domain.one/certfile.pem")),
    copy(data(C, "fake.domain.two/host.cfg"),
         suite_priv(C, "etc/fake.domain.two/host.cfg")),
    copy(data(C, "fake.domain.two/host.cfg"),
         suite_priv(C, "etc/fake.domain.two/certfile.pem")).

then_vhost_config_works(_C) ->
    ?eq([{config, hosts, [<<"fake.domain.one">>, <<"fake.domain.two">>]}],
        ets:lookup(config, hosts)),
    ?eq(false, is_empty(ets:lookup(local_config,
                                   {domain_certfile, <<"fake.domain.one">>}))),
    ?eq(false, is_empty(ets:lookup(local_config,
                                   {domain_certfile, <<"fake.domain.two">>}))),
    ?eq(true, gen_mod:is_loaded(<<"fake.domain.one">>, mod_ping)),
    ?eq(true, gen_mod:is_loaded(<<"fake.domain.two">>, mod_roster)),
    ?eq(true, gen_mod:is_loaded(<<"fake.domain.two">>, mod_offline)),
    ?eq(false, gen_mod:is_loaded(<<"fake.domain.one">>, mod_roster)),
    ?eq(false, gen_mod:is_loaded(<<"fake.domain.one">>, mod_offline)),
    ?eq(false, gen_mod:is_loaded(<<"fake.domain.two">>, mod_ping)).

cluster_smoke(C) ->
    SlaveNode = slave_node(C),
    copy(data(C, "ejabberd.no_listeners.cfg"), data(C, "ejabberd.cfg")),
    {ok, _} = start_ejabberd_with_config(C, "ejabberd.cfg"),
    {ok, _} = start_remote_ejabberd_with_config(SlaveNode, C, "ejabberd.cfg"),
    maybe_join_cluster(SlaveNode),
    [_,_] = ejabberd_config:config_states(),
    % cleanup
    ok = stop_ejabberd(),
    stop_remote_ejabberd(SlaveNode),
    ok.

change_module_option_with_node_param_opts(C) ->
    SlaveNode = slave_node(C),
    copy(data(C, "ejabberd.no_listeners.node_specific_node1_v1.cfg"), data(C, "ejabberd_n1.cfg")),
    copy(data(C, "ejabberd.no_listeners.node_specific_node2_v1.cfg"), data(C, "ejabberd_n2.cfg")),
    {ok, _} = start_ejabberd_with_config(C, "ejabberd_n1.cfg"),
    {ok, _} = start_remote_ejabberd_with_config(SlaveNode, C, "ejabberd_n2.cfg"),
    maybe_join_cluster(SlaveNode),
    copy(data(C, "ejabberd.no_listeners.node_specific_node1_v2.cfg"), data(C, "ejabberd_n1.cfg")),
    copy(data(C, "ejabberd.no_listeners.node_specific_node2_v2.cfg"), data(C, "ejabberd_n2.cfg")),
    {ok,_} = ejabberd_config:reload_cluster(),
    % cleanup
    ok = stop_ejabberd(),
    stop_remote_ejabberd(SlaveNode),
    ok.

change_module_option_with_node_specific_mods(C) ->
    SlaveNode = slave_node(C),
    copy(data(C, "ejabberd.no_listeners.node_specific_module_node1_v1.cfg"), data(C, "ejabberd_n1.cfg")),
    copy(data(C, "ejabberd.no_listeners.node_specific_module_node2_v1.cfg"), data(C, "ejabberd_n2.cfg")),
    {ok, _} = start_ejabberd_with_config(C, "ejabberd_n1.cfg"),
    {ok, _} = start_remote_ejabberd_with_config(SlaveNode, C, "ejabberd_n2.cfg"),
    maybe_join_cluster(SlaveNode),
%   copy(data(C, "ejabberd.no_listeners.node_specific_module_node1_v2.cfg"), data(C, "ejabberd_n1.cfg")),
%   copy(data(C, "ejabberd.no_listeners.node_specific_module_node2_v2.cfg"), data(C, "ejabberd_n2.cfg")),
    {ok,_} = ejabberd_config:reload_cluster(),
    % cleanup
    ok = stop_ejabberd(),
    stop_remote_ejabberd(SlaveNode),
    ok.

module_deps_work_correctly_with_reload_cluster(C) ->
    %% Just to ensure
    mnesia:clear_table(config),
    stop_ejabberd(),
    copy(data(C, "ejabberd.no_listeners.gd.node1_v1.cfg"), data(C, "ejabberd_n1.cfg")),
    mock_gd_modules(),
    {ok, _} = start_ejabberd_with_config(C, "ejabberd_n1.cfg"),
    ejabberd_config:assert_local_config_reloaded(),
    %% Cleaning in end_per_testcase
    ok.

%%
%% Helpers
%%

gd_modules() ->
    [
        mod_global_distrib,
        mod_global_distrib_bounce,
        mod_global_distrib_disco,
        mod_global_distrib_hosts_refresher,
        mod_global_distrib_mapping,
        mod_global_distrib_receiver,
        mod_global_distrib_sender
    ].

mock_gd_modules() ->
    [mock_module(M) || M <- gd_modules()].

mock_module(M) ->
    meck:new(M, [no_link, unstick, passthrough]),
    meck:expect(M, start, fun(_Host, _Opts) -> ok end),
    meck:expect(M, stop, fun(_Host) -> ok end),
    ok.

is_empty([]) -> true;
is_empty(_) -> false.

get_ejabberd_cfg(Config, Name) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join([DataDir, Name]).

start_ejabberd_with_config(Config, ConfigFile) ->
    use_config_file(Config, ConfigFile),
    {ok, _} = start_ejabberd(Config).

multiple_modules_sections() ->
    [{local_config, {modules, <<"localhost">>}, [{mod_offline, []}]},
     {local_config, {modules, <<"localhost">>}, [{mod_adhoc, []}]}].

coalesced_modules_section() ->
    [{{modules,<<"localhost">>}, [{mod_adhoc,[]},
                                  {mod_offline,[]}]}].

get_module_pid(ModuleProcName) ->
    %% We can't generate the proc name here using gen_mod:get_module_proc/2
    %% from an XMPP domain and a module name,
    %% since some modules use a macro embedded inside the module
    %% as the second argument to gen_mod:get_module_proc/2.
    %% We have to rely on the caller to **just know what he's doing**.
    EjabberdProcesses = supervisor:which_children(ejabberd_sup),
    {ModuleProcName,
     ModulePid, _, _} = lists:keyfind(ModuleProcName, 1, EjabberdProcesses),
    {ModuleProcName, ModulePid}.



times(N, E) -> times(N, E, []).

times(0, _, Acc) -> Acc;
times(N, E, Acc) -> times(N-1, E, [E | Acc]).


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
