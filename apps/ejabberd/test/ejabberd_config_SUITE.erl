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
     split_config].

groups() ->
    [{reload_local, [], [coalesce_multiple_local_config_options,
                         add_a_module,
                         delete_a_module,
                         reload_a_module]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%
%% Tests
%%

smoke(Config) ->
    % when
    start_ejabberd_with_config(Config, "ejabberd.default.cfg"),
    % then
    ?assert(lists:keymember(ejabberd, 1, application:which_applications())),
    % cleanup
    ok = stop_ejabberd().

coalesce_multiple_local_config_options(_Config) ->
    F = fun ejabberd_config:group_host_changes/1,
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
    application:load(ejabberd),
    application:set_env(ejabberd, config, suite_priv(Config, "etc/ejabberd.cfg")),
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

is_empty([]) -> true;
is_empty(_) -> false.

%%
%% Helpers
%%

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
