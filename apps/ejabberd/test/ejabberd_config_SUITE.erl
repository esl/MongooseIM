-module(ejabberd_config_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(ne(A, B), ?assertNot(A == B)).

-import(ejabberd_helper, [start_ejabberd/1,
                          stop_ejabberd/0,
                          use_config_file/2]).

all() ->
    [smoke,
     {group, reload_local}].

groups() ->
    [{reload_local, [], [coalesce_multiple_local_config_options,
                         add_a_module,
                         delete_a_module,
                         reload_a_module]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%
%% Tests
%%

smoke(Config) ->
    % given
    start_ejabberd_with_config(Config, "ejabberd.default.cfg"),
    % when/then
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

%%
%% Helpers
%%

start_ejabberd_with_config(Config, ConfigFile) ->
    use_config_file(Config, ConfigFile),
    ok = start_ejabberd(Config).

multiple_modules_sections() ->
    [{local_config, {modules, <<"localhost">>}, [{mod_offline, []}]},
     {local_config, {modules, <<"localhost">>}, [{mod_adhoc, []}]}].

coalesced_modules_section() ->
    [{{modules,<<"localhost">>}, [{mod_adhoc,[]},
                                  {mod_offline,[]}]}].

copy(Src, Dst) ->
    {ok, _} = file:copy(Src, Dst).

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

data(Config, Path) ->
    rel(Config, data_dir, Path).

priv(Config, Path) ->
    rel(Config, priv_dir, Path).

rel(Config, To, Path) ->
    Dir = proplists:get_value(To, Config),
    filename:join([Dir, Path]).
