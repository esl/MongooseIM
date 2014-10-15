-module(ejabberd_config_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-import(ejabberd_helper, [start_ejabberd/1,
                          stop_ejabberd/0,
                          use_config_file/2]).

all() ->
    [smoke,
     {group, reload_local}].

groups() ->
    [{reload_local, [], [coalesce_multiple_local_config_options,
                         add_a_module]}].

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

add_a_module(Config) ->
    % given a running server with a specific module off
    copy_config(Config, "ejabberd.default.cfg", "ejabberd.cfg"),
    start_ejabberd_with_config(Config, "ejabberd.cfg"),
    ?eq(false, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
    % when adding the module to the configuration
    copy_config(Config, "ejabberd.with_mod_offline.cfg", "ejabberd.cfg"),
    ejabberd_config:reload_local(),
    % then the new module gets started
    ?eq(true, gen_mod:is_loaded(<<"localhost">>, mod_offline)),
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

copy_config(Config, Src, Dst) ->
    DataDir = proplists:get_value(data_dir, Config),
    SrcConfigPath = filename:join([DataDir, Src]),
    DstConfigPath = filename:join([DataDir, Dst]),
    {ok, _} = file:copy(SrcConfigPath, DstConfigPath).
