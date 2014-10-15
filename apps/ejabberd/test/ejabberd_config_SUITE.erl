-module(ejabberd_config_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-import(ejabberd_helper, [start_ejabberd/1,
                          stop_ejabberd/0,
                          use_config_file/2]).

all() ->
    [smoke].

groups() ->
    [].

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

%%
%% Helpers
%%

start_ejabberd_with_config(Config, ConfigFile) ->
    use_config_file(Config, ConfigFile),
    ok = start_ejabberd(Config).
