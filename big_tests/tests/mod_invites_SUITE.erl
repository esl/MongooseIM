-module(mod_invites_SUITE).

-compile([export_all, nowarn_export_all]).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-import(config_parser_helper, [default_mod_config/1, mod_config/2]).

-define(IS_EMPTY(X), (X =:= #{})).
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, soft_version}, {group, soft_version_with_os}].

groups() ->
    [{soft_version, [parallel], [version_service_discovery, ask_for_version]},
     {soft_version_with_os, [parallel], [version_service_discovery, ask_for_version_with_os]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(soft_version, Config) ->
    dynamic_modules:start(domain_helper:host_type(), mod_version, default_mod_config(mod_version)),
    Config;
init_per_group(soft_version_with_os, Config) ->
    ModuleConfig = mod_config(mod_version, #{os_info => true}),
    dynamic_modules:start(domain_helper:host_type(), mod_version, ModuleConfig),
    Config.

end_per_group(_Group, Config) ->
    dynamic_modules:stop(domain_helper:host_type(), mod_version),
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------
