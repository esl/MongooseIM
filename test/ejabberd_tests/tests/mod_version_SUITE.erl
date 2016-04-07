-module(mod_version_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_version}].

groups() ->
    [{mod_version, [], [version_service_discovery]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    dynamic_modules:start(<<"localhost">>, mod_version, []),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    dynamic_modules:stop(<<"localhost">>, mod_version),
    escalus:end_per_suite(Config).

init_per_group(mod_version, Config) ->
    escalus:create_users(Config, escalus:get_users([alice])).

end_per_group(mod_version, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

version_service_discovery(Config) ->
    escalus:story(Config, [{alice, 1}],
        fun(Alice) ->
            ServJID = escalus_client:server(Alice),
            Result = escalus:send_and_wait(Alice,
                                           escalus_stanza:disco_info(ServJID)),
            escalus:assert(is_iq_result, Result),
            escalus:assert(has_feature, [?NS_SOFT_VERSION], Result)
        end).
