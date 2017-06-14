-module(mod_foreign_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(OPTS,
        [
         {backends, [
                     {http, [{pool_size, 100}]}
                    ]}
        ]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_foreign}]. %%, {group, other}].


groups() ->
    [{mod_foreign, [], [
                        foreign_event_item_discovery
                       ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    dynamic_modules:start(host(), mod_foreign, ?OPTS),
    escalus:create_users(Config, escalus:get_users([bob])).

end_per_group(_, Config) ->
    dynamic_modules:stop(host(), mod_foreign),
    escalus:delete_users(Config, escalus:get_users([bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

foreign_event_item_discovery(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_items(ServJID)),
              escalus:assert(is_iq_result, Result),
              escalus:assert(has_item, [foreign_service(Bob)], Result)
      end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

foreign_service(Client) ->
    <<"foreign.", (escalus_client:server(Client))/binary>>.

host() ->
    ct:get_config({hosts, mim, domain}).
