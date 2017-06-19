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
-define(NS_FOREIGN_EVENT, <<"urn:xmpp:foreign_event:0">>).
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_foreign}]. %%, {group, other}].


groups() ->
    [{mod_foreign, [], [
                        foreign_event_item_discovery,
                        foreign_event_without_request_node,
                        foreign_event_without_request_type,
                        foreign_event_with_unknown_request_type,
                        foreign_event_http_success
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
%% Bad request tests
%%--------------------------------------------------------------------

foreign_event_without_request_node(Config) ->
    escalus:story(Config, [{bob, 1}],
                  fun(Bob) ->
                      ForeignEventJID = foreign_service(Bob),
                      Stanza = foreign_event_iq(ForeignEventJID, []),

                      Result = escalus:send_and_wait(Bob, Stanza),

                      escalus:assert(is_iq_error, Result),
                      Error = exml_query:subelement(Result, <<"error">>),
                      ?assertNotEqual(undefined, Error),
                      ErrorCode = exml_query:attr(Error, <<"code">>),
                      ErrorType = exml_query:attr(Error, <<"type">>),
                      ?assertEqual(<<"400">>, ErrorCode),
                      ?assertEqual(<<"modify">>, ErrorType),
                      ?assertMatch([_], exml_query:paths(Error, [{element, <<"bad-request">>}]))
                  end).

foreign_event_without_request_type(Config) ->
    escalus:story(Config, [{bob, 1}],
                  fun(Bob) ->
                      ForeignEventJID = foreign_service(Bob),
                      Stanza = foreign_event_iq(ForeignEventJID,
                                                [#xmlel{name = <<"request">>}]),

                      Result = escalus:send_and_wait(Bob, Stanza),

                      escalus:assert(is_iq_error, Result),
                      Error = exml_query:subelement(Result, <<"error">>),
                      ?assertNotEqual(undefined, Error),
                      ErrorCode = exml_query:attr(Error, <<"code">>),
                      ErrorType = exml_query:attr(Error, <<"type">>),
                      ?assertEqual(<<"400">>, ErrorCode),
                      ?assertEqual(<<"modify">>, ErrorType),
                      ?assertMatch([_], exml_query:paths(Error, [{element, <<"bad-request">>}]))
                  end).


%%--------------------------------------------------------------------
%% Feature not implemented test
%%--------------------------------------------------------------------

foreign_event_with_unknown_request_type(Config) ->
    escalus:story(Config, [{bob, 1}],
                  fun(Bob) ->
                      ForeignEventJID = foreign_service(Bob),
                      Stanza = foreign_event_iq(ForeignEventJID,
                                                [#xmlel{name = <<"request">>,
                                                        attrs = [{<<"type">>, <<"amqp">>}]}]),

                      Result = escalus:send_and_wait(Bob, Stanza),

                      escalus:assert(is_iq_error, Result),
                      Error = exml_query:subelement(Result, <<"error">>),
                      ?assertNotEqual(undefined, Error),
                      ErrorCode = exml_query:attr(Error, <<"code">>),
                      ErrorType = exml_query:attr(Error, <<"type">>),
                      ?assertEqual(<<"501">>, ErrorCode),
                      ?assertEqual(<<"cancel">>, ErrorType),
                      ?assertMatch([_], exml_query:paths(Error, [{element, <<"feature-not-implemented">>}]))
                  end).

%%--------------------------------------------------------------------
%% Success HTTP event
%%--------------------------------------------------------------------

foreign_event_http_success(Config) ->
    escalus:story(Config, [{bob, 1}],
                  fun(Bob) ->
                      ForeignEventJID = foreign_service(Bob),
                      Stanza = foreign_event_iq(ForeignEventJID,
                                                [#xmlel{name = <<"request">>,
                                                        attrs = [{<<"type">>, <<"http">>}]}]),

                      Result = escalus:send_and_wait(Bob, Stanza),

                      escalus:assert(is_iq_result, Result)
                  end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

foreign_service(Client) ->
    <<"foreign.", (escalus_client:server(Client))/binary>>.

host() ->
    ct:get_config({hosts, mim, domain}).

foreign_event_iq(To, Children) ->
    ForeignEvent = #xmlel{name = <<"foreign-event">>,
                          attrs = [{<<"xmlns">>, ?NS_FOREIGN_EVENT}],
                          children = Children},
    escalus_stanza:iq(To, <<"set">>, [ForeignEvent]).
