-module(mod_foreign_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_FOREIGN_EVENT_HTTP, <<"urn:xmpp:foreign_event:http:0">>).

-define(REQUEST,  <<"<request xmlns='", ?NS_FOREIGN_EVENT_HTTP/binary, "' "
                    "type='http' "
                    "url='http://localhost:8080' "
                    "method='get'>"
                    "<header name='content-type'>application-json</header>"
                    "<header name='token'>token</header>"
                    "<payload>'{\"key\":\"value\"}'</payload>"
                    "</request>">>).

-define(RESPONSE,  <<"<response xmlns='", ?NS_FOREIGN_EVENT_HTTP/binary, "' "
                     "type='http' "
                     "status='200'>"
                     "<header name='content-type'>application-json</header>"
                     "<header name='token'>token</header>"
                     "<payload>'{\"key\":\"value\"}'</payload>"
                     "</response>">>).

-define(PUBLISH_PUBSUB(Type, NodeName),
        <<"<publish type='", Type/binary, "' "
                   "to='pubsub' "
                   "name='", NodeName/binary, "'/>">>).


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
                        foreign_event_http_success,
                        publishes_to_pubsub,
                        http_request_with_pubsub_publication
                       ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config2 = dynamic_modules:save_modules(host(), Config),
    dynamic_modules:ensure_modules(host(), required_modules()),
    http_helper:start(8080, '_', fun process_request/1),
    escalus:init_per_suite(Config2).


end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(host(), Config),
    http_helper:stop(),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

foreign_event_item_discovery(Config) ->
    escalus:fresh_story(
      Config,
      [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_items(ServJID)),
              escalus:assert(is_iq_result, Result),
              escalus:assert(has_item, [foreign_service()], Result)
      end).

%%--------------------------------------------------------------------
%% Bad request tests
%%--------------------------------------------------------------------

foreign_event_without_request_node(Config) ->
    escalus:fresh_story(Config, [{bob, 1}],
                  fun(Bob) ->
                      ForeignEventJID = foreign_service(),
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
    escalus:fresh_story(Config, [{bob, 1}],
                  fun(Bob) ->
                      ForeignEventJID = foreign_service(),
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
    escalus:fresh_story(Config, [{bob, 1}],
                  fun(Bob) ->
                      ForeignEventJID = foreign_service(),
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
    escalus:fresh_story(
      Config,
      [{bob, 1}],
      fun(Bob) ->
              %% GIVEN
              {ok, Request} = exml:parse(?REQUEST),
              ForeignEventJID = foreign_service(),
              Stanza = foreign_event_iq(ForeignEventJID, [Request]),

              %% WHEN
              Result = escalus:send_and_wait(Bob, Stanza),

              %% THEN
              escalus:assert(is_iq_result, Result)
      end).

%%--------------------------------------------------------------------
%% Publishing 
%%--------------------------------------------------------------------


publishes_to_pubsub(Config) ->
    escalus:fresh_story(
      Config,
      [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              Node = ensure_pubsub_node(Bob),
              pubsub_tools:subscribe(Alice, Node, []),
              {ok, Request} = exml:parse(?REQUEST),
              U = escalus_client:username(Bob),
              S = escalus_client:server(Bob),
              R = escalus_client:resource(Bob),
              Lower = fun(X) ->
                              list_to_binary(string:to_lower(binary_to_list(X)))
                      end,
              Jid = {jid, U, S, R, Lower(U), Lower(S), Lower(R)},

              %% WHEN
              {result, [Result]} = ejabberd_node_utils:call_fun(mod_foreign, publish_to_pubsub,
                                                              [host(),
                                                               Jid,
                                                               pubsub_node_name(Node),
                                                               [Request]]),
              ItemId = exml_query:path(Result, [{element, <<"publish">>}, {element, <<"item">>}, {attr, <<"id">>}]),


              %% THEN
              pubsub_tools:receive_item_notification(Alice, ItemId, Node, [{with_payload, false}]),
              teardown_pubsub_node(Bob, Node)
      end).

http_request_with_pubsub_publication(Config) ->
    escalus:fresh_story(
      Config,
      [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              Node = ensure_pubsub_node(Bob),
              pubsub_tools:subscribe(Alice, Node, []),
              NodeName = pubsub_node_name(Node),
              {ok, Request} = exml:parse(?REQUEST),
              [PubRequest, PubResponse] =
                  [begin
                       {ok, El} = exml:parse(?PUBLISH_PUBSUB(Type, NodeName)),
                       El
                   end || Type <- [<<"request">>, <<"response">>]],
              ForeignEventJID = foreign_service(),
              Stanza = foreign_event_iq(ForeignEventJID,
                                        [PubRequest, PubResponse, Request]),

              %% WHEN
              Result = escalus:send_and_wait(Bob, Stanza),

              %% THEN
              escalus:assert(is_iq_result, Result),
              Received = escalus:wait_for_stanzas(Alice, 2),
              escalus:assert_many([is_message, is_message], Received),
              teardown_pubsub_node(Bob, Node)
      end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

host() ->
    ct:get_config({hosts, mim, domain}).

foreign_event_iq(To, Children) ->
    ForeignEvent = #xmlel{name = <<"foreign-event">>,
                          attrs = [{<<"xmlns">>, ?NS_FOREIGN_EVENT}],
                          children = Children},
    escalus_stanza:iq(To, <<"set">>, [ForeignEvent]).

foreign_service() ->
    <<"foreign.", (host())/binary>>.

pubsub_service() ->
    <<"pubsub.", (host())/binary>>.

node_addr() ->
    <<"pubsub.", (host())/binary>>.

rand_name(Prefix) ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    <<Prefix/binary, "_", Suffix/binary>>.

pubsub_node_name() ->
    rand_name(<<"foreign-event">>).

pubsub_node() ->
    {node_addr(), pubsub_node_name()}.

pubsub_node_name({_, NodeName}) ->
    NodeName.

required_modules() ->
    [{mod_pubsub, [
                   {plugins, [<<"dag">>]},
                   {nodetree, <<"dag">>},
                   {host, "pubsub.@HOST@"}
                  ]},
     {mod_foreign, ?OPTS}
    ].

ensure_pubsub_node(User) ->
    Node = pubsub_node(),
    pubsub_tools:create_node(User, Node, []),
    Node.

teardown_pubsub_node(User, Node) ->
    pubsub_tools:delete_node(User, Node, []).

process_request(Req0) ->
    Headers = cowboy_req:headers(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    cowboy_req:reply(200, Headers, Body, Req1).
