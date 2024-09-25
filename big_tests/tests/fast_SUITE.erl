-module(fast_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-define(NS_SASL_2, <<"urn:xmpp:sasl:2">>).
-define(NS_FAST, <<"urn:xmpp:fast:0">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, basic}
    ].

groups() ->
    [
     {basic, [parallel],
      [
       server_announces_fast,
       request_token_with_initial_authentication
      ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = load_modules(Config),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

load_modules(Config) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    sasl2_helper:load_all_sasl2_modules(HostType),
    Config1.

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------

server_announces_fast(Config) ->
    Steps = [create_connect_tls, start_stream_get_features],
    #{features := Features} = sasl2_helper:apply_steps(Steps, Config),
    Fast = exml_query:path(Features, [{element_with_ns, <<"authentication">>, ?NS_SASL_2},
                                      {element, <<"inline">>},
                                      {element_with_ns, <<"fast">>, ?NS_FAST}]),
    ?assertNotEqual(undefined, Fast),
    ct:fail(Fast),
    ok.

request_token_with_initial_authentication(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_request_token},
             receive_features, has_no_more_stanzas],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Fast = exml_query:path(Success, [{element_with_ns, <<"token">>, ?NS_FAST}]),
    Expire = exml_query:attr(Fast, <<"expire">>),
    Token = exml_query:attr(Fast, <<"token">>),
    ct:fail({Expire, Token}).

auth_and_request_token(Config, Client, Data) ->
    Extra = [request_token()],
    bind2_SUITE:plain_auth(Config, Client, Data, [], Extra).

%% <request-token xmlns='urn:xmpp:fast:0' mechanism='HT-SHA-256-ENDP'/>
request_token() ->
    #xmlel{name = <<"request-token">>,
           attrs = [{<<"xmlns">>, ?NS_FAST},
                    {<<"mechanism">>, <<"HT-SHA-256-ENDP">>}]}.
