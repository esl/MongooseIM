-module(push_pubsub_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include("push_helper.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, disco},
        {group, allocate},
        {group, pubsub_publish},
        {group, rest_integration_v2}
    ].

groups() ->
    G = [
         {disco, [], [has_disco_identity]},
         {allocate, [], [allocate_basic_node]},
         {pubsub_publish, [], [
                               publish_fails_with_invalid_item,
                               publish_fails_with_no_options,
                               publish_succeeds_with_valid_options,
                               push_node_can_be_configured_to_whitelist_publishers
                              ]},
         {rest_integration_v2, [], [
                                    rest_service_called_with_correct_path_v2,
                                    rest_service_gets_correct_payload_v2,
                                    rest_service_gets_correct_payload_silent_v2
                                   ]}
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(cowboy),

    %% For mocking with unnamed functions
    mongoose_helper:inject_module(?MODULE),

    %% Start modules
    Config2 = dynamic_modules:save_modules(domain(), Config),
    Config3 = escalus:init_per_suite(Config2),
    escalus:create_users(Config3, escalus:get_users([bob, alice])).
end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:delete_users(Config, escalus:get_users([bob, alice])),
    escalus:end_per_suite(Config).

init_per_group(rest_integration_v1, Config) ->
    restart_modules(Config, "v1");
init_per_group(rest_integration_v2, Config) ->
    restart_modules(Config, "v2");
init_per_group(_, Config) ->
    restart_modules(Config, "v2").

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    MongoosePushMockPort = setup_mock_rest(),

    %% Start HTTP pool
    HTTPOpts = [
        {server, "http://localhost:" ++ integer_to_list(MongoosePushMockPort)}
    ],
    PoolOpts = [{strategy, available_worker}, {workers, 20}],
    rpc(mongoose_wpool, start_configured_pools,
        [[{http, global, mongoose_push_http, PoolOpts, HTTPOpts}]]),
    escalus:init_per_testcase(CaseName, Config).


end_per_testcase(CaseName, Config) ->
    rpc(mongoose_wpool, stop, [http, global, mongoose_push_http]),
    teardown_mock_rest(),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% GROUP disco
%%--------------------------------------------------------------------
has_disco_identity(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Server = pubsub_tools:node_addr(?PUBSUB_SUB_DOMAIN ++ "."),
            escalus:send(Alice, escalus_stanza:disco_info(Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            escalus:assert(has_identity, [<<"pubsub">>, <<"push">>], Stanza)
        end).

%%--------------------------------------------------------------------
%% GROUP allocate
%%--------------------------------------------------------------------

allocate_basic_node(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = push_pubsub_node(),
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}])
        end).

%%--------------------------------------------------------------------
%% GROUP pubsub_publish
%%--------------------------------------------------------------------

publish_fails_with_invalid_item(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = push_pubsub_node(),
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),

            Item =
                #xmlel{name = <<"invalid-item">>,
                       attrs = [{<<"xmlns">>, ?NS_PUSH}]},

            Publish = escalus_pubsub_stanza:publish(Alice, <<"itemid">>, Item, <<"id">>, Node),
            escalus:send(Alice, Publish),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Alice)),

            ok

        end).

publish_fails_with_no_options(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = push_pubsub_node(),
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),

            ContentFields = [
                {<<"FORM_TYPE">>, ?PUSH_FORM_TYPE},
                {<<"message-count">>, <<"1">>},
                {<<"last-message-sender">>, <<"senderId">>},
                {<<"last-message-body">>, <<"message body">>}
            ],

            Item =
                #xmlel{name = <<"notification">>,
                       attrs = [{<<"xmlns">>, ?NS_PUSH}],
                       children = [push_helper:make_form(ContentFields)]},

            Publish = escalus_pubsub_stanza:publish(Alice, <<"itemid">>, Item, <<"id">>, Node),
            escalus:send(Alice, Publish),
            escalus:assert(is_error, [<<"cancel">>, <<"conflict">>],
                           escalus:wait_for_stanza(Alice)),

            ok

        end).

publish_succeeds_with_valid_options(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = push_pubsub_node(),
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),

            Content = [
                {<<"message-count">>, <<"1">>},
                {<<"last-message-sender">>, <<"senderId">>},
                {<<"last-message-body">>, <<"message body">>}
            ],

            Options = [
                {<<"device_id">>, <<"sometoken">>},
                {<<"service">>, <<"apns">>}
            ],

            PublishIQ = publish_iq(Alice, Node, Content, Options),
            escalus:send(Alice, PublishIQ),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

            ok

        end).

push_node_can_be_configured_to_whitelist_publishers(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Node = push_pubsub_node(),
            Configuration = [{<<"pubsub#access_model">>, <<"whitelist">>},
                             {<<"pubsub#publish_model">>, <<"publishers">>}],
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>},
                                                   {config, Configuration}]),

            ActiveConfig = pubsub_tools:get_configuration(Alice, Node, []),
            ?assertMatch({_, _, <<"whitelist">>}, lists:keyfind(<<"pubsub#access_model">>, 1, ActiveConfig)),
            ?assertMatch({_, _, <<"publishers">>}, lists:keyfind(<<"pubsub#publish_model">>, 1, ActiveConfig)),

            Content = [
                {<<"message-count">>, <<"1">>},
                {<<"last-message-sender">>, <<"senderId">>},
                {<<"last-message-body">>, <<"message body">>}
            ],

            Options = [
                {<<"device_id">>, <<"sometoken">>},
                {<<"service">>, <<"apns">>}
            ],

            PublishIQ = publish_iq(Bob, Node, Content, Options),
            escalus:send(Bob, PublishIQ),
            escalus:assert(is_error, [<<"auth">>, <<"forbidden">>],
                           escalus:wait_for_stanza(Bob)),


            ok

        end).

%%--------------------------------------------------------------------
%% GROUP rest_integration
%%--------------------------------------------------------------------


rest_service_called_with_correct_path(Version, Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = setup_pubsub(Alice),
            {Notification, Options} = prepare_notification(),
            send_notification(Alice, Node, Notification, Options),
            {Req, _Body} = get_mocked_request(),

            ?assertMatch(<<"POST">>, cowboy_req:method(Req)),
            ?assertMatch(Version, cowboy_req:binding(level1, Req)),
            ?assertMatch(<<"notification">>, cowboy_req:binding(level2, Req)),
            ?assertMatch(<<"sometoken">>, cowboy_req:binding(level3, Req)),
            ?assertMatch(undefined, cowboy_req:binding(level4, Req))
        end).

rest_service_called_with_correct_path_v1(Config) ->
    rest_service_called_with_correct_path(<<"v1">>, Config).

rest_service_called_with_correct_path_v2(Config) ->
    rest_service_called_with_correct_path(<<"v2">>, Config).


rest_service_gets_correct_payload_v1(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = setup_pubsub(Alice),
            {Notification, Options} = prepare_notification(),
            send_notification(Alice, Node, Notification, Options),
            {_, Body} = get_mocked_request(),

            ?assertMatch(#{<<"service">> := <<"some_awesome_service">>}, Body),
            ?assertMatch(#{<<"badge">> := 876}, Body),
            ?assertMatch(#{<<"title">> := <<"senderId">>}, Body),
            ?assertMatch(#{<<"tag">> := <<"senderId">>}, Body),
            ?assertMatch(#{<<"mode">> := <<"selected_mode">>}, Body),
            ?assertMatch(#{<<"body">> := <<"message body 576364!!">>}, Body)
        end).

rest_service_gets_correct_payload_v2(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = setup_pubsub(Alice),
            {Notification, Options} = prepare_notification(),
            send_notification(Alice, Node, Notification, Options),
            {_, Body} = get_mocked_request(),


            ?assertMatch(#{<<"service">> := <<"some_awesome_service">>}, Body),
            ?assertMatch(#{<<"mode">> := <<"selected_mode">>}, Body),
            ?assertMatch(#{<<"topic">> := <<"some_topic">>}, Body),
            ?assert(not maps:is_key(<<"data">>, Body)),
            ?assertMatch(#{<<"alert">> := #{<<"badge">> := 876}}, Body),
            ?assertMatch(#{<<"alert">> := #{<<"title">> := <<"senderId">>}}, Body),
            ?assertMatch(#{<<"alert">> := #{<<"tag">> := <<"senderId">>}}, Body),
            ?assertMatch(#{<<"alert">> := #{<<"body">> := <<"message body 576364!!">>}}, Body)

        end).

rest_service_gets_correct_payload_silent_v2(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = setup_pubsub(Alice),
            {Notification, Options} = prepare_notification([{<<"silent">>, <<"true">>}]),
            send_notification(Alice, Node, Notification, Options),
            {_, Body} = get_mocked_request(),

            ?assertMatch(#{<<"service">> := <<"some_awesome_service">>}, Body),
            ?assertMatch(#{<<"mode">> := <<"selected_mode">>}, Body),
            ?assertMatch(#{<<"topic">> := <<"some_topic">>}, Body),
            ?assert(not maps:is_key(<<"alert">>, Body)),
            ?assertMatch(#{<<"data">> := #{<<"message-count">> := 876}}, Body),
            ?assertMatch(#{<<"data">> := #{<<"last-message-sender">> := <<"senderId">>}}, Body),
            ?assertMatch(#{<<"data">> := #{<<"last-message-body">> := <<"message body 576364!!">>}}, Body)

        end).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

send_notification(User, Node, Notification, Options) ->
    PublishIQ = publish_iq(User, Node, Notification, Options),
    escalus:send(User, PublishIQ),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)).

get_mocked_request() ->
    {Req, BodyRaw} = next_rest_req(),
    Body = jiffy:decode(BodyRaw, [return_maps]),
    {Req, Body}.

prepare_notification() ->
    prepare_notification([]).
prepare_notification(CustomOptions) ->
    Notification = [
        {<<"message-count">>, <<"876">>},
        {<<"last-message-sender">>, <<"senderId">>},
        {<<"last-message-body">>, <<"message body 576364!!">>}
    ],

    Options = [
        {<<"device_id">>, <<"sometoken">>},
        {<<"service">>, <<"some_awesome_service">>},
        {<<"mode">>, <<"selected_mode">>},
        {<<"topic">>, <<"some_topic">>}
    ],

    {Notification, Options ++ CustomOptions}.

setup_pubsub(User) ->
    Node = push_pubsub_node(),
    pubsub_tools:create_node(User, Node, [{type, <<"push">>}]),
    Node.

%% ----------------------------------
%% Stanzas
%% ----------------------------------

publish_iq(Client, Node, Content, Options) ->
    ContentFields = [{<<"FORM_TYPE">>, ?PUSH_FORM_TYPE}] ++ Content,
    OptionFileds = [{<<"FORM_TYPE">>, ?NS_PUBSUB_PUB_OPTIONS}] ++ Options,

    Item =
        #xmlel{name = <<"notification">>,
               attrs = [{<<"xmlns">>, ?NS_PUSH}],
               children = [push_helper:make_form(ContentFields)]},
    OptionsEl =
        #xmlel{name = <<"publish-options">>, children = [push_helper:make_form(OptionFileds)]},

    Publish = escalus_pubsub_stanza:publish(Client, <<"itemid">>, Item, <<"id">>, Node),
    #xmlel{children = [#xmlel{} = PubsubEl]} = Publish,
    NewPubsubEl = PubsubEl#xmlel{children = PubsubEl#xmlel.children ++ [OptionsEl]},
    Publish#xmlel{children = [NewPubsubEl]}.


%% ----------------------------------
%% Other helpers
%% ----------------------------------

domain() ->
    ct:get_config({hosts, mim, domain}).

push_pubsub_node() ->
    pubsub_tools:pubsub_node_with_subdomain(?PUBSUB_SUB_DOMAIN ++ ".").

parse_form(#xmlel{name = <<"x">>} = Form) ->
    parse_form(exml_query:subelements(Form, <<"field">>));
parse_form(Fields) when is_list(Fields) ->
    lists:map(
        fun(Field) ->
            {exml_query:attr(Field, <<"var">>),
             exml_query:path(Field, [{element, <<"value">>}, cdata])}
        end, Fields).

-spec rpc(M :: atom(), F :: atom(), A :: [term()]) -> term().
rpc(M, F, A) ->
    Node = ct:get_config({hosts, mim, node}),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_rpc:call(Node, M, F, A, 10000, Cookie).

bare_jid(JIDOrClient) ->
    ShortJID = escalus_client:short_jid(JIDOrClient),
    list_to_binary(string:to_lower(binary_to_list(ShortJID))).

%% ----------------------------------------------
%% REST mock handler
setup_mock_rest() ->
    TestPid = self(),
    HandleFun = fun(Req) -> handle(Req, TestPid) end,
    {ok, _} = http_helper:start(0, "/[:level1/[:level2/[:level3/[:level4]]]]",
                                          HandleFun),
    http_helper:port().

handle(Req, Master) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Master ! {rest_req, Req2, Body},
    cowboy_req:reply(204, #{}, <<>>, Req).

teardown_mock_rest() ->
    http_helper:stop().

next_rest_req() ->
    receive
        {rest_req, Req, Body} ->
            {Req, Body}
    after timer:seconds(5) ->
        throw(rest_mock_timeout)
    end.

pubsub_host(Host) ->
    ?PUBSUB_SUB_DOMAIN ++ "." ++ Host.

%% Module config
required_modules(APIVersion) ->
    [{mod_pubsub, [
        {plugins, [<<"dag">>, <<"push">>]},
        {nodetree, <<"dag">>},
        {host, ?PUBSUB_SUB_DOMAIN ++ ".@HOST@"}
    ]},
     {mod_push_service_mongoosepush, [
         {pool_name, mongoose_push_http},
         {api_version, APIVersion}
     ]}].

restart_modules(Config, APIVersion) ->
    dynamic_modules:restore_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), required_modules(APIVersion)),
    Config.
