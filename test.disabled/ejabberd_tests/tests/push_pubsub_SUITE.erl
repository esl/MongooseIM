-module(push_pubsub_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_PUSH,                <<"urn:xmpp:push:0">>).
-define(NS_XDATA,               <<"jabber:x:data">>).
-define(NS_PUBSUB_PUB_OPTIONS,  <<"http://jabber.org/protocol/pubsub#publish-options">>).
-define(PUSH_FORM_TYPE,         <<"urn:xmpp:push:summary">>).
-define(MUCHOST,                <<"muclight.localhost">>).

-define(MUC_HOST,               <<"muc.localhost">>).
-define(NS_HTTP_UPLOAD,         <<"urn:xmpp:http:upload">>).
-define(PUSH_OPTS,
    [
        {backend, mnesia}
    ]).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, allocate}
    ].

groups() ->
    [
        {allocate, [], [
            allocate_basic_node,
            has_disco_identity,
            publish_fails_with_invalid_item,
            publish_fails_with_no_options,
            publish_succeeds_with_valid_options
        ]}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    %% For mocking with unnamed functions
    {_Module, Binary, Filename} = code:get_object_code(?MODULE),
    rpc(code, load_binary, [?MODULE, Filename, Binary]),

    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), required_modules()),
    escalus:init_per_suite(Config2).
end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_suite(Config).

init_per_group(_, Config0) ->
    Config = [{push_config, ?PUSH_OPTS} | Config0],
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Host, mod_push, ?PUSH_OPTS),
    escalus:create_users(Config, escalus:get_users([bob, alice])).

end_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_push),
    escalus:delete_users(Config, escalus:get_users([bob, alice])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).


end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% GROUP allocate
%%--------------------------------------------------------------------

allocate_basic_node(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = pubsub_node(),
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}])
        end).

has_disco_identity(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Server = escalus_client:server(Alice),
            escalus:send(Alice, escalus_stanza:disco_info(Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            escalus:assert(has_identity, [<<"pubsub">>, <<"push">>], Stanza)
        end).

publish_fails_with_invalid_item(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Node = pubsub_node(),
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
            Node = pubsub_node(),
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),

            ContentFields =
            [
                {<<"FORM_TYPE">>, ?PUSH_FORM_TYPE},
                {<<"message-count">>, <<"1">>},
                {<<"last-message-sender">>, <<"senderId">>},
                {<<"last-message-body">>, <<"message body">>}
            ],

            Item =
            #xmlel{name = <<"notification">>,
                   attrs = [{<<"xmlns">>, ?NS_PUSH}],
                   children = [make_form(ContentFields)]},

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
            Node = pubsub_node(),
            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),

            ContentFields =
            [
                {<<"FORM_TYPE">>, ?PUSH_FORM_TYPE},
                {<<"message-count">>, <<"1">>},
                {<<"last-message-sender">>, <<"senderId">>},
                {<<"last-message-body">>, <<"message body">>}
            ],

            OptionFileds =
            [
                {<<"FORM_TYPE">>, ?NS_PUBSUB_PUB_OPTIONS},
                {<<"secret">>, <<"sometoken">>},
                {<<"platform">>, <<"android">>}
            ],

            Item =
                #xmlel{name = <<"notification">>,
                       attrs = [{<<"xmlns">>, ?NS_PUSH}],
                       children = [make_form(ContentFields)]},
            Options =
                #xmlel{name = <<"publish-options">>, children = [make_form(OptionFileds)]},

            Publish = escalus_pubsub_stanza:publish(Alice, <<"itemid">>, Item, <<"id">>, Node),
            #xmlel{children = [#xmlel{} = PubsubEl]} = Publish,
            NewPubsubEl = PubsubEl#xmlel{children = PubsubEl#xmlel.children ++ [Options]},
            PublishIQ = Publish#xmlel{children = [NewPubsubEl]},
            escalus:send(Alice, PublishIQ),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),

            ok

        end).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

%% ----------------------------------
%% Stanzas
%% ----------------------------------

disable_stanza(JID, undefined) ->
    disable_stanza([
                       {<<"xmlns">>, <<"urn:xmpp:push:0">>},
                       {<<"jid">>, JID}
                   ]);
disable_stanza(JID, Node) ->
    disable_stanza([
                       {<<"xmlns">>, <<"urn:xmpp:push:0">>},
                       {<<"jid">>, JID},
                       {<<"node">>, Node}
                   ]).
disable_stanza(JID) when is_binary(JID) ->
    disable_stanza(JID, undefined);
disable_stanza(Attrs) when is_list(Attrs) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"disable">>, attrs = Attrs}]).

enable_stanza(JID, Node) ->
    enable_stanza(JID, Node, undefined).
enable_stanza(JID, Node, FormFields) ->
    enable_stanza(JID, Node, FormFields, ?NS_PUBSUB_PUB_OPTIONS).
enable_stanza(JID, Node, FormFields, FormType) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>, attrs = [
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID},
        {<<"node">>, Node}
    ], children = maybe_form(FormFields, FormType)}]).

maybe_form(undefined, _FormType) ->
    [];
maybe_form(FormFields, FormType) ->
    [make_form([{<<"FORM_TYPE">>, FormType} | FormFields])].

make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Name, Value) || {Name, Value} <- Fields]}.

make_form_field(Name, Value) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlcdata{content = Value}]}.



%% ----------------------------------
%% Other helpers
%% ----------------------------------

domain() ->
    ct:get_config({hosts, mim, domain}).

node_addr() ->
    Domain = domain(),
    <<"pubsub.", Domain/binary>>.

rand_name(Prefix) ->
    Suffix = base64:encode(crypto:rand_bytes(5)),
    <<Prefix/binary, "_", Suffix/binary>>.

pubsub_node_name() ->
    rand_name(<<"princely_musings">>).

pubsub_node() ->
    {node_addr(), pubsub_node_name()}.

parse_form(#xmlel{name = <<"x">>} = Form) ->
    parse_form(exml_query:subelements(Form, <<"field">>));
parse_form(Fields) when is_list(Fields) ->
    lists:map(
        fun(Field) ->
            {exml_query:attr(Field, <<"var">>), exml_query:cdata(Field)}
        end, Fields).

-spec rpc(M :: atom(), F :: atom(), A :: [term()]) -> term().
rpc(M, F, A) ->
    Node = ct:get_config({hosts, mim, node}),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, 10000, Cookie).

bare_jid(JIDOrClient) ->
    ShortJID = escalus_client:short_jid(JIDOrClient),
    list_to_binary(string:to_lower(binary_to_list(ShortJID))).

required_modules() ->
    [{mod_pubsub, [
        {plugins, [<<"dag">>, <<"push">>]},
        {nodetree, <<"dag">>},
        {host, "pubsub.@HOST@"}
    ]}].