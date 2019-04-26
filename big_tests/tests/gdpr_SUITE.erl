-module(gdpr_SUITE).

%% Tests for features related to GDPR compliance.

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
         retrieve_vcard/1,
         retrieve_roster/1,
         retrieve_mam/1,
         retrieve_offline/1,
         retrieve_pubsub_payloads/1,
         retrieve_created_pubsub_nodes/1,
         retrieve_all_pubsub_data/1,
         dont_retrieve_other_user_pubsub_payload/1,
         retrieve_pubsub_subscriptions/1,
         retrieve_private_xml/1,
         retrieve_inbox/1,
         retrieve_logs/1
        ]).
-export([
         data_is_not_retrieved_for_missing_user/1
        ]).

-import(ejabberdctl_helper, [ejabberdctl/3]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

%% -------------------------------------------------------------
%% Common Test stuff
%% -------------------------------------------------------------

suite() ->
    escalus:suite().

all() ->
    [
     {group, retrieve_personal_data},
     {group, retrieve_personal_data_with_mods_disabled},
     {group, retrieve_negative}
    ].

groups() ->
    %% **DON'T** make any of these groups parallel, because calling mongooseimctl
    %% in parallel is broken!
    [
     {retrieve_personal_data, [], [
                                           retrieve_vcard,
                                           retrieve_roster,
                                           %retrieve_mam,
                                           %retrieve_offline,
                                           retrieve_private_xml,
                                           %retrieve_inbox,
                                           retrieve_logs,
                                           {group, retrieve_personal_data_pubsub}
                                          ]},
     {retrieve_personal_data_pubsub, [], [
                                          retrieve_pubsub_payloads,
                                          dont_retrieve_other_user_pubsub_payload,
                                          retrieve_pubsub_subscriptions,
                                          retrieve_created_pubsub_nodes,
                                          retrieve_all_pubsub_data
                                         ]},
     {retrieve_personal_data_with_mods_disabled, [], [
                                                      retrieve_vcard,
                                                      retrieve_logs,
                                                      retrieve_roster,
                                                      retrieve_all_pubsub_data
                                                     ]},
     {retrieve_negative, [], [
                              data_is_not_retrieved_for_missing_user
                             ]}
    ].

init_per_suite(Config) ->
    Config1 = [{{ejabberd_cwd, mim()}, get_mim_cwd()} | dynamic_modules:save_modules(domain(), Config)],
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    delete_files(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(retrieve_personal_data_with_mods_disabled, Config) ->
    dynamic_modules:ensure_modules(domain(), pubsub_required_modules()),
    [{disable_module, true} | Config];
init_per_group(retrieve_personal_data_pubsub, Config) ->
    dynamic_modules:ensure_modules(domain(), pubsub_required_modules()),
    Config;
init_per_group(_GN, Config) ->
    Config.

end_per_group(_GN, Config) ->
    Config.

init_per_testcase(retrieve_inbox = CN, Config) ->
    case (not ct_helper:is_ct_running())
         orelse mongoose_helper:is_rdbms_enabled(domain()) of
        true ->
            dynamic_modules:ensure_modules(domain(), inbox_required_modules()),
            escalus:init_per_testcase(CN, Config);
        false ->
            {skip, require_rdbms}
    end;
init_per_testcase(retrieve_vcard = CN, Config) ->
    case vcard_update:is_vcard_ldap() of
        true ->
            {skip, skipped_for_simplicity_for_now}; % TODO: Fix the case for LDAP as well
        _ ->
            escalus:init_per_testcase(CN, Config)
    end;
init_per_testcase(retrieve_mam = CN, Config) ->
    case pick_backend_for_mam() of
        skip ->
            {skip, no_supported_backends};
        Backend ->
            dynamic_modules:ensure_modules(domain(), mam_required_modules(Backend)),
            escalus:init_per_testcase(CN, Config)
    end;
init_per_testcase(CN, Config) ->
    escalus:init_per_testcase(CN, Config).

end_per_testcase(CN, Config) ->
    escalus:end_per_testcase(CN, Config).

inbox_required_modules() ->
    [{mod_inbox, []}].

pick_backend_for_mam() ->
    BackendsList = [
                    {mam_helper:is_cassandra_enabled(domain()), cassandra},
                    {mam_helper:is_riak_enabled(domain()), riak},
                    {mam_helper:is_elasticsearch_enabled(domain()), elasticsearch},
                    {mongoose_helper:is_rdbms_enabled(domain()), rdbms}
                   ],
    lists:foldl(fun({true, Backend}, skip) ->
                        Backend;
                   (_, BackendOrSkip) ->
                        BackendOrSkip
                end, skip, BackendsList).

mam_required_modules(Backend) ->
    [{mod_mam_meta, [{backend, Backend}, {pm, []}]}].

pubsub_required_modules() ->
    [{mod_caps, []}, {mod_pubsub, [
                                   {backend, mongoose_helper:mnesia_or_rdbms_backend()},
                                   {host, "pubsub.@HOST@"},
                                   {nodetree, <<"tree">>},
                                   {plugins, [<<"flat">>, <<"pep">>, <<"push">>]}
                                  ]
                     }].

%% -------------------------------------------------------------
%% Test cases
%% -------------------------------------------------------------

%% ------------------------- Data retrieval - per type verification -------------------------

retrieve_vcard(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            AliceFields = [{<<"FN">>, <<"Alice">>}, {<<"LN">>, <<"Ecila">>}],
            AliceSetResultStanza
            = escalus:send_and_wait(Alice, escalus_stanza:vcard_update(AliceFields)),
            escalus:assert(is_iq_result, AliceSetResultStanza),
            AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
            AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
            ExpectedHeader = ["jid", "vcard"],
            ExpectedItems = [
                             #{ "jid" => [{contains, AliceU},
                                          {contains, AliceS}],
                                "vcard" => [{contains, "Alice"},
                                            {contains, "Ecila"}] }
                            ],
            maybe_stop_and_unload_module(mod_vcard, mod_vcard_backend, Config),
            retrieve_and_validate_personal_data(
              Alice, Config, "vcard", ExpectedHeader, ExpectedItems)
        end).

retrieve_roster(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            escalus_story:make_all_clients_friends([Alice, Bob]),
            BobU = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
            BobS = escalus_utils:jid_to_lower(escalus_client:server(Bob)),
            ExpectedHeader = ["jid", "name", "subscription",
                              "ask", "groups", "askmessage", "xs"],
            ExpectedItems = [
                             #{ "jid" => [{contains,  BobU}, {contains, BobS}] }
                            ],
            maybe_stop_and_unload_module(mod_roster, mod_roster_backend, Config),
            retrieve_and_validate_personal_data(
                Alice, Config, "roster", ExpectedHeader, ExpectedItems)
        end).

retrieve_mam(_Config) ->
    ok.

retrieve_offline(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            mongoose_helper:logout_user(Config, Alice),
            Body = <<"Here's Johnny!">>,
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Body)),
            %% Well, jid_to_lower works for any binary :)
            AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
            AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
            mongoose_helper:wait_until(
              fun() ->
                      mongoose_helper:successful_rpc(mod_offline_backend, count_offline_messages,
                                                     [AliceU, AliceS, 1])
              end, 1),

            BobJid = escalus_client:short_jid(Bob),
            ExpectedHeader = ["timestamp", "from", "to", "packet"],
            ExpectedItems = [
                             #{ "packet" => [{contains, Body}], "from" => BobJid }
                            ],
            maybe_stop_and_unload_module(mod_offline, mod_offline_backend, Config),
            retrieve_and_validate_personal_data(
              Alice, Config, "offline", ExpectedHeader, ExpectedItems)
        end).

retrieve_pubsub_payloads(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Node1 = {_Domain, NodeName1} = pubsub_tools:pubsub_node(),
        Node2 = {_Domain, NodeName2} = pubsub_tools:pubsub_node(),
        {BinItem1, StringItem1} = item_content(<<"Item1Data">>),
        {BinItem2, StringItem2} = item_content(<<"Item2Data">>),
        {BinItem3, StringItem3} = item_content(<<"Item3Data">>),
        {BinOther, StringOther} = item_content(<<"OtherItemData">>),

        pubsub_tools:publish(Alice, <<"Item1">>, Node1, [{with_payload, BinItem1}]),
        pubsub_tools:publish(Alice, <<"Item2">>, Node1, [{with_payload, BinItem2}]),
        pubsub_tools:publish(Alice, <<"Item3">>, Node1, [{with_payload, BinItem3}]),
        pubsub_tools:publish(Alice, <<"OtherItem">>, Node2, [{with_payload, BinOther}]),

        ExpectedItems = [pubsub_payloads_row_map(NodeName1, "Item1", StringItem1),
                         pubsub_payloads_row_map(NodeName1, "Item2",StringItem2),
                         pubsub_payloads_row_map(NodeName1, "Item3", StringItem3),
                         pubsub_payloads_row_map(NodeName2, "OtherItem", StringOther)],

        maybe_stop_and_unload_module(mod_vcard, mod_vcard_backend, Config),
        retrieve_and_validate_personal_data(
            Alice, Config, "pubsub_payloads", ["node_name", "item_id", "payload"], ExpectedItems)
                                              end).

dont_retrieve_other_user_pubsub_payload(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Node1 = {_Domain, NodeName1} = pubsub_tools:pubsub_node(),
        pubsub_tools:create_node(Alice, Node1, []),
        AffChange = [{Bob, <<"publish-only">>}],

        {BinItem1, StringItem1} = item_content(<<"Item1Data">>),
        {BinItem2, StringItem2} = item_content(<<"Item2Data">>),

        pubsub_tools:set_affiliations(Alice, Node1, AffChange, []),
        pubsub_tools:publish(Alice, <<"Item1">>, Node1, [{with_payload, {true, BinItem1}}]),
        pubsub_tools:publish(Bob, <<"Item2">>, Node1, [{with_payload, {true, BinItem2}}]),

        retrieve_and_validate_personal_data(
            Alice, Config, "pubsub_payloads", ["node_name", "item_id", "payload"],
            [pubsub_payloads_row_map(NodeName1, "Item1", StringItem1)]),

        retrieve_and_validate_personal_data(
            Bob, Config, "pubsub_payloads", ["node_name","item_id", "payload"],
            [pubsub_payloads_row_map(NodeName1, "Item2", StringItem2)]),

        pubsub_tools:delete_node(Alice, Node1, [])
                                              end).

retrieve_created_pubsub_nodes(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Node1 = {_Domain, NodeName1} = pubsub_tools:pubsub_node(),
        Node2 = {_Domain, NodeName2} = pubsub_tools:pubsub_node(),
        Node3 = {_Domain, NodeName3} = pubsub_tools:pubsub_node(),

        NodeNS = random_node_ns(),
        PepNode = make_pep_node_info(Alice, NodeNS),
        AccessModel = {<<"pubsub#access_model">>, <<"authorize">>},

        pubsub_tools:create_node(Alice, Node1, []),
        pubsub_tools:create_node(Alice, Node2, []),
        pubsub_tools:create_node(Alice, PepNode, [{config, [AccessModel]}]),
        pubsub_tools:create_node(Bob, Node3, [{type, <<"push">>}]),

        ExpectedHeader = ["node_name", "type"],

        retrieve_and_validate_personal_data(
            Alice, Config, "pubsub_nodes", ExpectedHeader,
            [pubsub_nodes_row_map(NodeName1, "flat"),
             pubsub_nodes_row_map(NodeName2, "flat"),
             pubsub_nodes_row_map(NodeNS, "pep")
                ]),

        retrieve_and_validate_personal_data(
            Bob, Config, "pubsub_nodes", ExpectedHeader,
            [pubsub_nodes_row_map(NodeName3, "push")]),


        Nodes = [{Alice, Node1}, {Alice, Node2}, {Alice, PepNode}, {Bob, Node3}],
        [pubsub_tools:delete_node(User, Node, []) || {User, Node} <- Nodes]
                                                        end).

retrieve_pubsub_subscriptions(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Node = {_Domain, NodeName} = pubsub_tools:pubsub_node(),
            pubsub_tools:create_node(Alice, Node, []),
            pubsub_tools:subscribe(Bob, Node, []),
            retrieve_and_validate_personal_data(Bob, Config, "pubsub_subscriptions", ["node_name"],
                [pubsub_subscription_row_map(NodeName)]),

            pubsub_tools:delete_node(Alice, Node, [])
        end).

retrieve_all_pubsub_data(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Node1 = {_Domain, NodeName1} = pubsub_tools:pubsub_node(),
        Node2 = {_Domain, NodeName2} = pubsub_tools:pubsub_node(),
        Node3 = {_Domain, NodeName3} = pubsub_tools:pubsub_node(),
        pubsub_tools:create_node(Alice, Node1, []),
        pubsub_tools:create_node(Alice, Node2, []),
        pubsub_tools:create_node(Bob, Node3, []),
        AffChange = [{Bob, <<"publish-only">>}],
        pubsub_tools:set_affiliations(Alice, Node1, AffChange, []),
        pubsub_tools:subscribe(Bob, Node2, []),

        {BinItem1, StringItem1} = item_content(<<"Item1Data">>),
        {BinItem2, StringItem2} = item_content(<<"Item2Data">>),
        {BinItem3, StringItem3} = item_content(<<"Item3Data">>),

        pubsub_tools:publish(Alice, <<"Item1">>, Node1, [{with_payload, {true, BinItem1}}]),
        pubsub_tools:publish(Alice, <<"Item2">>, Node2, [{with_payload, {true, BinItem2}}]),
        pubsub_tools:receive_item_notification(Bob, <<"Item2">>, Node2, []),
        pubsub_tools:publish(Bob, <<"Item3">>, Node1, [{with_payload, {true, BinItem3}}]),

        maybe_stop_and_unload_module(mod_pubsub, mod_pubsub_db_backend, Config),
        %% Bob has one subscription, one node created and one payload sent
        retrieve_and_validate_personal_data(
            Bob, Config, "pubsub_subscriptions", ["node_name"],
            [pubsub_subscription_row_map(NodeName2)]),

        retrieve_and_validate_personal_data(
            Bob, Config, "pubsub_nodes", ["node_name", "type"],
            [pubsub_nodes_row_map(NodeName3, "flat")]),

        retrieve_and_validate_personal_data(
            Bob, Config, "pubsub_payloads", ["node_name", "item_id", "payload"],
            [pubsub_payloads_row_map(NodeName1, "Item3", StringItem3)]),

        %% Alice has two nodes created and two payloads sent
        retrieve_and_validate_personal_data(
            Alice, Config, "pubsub_nodes", ["node_name", "type"],
            [pubsub_nodes_row_map(NodeName1, "flat"),
             pubsub_nodes_row_map(NodeName2, "flat")]),
        retrieve_and_validate_personal_data(
            Alice, Config, "pubsub_payloads", ["node_name", "item_id","payload"],
            [pubsub_payloads_row_map(NodeName1, "Item1", StringItem1),
             pubsub_payloads_row_map(NodeName2, "Item2", StringItem2)])
      end).


retrieve_private_xml(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            NS = <<"alice:gdpr:ns">>,
            Content = <<"dGhlcmUgYmUgZHJhZ29ucw==">>,
            XML = #xmlel{ name = <<"fingerprint">>,
                          attrs = [{<<"xmlns">>, NS}],
                          children = [#xmlcdata{ content = Content }]},
            PrivateStanza = escalus_stanza:private_set(XML),
            escalus_client:send(Alice, PrivateStanza),
            escalus:assert(is_iq_result, [PrivateStanza], escalus_client:wait_for_stanza(Alice)),
            ExpectedHeader = ["ns", "xml"], % TODO?
            ExpectedItems = [
                             #{ "xml" => [{contains, "alice:gdpr:ns"},
                                          {contains, binary_to_list(Content)}] }
                            ],
            retrieve_and_validate_personal_data(
              Alice, Config, "private", ExpectedHeader, ExpectedItems)
        end).

retrieve_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Body = <<"With spam?">>,
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Body)),
            Msg = escalus:wait_for_stanza(Alice),
            escalus:assert(is_chat_message, [Body], Msg),

            BobJid = escalus_client:short_jid(Bob),
            ExpectedHeader = ["jid", "content", "unread_count", "msg_id", "timestamp"],
            ExpectedItems = [
                             #{ "content" => Body, "jid" => BobJid }
                            ],
            retrieve_and_validate_personal_data(
              Alice, Config, "inbox", ExpectedHeader, ExpectedItems)
        end).

retrieve_logs(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
            User = string:to_lower(binary_to_list(escalus_client:username(Alice))),
            Domain = string:to_lower(binary_to_list(escalus_client:server(Alice))),
            JID = string:to_upper(binary_to_list(escalus_client:short_jid(Alice))),
            MIM2Node = distributed_helper:mim2(),
            mongoose_helper:successful_rpc(net_kernel, connect_node, [MIM2Node]),
            mongoose_helper:successful_rpc(MIM2Node, error_logger, error_msg,
                                           ["event=disturbance_in_the_force, jid=~s", [JID]]),
            Dir = request_and_unzip_personal_data(User, Domain, Config),
            Filename = filename:join(Dir, "logs-" ++ atom_to_list(MIM2Node) ++ ".txt"),
            {ok, Content} = file:read_file(Filename),
            {match, _} = re:run(Content, "disturbance_in_the_force")
        end).

%% ------------------------- Data retrieval - Negative case -------------------------

data_is_not_retrieved_for_missing_user(Config) ->
    {Filename, 1, _} = retrieve_personal_data("non-person", "oblivion", Config),
    {error, _} = file:read_file_info(Filename).

%% -------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------

domain() ->
    <<"localhost">>. % TODO: Make dynamic?

maybe_stop_and_unload_module(Module, BackendProxy, Config) ->
    case proplists:get_value(disable_module, Config) of
        true ->
            dynamic_modules:stop(domain(), Module),
            mongoose_helper:successful_rpc(code, purge, [BackendProxy]),
            true = mongoose_helper:successful_rpc(code, delete, [BackendProxy]);
        _ ->
            ok
    end.

retrieve_and_validate_personal_data(Alice, Config, FilePrefix, ExpectedHeader, ExpectedItems) ->
    PersonalCSV = retrieve_and_decode_personal_data(Alice, Config, FilePrefix),
    PersonalMaps = csv_to_maps(ExpectedHeader, PersonalCSV),
    try validate_personal_maps(PersonalMaps, ExpectedItems) of
        _ -> ok
    catch
        C:R ->
            ct:fail(#{
              class => C,
              reason => R,
              stacktrace => erlang:get_stacktrace(),
              personal_maps => PersonalMaps,
              expected_items => ExpectedItems
             })
    end.

csv_to_maps(ExpectedHeader, [ExpectedHeader | Rows]) ->
    lists:foldl(fun(Row, Maps) -> [ csv_row_to_map(ExpectedHeader, Row) | Maps ] end, [], Rows).

csv_row_to_map(Header, Row) ->
    maps:from_list(lists:zip(Header, Row)).

validate_personal_maps(PersonalMaps, ExpectedItems) ->
    validate_sorted_personal_maps(lists:sort(PersonalMaps), lists:sort(ExpectedItems)).

validate_sorted_personal_maps([], []) -> ok;
validate_sorted_personal_maps(UnexpectedRecords, []) ->
    erlang:error("Unexpected records left ~p", [UnexpectedRecords]);
validate_sorted_personal_maps([Map | RMaps], [Checks | RChecks]) ->
    maps:fold(fun(K, Conditions, _) ->
                      validate_personal_item(maps:get(K, Map), Conditions)
              end, ok, Checks),
validate_sorted_personal_maps(RMaps, RChecks).

validate_personal_item(_Value, []) ->
    ok;
validate_personal_item(ExactValue, ExactValue) ->
    ok;
validate_personal_item(Value, [{contains, String} | RConditions]) ->
    {match, _} = re:run(Value, String),
    validate_personal_item(Value, RConditions).

retrieve_and_decode_personal_data(Client, Config, FilePrefix) ->
    User = escalus_client:username(Client),
    Domain = escalus_client:server(Client),
    Dir = request_and_unzip_personal_data(User, Domain, Config),
    CSVPath = filename:join(Dir, FilePrefix ++ ".csv"),
    {ok, Content} = file:read_file(CSVPath),
    % We expect non-empty list because it must contain at least header with columns names
    [_ | _] = csv:decode_binary(Content).

request_and_unzip_personal_data(User, Domain, Config) ->
    {Filename, 0, _} = retrieve_personal_data(User, Domain, Config),
    FullPath = get_mim_cwd() ++ "/" ++ Filename,
    Dir = Filename ++ ".unzipped",
    {ok, _} = zip:extract(FullPath, [{cwd,Dir}]),
    Dir.

retrieve_personal_data(User, Domain, Config) ->
    Filename = random_filename(Config),
    {CommandOutput, Code} = ejabberdctl("retrieve_personal_data", [User, Domain, Filename], Config),
    {Filename, Code, CommandOutput}.

random_filename(Config) ->
    TCName = atom_to_list(?config(tc_name, Config)),
    TCName ++ "." ++ integer_to_list(erlang:system_time()) ++ ".zip".

get_mim_cwd() ->
    {ok, Cwd} = rpc(mim(), file, get_cwd, []),
    Cwd.

delete_files() ->
    Cwd = get_mim_cwd(),
    {ok, Filenames} = rpc(mim(), file, list_dir, [Cwd]),
    FilteredFilenames = lists:filter(
        fun is_file_to_be_deleted/1,
        Filenames),
    lists:foreach(
        fun(Filename) -> rpc(mim(), file, delete, [Cwd ++ "/" ++ Filename]) end,
        FilteredFilenames),
    ok.

is_file_to_be_deleted(Filename) ->
    DeletableRegexes = ["\.csv", "\.zip"],
    lists:any(
        fun(Regex) ->
            re:run(Filename, Regex) =/= nomatch
        end,
    DeletableRegexes).

pubsub_payloads_row_map(Node, ItemId, Payload) ->
    #{"node_name" => binary_to_list(Node), "item_id" => ItemId, "payload" => Payload}.

pubsub_nodes_row_map(Node, Type) ->
    #{"node_name" => binary_to_list(Node), "type" => Type}.

pubsub_subscription_row_map(Node) ->
    #{"node_name" => binary_to_list(Node)}.

make_pep_node_info(Client, NodeName) ->
    {escalus_utils:jid_to_lower(escalus_utils:get_short_jid(Client)), NodeName}.

random_node_ns() ->
    base64:encode(crypto:strong_rand_bytes(16)).

item_content(Data) ->
    Bin = item_content_xml(Data),
    {Bin, binary_to_list(exml:to_binary(Bin))}.

item_content_xml(Data) ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}],
           children = [#xmlcdata{content = Data}]}.

