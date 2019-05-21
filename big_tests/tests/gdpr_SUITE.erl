-module(gdpr_SUITE).

%% Tests for features related to GDPR compliance.

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include("inbox.hrl").
-include("muc_light.hrl").

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
         retrieve_vcard/1,
         remove_vcard/1,
         remove_private/1,
         remove_multiple_private_xmls/1,
         dont_remove_other_user_private_xml/1,
         retrieve_roster/1,
         retrieve_mam_pm/1,
         retrieve_mam_muc_light/1,
         retrieve_mam_pm_and_muc_light_interfere/1,
         retrieve_mam_pm_and_muc_light_dont_interfere/1,
         remove_roster/1,
         retrieve_offline/1,
         remove_offline/1,
         retrieve_pubsub_payloads/1,
         retrieve_created_pubsub_nodes/1,
         retrieve_all_pubsub_data/1,
         dont_retrieve_other_user_pubsub_payload/1,
         retrieve_pubsub_subscriptions/1,
         retrieve_private_xml/1,
         dont_retrieve_other_user_private_xml/1,
         retrieve_multiple_private_xmls/1,
         retrieve_inbox/1,
         remove_inbox/1,
         retrieve_inbox_for_multiple_messages/1,
         retrieve_inbox_muclight/1,
         retrieve_inbox_muc/1,
         remove_inbox_muclight/1,
         remove_inbox_muc/1,
         retrieve_logs/1
        ]).
-export([
         data_is_not_retrieved_for_missing_user/1
        ]).

-import(ejabberdctl_helper, [ejabberdctl/3]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

-import(muc_light_helper, [room_bin_jid/1, stanza_destroy_room/1]).

-define(ROOM, <<"tt1">>).

-define(MUCLIGHTHOST, <<"muclight.@HOST@">>).
-define(MUCHOST, <<"muc.@HOST@">>).

%% -------------------------------------------------------------
%% Common Test stuff
%% -------------------------------------------------------------

suite() ->
    escalus:suite().

all() ->
    [
     {group, retrieve_personal_data},
     {group, retrieve_personal_data_with_mods_disabled},
     {group, retrieve_negative},
     {group, remove_personal_data},
     {group, remove_personal_data_with_mods_disabled}
    ].

groups() ->
    %% **DON'T** make any of these groups parallel, because calling mongooseimctl
    %% in parallel is broken!
    [
     {retrieve_personal_data, [], [
                                   retrieve_vcard,
                                   retrieve_roster,
                                   retrieve_offline,
                                   retrieve_inbox,
                                   retrieve_logs,
                                   {group, retrieve_personal_data_pubsub},
                                   {group, retrieve_personal_data_private_xml},
                                   {group, retrieve_personal_data_mam},
                                   {group, retrieve_personal_data_inbox}
                                  ]},
    {retrieve_personal_data_inbox, [],[
        retrieve_inbox,
        retrieve_inbox_for_multiple_messages,
        retrieve_inbox_muclight,
        retrieve_inbox_muc
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
                                                      {group, retrieve_personal_data_inbox},
                                                      retrieve_offline,
                                                      retrieve_logs,
                                                      retrieve_roster,
                                                      retrieve_all_pubsub_data,
                                                      retrieve_multiple_private_xmls,
                                                      {group, retrieve_personal_data_mam}
                                                     ]},
     {retrieve_personal_data_private_xml, [], [
                                               retrieve_private_xml,
                                               dont_retrieve_other_user_private_xml,
                                               retrieve_multiple_private_xmls
                                              ]},
     {retrieve_negative, [], [
                              data_is_not_retrieved_for_missing_user
                             ]},
     {retrieve_personal_data_mam, [], [
                                       {group, retrieve_personal_data_mam_rdbms},
                                       {group, retrieve_personal_data_mam_riak},
                                       {group, retrieve_personal_data_mam_cassandra},
                                       {group, retrieve_personal_data_mam_elasticsearch}
                                      ]},
     {retrieve_personal_data_mam_rdbms, [], mam_testcases()},
     {retrieve_personal_data_mam_riak, [], mam_testcases()},
     {retrieve_personal_data_mam_cassandra, [], mam_testcases()},
     {retrieve_personal_data_mam_elasticsearch, [], mam_testcases()},
     {remove_personal_data, [], removal_testcases()},
     {remove_personal_data_with_mods_disabled, [], removal_testcases()},
     {remove_personal_data_inbox, [], [remove_inbox, remove_inbox_muclight, remove_inbox_muc]}].

removal_testcases() ->
    [
        remove_vcard,
        remove_roster,
        remove_offline,
        remove_private,
        remove_multiple_private_xmls,
        dont_remove_other_user_private_xml,
        {group, remove_personal_data_inbox}
    ].


mam_testcases() ->
    [
        retrieve_mam_pm,
        retrieve_mam_muc_light,
        retrieve_mam_pm_and_muc_light_interfere,
        retrieve_mam_pm_and_muc_light_dont_interfere
    ].

init_per_suite(Config) ->
    Config1 = [{{ejabberd_cwd, mim()}, get_mim_cwd()} | dynamic_modules:save_modules(domain(), Config)],
    gdpr_removal_for_disabled_modules(true),
    muc_helper:load_muc(muc_domain()),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    delete_files(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus_fresh:clean(),
    gdpr_removal_for_disabled_modules(false),
    escalus:end_per_suite(Config).

gdpr_removal_for_disabled_modules(Flag) ->
    mongoose_helper:successful_rpc(ejabberd_config, add_global_option, [gdpr_removal_for_disabled_modules, Flag]).

init_per_group(GN, Config) when GN =:= retrieve_personal_data_with_mods_disabled;
                                GN =:= remove_personal_data_with_mods_disabled->
    dynamic_modules:ensure_modules(domain(), pubsub_required_modules()),
    [{disable_module, true} | Config];
init_per_group(retrieve_personal_data_pubsub, Config) ->
    dynamic_modules:ensure_modules(domain(), pubsub_required_modules()),
    Config;
init_per_group(retrieve_personal_data_mam_rdbms, Config) ->
    try_backend_for_mam(Config, rdbms);
init_per_group(retrieve_personal_data_mam_riak, Config) ->
    try_backend_for_mam(Config, riak);
init_per_group(retrieve_personal_data_mam_cassandra, Config) ->
    try_backend_for_mam(Config, cassandra);
init_per_group(retrieve_personal_data_mam_elasticsearch, Config) ->
    try_backend_for_mam(Config, elasticsearch);
init_per_group(retrieve_personal_data_inbox = GN, Config) ->
    init_inbox(GN, Config, muclight);
init_per_group(remove_personal_data_inbox = GN, Config) ->
    init_inbox(GN, Config, muclight);
init_per_group(_GN, Config) ->
    Config.

end_per_group(_GN, Config) ->
    Config.


try_backend_for_mam( Config,Backend) ->
    case is_backend_enabled(Backend) of
        true -> [{mam_backend, Backend} | Config];
        false -> {skip, backend_is_not_configured}
    end.

is_backend_enabled(rdbms)         -> mongoose_helper:is_rdbms_enabled(domain());
is_backend_enabled(riak)          -> mam_helper:is_riak_enabled(domain());
is_backend_enabled(cassandra)     -> mam_helper:is_cassandra_enabled(domain());
is_backend_enabled(elasticsearch) -> mam_helper:is_elasticsearch_enabled(domain()).


init_per_testcase(remove_offline = CN, Config) ->
    offline_started(),
    escalus:init_per_testcase(CN, Config);
init_per_testcase(CN, Config) when
      CN =:= remove_inbox;
      CN =:= retrieve_inbox;
      CN =:= remove_inbox_muclight;
      CN =:= retrieve_inbox_muclight ->
    Config1 = init_inbox(CN, Config, muclight),
    Config1;
init_per_testcase(CN, Config) when CN =:= retrieve_inbox_muc;
                                   CN =:= remove_inbox_muc ->
    muc_helper:load_muc(muc_domain()),
    Config0 = init_inbox(CN, Config, muc),
    Config0;

init_per_testcase(retrieve_vcard = CN, Config) ->
    case vcard_update:is_vcard_ldap() of
        true ->
            {skip, skipped_for_simplicity_for_now}; % TODO: Fix the case for LDAP as well
        _ ->
            escalus:init_per_testcase(CN, Config)
    end;
init_per_testcase(remove_vcard = CN, Config) ->
    case vcard_update:is_vcard_ldap() of
        true ->
            {skip, skipped_for_simplicity_for_now}; % TODO: Fix the case for LDAP as well
        _ ->
            vcard_started(),
            escalus:init_per_testcase(CN, Config)
    end;
init_per_testcase(CN, Config) when CN =:= remove_private;
                                   CN =:= dont_remove_other_user_private_xml;
                                   CN =:= remove_multiple_private_xmls ->
    private_started(),
    escalus:init_per_testcase(CN, Config);

init_per_testcase(CN, Config) when CN =:= retrieve_mam_muc_light;
                                   CN =:= retrieve_mam_pm_and_muc_light_interfere;
                                   CN =:= retrieve_mam_pm_and_muc_light_dont_interfere;
                                   CN =:= retrieve_mam_pm ->
    case proplists:get_value(mam_backend, Config, skip) of
        skip ->
            {skip, no_mam_backend_configured};
        Backend ->
            dynamic_modules:restore_modules(domain(), Config),
            RequiredModules = mam_required_modules(CN, Backend),
            dynamic_modules:ensure_modules(domain(), RequiredModules),
            ct:log("required modules: ~p~n", [RequiredModules]),
            escalus:init_per_testcase(CN, [{mam_modules, RequiredModules} | Config])
    end;
init_per_testcase(remove_roster = CN, Config) ->
    Backend = pick_enabled_backend(),
    dynamic_modules:ensure_modules(domain(), [{mod_roster, [{backend, Backend}]}]),
    escalus:init_per_testcase(CN, Config);
init_per_testcase(CN, Config) ->
    escalus:init_per_testcase(CN, Config).


end_per_testcase(CN, Config) when CN =:= retrieve_mam_muc_light;
                                  CN =:= retrieve_mam_pm_and_muc_light_interfere;
                                  CN =:= retrieve_mam_pm_and_muc_light_dont_interfere ->
    muc_light_helper:clear_db(),
    escalus:end_per_testcase(CN, Config);
%% mod_inbox
end_per_testcase(CN, Config) when
      CN =:= remove_inbox;
      CN =:= retrieve_inbox;
      CN =:= remove_inbox_muclight;
      CN =:= retrieve_inbox_muclight ->
    muc_light_helper:clear_db(),
    escalus:end_per_testcase(CN, Config);
end_per_testcase(CN, Config) when CN =:= retrieve_inbox_muc;
                                  CN =:= remove_inbox_muc ->
    muc_helper:unload_muc(),
    escalus:end_per_testcase(CN, Config);
end_per_testcase(CN, Config) ->
    escalus_fresh:clean(),
    escalus:end_per_testcase(CN, Config).

init_inbox(CN, Config, GroupChatType) ->
    case (not ct_helper:is_ct_running())
         orelse mongoose_helper:is_rdbms_enabled(domain()) of
        true ->
            dynamic_modules:ensure_modules(domain(), inbox_required_modules(GroupChatType)),
            escalus:init_per_testcase(CN, Config);
        false ->
            {skip, require_rdbms}
    end.
inbox_required_modules(Type) ->
    GroupChatModules = groupchat_module(Type),
    Inbox = {mod_inbox, [{aff_changes, true},
                         {remove_on_kicked, true},
                         {groupchat, [Type]},
                         {markers, [displayed]}]},
     GroupChatModules ++ [Inbox] .

groupchat_module(muc) ->
    [];
groupchat_module(muclight) ->
    [{mod_muc_light,
     [{host, binary_to_list(?MUCLIGHTHOST)},
      {backend, mongoose_helper:mnesia_or_rdbms_backend()},
      {rooms_in_rosters, true}]}].

muclight_domain() ->
    Domain = inbox_helper:domain(),
    <<"muclight.", Domain/binary>>.

mam_required_modules(retrieve_mam_pm, Backend) ->
    [{mod_mam_meta, [{backend, Backend},
                     {pm, [{archive_groupchats, false}]}]}];
mam_required_modules(CN, Backend) when CN =:= retrieve_mam_pm_and_muc_light_dont_interfere;
                                       CN =:= retrieve_mam_muc_light ->
    [{mod_mam_meta, [{backend, Backend},
                     {pm, [{archive_groupchats, false}]},
                     {muc, [{host, "muclight.@HOST@"}]}]},
     {mod_muc_light, [{host, "muclight.@HOST@"}]}];
mam_required_modules(retrieve_mam_pm_and_muc_light_interfere, Backend) ->
    [{mod_mam_meta, [{backend, Backend},
                     {rdbms_message_format, simple}, %% ignored for any other than rdbms backend
                     {pm, [{archive_groupchats, true}]},
                     {muc, [{host, "muclight.@HOST@"}]}]},
     {mod_muc_light, [{host, "muclight.@HOST@"}]}].

pick_enabled_backend() ->
    BackendsList = [
        {mam_helper:is_riak_enabled(domain()), riak},
        {mongoose_helper:is_rdbms_enabled(domain()), rdbms}
    ],
    proplists:get_value(true, BackendsList, mnesia).


vcard_required_modules() ->
    [{mod_vcard, [{backend, pick_enabled_backend()}]}].

offline_required_modules() ->
    [{mod_offline, [{backend, pick_enabled_backend()}]}].

pubsub_required_modules() ->
    [{mod_caps, []}, {mod_pubsub, [
                                   {backend, mongoose_helper:mnesia_or_rdbms_backend()},
                                   {host, "pubsub.@HOST@"},
                                   {nodetree, <<"tree">>},
                                   {plugins, [<<"flat">>, <<"pep">>, <<"push">>]}
                                  ]
                     }].

vcard_started() ->
    dynamic_modules:ensure_modules(domain(), vcard_required_modules()).

offline_started() ->
    dynamic_modules:ensure_modules(domain(), offline_required_modules()).

private_required_modules() ->
    [{mod_private, [{backend, pick_enabled_backend()}]}].

private_started() ->
    dynamic_modules:ensure_modules(domain(), private_required_modules()).

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

remove_vcard(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceFields = [{<<"FN">>, <<"Alice">>}, {<<"LN">>, <<"Ecila">>}],
        AliceSetResultStanza
            = escalus:send_and_wait(Alice, escalus_stanza:vcard_update(AliceFields)),
        escalus:assert(is_iq_result, AliceSetResultStanza),
        AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
        AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),

        maybe_stop_and_unload_module(mod_vcard, mod_vcard_backend, Config),
        {0, _} = unregister(Alice, Config),

        mongoose_helper:wait_until(
            fun() ->
                mongoose_helper:successful_rpc(mod_vcard, get_personal_data,
                    [AliceU, AliceS])
            end, [{vcard,["jid","vcard"],[]}])

        end).

remove_private(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
        AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),

        %% Add some private data for Alice
        Element = #xmlel{name = <<"item">>,
                         attrs = [{<<"xmlns">>, <<"alice:private_remove:ns">>}],
                         children = [#xmlcdata{ content = <<"Something to declare">> }]},
        SetPrivateResult = escalus:send_and_wait(Alice,
                             escalus_stanza:private_set(Element)),
        escalus:assert(is_iq_result, SetPrivateResult),

        %% Verify the data is stored
        mongoose_helper:wait_until(
            fun() ->
                mongoose_helper:successful_rpc(mod_private, get_personal_data,
                    [AliceU, AliceS])
            end, [{private, ["ns","xml"],
                   [{<<"alice:private_remove:ns">>,
                     <<"<item xmlns='alice:private_remove:ns'>Something to declare</item>">>}]}]),

        maybe_stop_and_unload_module(mod_private, mod_private_backend, Config),

        %% Remove Alice
        {0, _} = unregister(Alice, Config),

        %% Expect Alice's data to be gone
        mongoose_helper:wait_until(
            fun() ->
                mongoose_helper:successful_rpc(mod_private, get_personal_data,
                    [AliceU, AliceS])
            end, [{private, ["ns","xml"], []}])

        end).

dont_remove_other_user_private_xml(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
        AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),

        %% Add some private data for Alice and Bob
        AliceNS = <<"alice:private:ns">>,
        AliceContent = <<"To be or not to be">>,
        BobNS = <<"bob:private:ns">>,
        BobContent = <<"This is the winter of our discontent">>,
        send_and_assert_private_stanza(Alice, AliceNS, AliceContent),
        send_and_assert_private_stanza(Bob, BobNS, BobContent),

        maybe_stop_and_unload_module(mod_private, mod_private_backend, Config),

        %% Remove Alice
        {0, _} = unregister(Alice, Config),

        %% Expect Alice's data to be gone
        mongoose_helper:wait_until(
            fun() ->
                mongoose_helper:successful_rpc(mod_private, get_personal_data,
                    [AliceU, AliceS])
            end, [{private, ["ns","xml"], []}]),

        %% Verify that Bob's data is left intact
        ExpectedHeader = ["ns", "xml"],
        ExpectedItems = [#{ "ns" => binary_to_list(BobNS),
                            "xml" => [{contains, binary_to_list(BobNS)},
                                      {contains, binary_to_list(BobContent)}] }
                        ],
        retrieve_and_validate_personal_data(
            Bob, Config, "private", ExpectedHeader, ExpectedItems)

    end).

remove_multiple_private_xmls(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
        AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),

        %% Add some private data for Alice for multiple keys
        NSsAndContents = [
                          {<<"alice:private:ns1">>, <<"Some text">>},
                          {<<"alice:private:ns2">>, <<"Other text for another key">>},
                          {<<"alice:private:ns3">>, <<"Even more of text">>}
                         ],
        lists:foreach(
            fun({NS, Content}) ->
                send_and_assert_private_stanza(Alice, NS, Content)
            end, NSsAndContents),
        ExpectedHeader = ["ns", "xml"],
        ExpectedItems = lists:map(
            fun({NS, Content}) ->
                #{ "ns" => binary_to_list(NS),
                   "xml" => [{contains, binary_to_list(NS)},
                         {contains, binary_to_list(Content)}]}
            end, NSsAndContents),

        maybe_stop_and_unload_module(mod_private, mod_private_backend, Config),

        %% Verify the data is stored
        retrieve_and_validate_personal_data(
          Alice, Config, "private", ExpectedHeader, ExpectedItems),

        %% Remove Alice
        {0, _} = unregister(Alice, Config),

        %% Expect all of Alice's data to be gone
        mongoose_helper:wait_until(
        fun() ->
            mongoose_helper:successful_rpc(mod_private, get_personal_data,
                [AliceU, AliceS])
        end, [{private, ["ns","xml"], []}])

     end).

retrieve_roster(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            escalus_story:make_all_clients_friends([Alice, Bob]),
            BobU = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
            BobS = escalus_utils:jid_to_lower(escalus_client:server(Bob)),
            ExpectedItems = [
                             #{ "jid" => [{contains,  BobU}, {contains, BobS}] }
                            ],
            maybe_stop_and_unload_module(mod_roster, mod_roster_backend, Config),
            retrieve_and_validate_personal_data(
                Alice, Config, "roster", expected_header(mod_roster), ExpectedItems)
        end).

remove_roster(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus_story:make_all_clients_friends([Alice, Bob]),
        AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
        AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
        ExpectedItems = [
                         #{ "jid" => [{contains,  AliceU}, {contains, AliceS}] }
                        ],

        maybe_stop_and_unload_module(mod_roster, mod_roster_backend, Config),
        {0, _} = unregister(Alice, Config),

        mongoose_helper:wait_until(
            fun() ->
                mongoose_helper:successful_rpc(mod_roster, get_personal_data,
                    [AliceU, AliceS])
            end,
            [{roster, expected_header(mod_roster), []}]),
            retrieve_and_validate_personal_data(
                Bob, Config, "roster", expected_header(mod_roster), ExpectedItems)

        end).

retrieve_mam_pm(Config) ->
    F = fun(Alice, Bob) ->
            Msg1 = <<"1some simple pm message">>,
            Msg2 = <<"2another simple pm message">>,
            Msg3 = <<"3third simple pm message">>,
            escalus:send(Alice, escalus_stanza:chat_to(Bob, Msg1)),
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg2)),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, Msg3)),
            [mam_helper:wait_for_archive_size(User, 3) || User <- [Alice, Bob]],
            AliceJID = escalus_client:full_jid(Alice),
            BobJID = escalus_client:full_jid(Bob),

            ExpectedHeader = ["id", "from", "message"],
            ExpectedItems = [
                                #{"message" => [{contains, Msg1}], "from" => [{jid, AliceJID}]},
                                #{"message" => [{contains, Msg3}], "from" => [{jid, AliceJID}]},
                                #{"message" => [{contains, Msg2}], "from" => [{jid, BobJID}]}
                            ],

            BackendModule = choose_mam_backend(Config, mam),
            maybe_stop_and_unload_module(mod_mam, BackendModule, Config),

            retrieve_and_validate_personal_data(
                Alice, Config, "mam_pm", ExpectedHeader, ExpectedItems, ["from", "message"]),
            retrieve_and_validate_personal_data(
                Bob, Config, "mam_pm", ExpectedHeader, ExpectedItems, ["from", "message"])
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

retrieve_mam_muc_light(Config) ->
    F = fun(Alice, Bob, Kate) ->
            RoomJid = muc_light_helper:given_muc_light_room(undefined, Alice, [{Bob, member}, {Kate, member}]),
            [Room, Domain] = binary:split(RoomJid, <<"@">>),
            Body1 = <<"1some simple muc message">>,
            Body2 = <<"2another one">>,
            Body3 = <<"3third message">>,

            M1 = muc_light_helper:when_muc_light_message_is_sent(Alice, Room, Body1, <<"Id1">>),
            muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob, Kate], M1),
            M2 = muc_light_helper:when_muc_light_message_is_sent(Alice, Room, Body2, <<"Id2">>),
            muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob, Kate], M2),
            M3 = muc_light_helper:when_muc_light_message_is_sent(Bob, Room, Body3, <<"Id3">>),
            muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob, Kate], M3),


            mam_helper:wait_for_room_archive_size(Domain, Room, 4),


            ExpectedItemsAlice = [#{"message" => [{contains, binary_to_list(Body1)}]},
                                  #{"message" => [{contains, binary_to_list(Body2)}]}],

            ExpectedItemsBob = [#{"message" => [{contains, binary_to_list(Body3)}]}],

            BackendModule = choose_mam_backend(Config, mam_muc),
            maybe_stop_and_unload_module(mod_mam_muc, BackendModule, Config),

            retrieve_and_validate_personal_data(
                Alice, Config, "mam_muc", ["id", "message"], ExpectedItemsAlice, ["message"]),
            retrieve_and_validate_personal_data(
                Bob, Config, "mam_muc", ["id", "message"], ExpectedItemsBob, ["message"]),
            refute_personal_data(Kate, Config, "mam_muc")
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], F).

retrieve_mam_pm_and_muc_light_dont_interfere(Config) ->
    F = fun(Alice, Bob, Kate) ->
            RoomJid = muc_light_helper:given_muc_light_room(undefined, Alice,
                                                            [{Bob, member}, {Kate, member}]),
            [Room, Domain] = binary:split(RoomJid, <<"@">>),
            BodyMucAlice = <<"some simple muc message from Alice">>,
            BodyMucBob = <<"some simple muc message from Bob">>,
            BodyPmAlice = <<"some simple pm message from Alice">>,
            BodyPmBob = <<"some simple pm message from Bob">>,

            M1 = muc_light_helper:when_muc_light_message_is_sent(Alice, Room, BodyMucAlice, <<"Id1">>),
            muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob, Kate], M1),
            M2 = muc_light_helper:when_muc_light_message_is_sent(Bob, Room, BodyMucBob, <<"Id2">>),
            muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob, Kate], M2),

            mam_helper:wait_for_room_archive_size(Domain, Room, 3),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, BodyPmAlice)),
            escalus:send(Bob, escalus_stanza:chat_to(Alice, BodyPmBob)),

            [mam_helper:wait_for_archive_size(User, 2) || User <- [Alice, Bob]],


            MamBackends = choose_mam_backend(Config, mam),
            MamMucBackends = choose_mam_backend(Config, mam_muc),
            maybe_stop_and_unload_module(mod_mam, MamBackends, Config),
            maybe_stop_and_unload_module(mod_mam_muc, MamMucBackends, Config),

            false = mongoose_helper:successful_rpc(mod_mam_meta, get_mam_module_opt,
                                                   [domain(), mod_mam, archive_groupchats, undefined]),

            AliceDir = retrieve_all_personal_data(Alice, Config),
            BobDir = retrieve_all_personal_data(Bob, Config),
            KateDir = retrieve_all_personal_data(Kate, Config),

            validate_personal_data(
                AliceDir, "mam_muc", ["id", "message"],
                [#{"message" => [{contains, binary_to_list(BodyMucAlice)}]}], []),
            validate_personal_data(
                BobDir, "mam_muc", ["id", "message"],
                [#{"message" => [{contains, binary_to_list(BodyMucBob)}]}], []),

            PM = [#{"message" => [{contains, BodyPmAlice}],
                    "from" => [{jid, escalus_client:full_jid(Alice)}]},
                  #{"message" => [{contains, BodyPmBob}],
                    "from" => [{jid, escalus_client:full_jid(Bob)}]}],
            validate_personal_data(AliceDir, "mam_pm", ["id", "from", "message"], PM, ["from", "message"]),
            validate_personal_data(BobDir, "mam_pm", ["id", "from", "message"], PM, ["from", "message"]),
            refute_personal_data(KateDir, "mam_pm"),
            refute_personal_data(KateDir, "mam_muc")
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], F).

retrieve_mam_pm_and_muc_light_interfere(Config) ->
    F = fun(Alice, Bob, Kate) ->
            RoomJid = muc_light_helper:given_muc_light_room(undefined, Alice,
                                                            [{Bob, member}, {Kate, member}]),
            [Room, Domain] = binary:split(RoomJid, <<"@">>),
            BodyMucAlice = <<"some simple muc message from Alice">>,
            BodyMucBob = <<"some simple muc message from Bob">>,
            BodyPmAlice = <<"some simple pm message from Alice">>,
            BodyPmBob = <<"some simple pm message from Bob">>,

            M1 = muc_light_helper:when_muc_light_message_is_sent(Alice, Room, BodyMucAlice, <<"Id1">>),
            muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob, Kate], M1),
            M2 = muc_light_helper:when_muc_light_message_is_sent(Bob, Room, BodyMucBob, <<"Id2">>),
            muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob, Kate], M2),

            mam_helper:wait_for_room_archive_size(Domain, Room, 3),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, BodyPmAlice)),
            escalus:send(Bob, escalus_stanza:chat_to(Alice, BodyPmBob)),

            [mam_helper:wait_for_archive_size(User, 5) || User <- [Alice, Bob]],
            mam_helper:wait_for_archive_size(Kate, 3),

            MamBackends = choose_mam_backend(Config, mam),
            MamMucBackends = choose_mam_backend(Config, mam_muc),
            maybe_stop_and_unload_module(mod_mam, MamBackends, Config),
            maybe_stop_and_unload_module(mod_mam_muc, MamMucBackends, Config),

            true = mongoose_helper:successful_rpc(mod_mam_meta, get_mam_module_opt,
                                                   [domain(), mod_mam, archive_groupchats, undefined]),

            AliceDir = retrieve_all_personal_data(Alice, Config),
            BobDir = retrieve_all_personal_data(Bob, Config),
            KateDir = retrieve_all_personal_data(Kate, Config),

            validate_personal_data(
                AliceDir, "mam_muc", ["id", "message"],
                [#{"message" => [{contains, binary_to_list(BodyMucAlice)}]}], []),
            validate_personal_data(
                BobDir, "mam_muc", ["id", "message"],
                [#{"message" => [{contains, binary_to_list(BodyMucBob)}]}], []),

            AliceRoomJid = <<RoomJid/binary, "/", (escalus_client:short_jid(Alice))/binary>>,
            BobRoomJid = <<RoomJid/binary, "/", (escalus_client:short_jid(Bob))/binary>>,

            MucPM = [#{"message" => [{contains, "urn:xmpp:muclight:0#affiliations"}],
                       "from" => [{jid, RoomJid}]},
                     #{"message" => [{contains, BodyMucAlice}], "from" => [{jid, AliceRoomJid}]},
                     #{"message" => [{contains, BodyMucBob}], "from" => [{jid, BobRoomJid}]}],
            AllPM = MucPM ++ [#{"message" => [{contains, BodyPmAlice}],
                                "from" => [{jid, escalus_client:full_jid(Alice)}]},
                              #{"message" => [{contains, BodyPmBob}],
                                    "from" => [{jid, escalus_client:full_jid(Bob)}]}],
            SortFn = muc_msg_first(RoomJid),
            validate_personal_data(AliceDir, "mam_pm", ["id", "from", "message"], AllPM, SortFn),
            validate_personal_data(BobDir, "mam_pm", ["id", "from", "message"], AllPM, SortFn),
            validate_personal_data(KateDir, "mam_pm", ["id", "from", "message"], MucPM, SortFn),
            refute_personal_data(KateDir, "mam_muc")
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], F).

retrieve_offline(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            mongoose_helper:logout_user(Config, Alice),
            Body1 = <<"1Hey!">>,
            Body2 = <<"2Here is Johnny!">>,
            Body3 = <<"3Where is Johnny ?">>,
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Body1)),
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Body2)),
            escalus:send(Kate, escalus_stanza:chat_to(Alice, Body3)),
            %% Well, jid_to_lower works for any binary :)
            AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
            AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
            mongoose_helper:wait_until(
              fun() ->
                      mongoose_helper:successful_rpc(mod_offline_backend, count_offline_messages,
                                                     [AliceU, AliceS, 10])
              end, 3),

            BobJid = escalus_client:full_jid(Bob),
            AliceJid = escalus_client:short_jid(Alice),
            KateJid = escalus_client:full_jid(Kate),
            ExpectedHeader = ["timestamp", "from", "to", "packet"],
            Expected = [{Body1, BobJid, AliceJid}, {Body2, BobJid, AliceJid}, {Body3, KateJid, AliceJid}],

            ExpectedItems = lists:map(fun({Body, From ,To}) ->
                #{ "packet" => [{contains, Body}],
                    "from" => binary_to_list(From),
                    "to" => binary_to_list(To),
                    "timestamp" => [{validate, fun validate_datetime/1}]}
            end, Expected),

            maybe_stop_and_unload_module(mod_offline, mod_offline_backend, Config),

            retrieve_and_validate_personal_data(
              Alice, Config, "offline", ExpectedHeader, ExpectedItems, ["packet"])
        end).

remove_offline(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            mongoose_helper:logout_user(Config, Alice),
            Body1 = <<"Hey!">>,
            Body2 = <<"Here is Johnny!">>,
            Body3 = <<"Where is Johnny ?">>,
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Body1)),
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Body2)),
            escalus:send(Kate, escalus_stanza:chat_to(Alice, Body3)),
            %% Well, jid_to_lower works for any binary :)
            AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
            AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
            mongoose_helper:wait_until(
              fun() ->
                      mongoose_helper:successful_rpc(mod_offline_backend, count_offline_messages,
                                                     [AliceU, AliceS, 10])
              end, 3),

            maybe_stop_and_unload_module(mod_offline, mod_offline_backend, Config),
            {0, _} = unregister(Alice, Config),

            mongoose_helper:wait_until(
                fun() ->
                    mongoose_helper:successful_rpc(mod_offline, get_personal_data,
                        [AliceU, AliceS])
                end, [{offline, ["timestamp","from", "to", "packet"],[]}])
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
                         pubsub_payloads_row_map(NodeName1, "Item2", StringItem2),
                         pubsub_payloads_row_map(NodeName1, "Item3", StringItem3),
                         pubsub_payloads_row_map(NodeName2, "OtherItem", StringOther)],

        maybe_stop_and_unload_module(mod_pubsub, mod_pubsub_db_backend, Config),
        retrieve_and_validate_personal_data(Alice, Config, "pubsub_payloads",
                                            ["node_name", "item_id", "payload"],
                                            ExpectedItems, ["item_id"])
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
        Node1 = {_Domain, NodeName1} = {pubsub_tools:node_addr(), <<"node1">>},
        Node2 = {_Domain, NodeName2} = {pubsub_tools:node_addr(), <<"node2">>},
        Node3 = {_Domain, NodeName3} = {pubsub_tools:node_addr(), <<"node3">>},

        NodeNS = <<"myns">>,
        PepNode = make_pep_node_info(Alice, NodeNS),
        AccessModel = {<<"pubsub#access_model">>, <<"authorize">>},

        pubsub_tools:create_node(Alice, Node1, []),
        pubsub_tools:create_node(Alice, Node2, []),
        pubsub_tools:create_node(Alice, PepNode, [{config, [AccessModel]}]),
        pubsub_tools:create_node(Bob, Node3, [{type, <<"push">>}]),

        ExpectedHeader = ["node_name", "type"],

        retrieve_and_validate_personal_data(
            Alice, Config, "pubsub_nodes", ExpectedHeader,
            [pubsub_nodes_row_map(NodeNS, "pep"),
             pubsub_nodes_row_map(NodeName1, "flat"),
             pubsub_nodes_row_map(NodeName2, "flat")]),

        retrieve_and_validate_personal_data(
            Bob, Config, "pubsub_nodes", ExpectedHeader,
            [pubsub_nodes_row_map(NodeName3, "push")]),


        Nodes = [{Alice, PepNode}, {Alice, Node1}, {Alice, Node2}, {Bob, Node3}],
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
        Node1 = {_Domain, NodeName1} = {pubsub_tools:node_addr(), <<"node1xx">>},
        Node2 = {_Domain, NodeName2} = {pubsub_tools:node_addr(), <<"node2xx">>},
        Node3 = {_Domain, NodeName3} = {pubsub_tools:node_addr(), <<"node3xx">>},
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
             pubsub_payloads_row_map(NodeName2, "Item2", StringItem2)]),

        dynamic_modules:ensure_modules(domain(), pubsub_required_modules()),
        Nodes = [{Alice, Node1}, {Alice, Node2}, {Bob, Node3}],
        [pubsub_tools:delete_node(User, Node, []) || {User, Node} <- Nodes]
      end).


retrieve_private_xml(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            NS = <<"alice:gdpr:ns">>,
            Content = <<"dGhlcmUgYmUgZHJhZ29ucw==">>,
            send_and_assert_private_stanza(Alice, NS, Content),
            ExpectedHeader = ["ns", "xml"],
            ExpectedItems = [#{ "ns" => binary_to_list(NS),
                                "xml" => [{contains, binary_to_list(NS)},
                                          {contains, binary_to_list(Content)}] }
                            ],
            retrieve_and_validate_personal_data(
              Alice, Config, "private", ExpectedHeader, ExpectedItems)
        end).

dont_retrieve_other_user_private_xml(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            AliceNS = <<"alice:gdpr:ns">>,
            AliceContent = <<"To be or not to be">>,
            BobNS = <<"bob:gdpr:ns">>,
            BobContent = <<"This is the winter of our discontent">>,
            send_and_assert_private_stanza(Alice, AliceNS, AliceContent),
            send_and_assert_private_stanza(Bob, BobNS, BobContent),
            ExpectedHeader = ["ns", "xml"],
            ExpectedItems = [#{ "ns" => binary_to_list(AliceNS),
                                "xml" => [{contains, binary_to_list(AliceNS)},
                                          {contains, binary_to_list(AliceContent)}] }
                            ],
            retrieve_and_validate_personal_data(
              Alice, Config, "private", ExpectedHeader, ExpectedItems)
        end).

retrieve_multiple_private_xmls(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            NSsAndContents = [
                              {<<"alice:gdpr:ns1">>, <<"You do not talk about FIGHT CLUB.">>},
                              {<<"alice:gdpr:ns2">>, <<"You do not talk about FIGHT CLUB.">>},
                              {<<"alice:gdpr:ns3">>, <<"If someone says stop or goes limp,"
                                                       " taps out the fight is over.">>},
                              {<<"alice:gdpr:ns4">>, <<"Only two guys to a fight.">>},
                              {<<"alice:gdpr:ns5">>, <<"One fight at a time.">>}
                             ],
            lists:foreach(
                fun({NS, Content}) ->
                    send_and_assert_private_stanza(Alice, NS, Content)
                end, NSsAndContents),
            ExpectedHeader = ["ns", "xml"],
            ExpectedItems = lists:map(
                fun({NS, Content}) ->
                    #{ "ns" => binary_to_list(NS),
                       "xml" => [{contains, binary_to_list(NS)},
                                 {contains, binary_to_list(Content)}]}
                end, NSsAndContents),

            maybe_stop_and_unload_module(mod_private, mod_private_backend, Config),
            retrieve_and_validate_personal_data(
              Alice, Config, "private", ExpectedHeader, ExpectedItems)
        end).

retrieve_inbox_muclight(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        muc_light_helper:given_muc_light_room(?ROOM, Alice, [{Bob, member}]),
        Domain = muclight_domain(),

        Body = <<"Are you sure?">>,
        Res = muc_light_helper:when_muc_light_message_is_sent(Alice, ?ROOM, Body, <<"9128">>),
        muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob], Res),
        ExpectedHeader = ["jid", "content", "unread_count", "timestamp"],
        ExpectedAliceItems = [#{ "jid" => [{contains, <<?ROOM/binary, $@, Domain/binary>>}],
                                 "unread_count" => "0" }],
        %% MUC Light affiliations are also stored in inbox
        ExpectedBobItems = [#{ "jid" => [{contains, <<?ROOM/binary, $@, Domain/binary>>}],
                               "unread_count" => "2" }],

        retrieve_and_validate_personal_data(
          Alice, Config, "inbox", ExpectedHeader, ExpectedAliceItems),
        retrieve_and_validate_personal_data(
          Bob, Config, "inbox", ExpectedHeader, ExpectedBobItems),

        StanzaDestroy = escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_DESTROY, []), room_bin_jid(?ROOM)),
        escalus:send(Alice, StanzaDestroy),
        ok
        end).

retrieve_inbox_muc(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {ok, Room} = given_fresh_muc_room(Alice#client.props, []),
        Users = [Alice, Bob],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        RoomAddr = muc_helper:room_address(Room),

        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, [Bob]),
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),

        ExpectedHeader = ["jid", "content", "unread_count", "timestamp"],

        ExpectedBobItems = [#{
                                "content" => [{contains, Msg}],
                                "jid" => [{contains, RoomAddr}],
                                "unread_count" => "0" }],

         retrieve_and_validate_personal_data(
           Bob, Config, "inbox", ExpectedHeader, ExpectedBobItems),
        ExpectedAliceItems = [#{
                                "content" => [{contains, Msg}],
                                "jid" => [{contains, RoomAddr}],
                                "unread_count" => "1" }],

         retrieve_and_validate_personal_data(
           Alice, Config, "inbox", ExpectedHeader, ExpectedAliceItems),
        ok
      end).

retrieve_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            BobU = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
            BobS = escalus_utils:jid_to_lower(escalus_client:server(Bob)),
            AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
            AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
            Body = <<"With spam?">>,
            send_and_assert_is_chat_message(Bob, Alice, Body),
            ExpectedHeader = ["jid", "content", "unread_count", "timestamp"],
            ExpectedAliceItems = [#{ "content" => [{contains, Body}],
                                     "jid" => [{contains, BobS},
                                               {contains, BobU}],
                                     "unread_count" => "1" }],
            ExpectedBobItems = [#{ "content" => [{contains, Body}],
                                    "jid" => [{contains, AliceS},
                                              {contains, AliceU}],
                                    "unread_count" => "0" }],
            retrieve_and_validate_personal_data(
              Alice, Config, "inbox", ExpectedHeader, ExpectedAliceItems),
            retrieve_and_validate_personal_data(
              Bob, Config, "inbox", ExpectedHeader, ExpectedBobItems)
        end).

remove_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
            AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
            Body = <<"With spam?">>,
            send_and_assert_is_chat_message(Bob, Alice, Body),

            ExpectedHeader = ["jid", "content", "unread_count", "timestamp"],

            maybe_stop_and_unload_module(mod_inbox, mod_inbox_backend, Config),
            {0, _} = unregister(Alice, Config),

            mongoose_helper:wait_until(
              fun() ->
                      mongoose_helper:successful_rpc(mod_inbox, get_personal_data,
                                                     [AliceU, AliceS])
              end, [{inbox, ExpectedHeader, []}]),

            ExpectedBobItems = [
                             #{ "content" => [{contains, Body}],
                                "jid" => [{contains, AliceS},
                                          {contains, AliceU}],
                                "unread_count" => "0" }
                            ],
            retrieve_and_validate_personal_data(
              Bob, Config, "inbox", ExpectedHeader, ExpectedBobItems)
        end).

remove_inbox_muclight(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
        AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
        Domain = muclight_domain(),
        Room = <<"ttt2">>,
        muc_light_helper:given_muc_light_room(Room, Alice, [{Bob, member}]),

        Body = <<"Are you sure?">>,
        Res = muc_light_helper:when_muc_light_message_is_sent(Alice, Room , Body, <<"9128">>),
        muc_light_helper:then_muc_light_message_is_received_by([Alice, Bob], Res),

        ExpectedHeader = ["jid", "content", "unread_count", "timestamp"],

        muc_light_helper:user_leave(Room, Bob, [{Alice, owner}]),
        maybe_stop_and_unload_module(mod_inbox, mod_inbox_backend, Config),
        {0, _} = unregister(Alice, Config),

        %% MUC Light affiliations are also stored in inbox
        ExpectedBobItems = [#{
                                "jid" => [{contains, <<Room/binary, $@, Domain/binary>>}],
                                "unread_count" => "3" }
                           ],

         retrieve_and_validate_personal_data(
           Bob, Config, "inbox", ExpectedHeader, ExpectedBobItems),

        mongoose_helper:wait_until(
          fun() ->
                  mongoose_helper:successful_rpc(mod_inbox, get_personal_data,
                                                 [AliceU, AliceS])
          end, [{inbox, ExpectedHeader, []}]),

        timer:sleep(5000),
        StanzaDestroy = escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_DESTROY, []),
                                      room_bin_jid(Room)),
        escalus:send(Alice, StanzaDestroy),
        ok
        end).

remove_inbox_muc(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
        AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
        {ok, Room} = given_fresh_muc_room(Alice#client.props, []),

        Users = [Alice, Bob],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        RoomAddr = muc_helper:room_address(Room),

        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, [Bob]),
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),

        ExpectedHeader = ["jid", "content", "unread_count", "timestamp"],

        maybe_stop_and_unload_module(mod_inbox, mod_inbox_backend, Config),
        {0, _} = unregister(Alice, Config),

        escalus:wait_for_stanza(Bob),
        mongoose_helper:wait_until(
        fun() ->
                mongoose_helper:successful_rpc(mod_inbox, get_personal_data,
                                               [AliceU, AliceS])
        end, [{inbox, ExpectedHeader, []}]),

        ExpectedBobItems = [#{
                                "content" => [{contains, Msg}],
                                "jid" => [{contains, RoomAddr}],
                                "unread_count" => "0" }],

         retrieve_and_validate_personal_data(
           Bob, Config, "inbox", ExpectedHeader, ExpectedBobItems),
        ok
      end).

retrieve_inbox_for_multiple_messages(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Bodies = [ <<"Nobody exists on purpose.">>,
                       <<"Nobody belongs anywhere.">>,
                       <<"We're all going to die.">>,
                       <<"Come watch TV.">>],
            lists:foreach(fun(Body) -> send_and_assert_is_chat_message(Bob, Alice, Body) end, Bodies),
            BobU = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
            BobS = escalus_utils:jid_to_lower(escalus_client:server(Bob)),

            ExpectedHeader = ["jid", "content", "unread_count", "timestamp"],
            ExpectedAliceItems = [#{ "content" => [{contains, lists:last(Bodies)}],
                                     "jid" => [{contains, BobS},
                                               {contains, BobU}],
                                     "unread_count" => integer_to_list(length(Bodies)) }],
            retrieve_and_validate_personal_data(
              Alice, Config, "inbox", ExpectedHeader, ExpectedAliceItems)
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




muc_domain() ->
    Domain = inbox_helper:domain(),
    <<"muc.", Domain/binary>>.

maybe_stop_and_unload_module(Module, Backends, Config) when is_list(Backends)->
    case proplists:get_value(disable_module, Config) of
        true ->
            dynamic_modules:stop(domain(), Module),
            [delete_backend(B) || B <- Backends];
        _ ->
            ok
    end;
maybe_stop_and_unload_module(Module, BackendProxy, Config) ->
    maybe_stop_and_unload_module(Module, [BackendProxy], Config).

delete_backend(BackendProxy)->
    dynamic_modules:stop(domain(), BackendProxy),
    mongoose_helper:successful_rpc(code, purge, [BackendProxy]),
    true = mongoose_helper:successful_rpc(code, delete, [BackendProxy]).

retrieve_and_validate_personal_data(User, Config, FilePrefix, ExpectedHeader, ExpectedItems) ->
    Dir = retrieve_all_personal_data(User, Config),
    validate_personal_data(Dir, FilePrefix, ExpectedHeader, ExpectedItems, ExpectedHeader).

retrieve_and_validate_personal_data(User, Config, FilePrefix, ExpectedHeader, ExpectedItems, SortBy) ->
    Dir = retrieve_all_personal_data(User, Config),
    validate_personal_data(Dir, FilePrefix, ExpectedHeader, ExpectedItems, SortBy).

validate_personal_data(Dir, FilePrefix, ExpectedHeader, ExpectedItems, SortBy) ->
    PersonalCSV = decode_personal_data(Dir, FilePrefix),
    UnsortedMaps = csv_to_maps(ExpectedHeader, PersonalCSV),
    PersonalMaps = lists:sort(get_sort_fn(SortBy), UnsortedMaps),
    try validate_personal_maps(PersonalMaps, ExpectedItems) of
        _ -> ok
    catch
        C:R ->
            ct:fail(#{
                        class => C,
                        reason => R,
                        stacktrace => erlang:get_stacktrace(),
                        sorted_by => SortBy,
                        personal_maps => PersonalMaps,
                        expected_items => ExpectedItems
                    })
    end.


get_sort_fn(SortBy) when is_list(SortBy) ->
    %% if SortBy is [], than original list remains unsorted.
    fun(Map1, Map2) -> compare_maps(SortBy, Map1, Map2) end;
get_sort_fn(SortFn) when is_function(SortFn, 2) ->
    SortFn.

compare_maps([], _, _) -> true;
compare_maps([Key | T], Map1, Map2) ->
    #{Key:=Val1} = Map1,
    #{Key:=Val2} = Map2,
    if
        Val1 =:= Val2 -> compare_maps(T, Map1, Map2);
        Val1 > Val2 -> false;
        Val1 < Val2 -> true
    end.

muc_msg_first(MucJid) ->
    N = erlang:byte_size(MucJid),
    fun(#{"from" := JID1}, #{"from" := JID2}) ->
        case {JID1, JID2} of
            {<<MucJid:N/binary, _/binary>>, <<MucJid:N/binary, _/binary>>} ->
                JID1 =< JID2;
            {<<MucJid:N/binary, _/binary>>, _} ->
                true;
            {_, <<MucJid:N/binary, _/binary>>} ->
                false;
            {_, _} ->
                JID1 =< JID2
        end
    end.

csv_to_maps(ExpectedHeader, [ExpectedHeader | Rows]) ->
    lists:foldl(fun(Row, Maps) -> [ csv_row_to_map(ExpectedHeader, Row) | Maps ] end, [], Rows).

csv_row_to_map(Header, Row) ->
    maps:from_list(lists:zip(Header, Row)).

validate_personal_maps(PersonalMaps, ExpectedItems) ->
    validate_sorted_personal_maps(PersonalMaps, ExpectedItems).

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
validate_personal_item(Value, [{jid, ExpectedValue} | RConditions]) ->
    JID = escalus_utils:jid_to_lower(to_binary(Value)),
    JID = escalus_utils:jid_to_lower(to_binary(ExpectedValue)),
    validate_personal_item(Value, RConditions);
validate_personal_item(Value, [{contains, String} | RConditions]) ->
    {match, _} = re:run(Value, String),
    validate_personal_item(Value, RConditions);
validate_personal_item(Value, [{validate, Validator} | RConditions]) when is_function(Validator) ->
    true = Validator(Value),
    validate_personal_item(Value, RConditions).

to_binary(List) when is_list(List)       -> list_to_binary(List);
to_binary(Binary) when is_binary(Binary) -> Binary.

decode_personal_data(Dir, FilePrefix) ->
    CSVPath = filename:join(Dir, FilePrefix ++ ".csv"),
    {ok, Content} = file:read_file(CSVPath),
    % We expect non-empty list because it must contain at least header with columns names
    [_ | _] = csv:decode_binary(Content).

refute_personal_data(Client, Config, FilePrefix) ->
    Dir = retrieve_all_personal_data(Client, Config),
    refute_personal_data(Dir, FilePrefix).

refute_personal_data(Dir, FilePrefix) ->
    CSVPath = filename:join(Dir, FilePrefix ++ ".csv"),
    false = filelib:is_regular(CSVPath).

retrieve_all_personal_data(Client, Config) ->
    User = escalus_client:username(Client),
    Domain = escalus_client:server(Client),
    request_and_unzip_personal_data(User, Domain, Config).

request_and_unzip_personal_data(User, Domain, Config) ->
    {Filename, 0, _} = retrieve_personal_data(User, Domain, Config),
    FullPath = get_mim_cwd() ++ "/" ++ Filename,
    Dir = make_dir_name(Filename, User),
    ct:log("extracting logs ~s", [Dir]),
    {ok, _} = zip:extract(FullPath, [{cwd, Dir}]),
    Dir.

make_dir_name(Filename, User) when is_binary(User) ->
    make_dir_name(Filename, binary_to_list(User));
make_dir_name(Filename, User) when is_list(User) ->
    Filename ++ "." ++ User ++ ".unzipped".

retrieve_personal_data(User, Domain, Config) ->
    Filename = random_filename(Config),
    {CommandOutput, Code} = ejabberdctl("retrieve_personal_data", [User, Domain, Filename], Config),
    {Filename, Code, CommandOutput}.

unregister(Client, Config) ->
    User = escalus_client:username(Client),
    Domain = escalus_client:server(Client),
    {CommandOutput, Code} = ejabberdctl("unregister", [User, Domain], Config),
    {Code, CommandOutput}.

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

item_content(Data) ->
    Bin = item_content_xml(Data),
    {Bin, binary_to_list(exml:to_binary(Bin))}.

item_content_xml(Data) ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}],
           children = [#xmlcdata{content = Data}]}.

send_and_assert_private_stanza(User, NS, Content) ->
    XML = #xmlel{ name = <<"fingerprint">>,
                  attrs = [{<<"xmlns">>, NS}],
                  children = [#xmlcdata{ content = Content }]},
    PrivateStanza = escalus_stanza:private_set(XML),
    escalus_client:send(User, PrivateStanza),
    escalus:assert(is_iq_result, [PrivateStanza], escalus_client:wait_for_stanza(User)).

send_and_assert_is_chat_message(UserFrom, UserTo, Body) ->
    escalus:send(UserFrom, escalus_stanza:chat_to(UserTo, Body)),
    Msg = escalus:wait_for_stanza(UserTo),
    escalus:assert(is_chat_message, [Body], Msg).

validate_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    validate_date(Date),
    validate_time(Time).

validate_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    calendar:valid_date(Date1).

validate_time(Time) ->
  [T | _] = string:tokens(Time, "Z"),
  validate_time1(T).


validate_time1(Time) ->
    [H, M, S] = string:tokens(Time, ":"),
    check_list([{H, 24}, {M, 60}, {S, 60}]).

check_list(List) ->
    lists:all(fun({V, L}) -> I = list_to_integer(V), I >= 0 andalso I < L end, List).

choose_mam_backend(Config, mam) ->
    case proplists:get_value(mam_backend, Config) of
            rdbms -> mod_mam_rdbms_arch;
            riak -> mod_mam_riak_timed_arch_yz;
            cassandra -> [mod_mam_cassandra_arch, mod_mam_cassandra_arch_params];
            elasticsearch -> mod_mam_elasticsearch_arch
    end;
choose_mam_backend(Config, mam_muc) ->
    case proplists:get_value(mam_backend, Config) of
            rdbms -> mod_mam_muc_rdbms_arch;
            riak -> mod_mam_riak_timed_arch_yz;
            cassandra -> [mod_mam_muc_cassandra_arch, mod_mam_muc_cassandra_arch_params];
            elasticsearch -> mod_mam_muc_elasticsearch_arch
    end.

expected_header(mod_roster) -> ["jid", "name", "subscription",
                              "ask", "groups", "askmessage", "xs"].

given_fresh_muc_room(UserSpec, RoomOpts) ->
    Username = proplists:get_value(username, UserSpec),
    RoomName = muc_helper:fresh_room_name(Username),
    From = muc_helper:generate_rpc_jid({user, UserSpec}),
    muc_helper:create_instant_room(<<"localhost">>, RoomName, From, Username, RoomOpts),
    {ok, RoomName}.
