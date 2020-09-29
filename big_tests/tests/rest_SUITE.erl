%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(rest_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(rest_helper,
        [assert_inlist/2,
         assert_notinlist/2,
         decode_maplist/1,
         gett/2,
         post/2,
         putt/2,
         delete/1,
         gett/3,
         post/3,
         putt/3,
         delete/2]
    ).

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).
-define(OK, {<<"200">>, <<"OK">>}).
-define(CREATED, {<<"201">>, <<"Created">>}).
-define(NOCONTENT, {<<"204">>, <<"No Content">>}).
-define(ERROR, {<<"500">>, _}).
-define(NOT_FOUND, {<<"404">>, _}).
-define(NOT_AUTHORIZED, {<<"401">>, _}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds
-define(ATOMS, [name, desc, category, action, security_policy, args, result, sender]).

all() ->
    [
     {group, admin},
     {group, dynamic_module},
     {group, auth},
     {group, blank_auth},
     {group, roster}
    ].

groups() ->
    G = [{admin, [parallel], test_cases()},
         {auth, [parallel], auth_test_cases()},
         {blank_auth, [parallel], blank_auth_testcases()},
         {roster, [parallel], [list_contacts,
                               befriend_and_alienate,
                               befriend_and_alienate_auto]},
         {dynamic_module, [], [stop_start_command_module]}],
    ct_helper:repeat_all_until_all_ok(G).

auth_test_cases() ->
    [auth_passes_correct_creds,
     auth_fails_incorrect_creds].

blank_auth_testcases() ->
    [auth_always_passes_blank_creds].

test_cases() ->
    [commands_are_listed,
     non_existent_command_returns404,
     user_can_be_registered_and_removed,
     sessions_are_listed,
     session_can_be_kicked,
     messages_are_sent_and_received,
     stanzas_are_sent_and_received,
     messages_are_archived,
     messages_can_be_paginated,
     password_can_be_changed
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

host() ->
    ct:get_config({hosts, mim, domain}).

init_per_suite(Config) ->
    Config1 = rest_helper:maybe_enable_mam(mam_helper:backend(), host(), Config),
    Config2 = ejabberd_node_utils:init(Config1),
    escalus:init_per_suite(Config2).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    rest_helper:maybe_disable_mam(mam_helper:backend(), host()),
    escalus:end_per_suite(Config).

init_per_group(auth, Config) ->
    rest_helper:change_admin_creds({<<"ala">>, <<"makota">>}),
    Config;
init_per_group(blank_auth, Config) ->
    rest_helper:change_admin_creds(any),
    Config;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(auth, _Config) ->
    rest_helper:change_admin_creds(any);
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, mike])).

init_per_testcase(CaseName, Config) ->
    MAMTestCases = [messages_are_archived, messages_can_be_paginated],
    rest_helper:maybe_skip_mam_test_cases(CaseName, MAMTestCases, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

% Authorization
auth_passes_correct_creds(_Config) ->
    % try to login with the same creds
    {?OK, _Lcmds} = gett(admin, <<"/commands">>, {<<"ala">>, <<"makota">>}).

auth_fails_incorrect_creds(_Config) ->
    % try to login with different creds
    {?NOT_AUTHORIZED, _} = gett(admin, <<"/commands">>, {<<"ola">>, <<"mapsa">>}).

auth_always_passes_blank_creds(_Config) ->
    % we set control creds for blank
    rest_helper:change_admin_creds(any),
    % try with any auth
    {?OK, _Lcmds} = gett(admin, <<"/commands">>, {<<"aaaa">>, <<"bbbb">>}),
    % try with no auth
    {?OK, _Lcmds} = gett(admin, <<"/commands">>).

commands_are_listed(_C) ->
    {?OK, Lcmds} = gett(admin, <<"/commands">>),
    DecCmds = decode_maplist(Lcmds),
    assert_inlist(#{name => <<"list_methods">>}, DecCmds).

non_existent_command_returns404(_C) ->
    {?NOT_FOUND, _} = gett(admin, <<"/isitthereornot">>).

user_can_be_registered_and_removed(_Config) ->
    % list users
    {?OK, Lusers} = gett(admin, <<"/users/localhost">>),
    Domain = domain(),
    assert_inlist(<<"alice@", Domain/binary>>, Lusers),
    % create user
    CrUser = #{username => <<"mike">>, password => <<"nicniema">>},
    {?CREATED, _} = post(admin, <<"/users/localhost">>, CrUser),
    {?OK, Lusers1} = gett(admin, <<"/users/localhost">>),
    assert_inlist(<<"mike@", Domain/binary>>, Lusers1),
    % try to create the same user
    {?ERROR, _} = post(admin, <<"/users/localhost">>, CrUser),
    % delete user
    {?NOCONTENT, _} = delete(admin, <<"/users/localhost/mike">>),
    {?OK, Lusers2} = gett(admin, <<"/users/localhost">>),
    assert_notinlist(<<"mike@", Domain/binary>>, Lusers2),
    ok.

sessions_are_listed(_) ->
    % no session
    {?OK, Sessions} = gett(admin, "/sessions/localhost"),
    true = is_list(Sessions).

session_can_be_kicked(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        % Alice is connected
        Domain = domain(),
        {?OK, Sessions1} = gett(admin, "/sessions/localhost"),
        assert_inlist(<<"alice@", Domain/binary, "/res1">>, Sessions1),
        % kick alice
        {?NOCONTENT, _} = delete(admin, "/sessions/localhost/alice/res1"),
        escalus:wait_for_stanza(Alice),
        true = escalus_connection:wait_for_close(Alice, timer:seconds(1)),
        {?OK, Sessions2} = gett(admin, "/sessions/localhost"),
        assert_notinlist(<<"alice@", Domain/binary, "/res1">>, Sessions2)
    end).

messages_are_sent_and_received(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {M1, M2} = send_messages(Alice, Bob),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, [maps:get(body, M1)], Res),
        Res1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(body, M2)], Res1)
    end).

stanzas_are_sent_and_received(Config) ->
%%    this is to test the API for sending arbitrary stanzas, e.g. message with extra elements
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        send_extended_message(Alice, Bob),
        Res = escalus:wait_for_stanza(Bob),
        ?assertEqual(<<"attribute">>, exml_query:attr(Res, <<"extra">>)),
        ?assertEqual(<<"inside the sibling">>, exml_query:path(Res, [{element, <<"sibling">>}, cdata])),
        ok
                                                        end).

messages_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {M1, _M2} = send_messages(Alice, Bob),
        AliceJID = maps:get(to, M1),
        BobJID = maps:get(caller, M1),
        GetPath = lists:flatten(["/messages",
                                 "/", binary_to_list(AliceJID),
                                 "/", binary_to_list(BobJID),
                                 "?limit=10"]),
        mam_helper:maybe_wait_for_archive(Config),
        {?OK, Msgs} = gett(admin, GetPath),
        [Last, Previous|_] = lists:reverse(decode_maplist(Msgs)),
        <<"hello from Alice">> = maps:get(body, Last),
        AliceJID = maps:get(sender, Last),
        <<"hello from Bob">> = maps:get(body, Previous),
        BobJID = maps:get(sender, Previous),
        % now if we leave limit out we should get the same result
        GetPath1 = lists:flatten(["/messages",
                                  "/", binary_to_list(AliceJID),
                                  "/", binary_to_list(BobJID)]),
        mam_helper:maybe_wait_for_archive(Config),
        {?OK, Msgs1} = gett(admin, GetPath1),
        [Last1, Previous1|_] = lists:reverse(decode_maplist(Msgs1)),
        <<"hello from Alice">> = maps:get(body, Last1),
        AliceJID = maps:get(sender, Last1),
        <<"hello from Bob">> = maps:get(body, Previous1),
        BobJID = maps:get(sender, Previous1),
        % and we can do the same without specifying contact
        GetPath2 = lists:flatten(["/messages/", binary_to_list(AliceJID)]),
        mam_helper:maybe_wait_for_archive(Config),
        {?OK, Msgs2} = gett(admin, GetPath2),
        [Last2, Previous2|_] = lists:reverse(decode_maplist(Msgs2)),
        <<"hello from Alice">> = maps:get(body, Last2),
        AliceJID = maps:get(sender, Last2),
        <<"hello from Bob">> = maps:get(body, Previous2),
        BobJID = maps:get(sender, Previous2)
    end).

messages_can_be_paginated(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        rest_helper:fill_archive(Alice, Bob),
        mam_helper:maybe_wait_for_archive(Config),
        % recent msgs with a limit
        M1 = get_messages(AliceJID, BobJID, 10),
        ?assertEqual(6, length(M1)),
        M2 = get_messages(AliceJID, BobJID, 3),
        ?assertEqual(3, length(M2)),
        % older messages - earlier then the previous midnight
        PriorTo = rest_helper:make_timestamp(-1, {0, 0, 1}) div 1000,
        M3 = get_messages(AliceJID, BobJID, PriorTo, 10),
        ?assertEqual(4, length(M3)),
        [Oldest|_] = decode_maplist(M3),
        ?assertEqual(maps:get(body, Oldest), <<"A">>),
        % same with limit
        M4 = get_messages(AliceJID, BobJID, PriorTo, 2),
        ?assertEqual(2, length(M4)),
        [Oldest2|_] = decode_maplist(M4),
        ?assertEqual(maps:get(body, Oldest2), <<"B">>),
        ok
    end).

password_can_be_changed(Config) ->
    % bob logs in with his regular password
    escalus:story(Config, [{bob, 1}], fun(#client{} = _Bob) ->
        skip
    end),
    % we change password
    NewPass = <<"niemakrolika">>,
    {?NOCONTENT, _} = putt(admin, "/users/localhost/bob",
                           #{newpass => NewPass}),
    % he logs with his alternative password
    ConfigWithBobsAltPass = escalus_users:update_userspec(Config, bob, password, NewPass),
    escalus:story(ConfigWithBobsAltPass, [{bob, 1}], fun(#client{} = _Bob) ->
        ignore
    end),
    % we can't log with regular passwd anymore
    try escalus:story(Config, [{bob, 1}], fun(Bob) -> ?PRT("Bob", Bob) end) of
        _ -> ct:fail("bob connected with old password")
    catch error:{badmatch, _} ->
        ok
    end,
    % we change it back
    {?NOCONTENT, _} = putt(admin, "/users/localhost/bob",
                           #{newpass => <<"makrolika">>}),
    % now he logs again with the regular one
    escalus:story(Config, [{bob, 1}], fun(#client{} = _Bob) ->
        just_dont_do_anything
    end),
    ok.

list_contacts(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
            BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
            add_sample_contact(Bob, Alice),
            % list bob's contacts
            {?OK, R} = gett(admin, lists:flatten(["/contacts/", binary_to_list(BobJID)])),
            [R1] = decode_maplist(R),
            #{jid := AliceJID, subscription := <<"none">>, ask := <<"none">>} = R1,
            ok
        end
    ),
    ok.

befriend_and_alienate(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
            AliceS = binary_to_list(AliceJID),
            BobS = binary_to_list(BobJID),
            AlicePath = lists:flatten(["/contacts/", AliceS]),
            BobPath = lists:flatten(["/contacts/", BobS]),
            % rosters are empty
            check_roster_empty(AlicePath),
            check_roster_empty(BobPath),
            % adds them to rosters
            {?NOCONTENT, _} = post(admin, AlicePath, #{jid => BobJID}),
            {?NOCONTENT, _} = post(admin, BobPath, #{jid => AliceJID}),
            check_roster(BobPath, AliceJID, none, none),
            check_roster(AlicePath, BobJID, none, none),
            % now do the subscription sequence
            PutPathA = lists:flatten([AlicePath, "/", BobS]),
            {?NOCONTENT, _} = putt(admin, PutPathA, #{action => <<"subscribe">>}),
            check_roster(AlicePath, BobJID, none, out),
            PutPathB = lists:flatten([BobPath, "/", AliceS]),
            {?NOCONTENT, _} = putt(admin, PutPathB, #{action => <<"subscribed">>}),
            check_roster(AlicePath, BobJID, to, none),
            check_roster(BobPath, AliceJID, from, none),
            {?NOCONTENT, _} = putt(admin, PutPathB, #{action => <<"subscribe">>}),
            check_roster(BobPath, AliceJID, from, out),
            {?NOCONTENT, _} = putt(admin, PutPathA, #{action => <<"subscribed">>}),
            check_roster(AlicePath, BobJID, both, none),
            check_roster(BobPath, AliceJID, both, none),
            % now remove
            {?NOCONTENT, _} = delete(admin, PutPathA),
            check_roster_empty(AlicePath),
            check_roster(BobPath, AliceJID, none, none),
            {?NOCONTENT, _} = delete(admin, PutPathB),
            check_roster_empty(BobPath),
            APushes = lists:filter(fun escalus_pred:is_roster_set/1,
                                    escalus:wait_for_stanzas(Alice, 20)),
            AExp = [{none, none},
                    {none, subscribe},
                    {to, none},
                    {both, none},
                    {remove, none}],
            check_pushlist(AExp, APushes),
            BPushes = lists:filter(fun escalus_pred:is_roster_set/1,
                                    escalus:wait_for_stanzas(Bob, 20)),
            BExp = [{none, none},
                    {from, none},
                    {from, subscribe},
                    {both, none},
                    {to, none},
                    {none, none},
                    {remove, none}],
            check_pushlist(BExp, BPushes),
            ok
        end
    ),
    ok.


befriend_and_alienate_auto(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
            AliceS = binary_to_list(AliceJID),
            BobS = binary_to_list(BobJID),
            AlicePath = lists:flatten(["/contacts/", AliceS]),
            BobPath = lists:flatten(["/contacts/", BobS]),
            check_roster_empty(AlicePath),
            check_roster_empty(BobPath),
            ManagePath = lists:flatten(["/contacts/",
                                     AliceS,
                                     "/",
                                     BobS,
                                     "/manage"
            ]),
            {?NOCONTENT, _} = putt(admin, ManagePath, #{action => <<"connect">>}),
            check_roster(AlicePath, BobJID, both, none),
            check_roster(BobPath, AliceJID, both, none),
            {?NOCONTENT, _} = putt(admin, ManagePath, #{action => <<"disconnect">>}),
            check_roster_empty(AlicePath),
            check_roster_empty(BobPath),
            APushes = lists:filter(fun escalus_pred:is_roster_set/1,
                                   escalus:wait_for_stanzas(Alice, 20)),
            ct:pal("APushes: ~p", [APushes]),
            AExp = [{none, none},
                    {both, none},
                    {remove, none}],
            check_pushlist(AExp, APushes),
            BPushes = lists:filter(fun escalus_pred:is_roster_set/1,
                                   escalus:wait_for_stanzas(Bob, 20)),
            ct:pal("BPushes: ~p", [BPushes]),
            BExp = [{none, none},
                    {both, none},
                    {remove, none}],
            check_pushlist(BExp, BPushes),
            ok
        end
    ),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

send_messages(Alice, Bob) ->
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    M = #{caller => BobJID, to => AliceJID, body => <<"hello from Bob">>},
    {?NOCONTENT, _} = post(admin, <<"/messages">>, M),
    M1 = #{caller => AliceJID, to => BobJID, body => <<"hello from Alice">>},
    {?NOCONTENT, _} = post(admin, <<"/messages">>, M1),
    {M, M1}.

send_extended_message(From, To) ->
    M = #xmlel{name = <<"message">>,
               attrs = [{<<"from">>, escalus_client:full_jid(From)},
                        {<<"to">>, escalus_client:full_jid(To)},
                        {<<"extra">>, <<"attribute">>}],
               children = [#xmlel{name = <<"body">>,
                                  children = [#xmlcdata{content = <<"the body">>}]},
                           #xmlel{name = <<"sibling">>,
                                  children = [#xmlcdata{content = <<"inside the sibling">>}]}
               ]
    },
    M1 = #{stanza => exml:to_binary(M)},
    {?NOCONTENT, _} = post(admin, <<"/stanzas">>, M1),
    ok.

check_roster(Path, Jid, Subs, Ask) ->
    {?OK, R} = gett(admin, Path),
    S = atom_to_binary(Subs, latin1),
    A = atom_to_binary(Ask, latin1),
    Res = decode_maplist(R),
    [#{jid := Jid, subscription := S, ask := A}] = Res.

check_roster_empty(Path) ->
    {?OK, R} = gett(admin, Path),
    [] = decode_maplist(R).

get_messages(Me, Other, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Me),
                             "/", binary_to_list(Other),
                             "?limit=", integer_to_list(Count)]),
    {?OK, Msgs} = gett(admin, GetPath),
    Msgs.

get_messages(Me, Other, Before, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Me),
                             "/", binary_to_list(Other),
                             "?before=", integer_to_list(Before),
                             "&limit=", integer_to_list(Count)]),
    {?OK, Msgs} = gett(admin, GetPath),
    Msgs.

stop_start_command_module(_) ->
    %% Precondition: module responsible for resource is started. If we
    %% stop the module responsible for this resource then the same
    %% test will fail. If we start the module responsible for this
    %% resource then the same test will succeed. With the precondition
    %% described above we test both transition from `started' to
    %% `stopped' and from `stopped' to `started'.
    {?OK, _} = gett(admin, <<"/commands">>),
    {ok, _Opts} = dynamic_modules:stop(host(), mod_commands),
    {?NOT_FOUND, _} = gett(admin, <<"/commands">>),
    {ok, _} = dynamic_modules:start(host(), mod_commands, []),
    timer:sleep(200), %% give the server some time to build the paths again
    {?OK, _} = gett(admin, <<"/commands">>).

to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_list(V) ->
    V.

domain() ->
    ct:get_config({hosts, mim, domain}).

add_sample_contact(Bob, Alice) ->
    escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                 [<<"friends">>],
                 <<"Alicja">>)),
    Received = escalus:wait_for_stanzas(Bob, 2),
    escalus:assert_many([is_roster_set, is_iq_result], Received),
    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus:assert(count_roster_items, [1], Result),
    escalus:send(Bob, escalus_stanza:iq_result(Result)).

check_pushlist([], _Stanzas) ->
    ok;
check_pushlist(Expected, []) ->
    ?assertEqual(Expected, []);
check_pushlist(Expected, [Iq|StanzaTail]) ->
    [{ExpectedSub, ExpectedAsk}| TailExp] = Expected,
    case does_push_match(Iq, ExpectedSub, ExpectedAsk) of
        true ->
            check_pushlist(TailExp, StanzaTail);
        false ->
            check_pushlist(Expected, StanzaTail)
    end.

does_push_match(Iq, ExpectedSub, ExpectedAsk) ->
    [Subs] = exml_query:paths(Iq, [{element, <<"query">>},
        {element, <<"item">>},
        {attr, <<"subscription">>}]),
    AskList = exml_query:paths(Iq, [{element, <<"query">>},
        {element, <<"item">>},
        {attr, <<"ask">>}]),
    Ask = case AskList of
              [] -> <<"none">>;
              [A] -> A
          end,
    ESub = atom_to_binary(ExpectedSub, latin1),
    EAsk = atom_to_binary(ExpectedAsk, latin1),
    {Subs, Ask} == {ESub, EAsk}.
