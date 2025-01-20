%%==============================================================================
%% Copyright 2012-2020 Erlang Solutions Ltd.
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
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(rest_helper,
        [assert_inlist/2,
         assert_notinlist/2,
         decode_maplist/1,
         gett/2,
         gett/3,
         post/3,
         putt/3,
         delete/2]
    ).
-import(domain_helper, [host_type/0, domain/0]).

-define(PRT(X, Y), ct:log("~p: ~p", [X, Y])).
-define(OK, {<<"200">>, <<"OK">>}).
-define(CREATED, {<<"201">>, <<"Created">>}).
-define(NOCONTENT, {<<"204">>, <<"No Content">>}).
-define(NOT_FOUND, {<<"404">>, _}).
-define(NOT_AUTHORIZED, {<<"401">>, _}).
-define(FORBIDDEN, {<<"403">>, _}).
-define(BAD_REQUEST, {<<"400">>, _}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, admin},
     {group, auth},
     {group, blank_auth},
     {group, roster}
    ].

groups() ->
    [{admin, [parallel], test_cases()},
     {auth, [parallel], auth_test_cases()},
     {blank_auth, [parallel], blank_auth_testcases()},
     {roster, [parallel], roster_test_cases()}
    ].

auth_test_cases() ->
    [auth_passes_correct_creds,
     auth_fails_incorrect_creds].

blank_auth_testcases() ->
    [auth_passes_without_creds,
     auth_fails_with_creds].

test_cases() ->
    [non_existent_command_returns404,
     existent_command_with_missing_arguments_returns404,
     invalid_query_string,
     invalid_request_body,
     user_can_be_registered_and_removed,
     user_registration_errors,
     sessions_are_listed,
     session_can_be_kicked,
     session_kick_errors,
     messages_are_sent_and_received,
     message_errors,
     stanzas_are_sent_and_received,
     stanza_errors,
     messages_are_archived,
     message_archive_errors,
     messages_can_be_paginated,
     password_can_be_changed,
     password_change_errors
    ].

roster_test_cases() ->
    [list_contacts,
     befriend_and_alienate,
     befriend_and_alienate_auto,
     list_contacts_errors,
     add_contact_errors,
     subscription_errors,
     delete_contact_errors].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    Config2 = rest_helper:maybe_enable_mam(mam_helper:backend(), host_type(), Config1),
    Config3 = ejabberd_node_utils:init(Config2),
    escalus:init_per_suite(Config3).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
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
    MAMTestCases = [messages_are_archived, message_archive_errors, messages_can_be_paginated],
    rest_helper:maybe_skip_mam_test_cases(CaseName, MAMTestCases, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

rpc(M, F, A) ->
    distributed_helper:rpc(distributed_helper:mim(), M, F, A).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

% Authorization
auth_passes_correct_creds(_Config) ->
    % try to login with the same creds
    {?OK, _Users} = gett(admin, path("users", [domain()]), {<<"ala">>, <<"makota">>}).

auth_fails_incorrect_creds(_Config) ->
    % try to login with different creds
    {?NOT_AUTHORIZED, _} = gett(admin, path("users", [domain()]), {<<"ola">>, <<"mapsa">>}).

auth_passes_without_creds(_Config) ->
    % try with no auth
    {?OK, _Users} = gett(admin, path("users", [domain()])).

auth_fails_with_creds(_Config) ->
    % try with any auth
    {?NOT_AUTHORIZED, _} = gett(admin, path("users", [domain()]), {<<"aaaa">>, <<"bbbb">>}).

non_existent_command_returns404(_C) ->
    {?NOT_FOUND, _} = gett(admin, <<"/isitthereornot">>).

existent_command_with_missing_arguments_returns404(_C) ->
    {?NOT_FOUND, _} = gett(admin, <<"/contacts/">>).

invalid_query_string(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    BobJid = escalus_users:get_jid(Config1, bob),
    {?BAD_REQUEST, <<"Invalid query string">>} =
        gett(admin, <<"/messages/", AliceJid/binary, "/", BobJid/binary, "?kukurydza">>).

invalid_request_body(_Config) ->
    {?BAD_REQUEST, <<"Invalid request body">>} = post(admin, path("users"), <<"kukurydza">>).

user_can_be_registered_and_removed(_Config) ->
    % list users
    {?OK, Lusers} = gett(admin, path("users")),
    Domain = domain(),
    assert_inlist(<<"alice@", Domain/binary>>, Lusers),
    % create user
    CrUser = #{username => <<"mike">>, password => <<"nicniema">>},
    {?CREATED, _} = post(admin, path("users"), CrUser),
    {?OK, Lusers1} = gett(admin, path("users")),
    assert_inlist(<<"mike@", Domain/binary>>, Lusers1),
    % try to create the same user
    {?FORBIDDEN, _} = post(admin, path("users"), CrUser),
    % delete user
    {?NOCONTENT, _} = delete(admin, path("users", ["mike"])),
    {?OK, Lusers2} = gett(admin, path("users")),
    assert_notinlist(<<"mike@", Domain/binary>>, Lusers2).

user_registration_errors(_Config) ->
    {AnonUser, AnonDomain} = anon_us(),
    {?BAD_REQUEST, <<"Invalid JID", _/binary>>} =
        post(admin, path("users"), #{username => <<"m@ke">>, password => <<"nicniema">>}),
    {?BAD_REQUEST, <<"Missing password", _/binary>>} =
        post(admin, path("users"), #{username => <<"mike">>}),
    {?BAD_REQUEST, <<"Missing user name", _/binary>>} =
        post(admin, path("users"), #{password => <<"nicniema">>}),
    {?FORBIDDEN, <<"Can't register user", _/binary>>} =
        post(admin, path("users"), #{username => <<"mike">>, password => <<>>}),
    {?FORBIDDEN, <<"Can't register user", _/binary>>} =
        post(admin, <<"/users/", AnonDomain/binary>>, #{username => AnonUser,
                                                        password => <<"secret">>}),
    {?FORBIDDEN, <<"User does not exist or you are not authorized properly">>} =
        delete(admin, <<"/users/", AnonDomain/binary, "/", AnonUser/binary>>),
    {?BAD_REQUEST, <<"Invalid JID", _/binary>>} =
        delete(admin, path("users", ["@mike"])).

sessions_are_listed(_) ->
    % no session
    {?OK, Sessions} = gett(admin, path("sessions")),
    true = is_list(Sessions).

session_can_be_kicked(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        % Alice is connected
        AliceJid = jid:nameprep(escalus_client:full_jid(Alice)),
        AliceSessionPath = <<"/sessions/", (escalus_client:server(Alice))/binary,
                             "/", (escalus_client:username(Alice))/binary,
                             "/", (escalus_client:resource(Alice))/binary>>,
        {?OK, Sessions1} = gett(admin, path("sessions")),
        assert_inlist(AliceJid, Sessions1),
        % kick alice
        % mongoose_c2s:exit is an async operation
        {?NOCONTENT, _} = delete(admin, AliceSessionPath),
        escalus:wait_for_stanza(Alice),
        true = escalus_connection:wait_for_close(Alice, timer:seconds(1)),
        wait_helper:wait_until(
            fun() ->
                  {?OK, Sessions2} = gett(admin, path("sessions")),
                  lists:member(AliceJid, Sessions2)
            end, false),
        {?NOT_FOUND, <<"No active session">>} = delete(admin, AliceSessionPath),
        ok
    end).

session_kick_errors(_Config) ->
    {?BAD_REQUEST, <<"Missing user name">>} =
        delete(admin, <<"/sessions/", (domain())/binary>>),
    %% Resource is matched first, because Cowboy matches path elements from the right
    {?BAD_REQUEST, <<"Missing user name">>} =
        delete(admin, <<"/sessions/", (domain())/binary, "/resource">>).

messages_are_sent_and_received(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {M1, M2} = send_messages(Alice, Bob),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, [maps:get(body, M1)], Res),
        Res1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(body, M2)], Res1)
    end).

message_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceJID = escalus_users:get_jid(Config1, alice),
    BobJID = escalus_users:get_jid(Config1, bob),
    {?BAD_REQUEST, <<"Missing sender JID">>} =
        post(admin, "/messages", #{to => BobJID, body => <<"whatever">>}),
    {?BAD_REQUEST, <<"Missing recipient JID">>} =
        post(admin, "/messages", #{caller => AliceJID, body => <<"whatever">>}),
    {?BAD_REQUEST, <<"Missing message body">>} =
        post(admin, "/messages", #{caller => AliceJID, to => BobJID}),
    {?BAD_REQUEST, <<"Invalid recipient JID">>} =
        send_message_bin(AliceJID, <<"@noway">>),
    {?BAD_REQUEST, <<"Invalid sender JID">>} =
        send_message_bin(<<"@noway">>, BobJID),
    {?BAD_REQUEST, <<"User does not exist">>} =
        send_message_bin(<<"baduser@", (domain())/binary>>, BobJID),
    {?BAD_REQUEST, <<"User's domain does not exist">>} =
        send_message_bin(<<"baduser@baddomain">>, BobJID).

stanzas_are_sent_and_received(Config) ->
%%    this is to test the API for sending arbitrary stanzas, e.g. message with extra elements
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJid = escalus_client:full_jid(Alice),
        BobJid = escalus_client:full_jid(Bob),
        Stanza = extended_message(#{<<"from">> => AliceJid, <<"to">> => BobJid}),
        {?NOCONTENT, _} = send_stanza(Stanza),
        Res = escalus:wait_for_stanza(Bob),
        ?assertEqual(<<"attribute">>, exml_query:attr(Res, <<"extra">>)),
        ?assertEqual(<<"inside the sibling">>, exml_query:path(Res, [{element, <<"sibling">>}, cdata]))
    end).

stanza_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    BobJid = escalus_users:get_jid(Config1, bob),
    UnknownJid =  <<"baduser@", (domain())/binary>>,
    {?BAD_REQUEST, <<"Missing recipient JID">>} =
        send_stanza(extended_message(#{<<"from">> => AliceJid})),
    {?BAD_REQUEST, <<"Missing sender JID">>} =
        send_stanza(extended_message(#{<<"to">> => BobJid})),
    {?BAD_REQUEST, <<"Invalid recipient JID">>} =
        send_stanza(extended_message(#{<<"from">> => AliceJid, <<"to">> => <<"@invalid">>})),
    {?BAD_REQUEST, <<"Invalid sender JID">>} =
        send_stanza(extended_message(#{<<"from">> => <<"@invalid">>, <<"to">> => BobJid})),
    {?BAD_REQUEST, <<"User's domain does not exist">>} =
        send_stanza(extended_message(#{<<"from">> => <<"baduser@baddomain">>, <<"to">> => BobJid})),
    {?BAD_REQUEST, <<"User does not exist">>} =
        send_stanza(extended_message(#{<<"from">> => UnknownJid, <<"to">> => BobJid})),
    {?BAD_REQUEST, <<"Malformed stanza">>} =
        send_stanza(broken_message(#{<<"from">> => AliceJid, <<"to">> => BobJid})),
    {?BAD_REQUEST, <<"Missing stanza">>} =
        post(admin, <<"/stanzas">>, #{}).

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

message_archive_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    User = binary_to_list(escalus_users:get_username(Config1, alice)),
    Domain = binary_to_list(domain_helper:domain()),
    {?NOT_FOUND, <<"Missing owner JID">>} =
        gett(admin, "/messages"),
    {?BAD_REQUEST, <<"Invalid owner JID">>} =
        gett(admin, "/messages/@invalid"),
    {?BAD_REQUEST, <<"User does not exist">>} =
        gett(admin, "/messages/baduser@" ++ Domain),
    {?BAD_REQUEST, <<"Invalid interlocutor JID">>} =
        gett(admin, "/messages/" ++ User ++ "/@invalid"),
    {?BAD_REQUEST, <<"Invalid limit">>} =
        gett(admin, "/messages/" ++ User ++ "?limit=x"),
    {?BAD_REQUEST, <<"Invalid value of 'before'">>} =
        gett(admin, "/messages/" ++ User ++ "?before=x").

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
    {?NOCONTENT, _} = putt(admin, path("users", ["bob"]),
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
    {?NOCONTENT, _} = putt(admin, path("users", ["bob"]),
                           #{newpass => <<"makrolika">>}),
    % now he logs again with the regular one
    escalus:story(Config, [{bob, 1}], fun(#client{} = _Bob) ->
        just_dont_do_anything
    end).

password_change_errors(Config) ->
    Alice = binary_to_list(escalus_users:get_username(Config, alice)),
    {AnonUser, AnonDomain} = anon_us(),
    Args = #{newpass => <<"secret">>},
    {?FORBIDDEN, <<"User does not exist or you are not authorized properly">>} =
        putt(admin, <<"/users/", AnonDomain/binary, "/", AnonUser/binary>>, Args),
    {?BAD_REQUEST, <<"Missing user name">>} =
        putt(admin, path("users", []), Args),
    {?BAD_REQUEST, <<"Missing new password">>} =
        putt(admin, path("users", [Alice]), #{}),
    {?BAD_REQUEST, <<"Empty password">>} =
        putt(admin, path("users", [Alice]), #{newpass => <<>>}),
    {?BAD_REQUEST, <<"Invalid JID">>} =
        putt(admin, path("users", ["@invalid"]), Args).

anon_us() ->
    AnonConfig = [{escalus_users, escalus_ct:get_config(escalus_anon_users)}],
    AnonDomain = escalus_users:get_server(AnonConfig, jon),
    AnonUser = escalus_users:get_username(AnonConfig, jon),
    {AnonUser, AnonDomain}.

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
            {?NOCONTENT, _} = post(admin, AlicePath, #{jid => BobJID}), % it is idempotent
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
            ct:log("APushes: ~p", [APushes]),
            AExp = [{none, none},
                    {both, none},
                    {remove, none}],
            check_pushlist(AExp, APushes),
            BPushes = lists:filter(fun escalus_pred:is_roster_set/1,
                                   escalus:wait_for_stanzas(Bob, 20)),
            ct:log("BPushes: ~p", [BPushes]),
            BExp = [{none, none},
                    {both, none},
                    {remove, none}],
            check_pushlist(BExp, BPushes),
            ok
        end
    ),
    ok.

list_contacts_errors(_Config) ->
    {?NOT_FOUND, <<"Domain not found">>} = gett(admin, contacts_path("baduser@baddomain")).

add_contact_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    BobJID = escalus_users:get_jid(Config, bob),
    AliceS = binary_to_list(escalus_users:get_jid(Config1, alice)),
    DomainS = binary_to_list(domain()),
    {?BAD_REQUEST, <<"Missing JID">>} =
        post(admin, contacts_path(AliceS), #{}),
    {?BAD_REQUEST, <<"Invalid JID">>} =
        post(admin, contacts_path(AliceS), #{jid => <<"@invalidjid">>}),
    {?BAD_REQUEST, <<"Invalid user JID">>} =
        post(admin, contacts_path("@invalid_jid"), #{jid => BobJID}),
    {?NOT_FOUND, <<"The user baduser@", _/binary>>} =
        post(admin, contacts_path("baduser@" ++ DomainS), #{jid => BobJID}),
    {?NOT_FOUND, <<"Domain not found">>} =
        post(admin, contacts_path("baduser@baddomain"), #{jid => BobJID}).

subscription_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceS = binary_to_list(escalus_users:get_jid(Config1, alice)),
    BobS = binary_to_list(escalus_users:get_jid(Config1, bob)),
    DomainS = binary_to_list(domain()),
    {?BAD_REQUEST, <<"Invalid contact JID">>} =
        putt(admin, contacts_path(AliceS, "@invalid_jid"), #{action => <<"subscribe">>}),
    {?BAD_REQUEST, <<"Invalid user JID">>} =
        putt(admin, contacts_path("@invalid_jid", BobS), #{action => <<"subscribe">>}),
    {?BAD_REQUEST, <<"Missing action">>} =
        putt(admin, contacts_path(AliceS, BobS), #{}),
    {?BAD_REQUEST, <<"Missing action">>} =
        putt(admin, contacts_manage_path(AliceS, BobS), #{}),
    {?BAD_REQUEST, <<"Invalid action">>} =
        putt(admin, contacts_path(AliceS, BobS), #{action => <<"something stupid">>}),
    {?BAD_REQUEST, <<"Invalid action">>} =
        putt(admin, contacts_manage_path(AliceS, BobS), #{action => <<"off with his head">>}),
    {?BAD_REQUEST, <<"Invalid user JID">>} =
        putt(admin, contacts_manage_path("@invalid", BobS), #{action => <<"connect">>}),
    {?BAD_REQUEST, <<"Invalid contact JID">>} =
        putt(admin, contacts_manage_path(AliceS, "@bzzz"), #{action => <<"connect">>}),
    {?NOT_FOUND, <<"The user baduser@baddomain does not exist">>} =
        putt(admin, contacts_manage_path(AliceS, "baduser@baddomain"), #{action => <<"connect">>}),
    {?NOT_FOUND, <<"Domain not found">>} =
        putt(admin, contacts_manage_path("baduser@baddomain", AliceS), #{action => <<"connect">>}),
    {?NOT_FOUND, <<"Cannot remove", _/binary>>} =
        putt(admin, contacts_manage_path(AliceS, "baduser@" ++ DomainS), #{action => <<"disconnect">>}).

delete_contact_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceS = binary_to_list(escalus_users:get_jid(Config1, alice)),
    DomainS = binary_to_list(domain()),
    {?NOT_FOUND, <<"Cannot remove", _/binary>>} =
        delete(admin, contacts_path(AliceS, "baduser@" ++ DomainS)),
    {?BAD_REQUEST, <<"Missing contact JID">>} =
        delete(admin, contacts_path(AliceS)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

contacts_path(UserJID) ->
    "/contacts/" ++ UserJID.

contacts_path(UserJID, ContactJID) ->
    contacts_path(UserJID) ++ "/" ++ ContactJID.

contacts_manage_path(UserJID, ContactJID) ->
    contacts_path(UserJID, ContactJID) ++ "/manage".

send_messages(Alice, Bob) ->
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    M = #{caller => BobJID, to => AliceJID, body => <<"hello from Bob">>},
    {?NOCONTENT, _} = post(admin, <<"/messages">>, M),
    M1 = #{caller => AliceJID, to => BobJID, body => <<"hello from Alice">>},
    {?NOCONTENT, _} = post(admin, <<"/messages">>, M1),
    {M, M1}.

send_message_bin(BFrom, BTo) ->
    % this is to trigger invalid jid errors
    M = #{caller => BFrom, to => BTo, body => <<"whatever">>},
    post(admin, <<"/messages">>, M).

send_stanza(StanzaBin) ->
    post(admin, <<"/stanzas">>, #{stanza => StanzaBin}).

broken_message(Attrs) ->
    remove_last_character(extended_message(Attrs)).

remove_last_character(Bin) ->
    binary:part(Bin, 0, byte_size(Bin) - 1).

extended_message(Attrs) ->
    M = #xmlel{name = <<"message">>,
               attrs = Attrs#{<<"extra">> => <<"attribute">>},
               children = [#xmlel{name = <<"body">>,
                                  children = [#xmlcdata{content = <<"the body">>}]},
                           #xmlel{name = <<"sibling">>,
                                  children = [#xmlcdata{content = <<"inside the sibling">>}]}
                          ]
              },
    exml:to_binary(M).

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

to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_list(V) ->
    V.

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

path(Category) ->
    path(Category, []).

path(Category, Items) ->
    DomainStr = binary_to_list(domain()),
    string:join(["", Category, DomainStr | Items], "/").
