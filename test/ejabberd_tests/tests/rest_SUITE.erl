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

-import(rest_helper,
        [assert_inlist/2,
         assert_notinlist/2,
         decode_maplist/1,
         gett/1,
         post/2,
         putt/2,
         delete/1,
         gett/2,
         post/3,
         putt/3,
         delete/2]
    ).
-import(mam_helper, [rpc_apply/3]).

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).
-define(OK, {<<"200">>, <<"OK">>}).
-define(CREATED, {<<"201">>, <<"Created">>}).
-define(ERROR, {<<"500">>, _}).
-define(NOT_FOUND, {<<"404">>, _}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds
-define(ATOMS, [name, desc, category, action, security_policy, args, result, sender]).

all() ->
    [
     {group, admin},
     {group, dynamic_module}
    ].

groups() ->
    [{admin, [], test_cases()},
     {dynamic_module, [], [stop_start_command_module]}
    ].

test_cases() ->
    [commands_are_listed,
     non_existent_command_returns_404,
     user_can_be_registered_and_removed,
     sessions_are_listed,
     session_can_be_kicked,
     messages_are_sent_and_received,
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
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    rest_helper:maybe_disable_mam(proplists:get_value(mam_enabled, Config), host()),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

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

commands_are_listed(_C) ->
    {?OK, Lcmds} = gett(<<"/commands">>),
    DecCmds = decode_maplist(Lcmds),
    assert_inlist(#{name => <<"list_methods">>}, DecCmds).

non_existent_command_returns_404(_C) ->
    {?NOT_FOUND, _} = gett(<<"/isitthereornot">>).

user_can_be_registered_and_removed(_Config) ->
    % list users
    {?OK, Lusers} = gett(<<"/users/localhost">>),
    assert_inlist(<<"alice@localhost">>, Lusers),
    % create user
    CrUser = #{user => <<"mike">>, password => <<"nicniema">>},
    {?CREATED, _} = post(<<"/users/localhost">>, CrUser),
    {?OK, Lusers1} = gett(<<"/users/localhost">>),
    assert_inlist(<<"mike@localhost">>, Lusers1),
    % try to create the same user
    {?ERROR, _} = post(<<"/users/localhost">>, CrUser),
    % delete user
    {?OK, _} = delete(<<"/users/localhost/mike">>),
    {?OK, Lusers2} = gett(<<"/users/localhost">>),
    assert_notinlist(<<"mike@localhost">>, Lusers2),
    ok.

sessions_are_listed(_) ->
    % no session
    {?OK, Sessions} = gett("/sessions/localhost"),
    [] = Sessions.

session_can_be_kicked(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        % Alice is connected
        {?OK, Sessions1} = gett("/sessions/localhost"),
        assert_inlist(<<"alice@localhost/res1">>, Sessions1),
        % kick alice
        {?OK, _} = delete("/sessions/localhost/alice/res1"),
        escalus:wait_for_stanza(Alice),
        {?OK, Sessions2} = gett("/sessions/localhost"),
        [] = Sessions2
    end).

messages_are_sent_and_received(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {M1, M2} = send_messages(Alice, Bob),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, [maps:get(msg, M1)], Res),
        Res1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(msg, M2)], Res1)
    end).

send_messages(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        M = #{caller => BobJID, to => AliceJID, msg => <<"hello from Bob">>},
        {?OK, _} = post(<<"/messages">>, M),
        M1 = #{caller => AliceJID, to => BobJID, msg => <<"hello from Alice">>},
        {?OK, _} = post(<<"/messages">>, M1),
        {M, M1}.

messages_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {M1, _M2} = send_messages(Alice, Bob),
        AliceJID = maps:get(to, M1),
        BobJID = maps:get(caller, M1),
        GetPath = lists:flatten(["/messages/",binary_to_list(AliceJID),
                                 "/",binary_to_list(BobJID),"/10"]),
        mam_helper:maybe_wait_for_yz(Config),
        {?OK, Msgs} = gett(GetPath),
        ?PRT("Msgs", Msgs),
        [Last, Previous|_] = lists:reverse(decode_maplist(Msgs)),
        ?PRT("Last", Last),
        ?PRT("Previous", Previous),
        <<"hello from Alice">> = maps:get(body, Last),
        AliceJID = maps:get(sender, Last),
        <<"hello from Bob">> = maps:get(body, Previous),
        BobJID = maps:get(sender, Previous)
    end).

messages_can_be_paginated(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        fill_archive(Alice, Bob),
        mam_helper:maybe_wait_for_yz(Config),
        % recent msgs with a limit
        M1 = get_messages(AliceJID, BobJID, 10),
        ?assertEqual(6, length(M1)),
        M2 = get_messages(AliceJID, BobJID, 3),
        ?assertEqual(3, length(M2)),
        % older messages - earlier then the previous midnight
        PriorTo = make_timestamp(-1, {0, 0, 1}),
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
    escalus:story(Config, [{bob, 1}], fun(_Bob) ->
        skip
    end),
    % we change password
    {?OK, _} = putt("/users/localhost/bob", #{newpass => <<"niemakrolika">>}),
    % he logs with his alternative password
    escalus:story(Config, [{bob_altpass, 1}], fun(_Bob) ->
        ignore
    end),
    % we can't log with regular passwd anymore
    try escalus:story(Config, [{bob, 1}], fun(Bob) -> ?PRT("Bob", Bob) end) of
        _ -> ct:fail("bob connected with old password")
    catch error:{badmatch, _} ->
        ok
    end,
    % we change it back
    {?OK, _} = putt("/users/localhost/bob", #{newpass => <<"makrolika">>}),
    % now he logs again with the regular one
    escalus:story(Config, [{bob, 1}], fun(_Bob) ->
        just_dont_do_anything
    end),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

fill_archive(A, B) ->
    % here we generate messages sent one, two and three days ago at 10am
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    put_msg(A, B, <<"A">>, Today - 3),
    put_msg(B, A, <<"A">>, Today - 3),
    put_msg(A, B, <<"B">>, Today - 2),
    put_msg(B, A, <<"B">>, Today - 2),
    put_msg(A, B, <<"C">>, Today - 1),
    put_msg(B, A, <<"C">>, Today - 1).

put_msg(Aclient, Bclient, Content, Days) ->
    DateTime = {calendar:gregorian_days_to_date(Days), {10, 0, 0}},
    AArcId = make_arc_id(Aclient),
    BArcId = make_arc_id(Bclient),
    Msg = mam_helper:generate_msg_for_date_user(AArcId, BArcId, DateTime, Content),
    put_msg(Msg),
    ok.

put_msg({{MsgIdOwner, MsgIdRemote},
    {_FromBin, FromJID, FromArcID},
    {_ToBin, ToJID, ToArcID},
    {_, Source, _}, Packet}) ->
    Host = host(),
    ok = rpc_apply(mod_mam, archive_message, [Host, MsgIdOwner, FromArcID, FromJID, ToJID, Source, outgoing, Packet]),
    ok = rpc_apply(mod_mam, archive_message, [Host, MsgIdRemote, ToArcID, ToJID, FromJID, Source, incoming, Packet]),
    ok.

make_arc_id(Client) ->
    User = escalus_client:username(Client),
    Server = escalus_client:server(Client),
    Bin = escalus_client:short_jid(Client),
    Jid = rpc_apply(jid, make, [User, Server, <<"">>]),
    {Bin, Jid, rpc_apply(mod_mam, archive_id, [Server, User])}.

get_messages(Me, Other, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Me),
                             "/",binary_to_list(Other),
                             "/", integer_to_list(Count)]),
    {?OK, Msgs} = gett(GetPath),
    Msgs.

get_messages(Me, Other, Before, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Me),
                             "/", binary_to_list(Other),
                             "/", integer_to_list(Before),
                             "/", integer_to_list(Count)]),
    {?OK, Msgs} = gett(GetPath),
    Msgs.

make_timestamp(Offset, Time) ->
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    Dt = {calendar:gregorian_days_to_date(Today + Offset), Time},
    Tst = calendar:datetime_to_gregorian_seconds(Dt) - 62167219200,
    Tst.

stop_start_command_module(_) ->
    %% Precondition: module responsible for resource is started. If we
    %% stop the module responsible for this resource then the same
    %% test will fail. If we start the module responsible for this
    %% resource then the same test will succeed. With the precondition
    %% described above we test both transition from `started' to
    %% `stopped' and from `stopped' to `started'.
    {?OK, _} = gett(<<"/commands">>),
    {atomic, ok} = dynamic_modules:stop(host(), mod_mongoose_admin),
    {?NOT_FOUND, _} = gett(<<"/commands">>),
    ok = dynamic_modules:start(host(), mod_mongoose_admin, []),
    timer:sleep(200), %% give the server some time to build the paths again
    {?OK, _} = gett(<<"/commands">>).

to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_list(V) ->
    V.
