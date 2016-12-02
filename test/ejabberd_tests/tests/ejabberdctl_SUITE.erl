%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
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
-module(ejabberdctl_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml.hrl").

-import(ejabberdctl_helper, [ejabberdctl/3, rpc_call/3]).
-import(mongoose_helper, [auth_modules/0]).
-import(ejabberd_node_utils, [mim/0]).

-define(SINGLE_QUOTE_CHAR, $\').
-define(DOUBLE_QUOTE_CHAR, $\").

-record(offline_msg, {us, timestamp, expire, from, to, packet}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, accounts},
     {group, sessions},
     {group, vcard},
     {group, roster},
     {group, roster_advanced},
     {group, last},
     {group, private},
     {group, stanza},
     {group, stats},
     {group, basic}].

groups() ->
     [{accounts, [sequence], accounts()},
      {sessions, [sequence], sessions()},
      {vcard, [sequence], vcard()},
      {roster, [sequence], roster()},
      {last, [sequence], last()},
      {private, [sequence], private()},
      {stanza, [sequence], stanza()},
      {roster_advanced, [sequence], roster_advanced()},
      {basic, [sequence], basic()},
      {stats, [sequence], stats()}].

basic() ->
    [simple_register, simple_unregister, register_twice,
        backup_restore_mnesia,
        restore_mnesia_wrong,
        dump_and_load,
        load_mnesia_wrong,
        dump_table,
        get_loglevel,
        remove_old_messages_test,
        remove_expired_messages_test
    ].

accounts() -> [change_password, check_password_hash, check_password,
               check_account, ban_account, num_active_users, delete_old_users,
               delete_old_users_vhost].

sessions() -> [num_resources_num, kick_session, status,
               sessions_info, set_presence].

vcard() -> [vcard_rw, vcard2_rw, vcard2_multi_rw].

roster() -> [rosteritem_rw, presence_after_add_rosteritem,
             push_roster,
             push_roster_all,
             push_roster_alltoall].

roster_advanced() ->[process_rosteritems_list_simple,
                          process_rosteritems_list_nomatch,
                          process_rosteritems_list_advanced1,
                          process_rosteritems_list_advanced2,
                          process_rosteritems_delete_advanced,
                          process_rosteritems_delete_advanced2].

last() -> [set_last].

private() -> [private_rw].

stanza() -> [send_message, send_message_wrong_jid, send_stanza, send_stanzac2s_wrong].

stats() -> [stats_global, stats_host].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    Cwd0 = escalus_config:get_config(data_dir, Config),
    CwdTokens = string:tokens(Cwd0, "/"),
    Cwd =  [$/ | string:join(lists:sublist(CwdTokens, 1, length(CwdTokens)-2), "/")],
    TemplatePath = Cwd ++ "/roster.template",
    start_mod_admin_extra(),
    AuthMods = auth_modules(),
    Node = mim(),
    Config1 = ejabberd_node_utils:init(Node, Config),
    Config2 = escalus:init_per_suite([{ctl_auth_mods, AuthMods},
                                        {roster_template, TemplatePath} | Config1]),
    escalus:create_users(Config2, escalus:get_users([alice, mike, bob, kate])).

end_per_suite(Config) ->
    Config1 = lists:keydelete(ctl_auth_mods, 1, Config),
    delete_users(Config1),
    escalus:end_per_suite(Config1).

init_per_group(vcard, Config) ->
    case escalus_ejabberd:rpc(gen_mod,get_module_opt,
                              [ct:get_config(ejabberd_domain),
                               mod_vcard, backend, mnesia]) of
        ldap ->
            {skip, vcard_set_not_supported_with_ldap};
        _ ->
            Config
    end;

init_per_group(roster_advanced, Config) ->
    case escalus_ejabberd:rpc(gen_mod,get_module_opt,[ct:get_config(ejabberd_domain), mod_roster, backend, mnesia]) of
        mnesia ->
            Config;
        _ ->
            {skip, command_process_rosteritems_supports_only_mnesia}
    end;

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(Rosters, Config) when (Rosters == roster) or (Rosters == roster_advanced) ->
    TemplatePath = escalus_config:get_config(roster_template, Config),
    RegUsers = [atom_to_list(U) || {U, _} <- escalus_config:get_config(escalus_users, Config)],
    {ok, [Roster]} = file:consult(TemplatePath),
    io:format("Roster is ~p~n and registred is ~p",[Roster, RegUsers]),
    C = fun({U, S, _, _}) ->
        case lists:member(U, RegUsers) of
            true ->
                SB = string_to_binary(S),
                UB = string_to_binary(U),
                escalus_ejabberd:rpc(ejabberd_hooks, run, [remove_user, SB, [UB, SB]]);
            _ ->
               ok
        end
    end,
    lists:foreach(C, Roster),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config)
  % these cases are incompatible with domainless odbc schema
  when CaseName == delete_old_users_vhost
       orelse CaseName == stats_global
       orelse CaseName == stats_host ->
    {_, AuthMods} = lists:keyfind(ctl_auth_mods, 1, Config),
    case lists:member(ejabberd_auth_odbc, AuthMods) orelse
         lists:member(ejabberd_auth_ldap, AuthMods) of
        true -> {skip, vhost_odbc_incompatible};
        false -> escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(CaseName, Config)
    when CaseName == check_password_hash;
         CaseName == delete_old_users ->
    {_, AuthMods} = lists:keyfind(ctl_auth_mods, 1, Config),
    case lists:member(ejabberd_auth_ldap, AuthMods) of
        true -> {skip, not_fully_supported_with_ldap};
        false -> escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(delete_old_users, Config) ->
    Users = escalus_users:get_users([alice, bob, kate, mike]),
    lists:foreach(fun({_User, UserSpec}) ->
                {Username, Domain, Pass} = get_user_data(UserSpec, Config),
                escalus_ejabberd:rpc(ejabberd_auth, try_register, [Username, Domain, Pass])
        end, Users),
    escalus_cleaner:clean(Config),
    escalus:end_per_testcase(delete_old_users, Config);
end_per_testcase(CaseName, Config) ->
    escalus_cleaner:clean(Config),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_admin_extra_accounts tests
%%--------------------------------------------------------------------

change_password(Config) ->
    {User, Domain, OldPassword} = get_user_data(alice, Config),
    ejabberdctl("change_password", [User, Domain, <<OldPassword/binary, $2>>], Config),
    {error, {connection_step_failed, _, _}} = escalus_client:start_for(Config, alice, <<"newres">>),
    ejabberdctl("change_password", [User, Domain, OldPassword], Config),
    {ok, _Alice2} = escalus_client:start_for(Config, alice, <<"newres2">>).

check_password_hash(Config) ->
    {User, Domain, Pass} = get_user_data(alice, Config),
    MD5Hash = get_md5(Pass),
    MD5HashBad = get_md5(<<Pass/binary, "bad">>),
    SHAHash = get_sha(Pass),

    {_, 0} = ejabberdctl("check_password_hash", [User, Domain, MD5Hash, "md5"], Config),
    {_, ErrCode} = ejabberdctl("check_password_hash", [User, Domain, MD5HashBad, "md5"], Config),
    true = (ErrCode =/= 0), %% Must return code other than 0
    {_, 0} = ejabberdctl("check_password_hash", [User, Domain, SHAHash, "sha"], Config).

check_password(Config) ->
    {User, Domain, Pass} = get_user_data(alice, Config),

    {_, 0} = ejabberdctl("check_password", [User, Domain, Pass], Config),
    {_, ErrCode} = ejabberdctl("check_password", [User, Domain, <<Pass/binary, "Bad">>], Config),
    true = (ErrCode =/= 0). %% Must return code other than 0

check_account(Config) ->
    {User, Domain, _Pass} = get_user_data(alice, Config),

    {_, 0} = ejabberdctl("check_account", [User, Domain], Config),
    {_, ErrCode} = ejabberdctl("check_account", [<<User/binary, "Bad">>, Domain], Config),
    true = (ErrCode =/= 0). %% Must return code other than 0

ban_account(Config) ->
    {User, Domain, Pass} = get_user_data(mike, Config),

    {ok, Mike} = escalus_client:start_for(Config, mike, <<"newres">>),
    {_, 0} = ejabberdctl("ban_account", [User, Domain, "SomeReason"], Config),
    escalus:assert(is_stream_error, [<<"conflict">>, <<"SomeReason">>], escalus:wait_for_stanza(Mike)),
    {error, {connection_step_failed, _, _}} = escalus_client:start_for(Config, mike, <<"newres2">>),
    ejabberdctl("change_password", [User, Domain, Pass], Config).

num_active_users(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    {MikeName, Domain, _} = get_user_data(mike, Config),

    {Mega, Secs, _} = erlang:now(),
    Now = Mega*1000000+Secs,
    set_last(AliceName, Domain, Now),
    {Result, _} = ejabberdctl("num_active_users", [Domain, "5"], Config),
    set_last(MikeName, Domain, Now - 864000), %% Now - 10 days
    %We expect than number of active user in last 5 days is the same as before
    %the change above
    {Result, _} = ejabberdctl("num_active_users", [Domain, "5"], Config).

delete_old_users(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    {BobName, Domain, _} = get_user_data(bob, Config),
    {KateName, Domain, _} = get_user_data(kate, Config),
    {MikeName, Domain, _} = get_user_data(mike, Config),

    {Mega, Secs, _} = erlang:now(),
    Now = Mega*1000000+Secs,
    set_last(AliceName, Domain, Now),
    set_last(BobName, Domain, Now),
    set_last(MikeName, Domain, Now),

    {_, 0} = ejabberdctl("delete_old_users", ["10"], Config),
    {_, 0} = ejabberdctl("check_account", [AliceName, Domain], Config),
    {_, ErrCode} = ejabberdctl("check_account", [KateName, Domain], Config),
    true = (ErrCode =/= 0). %% Must return code other than 0

delete_old_users_vhost(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    {KateName, Domain, KatePass} = get_user_data(kate, Config),
    SecDomain = escalus_config:get_config(ejabberd_secondary_domain, Config),

    {Mega, Secs, _} = erlang:now(),
    Now = Mega*1000000+Secs,
    set_last(AliceName, Domain, Now-86400*30),

    {_, 0} = ejabberdctl("register", [KateName, SecDomain, KatePass], Config),
    {_, 0} = ejabberdctl("check_account", [KateName, SecDomain], Config),
    {_, 0} = ejabberdctl("delete_old_users_vhost", [SecDomain, "10"], Config),
    {_, 0} = ejabberdctl("check_account", [AliceName, Domain], Config),
    {_, ErrCode} = ejabberdctl("check_account", [KateName, SecDomain], Config),
    true = (ErrCode =/= 0). %% Must return code other than 0

%%--------------------------------------------------------------------
%% mod_admin_extra_accounts tests
%%--------------------------------------------------------------------

%% Checks both num_resources and resource_num
num_resources_num(Config) ->
    escalus:story(Config, [{alice, 3}, {bob, 1}], fun(_, Alice2, _, _) ->
                {Username, Domain, _} = get_user_data(alice, Config),
                ResName = binary_to_list(escalus_client:resource(Alice2)) ++ "\n",

                {"3\n", _} = ejabberdctl("num_resources", [Username, Domain], Config),
                {ResName, _} = ejabberdctl("resource_num", [Username, Domain, "2"], Config)
        end).

kick_session(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                Username = escalus_client:username(Alice),
                Domain = escalus_client:server(Alice),
                Resource = escalus_client:resource(Alice),

                {_, 0} = ejabberdctl("kick_session", [Username, Domain, Resource, "\"Because I can!\""], Config),
                Stanza = escalus:wait_for_stanza(Alice),
                escalus:assert(is_stream_error, [<<"conflict">>, <<"Because I can!">>], Stanza)
        end).

status(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {bob, 1}], fun(User1, User2, User3) ->
                PriDomain = escalus_client:server(User1),
                SecDomain = escalus_config:get_config(ejabberd_secondary_domain, Config),
                AwayPresence = escalus_stanza:presence_show(<<"away">>),
                escalus_client:send(User2, AwayPresence),

                {"2\n", _} = ejabberdctl("status_num", ["available"], Config),

                {"2\n", _} = ejabberdctl("status_num_host", [PriDomain, "available"], Config),
                {"0\n", _} = ejabberdctl("status_num_host", [SecDomain, "available"], Config),

                {StatusList, _} = ejabberdctl("status_list", ["available"], Config),
                match_user_status([User1, User3], StatusList),

                {StatusList2, _} = ejabberdctl("status_list_host", [PriDomain, "available"], Config),
                match_user_status([User1, User3], StatusList2),
                {[], _} = ejabberdctl("status_list_host", [SecDomain, "available"], Config)
        end).

sessions_info(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(User1, User2, User3) ->
                Username1 = escalus_client:username(User1),
                PriDomain = escalus_client:server(User1),
                SecDomain = escalus_config:get_config(ejabberd_secondary_domain, Config),
                AwayPresence = escalus_stanza:presence_show(<<"away">>),
                escalus_client:send(User2, AwayPresence),

                {UserList, _} = ejabberdctl("connected_users_info", [], Config),
                match_user_info([User1, User2, User3], UserList),

                {UserList2, _} = ejabberdctl("connected_users_vhost", [PriDomain], Config),
                match_user_info([User1, User2, User3], UserList2),
                {[], _} = ejabberdctl("connected_users_vhost", [SecDomain], Config),

                {UserList3, _} = ejabberdctl("user_sessions_info", [Username1, PriDomain], Config),
                match_user_info([User1], UserList3)
        end).

set_presence(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                Username = escalus_client:username(Alice),
                Domain = escalus_client:server(Alice),
                Resource = escalus_client:resource(Alice),

                {_, 0} = ejabberdctl("set_presence", [Username, Domain, Resource,
                                                      "available", "away", "mystatus", "10"], Config),
                Presence = escalus:wait_for_stanza(Alice),
                escalus:assert(is_presence_with_show, [<<"away">>], Presence),
                escalus:assert(is_presence_with_status, [<<"mystatus">>], Presence),
                escalus:assert(is_presence_with_priority, [<<"10">>], Presence)
        end).

%%--------------------------------------------------------------------
%% mod_admin_extra_vcard tests
%%--------------------------------------------------------------------

vcard_rw(Config) ->
    {Username, Domain, _} = get_user_data(alice, Config),

    {_, ExitCode} = ejabberdctl("get_vcard", [Username, Domain, "NICKNAME"], Config),
    true = (ExitCode /= 0),

    {_, 0} = ejabberdctl("set_vcard", [Username, Domain, "NICKNAME", "SomeNickname"], Config),
    {"SomeNickname\n", 0} = ejabberdctl("get_vcard", [Username, Domain, "NICKNAME"], Config).

vcard2_rw(Config) ->
    {Username, Domain, _} = get_user_data(alice, Config),

    {_, ExitCode} = ejabberdctl("get_vcard2", [Username, Domain, "ORG", "ORGNAME"], Config),
    true = (ExitCode /= 0),

    {_, 0} = ejabberdctl("set_vcard2", [Username, Domain, "ORG", "ORGNAME", "ESL"], Config),
    {"ESL\n", 0} = ejabberdctl("get_vcard2", [Username, Domain, "ORG", "ORGNAME"], Config).

vcard2_multi_rw(Config) ->
    {Username, Domain, _} = get_user_data(alice, Config),

    {_, ExitCode} = ejabberdctl("get_vcard2_multi", [Username, Domain, "ORG", "ORGUNIT"], Config),
    true = (ExitCode /= 0),

    {_, 0} = ejabberdctl("set_vcard2_multi", [Username, Domain, "ORG", "ORGUNIT", "'sales;marketing'"], Config),
    {OrgUnits0, 0} = ejabberdctl("get_vcard2_multi", [Username, Domain, "ORG", "ORGUNIT"], Config),
    OrgUnits = string:tokens(OrgUnits0, "\n"),
    2 = length(OrgUnits),
    true = (lists:member("sales", OrgUnits) andalso lists:member("marketing", OrgUnits)).

%%--------------------------------------------------------------------
%% mod_admin_extra_vcard tests
%%--------------------------------------------------------------------

rosteritem_rw(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                MikeJid = escalus_users:get_jid(Config, mike),

                {AliceName, Domain, _} = get_user_data(alice, Config),
                {BobName, Domain, _} = get_user_data(bob, Config),
                {MikeName, Domain, _} = get_user_data(mike, Config),

                {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, BobName,
                                                        Domain, "MyBob", "MyGroup", "both"], Config),
                {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, MikeName,
                                                        Domain, "\"My Mike\"", "\'My Group\'", "both"], Config),

                [Push1, Push2] = escalus:wait_for_stanzas(Alice, 2), % Check roster broadcasts
                escalus:assert(is_roster_set, Push1),
                escalus:assert(roster_contains, [BobJid], Push1),
                escalus:assert(is_roster_set, Push2),
                escalus:assert(roster_contains, [MikeJid], Push2),

                {Items1, 0} = ejabberdctl("get_roster", [AliceName, Domain], Config),
                match_roster([{BobName, Domain, "MyBob", "MyGroup", "both"},
                              {MikeName, Domain, "MyMike", "MyGroup", "both"}], Items1),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                escalus:assert(roster_contains, [BobJid], Roster1),
                escalus:assert(roster_contains, [MikeJid], Roster1),

                {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config),

                Push3 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_set, Push3),
                escalus:assert(roster_contains, [BobJid], Push3),

                {Items2, 0} = ejabberdctl("get_roster", [AliceName, Domain], Config),
                match_roster([{MikeName, Domain, "MyMike", "MyGroup", "both"}], Items2),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(MikeJid))  % cleanup
        end).

presence_after_add_rosteritem(Config) ->
     escalus:story(Config, [{alice, 1}, {bob,1}], fun(Alice, Bob) ->
                 BobJid = escalus_users:get_jid(Config, bob),
                 {AliceName, Domain, _} = get_user_data(alice, Config),
                 {BobName, Domain, _} = get_user_data(bob, Config),

                 {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, BobName,
                                                         Domain, "MyBob", "MyGroup", "both"], Config),

                 escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
                 escalus:assert(is_presence, escalus:wait_for_stanza(Bob)),

                 escalus:send(Alice, escalus_stanza:roster_remove_contact(BobJid))  % cleanup
         end).

push_roster(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                {AliceName, Domain, _} = get_user_data(alice, Config),
                TemplatePath = escalus_config:get_config(roster_template, Config),

                {_, 0} = ejabberdctl("push_roster", [TemplatePath, AliceName, Domain], Config),
                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                escalus:assert(roster_contains, [BobJid], Roster1),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(BobJid)) % cleanup
        end).

process_rosteritems_list_simple(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% given
        Action = "list",
        Subs = "any",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        Contact =string:to_lower(binary_to_list(escalus_client:short_jid(Bob))),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {BobName, Domain, _} = get_user_data(bob, Config),
        %% when
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, BobName, Domain, "MyBob", "MyGroup", "both"], Config),
        S = escalus:wait_for_stanzas(Alice, 2),
        {R, 0} = ejabberdctl("process_rosteritems", [Action, Subs, Asks, User, Contact], Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*"++Contact++".*"),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config)
    end).

process_rosteritems_list_nomatch(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% given
        Action = "list",
        Subs = "from:both",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        Contact =string:to_lower(binary_to_list(escalus_client:short_jid(Bob))),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {BobName, Domain, _} = get_user_data(bob, Config),
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, BobName,
                                                Domain, "MyBob", "MyGroup", "to"], Config),
        escalus:wait_for_stanzas(Alice, 2),
        %% when
        {R, 0} = ejabberdctl("process_rosteritems", [Action, Subs, Asks, User, Contact], Config),
        %% then
        nomatch = re:run(R, ".*Matches:.*"++Contact++".*"),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config)
    end).

process_rosteritems_list_advanced1(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {kate, 1}], fun(Alice, Mike, Kate) ->
        %% given
        Action = "list",
        Subs = "from:both",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactsRegexp = ContactMike ++ ":" ++ string:substr(binary_to_list(KateName), 1, 2) ++ ".*@.*",

        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, MikeName,
                                                Domain, "DearMike", "MyGroup", "both"], Config),
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, KateName,
                                                Domain, "BestFriend", "MyGroup", "both"], Config),
        escalus:wait_for_stanzas(Alice,4),
        %% when
        {R, 0} = ejabberdctl("process_rosteritems", [Action, Subs, Asks, User, ContactsRegexp], Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*"++ContactMike++".*"),
        {match, _} = re:run(R, ".*Matches:.*"++ContactKate++".*"),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config)
    end).

process_rosteritems_delete_advanced(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {kate, 1}], fun(Alice, Mike, Kate) ->
        %% given
        Action = "delete",
        Subs = "from",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactsRegexp = ".*" ++ string:substr(ContactMike, 3) ++
                           ":" ++ string:substr(ContactKate, 1,2) ++"@" ++ binary_to_list(Domain),
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, MikeName,
                                                Domain, "DearMike", "MyGroup", "from"], Config),
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, KateName,
                                                Domain, "Friend", "MyGroup", "from"], Config),
        escalus:wait_for_stanzas(Alice,4),
        %% when
        {R, 0} = ejabberdctl("process_rosteritems", [Action, Subs, Asks, User, ContactsRegexp], Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*"++ContactMike++".*"),
        nomatch = re:run(R, ".*Matches:.*"++ContactKate++".*"),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config)
    end).

process_rosteritems_list_advanced2(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {kate, 1}], fun(Alice, Mike, Kate) ->
        %% given
        Action = "list",
        Subs = "any",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactsRegexp = ".*e@lo.*",
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, MikeName,
                                                Domain, "DearMike", "MyGroup", "both"], Config),
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, KateName,
                                                Domain, "KateFromSchool", "MyGroup", "from"], Config),
        escalus:wait_for_stanzas(Alice,4),
        %% when
        {R, 0} = ejabberdctl("process_rosteritems", [Action, Subs, Asks, User, ContactsRegexp], Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*"++ContactMike++".*"),
        {match, _} = re:run(R, ".*Matches:.*"++ContactKate++".*"),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config)
    end).

process_rosteritems_delete_advanced2(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {mike, 1}, {kate, 1}], fun(Alice, Bob, Mike, Kate) ->
        %% given
        Action = "delete",
        Subs = "to:from",
        Asks = "any",
        User = "'al.c[e]@.*host:((b[o]b)|(mike))@loc.*t2'",
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {BobName, Domain, _} = get_user_data(bob, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactBob= string:to_lower(binary_to_list(escalus_client:short_jid(Bob))),
        ContactsReg = "'.ik[ea]@localho+.*:k@loc.*st:(alice)+@.*:no'",
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, MikeName,
                                                Domain, "DearMike", "MyGroup", "to"], Config),
        {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, KateName,
                                                Domain, "HateHerSheHasSoNiceLegs", "MyGroup", "to"], Config),
        {_, 0} = ejabberdctl("add_rosteritem", [BobName, Domain, AliceName,
                                                Domain, "Girlfriend", "MyGroup", "from"], Config),
        escalus:wait_for_stanzas(Alice,4),
        escalus:wait_for_stanzas(Bob, 2),
        %% when
        {R, 0} = ejabberdctl("process_rosteritems", [Action, Subs, Asks, User, ContactsReg], Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*" ++ ContactMike ++ ".*"),
        nomatch = re:run(R, ".*Matches:.*" ++ ContactKate ++ ".*"),
        nomatch = re:run(R, ".*Matches:.*" ++ ContactBob ++ ".*"),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config),
        {_, 0} = ejabberdctl("delete_rosteritem", [BobName, Domain, AliceName, Domain], Config)
    end).

push_roster_all(Config) ->
    escalus:story(Config, [{alice, 1}, {bob,1}], fun(Alice, Bob) ->
                TemplatePath = escalus_config:get_config(roster_template, Config),

                {_, 0} = ejabberdctl("push_roster_all", [TemplatePath], Config),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                BobJid = escalus_client:short_jid(Bob),
                escalus:assert(roster_contains, [BobJid], Roster1),

                escalus:send(Bob, escalus_stanza:roster_get()),
                Roster2 = escalus:wait_for_stanza(Bob),
                escalus:assert(is_roster_result, Roster2),
                AliceJid = escalus_client:short_jid(Alice),
                escalus:assert(roster_contains, [AliceJid], Roster2),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(bob)), % cleanup
                escalus:send(Bob, escalus_stanza:roster_remove_contact(alice)) % cleanup
        end).

push_roster_alltoall(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                MikeJid = escalus_users:get_jid(Config, mike),
                KateJid = escalus_users:get_jid(Config, kate),
                {_, Domain, _} = get_user_data(alice, Config),

                {_, 0} = ejabberdctl("push_roster_alltoall", [Domain, "MyGroup"], Config),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster = escalus:wait_for_stanza(Alice),

                escalus:assert(is_roster_result, Roster),
                escalus:assert(roster_contains, [BobJid], Roster),
                escalus:assert(roster_contains, [MikeJid], Roster),
                escalus:assert(roster_contains, [KateJid], Roster)
        end).

%%--------------------------------------------------------------------
%% mod_admin_extra_last tests
%%--------------------------------------------------------------------

set_last(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                {AliceName, Domain, _} = get_user_data(alice, Config),
                {BobName, Domain, _} = get_user_data(bob, Config),

                {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, BobName,
                                                        Domain, "MyBob", "MyGroup", "both"], Config),
                {_, 0} = ejabberdctl("add_rosteritem", [BobName, Domain, AliceName,
                                                        Domain, "MyAlice", "MyGroup", "both"], Config),

                escalus:wait_for_stanza(Alice), % ignore push

                Now = usec:to_sec(usec:from_now(erlang:now())),
                TS = integer_to_list(Now - 7200),
                {_, 0} = ejabberdctl("set_last", [BobName, Domain, TS, "Status"], Config),
                escalus:send(Alice, escalus_stanza:last_activity(BobJid)),
                LastAct = escalus:wait_for_stanza(Alice),
                escalus:assert(is_last_result, LastAct),
                Seconds = list_to_integer(binary_to_list(
                            exml_query:path(LastAct, [{element, <<"query">>}, {attr, <<"seconds">>}]))),
                true = (( (Seconds > 7100) andalso (Seconds < 7300) ) orelse Seconds),

                {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config), % cleanup
                {_, 0} = ejabberdctl("delete_rosteritem", [BobName, Domain, AliceName, Domain], Config)
        end).

%%--------------------------------------------------------------------
%% mod_admin_extra_private tests
%%--------------------------------------------------------------------

private_rw(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    XmlEl1 = "'<secretinfo xmlns=\"nejmspejs\">1</secretinfo>'",
    XmlEl2 = "'<secretinfo xmlns=\"inny\">2</secretinfo>'",

    {_, 0} = ejabberdctl("private_set", [AliceName, Domain, XmlEl1], Config),
    {_, 0} = ejabberdctl("private_set", [AliceName, Domain, XmlEl2], Config),

    {Result, 0} = ejabberdctl("private_get", [AliceName, Domain, "secretinfo", "nejmspejs"], Config),
    {ok, #xmlel{ name = <<"secretinfo">>, attrs = [{<<"xmlns">>, <<"nejmspejs">>}],
                children = [#xmlcdata{ content = <<"1">> }]}} = exml:parse(list_to_binary(Result)).

%%--------------------------------------------------------------------
%% mod_admin_extra_stanza tests
%%--------------------------------------------------------------------

send_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 2}], fun(Alice, Bob1, Bob2) ->
                {_, 0} = ejabberdctl("send_message_chat", [escalus_client:full_jid(Alice),
                                                           escalus_client:full_jid(Bob1),
                                                           "\"Hi Bob!\""], Config),
                Stanza1 = escalus:wait_for_stanza(Bob1),
                escalus:assert(is_chat_message, [<<"Hi Bob!">>], Stanza1),

                {_, 0} = ejabberdctl("send_message_headline", [escalus_client:full_jid(Alice),
                                                                    escalus_client:short_jid(Bob1),
                                                                    "Subj", "\"Hi Bob!!\""], Config),
                Stanza2 = escalus:wait_for_stanza(Bob1),
                Stanza3 = escalus:wait_for_stanza(Bob2),
                escalus:assert(is_headline_message, [<<"Subj">>, <<"Hi Bob!!">>], Stanza2),
                escalus:assert(is_headline_message, [<<"Subj">>, <<"Hi Bob!!">>], Stanza3)
        end).

send_message_wrong_jid(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {_, Err1} = ejabberdctl("send_message_chat", ["'@@#$%!!.§§£'",
                                                   escalus_client:full_jid(Bob),
                                                   "\"Hello bobby!\""], Config),
        {_, Err2} = ejabberdctl("send_message_headline", ["'%%@&@&@==//\///'",
                                                       escalus_client:short_jid(Bob),
                                                       "Subj", "\"Are
                                                       you there?\""],
                             Config),
        true = Err1 =/= 0,
        true = Err2 =/= 0,
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob)
    end).

send_stanza(Config) ->
    escalus:story(Config, [{alice, 1}, {bob,1}], fun(Alice, Bob) ->
                Domain = escalus_client:server(Alice),
                Resource = escalus_client:resource(Alice),
                {BobName, _, _} = get_user_data(bob, Config),
                BobJID = <<BobName/binary, $@, Domain/binary, $/, (escalus_client:resource(Bob))/binary>>,

                Stanza = re:replace(exml:to_binary(escalus_stanza:from(escalus_stanza:chat_to(Alice, "Hi"), BobJID)),
                                    <<?DOUBLE_QUOTE_CHAR>>, <<?SINGLE_QUOTE_CHAR>>, [global, {return, binary}]),
                {_, 0} = ejabberdctl("send_stanza_c2s", [BobName, Domain, Resource, <<$\", Stanza/binary, $\">>], Config),

                Message = escalus:wait_for_stanza(Alice),
                escalus:assert(is_chat_message, [<<"Hi">>], Message),
                escalus:assert(is_stanza_from, [Bob], Message)
        end).

send_stanzac2s_wrong(Config) ->
    escalus:story(Config, [{alice, 1}, {bob,1}], fun(Alice, Bob) ->
        Domain = escalus_client:server(Alice),
        Resource = escalus_client:resource(Alice),
        WrongBobName = "bobby_the_great",
        {BobName, _, _} = get_user_data(bob, Config),
        BobJID = <<BobName/binary, $@, Domain/binary, $/, (escalus_client:resource(Bob))/binary>>,
        Stanza = re:replace(exml:to_binary(escalus_stanza:from(escalus_stanza:chat_to(Alice, "Hi"), BobJID)),
                            <<?DOUBLE_QUOTE_CHAR>>, <<?SINGLE_QUOTE_CHAR>>, [global, {return, binary}]),
        StanzaWrong = <<"<iq type='get' id='234234'><xmlns='wrongwrong'>">>,
        {_, Err} = ejabberdctl("send_stanza_c2s", [WrongBobName, Domain, Resource, <<$\", Stanza/binary, $\">>],
                               Config),
        {_, Err2} = ejabberdctl("send_stanza_c2s", [BobName, Domain, Resource, <<$\", StanzaWrong/binary, $\">>],
                               Config),

        true = Err =/= 0,
        true = Err2 =/= 0,
        escalus_assert:has_no_stanzas(Alice)
    end).

%%--------------------------------------------------------------------
%% mod_admin_extra_stats tests
%%--------------------------------------------------------------------

stats_global(Config) ->
    escalus:story(Config, [{alice, 1}, {bob,1}], fun(_Alice, _Bob) ->
                RegisteredCount = length(escalus_config:get_config(escalus_users, Config, [])),
                Registered = integer_to_list(RegisteredCount) ++ "\n",

                {UpTime, 0} = ejabberdctl("stats", ["uptimeseconds"], Config),
                _ = list_to_integer(string:strip(UpTime, both, $\n)),
                {Registered, 0} = ejabberdctl("stats", ["registeredusers"], Config),

                {"2\n", 0} = ejabberdctl("stats", ["onlineusersnode"], Config),

                {"2\n", 0} = ejabberdctl("stats", ["onlineusers"], Config)
        end).

stats_host(Config) ->
    escalus:story(Config, [{alice, 1}, {bob,1}], fun(Alice, _Bob) ->
                RegisteredCount = length(escalus_config:get_config(escalus_users, Config, [])),
                Registered = integer_to_list(RegisteredCount) ++ "\n",

                PriDomain = escalus_client:server(Alice),
                SecDomain = escalus_config:get_config(ejabberd_secondary_domain, Config),

                {Registered, 0} = ejabberdctl("stats_host", ["registeredusers", PriDomain], Config),
                {"0\n", 0} = ejabberdctl("stats_host", ["registeredusers", SecDomain], Config),

                {"2\n", 0} = ejabberdctl("stats_host", ["onlineusers", PriDomain], Config),
                {"0\n", 0} = ejabberdctl("stats_host", ["onlineusers", SecDomain], Config)
        end).



%%-----------------------------------------------------------------
%% Improve coverage
%%-----------------------------------------------------------------



simple_register(Config) ->
    %% given
    Domain = escalus_ct:get_config(ejabberd_domain),
    {Name, Password} = {<<"tyler">>, <<"durden">>},
    %% when
    {_, 0} = ejabberdctl("register", [Name, Domain, Password], Config),
    {R2, 0} = ejabberdctl("registered_users", [Domain], Config),
    %% then
    {match, _} = re:run(R2, ".*(" ++binary_to_list(Name)++").*").

simple_unregister(Config) ->
    %% given
    Domain = escalus_ct:get_config(ejabberd_domain),
    {Name, _} = {<<"tyler">>, <<"durden">>},
    %% when
    {_, 0} = ejabberdctl("unregister", [Name, Domain], Config),
    {R2, 0} = ejabberdctl("registered_users", [Domain], Config),
    %% then
    nomatch = re:run(R2, ".*(" ++binary_to_list(Name)++").*").

register_twice(Config) ->
    %% given
    Domain = escalus_ct:get_config(ejabberd_domain),
    {Name,  Password} = {<<"tyler">>, <<"durden">>},
    %% when
    {_, 0} = ejabberdctl("register", [Name, Domain, Password], Config),
    {R, Code} = ejabberdctl("register", [Name, Domain, Password], Config),
    %% then
    {match, _} = re:run(R, ".*(already registered).*"),
    true = (Code =/= 0),
    {_, 0} = ejabberdctl("unregister", [Name, Domain], Config).



backup_restore_mnesia(Config) ->
    %% given
    TableName = passwd,
    TableSize = rpc_call(mnesia, table_info, [TableName, size]),
    io:format("Table size is ~n~p~n", [TableSize]),
    %% Table passwd should not be empty
    FileName = "backup_mnesia.bup",
    %% when
    {R, 0} = ejabberdctl("backup", [FileName], Config),
    nomatch = re:run(R, ".+"),
    rpc_call(mnesia, clear_table, [TableName]),
    0 = rpc_call(mnesia, table_info, [TableName, size]),
    {R2, 0} = ejabberdctl("restore", [FileName], Config),
    %% then
    nomatch = re:run(R2, ".+"),
    TableSize = rpc_call(mnesia, table_info, [TableName, size]).

restore_mnesia_wrong(Config) ->
    FileName = "file that doesnt exist13123.bup",
    {R2, _} = ejabberdctl("restore", [FileName], Config),
    {match, Code} = re:run(R2, ".+"),
    true = (Code =/= 0).

dump_and_load(Config) ->
    FileName = "dump.bup",
    TableName = passwd,
    %% Table passwd should not be empty
    TableSize = rpc_call(mnesia, table_info, [TableName, size]),
    {_, 0} = ejabberdctl("dump", [FileName], Config),
    rpc_call(mnesia, clear_table, [TableName]),
    0 = rpc_call(mnesia, table_info, [TableName, size]),
    {R, 0} = ejabberdctl("load", [FileName], Config),
    {match, _} = re:run(R, ".+"),
    TableSize = rpc_call(mnesia, table_info, [TableName, size]).

load_mnesia_wrong(Config) ->
    FileName = "file that doesnt existRHCP.bup",
    {R2, Code} = ejabberdctl("restore", [FileName], Config),
    {match, _} = re:run(R2, ".+"),
    true = (Code =/= 0).

dump_table(Config) ->
    FileName = "dump.mn",
    TableName = passwd,
    %% Table passwd should not be empty
    TableSize = rpc_call(mnesia, table_info, [TableName, size]),
    {_, 0} = ejabberdctl("dump_table", [FileName, atom_to_list(TableName)], Config),
    rpc_call(mnesia, clear_table, [TableName]),
    0 = rpc_call(mnesia, table_info, [TableName, size]),
    {R, 0} = ejabberdctl("load", [FileName], Config),
    {match, _} = re:run(R, ".+"),
    TableSize = rpc_call(mnesia, table_info, [TableName, size]).

get_loglevel(Config) ->
    {R, 0} = ejabberdctl("get_loglevel", [], Config),
    LogLevel = rpc_call(ejabberd_loglevel, get, []),
    RegList = [io_lib:format("(.|\n|\r)*loglevel for ~p is ~p(.|\n|\r)*", [M, Lev]) || {M, {Lev, _}} <- LogLevel],
    Regexp = lists:flatten(RegList),
    Len = length(R),
    {match, [{0, Len}]} = re:run(R, Regexp, [{capture, first}]).

remove_old_messages_test(Config) ->
    escalus:story(Config, [{alice, 1}], fun(_) ->
        %% given
        JidA = nick_to_jid(alice, Config),
        JidB = nick_to_jid(bob, Config),
        JidRecordAlice = rpc_call(jid, from_binary, [JidA]),
        JidRecordBob = rpc_call(jid, from_binary, [JidB]),
        Msg1 = escalus_stanza:chat_to(<<"bob@localhost">>, "Hi, how are you? Its old message!"),
        Msg2 = escalus_stanza:chat_to(<<"bob@localhost">>, "Hello its new message!"),
        OldTimestamp = fallback_timestamp(10, now()),
        OfflineOld = generate_offline_message(JidRecordAlice, JidRecordBob, Msg1, OldTimestamp),
        OfflineNew = generate_offline_message(JidRecordAlice, JidRecordBob, Msg2, now()),
        {jid, _, _, _, LUser, LServer, _} = JidRecordBob,
        rpc_call(mod_offline_backend, write_messages, [LUser, LServer, [OfflineOld, OfflineNew]]),
        %% when
        {_, 0} = ejabberdctl("delete_old_messages", ["1"], Config),
        {ok, SecondList} = rpc_call(mod_offline_backend, pop_messages, [LUser, LServer]),
        %% then
        1 = length(SecondList)
    end).

remove_expired_messages_test(Config) ->
    escalus:story(Config, [{mike, 1}], fun(_) ->
        %% given
        JidA = nick_to_jid(mike, Config),
        JidB = nick_to_jid(kate, Config),
        JidRecordMike = rpc_call(jid, from_binary, [JidA]),
        JidRecordKate = rpc_call(jid, from_binary, [JidB]),
        Msg1 = escalus_stanza:chat_to(<<"kate@localhost">>, "Rolling stones"),
        Msg2 = escalus_stanza:chat_to(<<"kate@localhost">>, "Arctic monkeys!"),
        Msg3 = escalus_stanza:chat_to(<<"kate@localhost">>, "More wine..."),
        Msg4 = escalus_stanza:chat_to(<<"kate@localhost">>, "kings of leon"),
        OldTimestamp = fallback_timestamp(10, now()),
        ExpirationTime = fallback_timestamp(2, now()),
        ExpirationTimeFuture= fallback_timestamp(-5, now()),
        OfflineOld = generate_offline_expired_message(JidRecordMike, JidRecordKate, Msg1, OldTimestamp, ExpirationTime),
        OfflineNow = generate_offline_expired_message(JidRecordMike, JidRecordKate, Msg2, now(), ExpirationTime),
        OfflineFuture = generate_offline_expired_message(JidRecordMike, JidRecordKate, Msg3, now(), ExpirationTimeFuture),
        OfflineFuture2 = generate_offline_expired_message(JidRecordMike, JidRecordKate, Msg4, OldTimestamp, ExpirationTimeFuture),
        {jid, _, _, _, LUser, LServer, _} = JidRecordKate,
        rpc_call(mod_offline_backend, write_messages, [LUser, LServer, [OfflineOld, OfflineNow, OfflineFuture, OfflineFuture2]]),
        %% when
        {_, 0} = ejabberdctl("delete_expired_messages", [], Config),
        {ok, SecondList} = rpc_call(mod_offline_backend, pop_messages, [LUser, LServer]),
        %% then
        2 = length(SecondList)
    end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------


nick_to_jid(UserName, Config) when is_atom(UserName) ->
    UserSpec = escalus_users:get_userspec(Config, UserName),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

generate_offline_message(From, To, Msg, TimeStamp) ->
    {jid, _, _, _, LUser, LServer, _} = To,
    #offline_msg{us = {LUser, LServer}, timestamp = TimeStamp, expire = never, from = From, to = To, packet = Msg}.

generate_offline_expired_message(From, To, Msg, TimeStamp, ExpirationTime) ->
    {jid, _, _, _, LUser, LServer, _} = To,
    #offline_msg{us = {LUser, LServer}, timestamp = TimeStamp, expire = ExpirationTime, from = From, to = To, packet = Msg}.


fallback_timestamp(Days, {MegaSecs, Secs, _MicroSecs}) ->
    S = MegaSecs * 1000000 + Secs - 60 * 60 * 24 * Days,
    MegaSecs1 = S div 1000000,
    Secs1 = S rem 1000000,
    {MegaSecs1, Secs1, 0}.


start_mod_admin_extra() ->
    Domain = ct:get_config(ejabberd_domain),
    ok = dynamic_modules:restart(Domain, mod_admin_extra, []).

get_user_data(User, Config) when is_atom(User) ->
    get_user_data(escalus_users:get_options(Config, User, <<"newres">>), Config);
get_user_data(User, _Config) ->
    {_, Password} = lists:keyfind(password, 1, User),
    {_, Username} = lists:keyfind(username, 1, User),
    {_, Domain} = lists:keyfind(server, 1, User),
    {Username, Domain, Password}.

get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(md5, AccountPass))]).
get_sha(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(sha, AccountPass))]).

set_last(User, Domain, TStamp) ->
    Mod = escalus_ejabberd:rpc(mod_admin_extra_last, get_lastactivity_module, [Domain]),
    Fun = escalus_ejabberd:rpc(mod_admin_extra_last, get_lastactivity_fun, [Domain]),
    escalus_ejabberd:rpc(Mod, Fun, [escalus_utils:jid_to_lower(User), Domain, TStamp, <<>>]).

delete_users(Config) ->
    Users = escalus_users:get_users([alice, bob, kate, mike]),
    lists:foreach(fun({_User, UserSpec}) ->
                {Username, Domain, _Pass} = get_user_data(UserSpec, Config),
                escalus_ejabberd:rpc(ejabberd_auth, remove_user, [Username, Domain])
        end, Users).

%%-----------------------------------------------------------------
%% Predicates
%%-----------------------------------------------------------------

match_user_status(Users, StatusTxt) ->
    Statuses = string:tokens(StatusTxt, "\n"),

    true = (length(Users) == length(Statuses)),
    match_user_status2(Users, Statuses).

match_user_status2([], _) ->
    true;
match_user_status2([User | UserR], Statuses) ->
    Username = binary_to_list(escalus_client:username(User)),
    Domain = binary_to_list(escalus_client:server(User)),
    Resource = binary_to_list(escalus_client:resource(User)),

    true = lists:any(fun(Status) ->
                [Username, Domain, Resource]
                =:=
                lists:sublist(string:tokens(Status, "\t"), 1, 3)
        end, Statuses),
    match_user_status2(UserR, Statuses).

match_user_info(Users, UsersTxt) ->
    UsersInfo = string:tokens(UsersTxt, "\n"),

    true = (length(Users) == length(UsersInfo)),
    match_user_info2(Users, UsersInfo).

match_user_info2([], _) ->
    true;
match_user_info2([User | UserR], UsersInfo) ->
    Username = binary_to_list(escalus_client:username(User)),
    Domain = binary_to_list(escalus_client:server(User)),
    Resource = binary_to_list(escalus_client:resource(User)),
    FullJID = Username ++ "@" ++ Domain ++ "/" ++ Resource,

    true = lists:any(fun(UserInfo) ->
                string:str(UserInfo, string:to_lower(FullJID)) =:= 1
        end, UsersInfo),
    match_user_info2(UserR, UsersInfo).

match_roster(ItemsValid, Items) ->
    ItemsTokens = [ string:tokens(ItemToken, "\t") || ItemToken <- string:tokens(Items, "\n") ],

    true = (length(ItemsValid) == length(ItemsTokens)),
    true = lists:all(fun({Username, Domain, Nick, Group, Sub}) ->
                    JID = escalus_utils:jid_to_lower(<<Username/binary, "@", Domain/binary >>),
                    lists:any(fun
                                ([RosterJID, Nick, Sub, "none", Group]) ->
                                    JID =:= escalus_utils:jid_to_lower(list_to_binary(RosterJID));
                                (_) ->
                                    false
                              end, ItemsTokens)
            end, ItemsValid).

string_to_binary(List) ->
    case erlang:system_info(otp_release) of
        [$R|_] ->
            list_to_binary(List);
        _ ->
            unicode:characters_to_binary(List)
    end.
