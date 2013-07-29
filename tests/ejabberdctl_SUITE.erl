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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, accounts}].

groups() ->
     [{accounts, [sequence], last()}].

accounts() -> [change_password, check_password_hash, check_password,
               check_account, ban_account, num_active_users, delete_old_users,
               delete_old_users_vhost].

sessions() -> [num_resources_num, kick_session, status,
               sessions_info, set_presence].

vcard() -> [vcard_rw, vcard2_rw, vcard2_multi_rw].

roster() -> [rosteritem_rw, push_roster, push_roster_all, push_roster_alltoall].

last() -> [set_last].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    {ok, EjdWD} = escalus_ejabberd:rpc(file, get_cwd, []),

    Cwd0 = escalus_config:get_config(data_dir, Config),
    CwdTokens = string:tokens(Cwd0, "/"),
    Cwd =  [$/ | string:join(lists:sublist(CwdTokens, 1, length(CwdTokens)-2), "/")],
    TemplatePath = Cwd ++ "/roster.template",

    start_mod_admin_extra(),
    NewConfig = escalus:init_per_suite([{ctl_path, EjdWD ++ "/bin/ejabberdctl"},
                                        {roster_template, TemplatePath} | Config]),
    escalus:create_users(NewConfig).

end_per_suite(Config) ->
    delete_users(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(delete_old_users, Config) ->
    Users = escalus_users:get_users(all),
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
    {'EXIT', _} = (catch escalus_client:send(Mike,
                                                 escalus_stanza:chat_to(Mike, <<"Hello myself!">>))),
    {error, {connection_step_failed, _, _}} = escalus_client:start_for(Config, mike, <<"newres2">>),
    ejabberdctl("change_password", [User, Domain, Pass], Config).

num_active_users(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    {MikeName, Domain, _} = get_user_data(mike, Config),
    
    {Mega, Secs, _} = erlang:now(),
    Now = Mega*1000000+Secs,
    set_last(AliceName, Domain, Now),
    set_last(MikeName, Domain, Now - 864000), %% Now - 10 days

    {"1\n", _} = ejabberdctl("num_active_users", [Domain, "5"], Config).

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
    escalus:story(Config, [3, 1], fun(_, Alice2, _, _) ->
                {Username, Domain, _} = get_user_data(alice, Config),
                ResName = binary_to_list(escalus_client:resource(Alice2)) ++ "\n",

                {"3\n", _} = ejabberdctl("num_resources", [Username, Domain], Config),
                {ResName, _} = ejabberdctl("resource_num", [Username, Domain, "2"], Config)
        end).

kick_session(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
                Username = escalus_client:username(Alice),
                Domain = escalus_client:server(Alice),
                Resource = escalus_client:resource(Alice),

                {_, 0} = ejabberdctl("kick_session", [Username, Domain, Resource, "'\"Because I can!\"'"], Config),
                Stanza = escalus:wait_for_stanza(Alice),
                escalus:assert(is_stream_error, [<<"conflict">>, <<"Because I can!">>], Stanza),
                {'EXIT', _} = (catch escalus_client:send(Alice,
                                                 escalus_stanza:chat_to(Alice, <<"Hello myself!">>)))
        end).

status(Config) ->
    escalus:story(Config, [1, 1, 1], fun(User1, User2, User3) ->
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
    escalus:story(Config, [1, 1, 1], fun(User1, User2, User3) ->
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
    escalus:story(Config, [1], fun(Alice) ->
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
    escalus:story(Config, [1], fun(Alice) ->
                {AliceName, Domain, _} = get_user_data(alice, Config),
                {BobName, Domain, _} = get_user_data(bob, Config),
                {MikeName, Domain, _} = get_user_data(mike, Config),
                
                {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, BobName,
                                                        Domain, "MyBob", "MyGroup", "both"], Config),
                {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, MikeName,
                                                        Domain, "MyMike", "MyGroup", "both"], Config),

                [Push1, Push2] = escalus:wait_for_stanzas(Alice, 2), % Check roster broadcasts
                escalus:assert(is_roster_set, Push1),
                escalus:assert(roster_contains, [bob], Push1),
                escalus:assert(is_roster_set, Push2),
                escalus:assert(roster_contains, [mike], Push2),

                {Items1, 0} = ejabberdctl("get_roster", [AliceName, Domain], Config),
                match_roster([{BobName, Domain, "MyBob", "MyGroup", "both"},
                              {MikeName, Domain, "MyMike", "MyGroup", "both"}], Items1),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                escalus:assert(roster_contains, [bob], Roster1),
                escalus:assert(roster_contains, [mike], Roster1),

                {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config),
                
                Push3 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_set, Push3),
                escalus:assert(roster_contains, [bob], Push3),

                {Items2, 0} = ejabberdctl("get_roster", [AliceName, Domain], Config),
                match_roster([{MikeName, Domain, "MyMike", "MyGroup", "both"}], Items2),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(mike))  % cleanup
        end).

push_roster(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
                {AliceName, Domain, _} = get_user_data(alice, Config),
                TemplatePath = escalus_config:get_config(roster_template, Config),
                
                {_, 0} = ejabberdctl("push_roster", [TemplatePath, AliceName, Domain], Config),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                escalus:assert(roster_contains, [bob], Roster1),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(bob)) % cleanup
        end).

push_roster_all(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
                TemplatePath = escalus_config:get_config(roster_template, Config),

                {_, 0} = ejabberdctl("push_roster_all", [TemplatePath], Config),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                escalus:assert(roster_contains, [bob], Roster1),
                
                escalus:send(Bob, escalus_stanza:roster_get()),
                Roster2 = escalus:wait_for_stanza(Bob),
                escalus:assert(is_roster_result, Roster2),
                escalus:assert(roster_contains, [alice], Roster2),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(bob)), % cleanup
                escalus:send(Bob, escalus_stanza:roster_remove_contact(alice)) % cleanup
        end).

push_roster_alltoall(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
                {_, Domain, _} = get_user_data(alice, Config),

                {_, 0} = ejabberdctl("push_roster_alltoall", [Domain, "MyGroup"], Config),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster = escalus:wait_for_stanza(Alice),
                
                escalus:assert(is_roster_result, Roster),
                escalus:assert(roster_contains, [bob], Roster),
                escalus:assert(roster_contains, [mike], Roster),
                escalus:assert(roster_contains, [kate], Roster)
        end).

%%--------------------------------------------------------------------
%% mod_admin_extra_last tests
%%--------------------------------------------------------------------

set_last(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
                {AliceName, Domain, _} = get_user_data(alice, Config),
                {BobName, Domain, _} = get_user_data(bob, Config),
                
                {_, 0} = ejabberdctl("add_rosteritem", [AliceName, Domain, BobName,
                                                        Domain, "MyBob", "MyGroup", "both"], Config),
                {_, 0} = ejabberdctl("add_rosteritem", [BobName, Domain, AliceName,
                                                        Domain, "MyAlice", "MyGroup", "both"], Config),

                escalus:wait_for_stanza(Alice), % ignore push

                {Mega, Secs, _} = erlang:now(),
                TS = integer_to_list(Mega*1000000+Secs-7200),

                {_, 0} = ejabberdctl("set_last", [BobName, Domain, TS, "Status"], Config),

                escalus:send(Alice, escalus_stanza:last_activity(bob)),
                LastAct = escalus:wait_for_stanza(Alice),
                escalus:assert(is_last_result, LastAct),
                Seconds = list_to_integer(binary_to_list(
                            exml_query:path(LastAct, [{element, <<"query">>}, {attr, <<"seconds">>}]))),
                true = ( (Seconds > 7100) andalso (Seconds < 7300) ),

                {_, 0} = ejabberdctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config), % cleanup
                {_, 0} = ejabberdctl("delete_rosteritem", [BobName, Domain, AliceName, Domain], Config)
        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

start_mod_admin_extra() ->
    Domain = ct:get_config(ejabberd_domain),
    ok = dynamic_modules:restart(Domain, mod_admin_extra, []).

normalize_args(Args) ->
    lists:map(fun
            (Arg) when is_binary(Arg) ->
                binary_to_list(Arg);
            (Arg) when is_list(Arg) ->
                Arg
        end, Args).

ejabberdctl(Cmd, Args, Config) ->
    CtlCmd = escalus_config:get_config(ctl_path, Config),
    run(string:join([CtlCmd, Cmd | normalize_args(Args)], " ")).

get_user_data(User, Config) when is_atom(User) ->
    get_user_data(escalus_users:get_options(Config, User, <<"newres">>), Config);
get_user_data(User, _Config) ->
    {_, Password} = lists:keyfind(password, 1, User),
    {_, Username} = lists:keyfind(username, 1, User),
    {_, Domain} = lists:keyfind(server, 1, User),
    {Username, Domain, Password}.

run(Cmd) -> 
    run(Cmd, 5000).

run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    loop(Port,[], Timeout).

loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, Data++NewData, Timeout);
        {Port, {exit_status, ExitStatus}} -> {Data, ExitStatus}
    after Timeout ->
            throw(timeout)
    end.

get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:md5(AccountPass))]).
get_sha(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:sha(AccountPass))]).

set_last(User, Domain, TStamp) ->
    Mod = escalus_ejabberd:rpc(mod_admin_extra_last, get_lastactivity_module, [Domain]),
    escalus_ejabberd:rpc(Mod, store_last_info, [User, Domain, TStamp, <<>>]).

delete_users(Config) ->
    Users = escalus_users:get_users(all),
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
                string:str(UserInfo, FullJID) =:= 1
        end, UsersInfo),
    match_user_info2(UserR, UsersInfo).

match_roster(ItemsValid, Items) ->
    ItemsTokens = [ string:tokens(ItemToken, "\t") || ItemToken <- string:tokens(Items, "\n") ],

    true = (length(ItemsValid) == length(ItemsTokens)),
    true = lists:all(fun({Username, Domain, Nick, Group, Sub}) ->
                    lists:any(fun(ItemTokens) ->
                                ItemTokens =:= [binary_to_list(Username) ++ "@" ++ binary_to_list(Domain),
                                                Nick, Sub, "none", Group]
                        end, ItemsTokens)
            end, ItemsValid).
