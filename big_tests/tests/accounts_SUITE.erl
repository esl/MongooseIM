-module(accounts_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(mongoose_helper, [wait_for_user/3]).
-import(auth_helper, [assert_event/2]).
-import(domain_helper, [domain/0, host_type/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds

all() ->
    [
     {group, register},
     {group, registration_watchers},
     {group, bad_registration},
     {group, bad_cancelation},
     {group, registration_timeout},
     {group, change_account_details},
     {group, change_account_details_store_plain},
     {group, utilities}
    ].

groups() ->
    [{register, [parallel], [register,
                             unregister,
                             already_registered,
                             registration_conflict,
                             check_unregistered]},
     {registration_watchers, [sequence], [admin_notify]},
     {bad_registration, [sequence], [null_password]},
     {bad_cancelation, [sequence], [bad_request_registration_cancelation,
                                    not_allowed_registration_cancelation]},
     {registration_timeout, [sequence], [registration_timeout,
                                         registration_failure_timeout]},
     {change_account_details, [parallel], change_password_tests()},
     {change_account_details_store_plain, [parallel], change_password_tests()},
     {utilities, [{group, user_info},
                  {group, users_number_estimate}]},
     {user_info, [parallel], [list_users,
                              list_selected_users,
                              count_users,
                              count_selected_users]},
     {users_number_estimate, [], [count_users_estimate]}
    ].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

change_password_tests() ->
    [change_password,
     change_password_to_null].
%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config1) ->
    instrument_helper:start(instrument_helper:declared_events(ejabberd_auth)),
    ok = dynamic_modules:ensure_modules(host_type(), required_modules()),
    Config2 = [{mod_register_options, mod_register_options()} | Config1],
    escalus:init_per_suite([{escalus_user_db, xmpp} | Config2]).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

required_modules() ->
    [{mod_register, mod_register_options()}].

mod_register_options() ->
    config_parser_helper:mod_config(mod_register, #{ip_access => [{allow, "127.0.0.0/8"},
                                                                  {deny, "0.0.0.0/0"}],
                                                    access => register}).

init_per_group(bad_cancelation, Config) ->
    escalus:create_users(Config, escalus:get_users([alice]));
init_per_group(change_account_details, Config) ->
    [{escalus_user_db,  {module, escalus_ejabberd}} |Config];
init_per_group(change_account_details_store_plain, Config) ->
    AuthOpts = mongoose_helper:auth_opts_with_password_format(plain),
    Config1 = mongoose_helper:backup_and_set_config_option(Config, auth, AuthOpts),
    [{escalus_user_db,  {module, escalus_ejabberd}} |Config1];
init_per_group(registration_timeout, Config) ->
    set_registration_timeout(Config);
init_per_group(utilities, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob]));
init_per_group(users_number_estimate, Config) ->
    AuthOpts = get_auth_opts(),
    NewAuthOpts = AuthOpts#{rdbms => #{users_number_estimate => true}},
    set_auth_opts(Config, NewAuthOpts);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(change_account_details, Config) ->
    escalus_fresh:clean(),
    [{escalus_user_db, xmpp} | Config];
end_per_group(change_account_details_store_plain, Config) ->
    escalus_fresh:clean(),
    mongoose_helper:restore_config(Config),
    [{escalus_user_db, xmpp} | Config];
end_per_group(bad_cancelation, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice]));
end_per_group(registration_timeout, Config) ->
    restore_registration_timeout(Config);
end_per_group(utilities, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob]));
end_per_group(users_number_estimate, Config) ->
    mongoose_helper:restore_config(Config);
end_per_group(_GroupName, Config) ->
    Config.

get_auth_opts() ->
    rpc(mim(), mongoose_config, get_opt, [{auth, host_type()}]).

set_auth_opts(Config, AuthOpts) ->
    rpc(mim(), ejabberd_auth, stop, [host_type()]),
    Config1 = mongoose_helper:backup_and_set_config_option(Config, {auth, host_type()}, AuthOpts),
    rpc(mim(), ejabberd_auth, start, [host_type()]),
    Config1.

init_per_testcase(admin_notify, Config) ->
    [{_, AdminSpec}] = escalus_users:get_users([admin]),
    [AdminU, AdminS, _AdminP] = escalus_users:get_usp(Config, AdminSpec),
    AdminJid = <<AdminU/binary, "@", AdminS/binary>>,
    enable_watcher(Config, AdminJid),
    escalus:init_per_testcase(admin_notify, Config);
init_per_testcase(not_allowed_registration_cancelation, Config) ->
    %% Use a configuration that will not allow inband cancelation (and registration).
    reload_mod_register_option(Config, access, none),
    escalus:init_per_testcase(not_allowed_registration_cancelation, Config);
init_per_testcase(registration_failure_timeout, Config) ->
    Config1 = deny_everyone_registration(Config),
    escalus:init_per_testcase(registration_failure_timeout, Config1);
init_per_testcase(CaseName, Config) when CaseName =:= list_selected_users;
                                         CaseName =:= count_selected_users ->
    case mongoose_helper:auth_modules() of
        [Mod | _] when Mod =:= ejabberd_auth_rdbms;
                       Mod =:= ejabberd_auth_internal ->
            escalus:init_per_testcase(CaseName, Config);
        Modules ->
            {skip, {"Queries for selected users not supported", Modules}}
    end;
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(register, Config) ->
    escalus:end_per_testcase(register, Config);
end_per_testcase(admin_notify, Config) ->
    disable_watcher(Config),
    escalus:delete_users(Config, escalus:get_users([alice, bob, admin])),
    escalus:end_per_testcase(admin_notify, Config);
end_per_testcase(registration_conflict, Config) ->
    escalus_users:delete_users(Config, escalus:get_users([alice])),
    escalus:end_per_testcase(registration_conflict, Config);
end_per_testcase(not_allowed_registration_cancelation, Config) ->
    restore_mod_register_options(Config),
    escalus:end_per_testcase(not_allowed_registration_cancelation, Config);
end_per_testcase(registration_timeout, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])),
    escalus:end_per_testcase(registration_timeout, Config);
end_per_testcase(registration_failure_timeout, Config) ->
    mongoose_helper:restore_config_option(Config, [{access, host_type()}, register]),
    escalus:end_per_testcase(registration_failure_timeout, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

register(Config) ->
    [{Name1, _UserSpec1}, {Name2, _UserSpec2}] = escalus_users:get_users([alice, bob]),
    Config1 = escalus_fresh:create_users(Config, escalus:get_users([Name1, Name2])),
    assert_event(auth_register_user, escalus_users:get_jid(Config1, Name1)),
    assert_event(auth_register_user, escalus_users:get_jid(Config1, Name2)),
    assert_event(auth_try_register, escalus_users:get_jid(Config1, Name1)),
    assert_event(auth_try_register, escalus_users:get_jid(Config1, Name2)),
    assert_event(auth_does_user_exist, escalus_users:get_jid(Config1, Name1)),
    assert_event(auth_does_user_exist, escalus_users:get_jid(Config1, Name2)).

unregister(Config) ->
    UserSpec = escalus_fresh:freshen_spec(Config, alice),
    escalus_users:create_user(Config, {alice, UserSpec}),
    escalus_users:delete_user(Config, {alice, UserSpec}),
    assert_event(auth_unregister_user, escalus_users:get_jid(Config, UserSpec)).

already_registered(Config) ->
    escalus_fresh:story(Config, [{alice, 1}], fun already_registered_story/1).

already_registered_story(Alice) ->
    AliceJid = escalus_utils:get_short_jid(Alice),
    assert_event(auth_register_user, AliceJid), % one event expected
    escalus:send(Alice, escalus_stanza:get_registration_fields()),
    Stanza = escalus:wait_for_stanza(Alice),
    escalus:assert(is_iq_result, Stanza),
    true = has_registered_element(Stanza),
    assert_event(auth_register_user, AliceJid). % still one event - nothing new

registration_conflict(Config) ->
    [Alice] = escalus_users:get_users([alice]),
    {ok, result, _Stanza} = escalus_users:create_user(Config, Alice),
    {ok, conflict, _Raw} = escalus_users:create_user(Config, Alice).

admin_notify(Config) ->
    [{Name1, UserSpec1}, {Name2, UserSpec2}] = escalus_users:get_users([alice, bob]),
    [{_, AdminSpec}] = escalus_users:get_users([admin]),
    Username1 = jid:str_tolower(escalus_users:get_username(Config, UserSpec1)),
    Username2 = jid:str_tolower(escalus_users:get_username(Config, UserSpec2)),
    [AdminU, AdminS, AdminP] = escalus_users:get_usp(Config, AdminSpec),

    rpc(mim(), ejabberd_auth, try_register, [mongoose_helper:make_jid(AdminU, AdminS), AdminP]),
    escalus:story(Config, [{admin, 1}], fun(Admin) ->
        assert_event(auth_authorize, escalus_utils:get_jid(Admin)),
        escalus:create_users(Config, escalus:get_users([Name1, Name2])),

            Predicates = [
                          fun(Stanza) ->
                                  Body = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
                                  escalus_pred:is_chat_message(Stanza)
                                  andalso
                                  re:run(Body, <<"registered">>, []) =/= nomatch
                                  andalso
                                  re:run(Body, Username, []) =/= nomatch
                          end
                          || Username <- [Username1, Username2]
                         ],
            escalus:assert_many(Predicates, escalus:wait_for_stanzas(Admin, 2))
        end).



null_password(Config) ->
    Details = escalus_fresh:freshen_spec(Config, alice),
    Alice = {alice, lists:keyreplace(password, 1, Details, {password, <<>>})},
    {error, _, Response} = escalus_users:create_user(Config, Alice),
    escalus:assert(is_iq_error, Response),
    %% This error response means there was no character data,
    %% i.e. elements `<password\>' or `<password></password>' where
    %% indeed present.
    {username, Name} = lists:keyfind(username, 1, Details),
    {server, Server} = lists:keyfind(server, 1, Details),
    escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Response),
    false = rpc(mim(), ejabberd_auth, does_user_exist, [mongoose_helper:make_jid(Name, Server)]).

check_unregistered(Config) ->
    [{_, UserSpec}] = escalus_users:get_users([bob]),
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    false = rpc(mim(), ejabberd_auth, does_user_exist, [mongoose_helper:make_jid(Username, Server)]).

bad_request_registration_cancelation(Config) ->

    %% To quote XEP 0077, section 3.2, table 1 (unregister error
    %% cases): "The <remove/> element [is] not the only child element
    %% of the <query/> element."
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        %% Alice sends bad cancelation request
        escalus:send(Alice, bad_cancelation_stanza()),

        %% Alice receives failure response
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Stanza),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], Stanza)

    end).

not_allowed_registration_cancelation(Config) ->

    %% To quote XEP 0077, section 3.2, table 1 (unregister error
    %% cases): "No sender is allowed to cancel registrations in-band."

    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        %% Alice sends cancelation request
        escalus:send(Alice, escalus_stanza:remove_account()),

        %% Alice receives failure response
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Stanza),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Stanza)

    end).

registration_timeout(Config) ->
    [Alice, Bob] = escalus_users:get_users([alice, bob]),

	%% The first user should be created successfully
	wait_for_user(Config, Alice, ?REGISTRATION_TIMEOUT),

    %% Creation of the second one should err because of not timing out yet
    {error, failed_to_register, Stanza} = escalus_users:create_user(Config, Bob),
    escalus:assert(is_iq_error, Stanza),
    %% Something else may be more acceptable for the assertion
    %% below... 2nd paragraph, section 3.1.1, XEP 0077: [...] a server
    %% MAY return a `<not-acceptable/>' stanza error if [...] an
    %% entity attempts to register a second identity after
    %% successfully completing the registration use case.
    escalus:assert(is_error, [<<"wait">>, <<"resource-constraint">>], Stanza),

    %% After timeout, the user should be registered successfully
    wait_for_user(Config, Bob, erlang:round(?REGISTRATION_TIMEOUT * 1.5 * 1000)).

registration_failure_timeout(Config) ->
    timer:sleep(timer:seconds(?REGISTRATION_TIMEOUT + 1)),
    [Alice] = escalus_users:get_users([alice]),

    %% Registration of the first user should fail because of access denial
    {error,failed_to_register,R} = escalus_users:create_user(Config, Alice),
    escalus:assert(is_iq_error, R),
    escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], R),

    %% Registration of a second one should fail because requests were
    %% made in quick succession
    {error,failed_to_register,S} = escalus_users:create_user(Config, Alice),
    escalus:assert(is_iq_error, S),
    escalus:assert(is_error, [<<"wait">>, <<"resource-constraint">>], S).

change_password(Config) ->

    escalus_fresh:story(Config, [{alice, 1}], fun(Alice) ->
        Username = escalus_client:username(Alice),
        escalus:send(Alice,
            Q = escalus_stanza:iq_set(?NS_INBAND_REGISTER,
                [#xmlel{name = <<"username">>,
                        children = [#xmlcdata{content = Username}]},
                 #xmlel{name = <<"password">>,
                        children = [#xmlcdata{content = strong_pwd()}]}])),

        R = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_result, [Q], R)

    end).

change_password_to_null(Config) ->

    %% Section 3.3, XEP 0077: If the user provides an empty password
    %% element or a password element that contains no XML character
    %% data (i.e., either <password/> or <password></password>), the
    %% server or service MUST NOT change the password to a null value,
    %% but instead MUST maintain the existing password.

    %% By the above, `end_per_testcase' should succeed. XEP 0077
    %% doesn't say how how an XMPP sever should respond, but since
    %% this is in IQ, it must: so we choose to require a `not-allowed'
    %% response.

    escalus_fresh:story(Config, [{alice, 1}], fun(Alice) ->
        Username = escalus_client:username(Alice),
        escalus:send(Alice,
            escalus_stanza:iq_set(?NS_INBAND_REGISTER,
                [#xmlel{name = <<"username">>,
                        children = [#xmlcdata{content = Username}]},
                 #xmlel{name = <<"password">>,
                        children = [#xmlcdata{content = <<"">>}]}])),

        R = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_error, R),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], R)

    end).

%% Tests for utility functions currently accessible only from the Erlang shell

list_users(_Config) ->
    Users = [{<<"alice">>, domain()}, {<<"bob">>, domain()}],
    ?assertEqual(Users, lists:sort(rpc(mim(), ejabberd_auth, get_vh_registered_users, [domain()]))).

list_selected_users(_Config) ->
    Alice = {<<"alice">>, domain()},
    Bob = {<<"bob">>, domain()},
    ?assertEqual([Alice], rpc(mim(), ejabberd_auth, get_vh_registered_users,
                              [domain(), [{from, 1}, {to, 1}]])),
    ?assertEqual([Bob], rpc(mim(), ejabberd_auth, get_vh_registered_users,
                            [domain(), [{from, 2}, {to, 10}]])),
    ?assertEqual([Alice], rpc(mim(), ejabberd_auth, get_vh_registered_users,
                              [domain(), [{prefix, <<"a">>}]])),
    ?assertEqual([Alice], rpc(mim(), ejabberd_auth, get_vh_registered_users,
                              [domain(), [{prefix, <<"a">>}, {from, 1}, {to, 10}]])),
    ?assertEqual([Bob], rpc(mim(), ejabberd_auth, get_vh_registered_users,
                            [domain(), [{prefix, <<>>}, {from, 2}, {to, 10}]])).

count_users(_Config) ->
    ?assertEqual(2, rpc(mim(), ejabberd_auth, get_vh_registered_users_number, [domain()])).

count_users_estimate(_Config) ->
    Count = rpc(mim(), ejabberd_auth, get_vh_registered_users_number, [domain()]),
    ?assert(is_integer(Count) andalso Count >= 0).

count_selected_users(_Config) ->
    ?assertEqual(1, rpc(mim(), ejabberd_auth, get_vh_registered_users_number,
                        [domain(), [{prefix, <<"a">>}]])).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

skip_if_mod_register_not_enabled(Config) ->
    case escalus_users:is_mod_register_enabled(Config) of
        true ->
            Config; % will create users inside test case
        _ ->
            {skip, mod_register_disabled}
    end.

strong_pwd() ->
    <<"Sup3r","c4li","fr4g1","l1571c","3xp1","4l1","d0c10u5">>.

set_registration_timeout(Config) ->
    mongoose_helper:backup_and_set_config_option(Config, registration_timeout,
                                                 ?REGISTRATION_TIMEOUT).

restore_registration_timeout(Config) ->
    mongoose_helper:restore_config_option(Config, registration_timeout).

deny_everyone_registration(Config) ->
    mongoose_helper:backup_and_set_config_option(Config, [{access, host_type()}, register],
                                                 [#{acl => all, value => deny}]).

has_registered_element(Stanza) ->
        [#xmlel{name = <<"registered">>}] =:= exml_query:paths(Stanza,
            [{element, <<"query">>}, {element, <<"registered">>}]).

bad_cancelation_stanza() ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"query">>,
        attrs = #{<<"xmlns">> => <<"jabber:iq:register">>},
        children = [#xmlel{name = <<"remove">>},
                    %% The <remove/> element is not the only child element of the
                    %% <query/> element.
                    #xmlel{name = <<"foo">>}]}]).

user_exists(Name, Config) ->
    {Name, Client} = escalus_users:get_user_by_name(Name),
    [Username, Server, _Pass] = escalus_users:get_usp(Config, Client),
    rpc(mim(), ejabberd_auth, does_user_exist, [mongoose_helper:make_jid(Username, Server)]).

reload_mod_register_option(Config, Key, Value) ->
    Host = host_type(),
    Args = proplists:get_value(mod_register_options, Config),
    Args1 = maps:put(Key, Value, Args),
    dynamic_modules:restart(Host, mod_register, Args1).

restore_mod_register_options(Config) ->
    Host = host_type(),
    Args = proplists:get_value(mod_register_options, Config),
    dynamic_modules:restart(Host, mod_register, Args).

enable_watcher(Config, Watcher) ->
    reload_mod_register_option(Config, registration_watchers, [Watcher]).

disable_watcher(Config) ->
    restore_mod_register_options(Config).
