-module(auth_methods_for_c2s_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(distributed_helper, [mim/0, rpc/4]).
-import(auth_helper, [assert_event/2]).

all() ->
    [
     {group, two_methods_enabled},
     {group, instrumentation}
    ].

groups() ->
    [
     {two_methods_enabled, [parallel],
      [
       can_login_with_allowed_method,
       cannot_login_with_not_allowed_method,
       can_login_to_another_listener
      ]},
     {instrumentation, [],
      [
       instrumentation_incremented_on_user_connect
      ]}
    ].

suite() ->
    distributed_helper:require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(instrumentation, Config) ->
    Config2 = set_auth_mod(Config),
    instrument_helper:start(events()),
    Config2;
init_per_group(_, Config0) ->
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    Config2 = modify_config_and_restart(Config1),
    instrument_helper:start(events()),
    escalus_cleaner:start(Config2).

end_per_group(instrumentation, _Config) ->
    instrument_helper:stop(),
    escalus_fresh:clean();
end_per_group(_, Config) ->
    instrument_helper:stop(),
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(mongooseim),
    escalus_fresh:clean().

init_per_testcase(TC, Config) ->
    Spec = escalus_fresh:freshen_spec(Config, alice),
    Clean = register_user(Config, Spec),
    [{clean_fn, Clean}, {spec, Spec}|escalus:init_per_testcase(TC, Config)].

end_per_testcase(TC, Config) ->
    Clean = proplists:get_value(clean_fn, Config),
    Clean(),
    escalus:end_per_testcase(TC, Config).

set_auth_mod(Config) ->
    Mod = case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
            true ->
                ejabberd_auth_rdbms;
            false ->
                ejabberd_auth_internal
        end,
    [{auth_mod, Mod} | Config].

modify_config_and_restart(Config) ->
    {Method, Mod} = case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
            true ->
                {"rdbms", ejabberd_auth_rdbms};
            false ->
                {"internal", ejabberd_auth_internal}
        end,
    NewConfigValues = [{auth_method, Method ++ "]\n  [auth.dummy"},
                       {auth_method_opts, false},
                       {allowed_auth_methods, "\"" ++ Method ++ "\""}],
    ejabberd_node_utils:modify_config_file(NewConfigValues, Config),
    ejabberd_node_utils:restart_application(mongooseim),
    [{auth_mod, Mod} | Config].

can_login_with_allowed_method(Config) ->
    Spec = proplists:get_value(spec, Config),
    {ok, _, _} = escalus_connection:start(Spec).

cannot_login_with_not_allowed_method(Config) ->
    Spec = proplists:get_value(spec, Config),
    {error, _} = escalus_connection:start([{password, <<"wrong">>}|Spec]).

can_login_to_another_listener(Config) ->
    Spec = proplists:get_value(spec, Config),
    TlsPort = ct:get_config({hosts, mim, c2s_tls_port}),
    Spec2 = [{port, TlsPort}, {ssl, true}, {ssl_opts, [{verify, verify_none}]},
             {password, <<"wrong">>} | Spec],
    {ok, Conn, _} = escalus_connection:start(Spec2),
    assert_event(auth_authorize, escalus_utils:get_jid(Conn)).

instrumentation_incremented_on_user_connect(Config) ->
    F = fun(Alice, Bob) ->
                Body = <<"Hello Bob">>,
                escalus:send(Alice, escalus_stanza:chat_to(Bob, Body)),
                escalus:assert(is_chat_message, [Body], escalus:wait_for_stanza(Bob)),
                assert_event(auth_authorize, escalus_utils:get_jid(Alice)),
                assert_event(auth_authorize, escalus_utils:get_jid(Bob))
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

%% Helpers
%% If dummy backend is enabled, it is not possible to create new users
%% (we check if an user does exist before registering the user).
register_user(Config, Spec) ->
    %% Calls backend module directly, so instrumentation is not called in this case.
    Mod = proplists:get_value(auth_mod, Config),
    #{username := User, server := Server,
      password := Password} = maps:from_list(Spec),
    LUser = jid:nodeprep(User),
    LServer = escalus_utils:jid_to_lower(Server),
    HostType = domain_helper:host_type(),
    ok = rpc(mim(), Mod, try_register,
        [HostType, LUser, LServer, Password]),
    fun() -> rpc(mim(), Mod, remove_user, [HostType, LUser, LServer]) end.

events() ->
    [{auth_authorize, #{host_type => domain_helper:host_type()}}].
