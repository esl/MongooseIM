-module(auth_methods_for_c2s_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(distributed_helper, [mim/0, rpc/4]).

all() ->
    [
     {group, two_methods_enabled},
     {group, metrics}
    ].

groups() ->
    [
     {two_methods_enabled, [parallel],
      [
       can_login_with_allowed_method,
       cannot_login_with_not_allowed_method,
       can_login_to_another_listener
      ]},
     {metrics, [],
      [
       metrics_incremented_on_user_connect
      ]}
    ].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(metrics, Config) ->
    Config;
init_per_group(_, Config0) ->
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    modify_config_and_restart(Config1),
    escalus_cleaner:start(Config1).

end_per_group(metrics, _Config) ->
    escalus_fresh:clean();
end_per_group(_, Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(mongooseim),
    escalus_fresh:clean().

init_per_testcase(TC, Config) ->
    Spec = escalus_fresh:freshen_spec(Config, alice),
    Clean = register_internal_user(Spec),
    [{clean_fn, Clean}, {spec, Spec}|escalus:init_per_testcase(TC, Config)].

end_per_testcase(TC, Config) ->
    Clean = proplists:get_value(clean_fn, Config),
    Clean(),
    escalus:end_per_testcase(TC, Config).

modify_config_and_restart(Config) ->
    NewConfigValues = [{auth_method, "internal]\n  [auth.dummy"},
                       {auth_method_opts, false},
                       {allowed_auth_methods, "\"internal\""}],
    ejabberd_node_utils:modify_config_file(NewConfigValues, Config),
    ejabberd_node_utils:restart_application(mongooseim).

can_login_with_allowed_method(Config) ->
    Spec = proplists:get_value(spec, Config),
    {ok, _, _} = escalus_connection:start(Spec).

cannot_login_with_not_allowed_method(Config) ->
    Spec = proplists:get_value(spec, Config),
    {error, _} = escalus_connection:start([{password, <<"wrong">>}|Spec]).

can_login_to_another_listener(Config) ->
    Spec = proplists:get_value(spec, Config),
    Spec2 = [{port, ct:get_config({hosts, mim, c2s_tls_port})},
             {password, <<"wrong">>}|Spec],
    {ok, _, _} = escalus_connection:start(Spec2).

metrics_incremented_on_user_connect(ConfigIn) ->
    F = fun(Alice, Bob) ->
                Body = <<"Hello Bob">>,
                escalus:send(Alice, escalus_stanza:chat_to(Bob, Body)),
                escalus:assert(is_chat_message, [Body], escalus:wait_for_stanza(Bob))
        end,
    HostType = domain_helper:host_type(),
    HostTypePrefix = domain_helper:make_metrics_prefix(HostType),
    MongooseMetrics = [{[HostTypePrefix, backends, auth, authorize], changed}],
    Config = [{mongoose_metrics, MongooseMetrics} | ConfigIn],
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

%% Helpers
%% If dummy backend is enabled, it is not possible to create new users
%% (we check if an user does exist before registering the user).
register_internal_user(Spec) ->
    #{username := User, server := Server,
      password := Password} = maps:from_list(Spec),
    LUser = jid:nodeprep(User),
    LServer = escalus_utils:jid_to_lower(Server),
    HostType = domain_helper:host_type(),
    rpc(mim(), ejabberd_auth_internal, try_register,
        [HostType, LUser, LServer, Password]),
    fun() -> rpc(mim(), ejabberd_auth_internal, remove_user,
                 [HostType, LUser, LServer]) end.
