%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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

-module(conf_reload_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(reload_helper, [reload_through_ctl/2]).

-define(RELOADED_DOMAIN, ct:get_config(ejabberd_reloaded_domain)).

-define(SAMPLE_USERNAME, <<"astrid">>).
-define(RELOADED_DOMAIN_USER, astrid).
-define(INITIAL_DOMAIN_USER, alice).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, xmpp_domain_local_reload}].

groups() ->
    [{xmpp_domain_local_reload, [],
      [
       domain_should_change,
       user_should_be_registered_and_unregistered_via_ctl,
       user_should_be_registered_and_unregistered_via_xmpp,
       user_should_be_disconnected_from_removed_domain
       ]}].

suite() ->
    [{required, ejabberd_reloaded_domain} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    ejabberd_node_utils:init(Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(xmpp_domain_local_reload, Config0) ->
    F = fun(Cfg) ->
                change_domain_in_config_file(Cfg)
        end,
    Config1 = create_user_in_initial_domain(?INITIAL_DOMAIN_USER,
                                            Config0),
    [{new_hosts_value, [?RELOADED_DOMAIN]},
     {modify_config_file_fun, F} | Config1].

end_per_group(xmpp_domain_local_reload, Config) ->
    delete_user_in_initial_domain(?INITIAL_DOMAIN_USER, Config).

init_per_testcase(CaseName, Config0) ->
    Config1 = escalus:init_per_testcase(CaseName, Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    run_config_file_modification_fun(Config1),
    Config1.

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config),
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(ejabberd).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

domain_should_change(Config) ->
    %% GIVEN
    NewHosts = ?config(new_hosts_value, Config),
    ?assertNot(NewHosts == get_ejabberd_hosts()),

    %% WHEN
    reload_through_ctl(default_node(Config), Config),

    %% THEN
    ?assertMatch(NewHosts, get_ejabberd_hosts()).

user_should_be_registered_and_unregistered_via_ctl(Config) ->
    %% GIVEN
    [NewHost] = ?config(new_hosts_value, Config),
    ?assertMatch({cannot_register, _},
                 register_user_by_ejabberd_admin(?SAMPLE_USERNAME, NewHost)),

    %% WHEN
    reload_through_ctl(default_node(Config), Config),

    %% THEN
    ?assertMatch({ok, _},
                 register_user_by_ejabberd_admin(?SAMPLE_USERNAME, NewHost)),
    ?assertMatch({ok, _},
                 unregister_user_by_ejabberd_admin(?SAMPLE_USERNAME, NewHost)).

user_should_be_registered_and_unregistered_via_xmpp(Config) ->
    %% GIVEN
    UserDomain = get_user_domain(?RELOADED_DOMAIN_USER),
    ?assert(lists:member(UserDomain, ?config(new_hosts_value, Config))),

    %% WHEN
    reload_through_ctl(default_node(Config), Config),

    %% THEN
    ?assertMatch(ok, create_user(?RELOADED_DOMAIN_USER, Config)),
    ?assertMatch({ok, result, _},
                 delete_user(?RELOADED_DOMAIN_USER, Config)).

user_should_be_disconnected_from_removed_domain(Config) ->
    %% GIVEN
    Conn = connect_user(?INITIAL_DOMAIN_USER, Config),

    %% WHEN
    reload_through_ctl(default_node(Config), Config),

    %% THEN
    ?assertNot(escalus_connection:is_connected(Conn)).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

get_ejabberd_hosts() ->
    escalus_ejabberd:rpc(ejabberd_config, get_global_option, [hosts]).

register_user_by_ejabberd_admin(User, Host) ->
    escalus_ejabberd:rpc(ejabberd_admin, register, [User, Host, <<"doctor">>]).

unregister_user_by_ejabberd_admin(User, Host) ->
    escalus_ejabberd:rpc(ejabberd_admin, unregister, [User, Host]).

change_domain_in_config_file(Config) ->
    ejabberd_node_utils:modify_config_file(
      [mk_value_for_hosts_pattern(?RELOADED_DOMAIN)], Config).

mk_value_for_hosts_pattern(Domain) ->
    {hosts, "[\"" ++ binary_to_list(Domain) ++ "\"]"}.

run_config_file_modification_fun(Config) ->
    Fun = ?config(modify_config_file_fun, Config),
    Fun(Config).

create_user_in_initial_domain(User, Config) ->
    escalus:create_users(Config, escalus:get_users([User])).

delete_user_in_initial_domain(User, Config) ->
    escalus:delete_users(Config, escalus:get_users([User])).

connect_user(User, Config) ->
    UserSpec = escalus_users:get_userspec(Config, User),
    {ok, Conn, _, _} = escalus_connection:start(UserSpec),
    Conn.

get_user_domain(User) ->
    {User, UserSpec} = escalus_users:get_user_by_name(User),
    proplists:get_value(server, UserSpec).

create_user(User, Config) ->
    {User, UserSpec} = escalus_users:get_user_by_name(User),
    Result = escalus_users:create_user(Config, {User, UserSpec}),
    escalus_users:verify_creation(Result).

delete_user(User, Config) ->
    {User, UserSpec} = escalus_users:get_user_by_name(User),
    escalus_users:delete_user(Config, {User, UserSpec}).

default_node(Config) ->
    Node = escalus_config:get_config(ejabberd_node, Config),
    Node == undefined andalso error(node_undefined, [Config]),
    Node.
