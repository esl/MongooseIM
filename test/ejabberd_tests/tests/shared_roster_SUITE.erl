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

-module(shared_roster_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(USERS, [alice, bob]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->

    [{group, shared_roster}].

groups() ->
    [{shared_roster,[sequence],[receive_presences,get_contacts,delete_user,add_user]}].



suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case get_auth_method() of
        ldap ->
            start_roster_module(ldap),
            escalus:init_per_suite([{escalus_user_db, {module, ldap_helper}}, {ldap_auth, true} | Config]);
        _ ->
            escalus:init_per_suite([{ldap_auth, false} | Config])
    end.

end_per_suite(Config) ->
    stop_roster_module(get_auth_method()),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    escalus:create_users(Config, escalus:get_users(?USERS)).

end_per_group(_, Config) ->
    escalus:delete_users(Config, escalus:get_users(?USERS)).

init_per_testcase(CaseName,Config) ->
    case proplists:get_value(ldap_auth, Config) of
        false ->
            {skip, no_shared_roster_available};
        _ ->
            escalus:init_per_testcase(CaseName,Config)
    end.

end_per_testcase(CaseName,Config) ->
    escalus:end_per_testcase(CaseName,Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

%% Receive presences from roster people
receive_presences(Config) ->
    escalus:story(Config,[{alice, 1}],fun(Alice) ->
        % Bob becomes available
        {ok, Bob} = escalus_client:start_for(Config, bob, <<"res1">>),
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),

        % Alice receives presence from Bob
        ReceivedA = escalus:wait_for_stanza(Alice),
        escalus:assert(is_presence,ReceivedA),
        escalus_assert:is_stanza_from(Bob,ReceivedA),

        no_stanzas([Alice]),
        escalus_client:stop(Bob)
    end).

get_contacts(Config) ->
    escalus:story(Config,[{alice, 1}],fun(Alice) ->
        % Alice sends get_roster iq
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        Roster=escalus_client:wait_for_stanza(Alice),

        % Roster contains all created users excluding Alice
        escalus:assert(is_roster_result,Roster),
        NumOfOtherUsers = length(escalus_users:get_users(?USERS))-1,
        escalus:assert(count_roster_items,[NumOfOtherUsers],Roster)
    end).

delete_user(Config) ->
    NumOfOtherUsers = length(escalus_users:get_users(?USERS)) - 2,
    %% wait to invalidate the roster group cache
    timer:sleep(1200),
    escalus:delete_users(Config, escalus:get_users([bob])),
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        Roster=escalus_client:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Roster),
        escalus:assert(count_roster_items, [NumOfOtherUsers], Roster)
    end).

add_user(Config) ->
    escalus:create_users(Config, escalus:get_users([bob])),
    timer:sleep(1200),
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        NumOfOtherUsers = length(escalus_users:get_users(?USERS)) - 1,

        escalus_client:send(Alice, escalus_stanza:roster_get()),
        Roster=escalus_client:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Roster),
        escalus:assert(count_roster_items, [NumOfOtherUsers], Roster)
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
start_roster_module(ldap) ->
    case escalus_ejabberd:rpc(gen_mod, start_module,
                              [ct:get_config(ejabberd_domain),
                               mod_shared_roster_ldap, get_ldap_args()]) of
        {badrpc, Reason} ->
            ct:fail("Cannot start module ~p reason ~p", [mod_shared_roster, Reason]);
        _ -> ok
    end;
start_roster_module(_) ->
    ok.

stop_roster_module(ldap)->
    case escalus_ejabberd:rpc(gen_mod, stop_module, [ct:get_config(ejabberd_domain), mod_shared_roster_ldap]) of
        {badrpc, Reason} ->
            ct:fail("Cannot stop module ~p reason ~p", [mod_shared_roster_ldap, Reason]);
        _ -> ok
    end;
stop_roster_module(_)->
    ok.

get_auth_method() ->
    XMPPDomain = ct:get_config(ejabberd_domain),
    escalus_ejabberd:rpc(ejabberd_config, get_local_option,[{auth_method, XMPPDomain}]).

get_ldap_args() ->
    [
     {ldap_base, "ou=Users,dc=esl,dc=com"},
     {ldap_groupattr, "ou"},
     {ldap_memberattr, "cn"},{ldap_userdesc, "cn"},
     {ldap_filter, "(objectClass=inetOrgPerson)"},
     {ldap_rfilter, "(objectClass=inetOrgPerson)"},
     {ldap_group_cache_validity, 1},
     {ldap_user_cache_validity, 1}
    ].

no_stanzas(Users) ->
    lists:foreach(fun escalus_assert:has_no_stanzas/1, Users).
