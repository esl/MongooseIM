-module(mod_filter_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(WAITING_TIMEOUT, 500).

all() ->
    [{group, messages}].

groups() ->
    [{messages, [sequence], [
                             allowed_messages_story,
                             denied_messages_story
                             ]}].

suite() ->
    escalus:suite().

domain() ->
    ct:get_config({hosts, mim, domain}).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    dynamic_modules:ensure_modules(required_modules()),
    update_rules(),
    escalus:init_per_suite(dynamic_modules:save_modules(domain(), Config)).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    dynamic_modules:ensure_modules(domain(), required_modules()),
    escalus:create_users(Config, escalus:get_users([alice, bob, astrid, kate])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, astrid, kate])).

init_per_testcase(CaseName, Config0) ->
    NewUsers = proplists:get_value(escalus_users, Config0) ++ escalus_ct:get_config(escalus_anon_users),
    Config = [{escalus_users, NewUsers} | Config0],
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    AnonJID = erlang:get(anon_user),
    mongoose_helper:clear_last_activity(Config, AnonJID),
    escalus:end_per_testcase(CaseName, Config).

required_modules() ->
  [{mod_filter, []}].

update_acl(ACLName, ACLRule) ->
    {atomic, ok} = rpc(mim(), acl, add, [global, ACLName, ACLRule]),
    ok.

update_access(AccessName, AccessRule) ->
    {atomic, ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, AccessName, global}, AccessRule]),
    ok.

% The configuration of rules is done using mim ACL and ACCESS.
% mod_filter allows  limit traffic between domains or users based on their type.
%
% On this `update_rules` example, next rules are applied:
%    1. The users of a private vhosts (localhost and sogndal)
%       can only chat with themselves. So that particular host
%       will have no connection to the exterior.
%    2. Anonymous users of some host (anonymous.localhost)
%       can only chat with shared single chat user(some bot, i.e kate@localhost), 
%       so that particular host will have no connection
%       to the other hosts (localhost, sogndal) listed in hosts.
update_rules() ->
    [ok = update_acl(ACLName, ACLRule) || {ACLName, ACLRule} <- acl_list()],
    [ok = update_access(AccessName, AccessRule) || {AccessName, AccessRule} <- access_list()].

%%sample acl list
acl_list()->
    [{domain1, {server_glob, <<"localhost">>}},
     {domain2, {server_glob, <<"sogndal">>}},
     {bot, {user, <<"kate">>, <<"localhost">>}},
     {anon_user, {server_glob, <<"anonymous.localhost">>}}].
%%sample access rules
access_list()->
    [{mod_filter, [{allow, all}]},
     {mod_filter_presence, [{allow, all}]},
     {mod_filter_iq, [{allow, all}]},
     {mod_filter_message, [
               {local, bot},
               {restrict_bot, anon_user},
               {restrict_dom1, domain1},
               {restrict_dom2, domain2}
     ]},
     {restrict_dom1, [{allow, domain1}, {deny, all}]},
     {restrict_dom2, [{allow, domain2}, {deny, all}]},
     {restrict_bot, [{allow, bot}, {deny, all}]}].
%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

user_can_send_to(Sender, Receiver) ->
    escalus_client:send(Sender, escalus_stanza:chat_to(Receiver, <<"Hi!">>)),
    escalus:assert(is_chat_message, [<<"Hi!">>], escalus:wait_for_stanza(Receiver)).

user_cannot_send_to(Sender, Receiver) ->
    escalus_client:send(Sender, escalus_stanza:chat_to(Receiver, <<"Hi!">>)),
    receiver_gets_nothing(Receiver).

receiver_gets_nothing(Receiver) ->
    ct:sleep(?WAITING_TIMEOUT),
    escalus_assert:has_no_stanzas(Receiver).

allowed_messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {jon, 1}], fun(Alice, Bob, Kate, Jon) ->
        erlang:put(anon_user, escalus_utils:get_jid(Jon)),
        %according to update_rules() example, anonymous can send message to bot.
        user_can_send_to(Jon, Kate),
        %according to update_rules() example, bot can send message to anonymous.
        user_can_send_to(Kate, Jon),
        %according to update_rules() example, user can send message to
        %other user within one host.
        user_can_send_to(Alice, Bob),
        %according to update_rules() example, user can send message to bot.
        user_can_send_to(Alice, Kate),
        %according to update_rules() example, bot can send message to user.
        user_can_send_to(Kate, Alice)
    end).

denied_messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {astrid, 1}, {bob, 1}, {jon, 1}], fun(Alice, Astrid, Bob, Jon) ->
        erlang:put(anon_user, escalus_utils:get_jid(Jon)),
        %according to update_rules() example, anonymous can't send message to user.
        user_cannot_send_to(Jon, Bob),
        %according to update_rules() example, user can't send message to anonymous.
        user_cannot_send_to(Alice, Jon),
        %according to update_rules() example, user 
        %can't send message to other user with different host.
        user_cannot_send_to(Astrid, Bob)
    end).
