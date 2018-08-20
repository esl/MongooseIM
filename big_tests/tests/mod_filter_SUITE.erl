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

all() ->
    [{group, messages}].

groups() ->
    [{messages, [sequence], [
                             allowed_messages_story,
                             denied_messages_story
                             ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = dynamic_modules:ensure_modules(required_modules()),
    update_rules(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob, astrid, kate])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, astrid, kate])).

init_per_testcase(CaseName, Config0) ->
    NewUsers = proplists:get_value(escalus_users, Config0) ++ escalus_ct:get_config(escalus_anon_users),
    Config = [{escalus_users, NewUsers}] ++ Config0,
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    AnonJID = erlang:get(anon_user),
    mongoose_helper:clear_last_activity(Config, AnonJID),
    escalus:end_per_testcase(CaseName, Config).

required_modules() ->
  [{mod_filter, []}].

% The configuration of rules is done using mim ACL and ACCESS.
% mod_filter allows  limit traffic between domains or users based on their type.
%
% On this `update_rules` example, next rules are applied:
%    1. the users of a private vhosts (localhost and sogndal)
%       can only chat with themselves. so that particular host
%       will have no connection to the exterior.
%    2. anonymous users of some host (anonymous.localhost)
%       can only chat with shared single chat user(some bot, i.e kate@localhost), 
%       so that particular host will have no connection
%       to the other hosts (localhost, sogndal) listed in hosts 
update_rules()->
    {atomic,ok} = rpc(mim(), acl, add, [global, domain1, {server_glob, <<"localhost">>}]),
    {atomic,ok} = rpc(mim(), acl, add, [global, domain2, {server_glob, <<"sogndal">>}]),
    {atomic,ok} = rpc(mim(), acl, add, [global, bot, {user, <<"kate">>, <<"localhost">>}]),
    {atomic,ok} = rpc(mim(), acl, add, [global, anon_user, {server_glob, <<"anonymous.localhost">>}]),


    {atomic,ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, mod_filter, global}, [
                                                                                        {allow, all}]]),
    {atomic,ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, mod_filter_presence, global},
                                                                                        [{allow, all}]]),
    {atomic,ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, mod_filter_message, global}, [
                                                                                        {local, bot},
                                                                                        {restrict_bot, anon_user},
                                                                                        {restrict_dom1, domain1},
                                                                                        {restrict_dom2, domain2}
                                                                                        ]]),
    {atomic,ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, mod_filter_iq, global}, 
                                                                                        [{allow, all}]]),

    %restriction for localhost
    {atomic,ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, restrict_dom1, global}, 
                                                                  [{allow, domain1}, {deny, all}]]),
    %restriction for sogndal
    {atomic,ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, restrict_dom2, global},
                                                                  [{allow, domain2}, {deny, all}]]),
    %restriction for bot
    {atomic,ok} = rpc(mim(), ejabberd_config, add_global_option, [{access, restrict_bot, global}, 
                                                                  [{allow, bot}, {deny, all}]]).
%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

allowed_messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {jon, 1}], fun(Alice, Bob, Kate, Jon) ->
        %according to update_rules() example, anonymous can send message to bot.
        erlang:put(anon_user, escalus_utils:get_jid(Jon)),
        escalus_client:send(Jon, escalus_stanza:chat_to(Kate, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus:wait_for_stanza(Kate)),

        %according to update_rules() example, bot can send message to anonymous.
        erlang:put(anon_user, escalus_utils:get_jid(Jon)),
        escalus_client:send(Kate, escalus_stanza:chat_to(Jon, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus:wait_for_stanza(Jon)),

        %according to update_rules() example, user can send message to
        %other user within one host.
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus:wait_for_stanza(Bob)),

        %according to update_rules() example, user can send message to bot.
        escalus_client:send(Alice, escalus_stanza:chat_to(Kate, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus:wait_for_stanza(Kate)),
    
        %according to update_rules() example, bot can send message to user.
        escalus_client:send(Kate, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus:wait_for_stanza(Alice))
    end).

denied_messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {astrid, 1}, {bob, 1}, {jon, 1}], fun(Alice, Astrid, Bob, Jon) ->
        erlang:put(anon_user, escalus_utils:get_jid(Jon)),

        %according to update_rules() example, anonymous can't send message to user.
        escalus_client:send(Jon, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        client_gets_nothing(Bob),
        
        %according to update_rules() example, user can't send message to anonymous.
        escalus_client:send(Alice, escalus_stanza:chat_to(Jon, <<"Hi!">>)),
        client_gets_nothing(Jon),

        %according to update_rules() example, user 
        %can't send message to other user with different host.
        escalus_client:send(Astrid, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        client_gets_nothing(Bob)
    end).


client_gets_nothing(Client) ->
    ct:sleep(500),
    escalus_assert:has_no_stanzas(Client).


