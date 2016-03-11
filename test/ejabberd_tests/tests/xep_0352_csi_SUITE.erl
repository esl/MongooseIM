-module(xep_0352_csi_SUITE).

-include_lib("exml/include/exml.hrl").

-compile([export_all]).

all() ->
    [{group, basic}].


groups() ->
    [{basic,
      [parallel, shuffle],
      [
       server_announces_csi,
       alice_is_inactive_and_no_stanza_arrived,
       alice_gets_msgs_after_activate
      ]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    [{escalus_user_db, {module, escalus_ejabberd}} | escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

server_announces_csi(Config) ->
    {_, Users} = escalus_fresh:create_fresh_users(Config, [{alice, 1}]),
    Spec = proplists:get_value(alice, Users),
    ct:print("~p", [Users]),
    Steps = [start_stream,
             stream_features,
             maybe_use_ssl,
             maybe_use_compression,
             authenticate,
             bind,
             session],
    {ok, _Client, _Props, Features} = escalus_connection:start(Spec, Steps),
    true = proplists:get_value(client_state_indication, Features).

alice_is_inactive_and_no_stanza_arrived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        given_client_is_inactive_and_message_sent(Alice, Bob),

        escalus_assert:has_no_stanzas(Alice)
    end).

alice_gets_msgs_after_activate(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %%Given
        given_client_is_inactive_and_message_sent(Alice, Bob),

        %%When client becomes active again
        escalus:send(Alice, csi_stanza(<<"active">>)),

        Msg = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, Msg)
    end).

given_client_is_inactive_and_message_sent(Alice, Bob) ->
    %%Given
    given_client_is_inactive(Alice),

    %%When
    escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice">>)),
    timer:sleep(timer:seconds(1)).


given_client_is_inactive(Alice) ->
    escalus:send(Alice, csi_stanza(<<"inactive">>)).


csi_stanza(Name) ->
    #xmlel{name = Name,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:csi:0">>}]}.
