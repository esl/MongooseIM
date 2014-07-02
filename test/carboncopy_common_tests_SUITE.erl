-module(carboncopy_common_tests_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").


all() ->
    [{group, mod_message_carbons_common_tests}].

all_tests() ->
    [private_message_test].

groups() ->
    [{mod_message_carbons_common_tests, [sequence], all_tests()}].

private_message_test(_) ->
	Msg = #xmlel{name = <<"message">>,
                            attrs = [{<<"type">>,<<"chat">>}],
                            children = [#xmlel{name = <<"private">>, attrs = [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]}
                            ]},
    ignore == classify_packet(Msg).

