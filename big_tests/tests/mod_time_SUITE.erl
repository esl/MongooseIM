%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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

-module(mod_time_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_time}].

groups() ->
    [{mod_time, [], [ask_for_time, time_service_discovery]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    dynamic_modules:start(<<"localhost">>, mod_time, []),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    dynamic_modules:stop(<<"localhost">>, mod_time),
    escalus:end_per_suite(Config).

init_per_group(mod_time, Config) ->
    escalus:create_users(Config, escalus:get_users([alice])).

end_per_group(mod_time, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice])).


init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Time request test
%%--------------------------------------------------------------------

ask_for_time(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Server = escalus_users:get_server(Config, alice),
        ID = escalus_stanza:id(),
        TimeStanza = time_request_stanza(Server, ID),
        escalus_client:send(Alice, TimeStanza),
        Reply = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_iq_result, Reply),
        escalus:assert(fun check_ns/1, Reply),
        {Tzo, Utc} = time_from_stanza(Reply),
        ?assertEqual(true, tzo_regex(Tzo)),
        ?assertEqual(true, utc_regex(Utc))
    end).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

time_service_discovery(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Client) ->
            ServJID = escalus_client:server(Client),
            Res = escalus:send_and_wait(Client,
                                        escalus_stanza:disco_info(ServJID)),
            escalus:assert(is_iq_result, Res),
            escalus:assert(has_feature, [?NS_TIME], Res)
        end).
%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

time_request_stanza(Server, ID) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"get">>},
                    {<<"id">>, ID}, {<<"to">>, Server}],
           children = [#xmlel{name = <<"time">>,
                              attrs = [{<<"xmlns">>, ?NS_TIME}]}]}.

check_ns(#xmlel{name = <<"iq">>, attrs = _, children = [Child]}) ->
    case Child of
        #xmlel{name = <<"time">>, attrs = [{<<"xmlns">>, ?NS_TIME}], children = _} -> true;
        _ -> false
    end;

check_ns(_) ->
    false.

time_from_stanza(#xmlel{name = <<"iq">>, attrs = _, children = [Child]}) ->
    case Child of
        #xmlel{name = <<"time">>, attrs = [{<<"xmlns">>, ?NS_TIME}], children = Times} ->
            case Times of
                [#xmlel{name = <<"tzo">>, attrs = _, children = [#xmlcdata{content = Tzo}]},
                 #xmlel{name = <<"utc">>, attrs = _, children = [#xmlcdata{content = Utc}]}] ->
                    {Tzo, Utc};
                _ -> no_timezone
            end;
        _ -> wrong_stanza
    end.

%% check XEP-0082: XMPP Date and Time Profiles
tzo_regex(Tzo) ->
    String = binary_to_list(Tzo),
    {match, [{0, 6}]} = re:run(String, "^[+|-][0-9]{2}:[0-9]{2}"),
    true.

utc_regex(Utc) ->
    String = binary_to_list(Utc),
    {match, _} = re:run(String, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{3})?Z"),
    true.
