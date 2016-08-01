%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
%%
%% Author: Joseph Yiasemides <joseph.yiasemides@erlang-solutions.com>
%% Description: Test HTTP Administration API for MUC Light
%%==============================================================================

-module(muc_light_http_api_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->

    [{group, positive}].

groups() ->

    [{positive, [shuffle], success_response()}].

success_response() ->

    [create_room].


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    dynamic_modules:start(<<"localhost">>, mod_muc_light,
        [{host, binary_to_list(muc_light_domain())},
         {rooms_in_rosters, true}]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    dynamic_modules:stop(<<"localhost">>, mod_muc_light),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

create_room(Config) ->    
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Domain = <<"localhost">>,
        Path = <<"/muc-lights", $/, Domain/binary>>,
        Name = <<"wonderland">>,
        Body = #{name => Name,
                 creator => escalus_utils:get_jid(Alice)
                },
        {{<<"201">>, _}, <<"">>} = rest_helper:post(Path, Body),
        [Item] = get_disco_rooms(Alice),
        MUCLightDomain = muc_light_domain(),
        is_room(<<Name/binary, $@, MUCLightDomain/binary>>, Item)
    end).


%%--------------------------------------------------------------------
%% Ancillary (borrowed and adapted from the MUC and MUC Light suites)
%%--------------------------------------------------------------------

get_disco_rooms(User) ->
    DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), muc_light_domain()),
    escalus:send(User, DiscoStanza),
    Stanza =  escalus:wait_for_stanza(User),
    XNamespaces = exml_query:paths(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    true = lists:member(?NS_DISCO_ITEMS, XNamespaces),
    escalus:assert(is_stanza_from, [muc_light_domain()], Stanza),
    exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]).

is_room(JID, Item) ->
    JID == exml_query:attr(Item, <<"jid">>).


%%--------------------------------------------------------------------
%% Constants
%%--------------------------------------------------------------------

muc_light_domain() ->
    <<"muclight.localhost">>.
