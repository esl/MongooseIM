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
-module(cluster_SUITE).
%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Tests
-export([distributed_login/1,
         submit_message/1]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml_stream.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, cluster}].

groups() ->
    [{cluster, [], [distributed_login, submit_message]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    Users = ct:get_config(escalus_cluster_users),
    Config1 = escalus:init_per_suite(Config),
    escalus:create_users(Config1, Users).

end_per_group(_, Config) ->
    %% Delete users from the master node.
    Users = filter_node1(ct:get_config(escalus_cluster_users)),
    escalus:delete_users(Config, Users).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Adhoc tests
%%--------------------------------------------------------------------

distributed_login(Config) ->
    Users = [{alice_node1, res1}, {alice_node2, res2}],
    F = fun(Alice1, Alice2) ->
        P1 = escalus:wait_for_stanza(Alice1),
        P2 = escalus:wait_for_stanza(Alice2),
        P3 = escalus:wait_for_stanza(Alice1),
        P4 = escalus:wait_for_stanza(Alice2),
        escalus:send(Alice1, escalus_stanza:chat_to(Alice2, <<"What?">>)),
        M2 = escalus:wait_for_stanza(Alice2),
        ct:pal("M2 ~p", [M2]),
        ok
        end,
    raw_story(Config, Users, F).

%% Querying the archive for messages
submit_message(Config) ->
    Users = [{alice_node1, res1}, {alice_node2, res2},
             {bob_node1, res1}, {bob_node2, res2}],
    F = fun(Alice1, Alice2, Bob1, Bob2) ->
        P1 = escalus:wait_for_stanza(Alice1),
        P2 = escalus:wait_for_stanza(Alice2),
        P3 = escalus:wait_for_stanza(Alice1),
        P4 = escalus:wait_for_stanza(Alice2),
        P5 = escalus:wait_for_stanza(Bob1),
        P6 = escalus:wait_for_stanza(Bob2),
        P7 = escalus:wait_for_stanza(Bob1),
        P8 = escalus:wait_for_stanza(Bob2),
        Msg = <<"OH, HAI!">>,
        %% Bob sends "OH, HAI!" to Alice's bare JID.
        escalus:send(Bob1, escalus_stanza:chat_to_short_jid(Alice1, Msg)),
        M1 = escalus:wait_for_stanza(Alice1),
        M2 = escalus:wait_for_stanza(Alice2),
        ct:pal("M1 ~p.", [M1]),
        ct:pal("M2 ~p.", [M2]),

        escalus:send(Bob2, escalus_stanza:chat_to_short_jid(Alice2, Msg)),
        M3 = escalus:wait_for_stanza(Alice1),
        M4 = escalus:wait_for_stanza(Alice2),
        ct:pal("M3 ~p.", [M3]),
        ct:pal("M4 ~p.", [M4]),

        %% Pass the message beetween nodes.
        escalus:send(Bob2, escalus_stanza:chat_to_short_jid(Alice1, Msg)),
        M5 = escalus:wait_for_stanza(Alice1),
        M6 = escalus:wait_for_stanza(Alice2),
        ct:pal("M5 ~p.", [M5]),
        ct:pal("M6 ~p.", [M6]),
        ok
        end,
    raw_story(Config, Users, F).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% @doc Like `escalus_story:story/3', but with presence stanzas.
raw_story(Config, Users, Story) ->
    try
        Clients = [start_client(Config, Name, prepare_resource(Res))
                   || {Name, Res} <- Users],
        apply(Story, Clients)
    after
        escalus_cleaner:clean(Config)
    end.

prepare_resource(Res) when is_binary(Res) -> Res;
prepare_resource(Res) when is_atom(Res)   -> atom_to_binary(Res, utf8).

start_client(Config, Name, Res) ->
    UserSpec = escalus_users:get_userspec(Config, Name),
    case escalus_client:start(Config, UserSpec, Res) of
    {ok, Client} ->
            escalus_story:send_initial_presence(Client),
            Client;
    {error, Reason} ->
            ct:pal("Failed to connect with name=~p, resource=~p because ~p.",
                   [Name, Res, Reason]),
            error(Reason)
    end.

filter_node1(UserDescs) ->
    [{Name, Opts} || {Name, Opts} <- UserDescs,
     <<"ejabberd_node1">> =:= proplists:get_value(host, Opts)].
