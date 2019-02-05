%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
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

-module(pubsub_backend_SUITE).
-compile(export_all).
-author('michael.uvarov@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, tree_handling}].

groups() ->
    [
     {tree_handling, [], tree_handling_tests()}
    ].

tree_handling_tests() ->
    [
     get_subnodes_case
    ].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = mnesia:create_schema([node()]),
    mnesia:start(),
    mod_pubsub_db_mnesia:start(),
    {ok, _} = application:ensure_all_started(stringprep),
    Config.

end_per_suite(Config) ->
    mod_pubsub_db_mnesia:stop(),
    mnesia:stop(),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Tree handling tests
%%--------------------------------------------------------------------

get_subnodes_case(_Config) ->
    mod_pubsub_db_mnesia:transaction(fun() ->
        get_subnodes_case_tr()
        end, #{}).

get_subnodes_case_tr() ->
    create_jojo_tree(),
    %% Get "root"-kinda nodes
    compare_lists(people_with_both_parents_unknown(), nodes_to_names(get_subnodes(<<>>))),
    %% Joseph has two children
    compare_lists([<<"Josuke Higashikata">>, <<"Holy Kujo">>],
                  nodes_to_names(get_subnodes(<<"Joseph Joestar">>))),
    %% Jotaro is a son of Holy
    compare_lists([<<"Jotaro Kujo">>],
                  nodes_to_names(get_subnodes(<<"Holy Kujo">>))),

    HolyDescendants = get_subnodes_tree(<<"Holy Kujo">>),
    HolyAncestors = get_parentnodes_tree(<<"Holy Kujo">>),

    %% level 0 is the last item
    {0, _} = lists:last(HolyDescendants),
    {0, _} = lists:last(HolyAncestors),

    %% Level 0 contains the node itself
    compare_lists([<<"Holy Kujo">>], nodes_to_names(proplists:get_value(0, HolyDescendants))),
    compare_lists([<<"Jotaro Kujo">>], nodes_to_names(proplists:get_value(1, HolyDescendants))),
    compare_lists([<<"Jolyne Cujoh">>], nodes_to_names(proplists:get_value(2, HolyDescendants))),
    compare_lists([], nodes_to_names(proplists:get_value(3, HolyDescendants))),
    undefined = proplists:get_value(4, HolyDescendants), %% No such item on depth 4

    compare_lists([<<"Holy Kujo">>], nodes_to_names(proplists:get_value(0, HolyAncestors))),
    compare_lists([<<"Joseph Joestar">>, <<"Suzi Q Joestar">>], nodes_to_names(proplists:get_value(1, HolyAncestors))),
    compare_lists([<<"George Joestar II">>, <<"Elizabeth Joestar">>], nodes_to_names(proplists:get_value(2, HolyAncestors))),
    compare_lists([<<"Erina Joestar">>, <<"Jonathan Joestar">>], nodes_to_names(proplists:get_value(3, HolyAncestors))),
    compare_lists([<<"George Joestar I">>, <<"Mary Joestar">>], nodes_to_names(proplists:get_value(4, HolyAncestors))),
    undefined = proplists:get_value(5, HolyAncestors),

    %% Dio not found
    [] = get_subnodes(<<"Dio">>),
    [ {1, []}, {0, [false]} ] = get_subnodes_tree(<<"Dio">>),
    [ {0, []} ] = get_parentnodes_tree(<<"Dio">>),
    ok.


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

make_node(Name, Parents) ->
    #pubsub_node{nodeid = {host(), Name}, parents = Parents}.

host() ->
    <<"localhost">>.

set_node(N) ->
    mod_pubsub_db_mnesia:set_node(N).

get_subnodes(Name) ->
    mod_pubsub_db_mnesia:get_subnodes(host(), Name).

get_subnodes_tree(Name) ->
    mod_pubsub_db_mnesia:get_subnodes_tree(host(), Name).

get_parentnodes_tree(Name) ->
    mod_pubsub_db_mnesia:get_parentnodes_tree(host(), Name).

%% https://jojo.fandom.com/wiki/Joestar_Family
create_jojo_tree() ->
    set_node(make_node(<<"George Joestar I">>, [])),
    set_node(make_node(<<"Mary Joestar">>, [])),

    set_node(make_node(<<"Jonathan Joestar">>,
                       [<<"George Joestar I">>, <<"Mary Joestar">>])),
    set_node(make_node(<<"Erina Joestar">>, [])),

    set_node(make_node(<<"George Joestar II">>,
                       [<<"Jonathan Joestar">>, <<"Erina Joestar">>])),
    set_node(make_node(<<"Elizabeth Joestar">>, [])),

    set_node(make_node(<<"Joseph Joestar">>,
                       [<<"George Joestar II">>, <<"Elizabeth Joestar">>])),
    set_node(make_node(<<"Tomoko Higashikata">>, [])),
    set_node(make_node(<<"Suzi Q Joestar">>, [])),

    set_node(make_node(<<"Josuke Higashikata">>,
                       [<<"Tomoko Higashikata">>, <<"Joseph Joestar">>])),
    set_node(make_node(<<"Holy Kujo">>,
                       [<<"Joseph Joestar">>, <<"Suzi Q Joestar">>])),

    set_node(make_node(<<"Sadao Kujo">>, [])),

    set_node(make_node(<<"Jotaro Kujo">>,
                       [<<"Holy Kujo">>, <<"Sadao Kujo">>])),

    set_node(make_node(<<"Jolyne Cujoh">>, [<<"Jotaro Kujo">>])),
    ok.

nodes_to_names(Nodes) ->
    [node_to_name(N) || N <- Nodes].

node_to_name(#pubsub_node{nodeid = {_, Name}}) ->
    Name.

people_with_both_parents_unknown() ->
    [<<"Elizabeth Joestar">>,
     <<"Erina Joestar">>,
     <<"George Joestar I">>,
     <<"Mary Joestar">>,
     <<"Sadao Kujo">>,
     <<"Suzi Q Joestar">>,
     <<"Tomoko Higashikata">>].

%% Compare lists ignoring order 
compare_lists(L1, L2) ->
    SL1 = lists:sort(L1),
    SL2 = lists:sort(L2),
    case SL1 of
        SL2 ->
            ok;
        _ ->
            ct:fail({compare_lists, {expected, SL1}, {provided, SL2}})
    end.
