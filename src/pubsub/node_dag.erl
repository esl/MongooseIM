%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%%
%%% @author Brian Cully <bjc@kublai.com>
%%% @end
%%% ====================================================================

-module(node_dag).
-behaviour(gen_pubsub_node).
-author('bjc@kublai.com').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([based_on/0, init/3, terminate/2, options/0, features/0,
         create_node_permission/6, publish_item/9, node_to_path/1,
         path_to_node/1]).

based_on() -> node_hometree.

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost).

options() ->
    [{node_type, leaf} | node_hometree:options()].

features() ->
    [<<"multi-collection">> | node_hometree:features()].

create_node_permission(_Host, _ServerHost, _Node, _ParentNode, _Owner, _Access) ->
    {result, true}.

publish_item(ServerHost, Nidx, Publisher, Model, MaxItems, ItemId, ItemPublisher, Payload,
             PublishOptions) ->
    case nodetree_dag:get_node(Nidx) of
        #pubsub_node{options = Options} ->
            case find_opt(node_type, Options) of
                collection ->
                    {error,
                        ?ERR_EXTENDED((mongoose_xmpp_errors:not_allowed()), <<"publish">>)};
                _ ->
                    node_flat:publish_item(ServerHost, Nidx, Publisher, Model, MaxItems,
                                           ItemId, ItemPublisher, Payload, PublishOptions)
            end;
        Err -> Err
    end.

find_opt(_, []) -> false;
find_opt(Option, [{Option, Value} | _]) -> Value;
find_opt(Option, [_ | T]) -> find_opt(Option, T).

node_to_path(Node) ->
    node_hometree:node_to_path(Node).

path_to_node(Path) ->
    node_hometree:path_to_node(Path).
