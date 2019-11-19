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
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @end
%%% ====================================================================

-module(node_pep).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-include("mongoose.hrl").
-include("pubsub.hrl").
-include("jlib.hrl").

%%% @doc The module <strong>{@module}</strong> is the pep PubSub plugin.
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-export([based_on/0, init/3, terminate/2, options/0, features/0,
         create_node_permission/6, delete_node/1,
         unsubscribe_node/4, node_to_path/1,
         get_entity_affiliations/2, get_entity_affiliations/3,
         get_entity_subscriptions/2, get_entity_subscriptions/4,
         should_delete_when_owner_removed/0
         ]).

based_on() ->  node_flat.

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    complain_if_modcaps_disabled(ServerHost),
    ok.

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost),
    ok.

options() ->
    [{deliver_payloads, true},
        {notify_config, false},
        {notify_delete, false},
        {notify_retract, false},
        {purge_offline, false},
        {persist_items, true},
        {max_items, 1},
        {subscribe, true},
        {access_model, presence},
        {roster_groups_allowed, []},
        {publish_model, publishers},
        {notification_type, headline},
        {max_payload_size, ?MAX_PAYLOAD_SIZE},
        {send_last_published_item, on_sub_and_presence},
        {deliver_notifications, true},
        {presence_based_delivery, true}].

features() ->
    [<<"create-nodes">>,
        <<"auto-create">>,
        <<"auto-subscribe">>,
        <<"delete-nodes">>,
        <<"delete-items">>,
        <<"filtered-notifications">>,
        <<"modify-affiliations">>,
        <<"outcast-affiliation">>,
        <<"persistent-items">>,
        <<"publish">>,
        <<"purge-nodes">>,
        <<"retract-items">>,
        <<"retrieve-affiliations">>,
        <<"retrieve-items">>,
        <<"retrieve-subscriptions">>,
        <<"subscribe">>].

create_node_permission(Host, _ServerHost, _Node, _ParentNode,
                       #jid{ luser = <<>>, lserver = Host, lresource = <<>> }, _Access) ->
    {result, true}; % pubsub service always allowed
create_node_permission(Host, ServerHost, _Node, _ParentNode,
                       #jid{ luser = User, lserver = Server } = Owner, Access) ->
    case acl:match_rule(ServerHost, Access, Owner) of
        allow ->
            case Host of
                {User, Server, _} -> {result, true};
                _ -> {result, false}
            end;
        _ ->
            {result, false}
    end.

delete_node(Nodes) ->
    {result, {_, _, Result}} = node_flat:delete_node(Nodes),
    {result, {[], Result}}.

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    case node_flat:unsubscribe_node(Nidx, Sender, Subscriber, SubId) of
        {error, Error} -> {error, Error};
        {result, _} -> {result, []}
    end.

get_entity_affiliations(Host, #jid{ lserver = D } = Owner) ->
    get_entity_affiliations(Host, D, jid:to_lower(Owner));
get_entity_affiliations(Host, {_, D, _} = Owner) ->
    get_entity_affiliations(Host, D, Owner).

get_entity_affiliations(Host, D, Owner) ->
    {ok, States} = mod_pubsub_db_backend:get_states_by_bare(Owner),
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun (#pubsub_state{stateid = {_, N}, affiliation = A}, Acc) ->
                    case gen_pubsub_nodetree:get_node(NodeTree, N) of
                        #pubsub_node{nodeid = {{_, D, _}, _}} = Node -> [{Node, A} | Acc];
                        _ -> Acc
                    end
            end,
            [], States),
    {result, Reply}.

get_entity_subscriptions(Host, #jid{ lserver = D, lresource = R } = Owner) ->
    get_entity_subscriptions(Host, D, R, Owner);
get_entity_subscriptions(Host, {_, D, R} = Owner) ->
    get_entity_subscriptions(Host, D, R, Owner).

get_entity_subscriptions(Host, D, R, Owner) ->
    LOwner = jid:to_lower(Owner),
    States = case R of
                 <<>> ->
                     {ok, States0} = mod_pubsub_db_backend:get_states_by_lus(LOwner),
                     States0;
                 _ ->
                     {ok, States0} = mod_pubsub_db_backend:get_states_by_bare_and_full(LOwner),
                     States0
             end,
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun (#pubsub_state{stateid = {J, N}, subscriptions = Ss}, Acc) ->
                    case gen_pubsub_nodetree:get_node(NodeTree, N) of
                        #pubsub_node{nodeid = {{_, D, _}, _}} = Node ->
                            accumulate_entity_subscriptions(J, Node, Ss, Acc);
                        _ ->
                            Acc
                    end
            end,
            [], States),
    {result, Reply}.

accumulate_entity_subscriptions(J, Node, Ss, Acc) ->
    lists:foldl(fun({subscribed, SubId}, Acc2) ->
                        [{Node, subscribed, SubId, J} | Acc2];
                   ({pending, _SubId}, Acc2) ->
                        [{Node, pending, J} | Acc2];
                   (S, Acc2) ->
                        [{Node, S, J} | Acc2]
                end, Acc, Ss).


node_to_path(Node) ->
    node_flat:node_to_path(Node).

should_delete_when_owner_removed() -> true.

%%%
%%% Internal
%%%

%% @doc Check mod_caps is enabled, otherwise show warning.
%% The PEP plugin for mod_pubsub requires mod_caps to be enabled in the host.
%% Check that the mod_caps module is enabled in that Jabber Host
%% If not, show a warning message in the ejabberd log file.
complain_if_modcaps_disabled(ServerHost) ->
    ?WARNING_MSG_IF(
       not gen_mod:is_loaded(ServerHost, mod_caps),
       "The PEP plugin is enabled in mod_pubsub "
       "of host ~p. This plugin requires mod_caps "
       "to be enabled, but it isn't.",
       [ServerHost]).
