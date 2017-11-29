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
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the default PubSub plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node
%%% types.</p>
%%% <p>PubSub plugin nodes are using the {@link gen_node} behaviour.</p>

-module(node_flat).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([init/3, terminate/2, options/0, features/0,
         create_node_permission/6, create_node/2, delete_node/1,
         purge_node/2, subscribe_node/8, unsubscribe_node/4,
         publish_item/9, delete_item/4, remove_extra_items/3,
         get_entity_affiliations/2, get_node_affiliations/1,
         get_affiliation/2, set_affiliation/3,
         get_entity_subscriptions/2, get_node_subscriptions/1,
         get_subscriptions/2, set_subscriptions/4,
         get_pending_nodes/2, get_states/1, get_state/2,
         set_state/1, get_items/7, get_items/3, get_item/7,
         get_item/2, set_item/1, get_item_name/3, node_to_path/1,
         path_to_node/1, can_fetch_item/2, is_subscribed/1]).

init(_Host, _ServerHost, _Opts) ->
    pubsub_subscription:init(),
    mnesia:create_table(pubsub_state,
        [{disc_copies, [node()]},
            {type, ordered_set},
            {attributes, record_info(fields, pubsub_state)}]),
    mnesia:create_table(pubsub_item,
        [{disc_only_copies, [node()]},
            {attributes, record_info(fields, pubsub_item)}]),
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{deliver_payloads, true},
        {notify_config, false},
        {notify_delete, false},
        {notify_retract, false},
        {purge_offline, false},
        {persist_items, true},
        {max_items, ?MAXITEMS},
        {subscribe, true},
        {access_model, open},
        {roster_groups_allowed, []},
        {publish_model, publishers},
        {notification_type, headline},
        {max_payload_size, ?MAX_PAYLOAD_SIZE},
        {send_last_published_item, never},
        {deliver_notifications, true},
        {presence_based_delivery, false}].

features() ->
    [<<"create-nodes">>,
        <<"auto-create">>,
        <<"access-authorize">>,
        <<"delete-nodes">>,
        <<"delete-items">>,
        <<"get-pending">>,
        <<"instant-nodes">>,
        <<"manage-subscriptions">>,
        <<"modify-affiliations">>,
        <<"outcast-affiliation">>,
        <<"persistent-items">>,
        <<"publish">>,
        <<"publish-only-affiliation">>,
        <<"purge-nodes">>,
        <<"retract-items">>,
        <<"retrieve-affiliations">>,
        <<"retrieve-items">>,
        <<"retrieve-subscriptions">>,
        <<"subscribe">>,
        <<"subscription-notifications">>].
%%<<"subscription-options">>

%% @doc Checks if the current user has the permission to create the requested node
%% <p>In flat node, any unused node name is allowed. The access parameter is also
%% checked. This parameter depends on the value of the
%% <tt>access_createnode</tt> ACL value in ejabberd config file.</p>
create_node_permission(Host, ServerHost, _Node, _ParentNode, Owner, Access) ->
    Allowed = case jid:to_lower(Owner) of
        {<<"">>, Host, <<"">>} ->
            true; % pubsub service always allowed
        _ ->
            acl:match_rule(ServerHost, Access, Owner) =:= allow
    end,
    {result, Allowed}.

create_node(Nidx, Owner) ->
    OwnerKey = jid:to_lower(jid:to_bare(Owner)),
    set_state(#pubsub_state{stateid = {OwnerKey, Nidx},
            affiliation = owner}),
    {result, {default, broadcast}}.

delete_node(Nodes) ->
    Tr = fun (#pubsub_state{stateid = {J, _}, subscriptions = Ss}) ->
            lists:map(fun (S) -> {J, S} end, Ss)
    end,
    Reply = lists:map(fun (#pubsub_node{id = Nidx} = PubsubNode) ->
                    {result, States} = get_states(Nidx),
                    lists:foreach(fun (#pubsub_state{stateid = {LJID, _}, items = Items}) ->
                                del_items(Nidx, Items),
                                del_state(Nidx, LJID)
                        end, States),
                    {PubsubNode, lists:flatmap(Tr, States)}
            end, Nodes),
    {result, {default, broadcast, Reply}}.

%% @doc <p>Accepts or rejects subcription requests on a PubSub node.</p>
%% <p>The mechanism works as follow:
%% <ul>
%% <li>The main PubSub module prepares the subscription and passes the
%% result of the preparation as a record.</li>
%% <li>This function gets the prepared record and several other parameters and
%% can decide to:<ul>
%%  <li>reject the subscription;</li>
%%  <li>allow it as is, letting the main module perform the database
%%  persistance;</li>
%%  <li>allow it, modifying the record. The main module will store the
%%  modified record;</li>
%%  <li>allow it, but perform the needed persistance operations.</li></ul>
%% </li></ul></p>
%% <p>The selected behaviour depends on the return parameter:
%%  <ul>
%%   <li><tt>{error, Reason}</tt>: an IQ error result will be returned. No
%%   subscription will actually be performed.</li>
%%   <li><tt>true</tt>: Subscribe operation is allowed, based on the
%%   unmodified record passed in parameter <tt>SubscribeResult</tt>. If this
%%   parameter contains an error, no subscription will be performed.</li>
%%   <li><tt>{true, PubsubState}</tt>: Subscribe operation is allowed, but
%%   the {@link mod_pubsub:pubsubState()} record returned replaces the value
%%   passed in parameter <tt>SubscribeResult</tt>.</li>
%%   <li><tt>{true, done}</tt>: Subscribe operation is allowed, but the
%%   {@link mod_pubsub:pubsubState()} will be considered as already stored and
%%   no further persistance operation will be performed. This case is used,
%%   when the plugin module is doing the persistance by itself or when it want
%%   to completly disable persistance.</li></ul>
%% </p>
%% <p>In the default plugin module, the record is unchanged.</p>
subscribe_node(Nidx, Sender, Subscriber, AccessModel,
            SendLast, PresenceSubscription, RosterGroup, _Options) ->
    SubKey = jid:to_lower(Subscriber),
    GenKey = jid:to_bare(SubKey),
    SenderMatchesSubscriber = jid:to_lower(jid:to_bare(Sender)) == GenKey,
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
        GenKey -> GenState;
        _ -> get_state(Nidx, SubKey)
    end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = SubState#pubsub_state.subscriptions,
    Whitelisted = lists:member(Affiliation, [member, publisher, owner]),
    PendingSubscription = lists:any(fun
                ({pending, _}) -> true;
                (_) -> false
            end,
            Subscriptions),
    case authorize_subscription(SenderMatchesSubscriber, Affiliation, PendingSubscription,
                               AccessModel, PresenceSubscription, RosterGroup, Whitelisted) of
        ok ->
            {NewSub, SubId} = case Subscriptions of
                [{subscribed, Id}|_] ->
                    {subscribed, Id};
                [] ->
                    Id = pubsub_subscription:make_subid(),
                    Sub = access_model_to_subscription(AccessModel),
                    set_state(SubState#pubsub_state{subscriptions = [{Sub, Id} | Subscriptions]}),
                    {Sub, Id}
            end,
            case {NewSub, SendLast} of
                {subscribed, never} ->
                    {result, {default, subscribed, SubId}};
                {subscribed, _} ->
                    {result, {default, subscribed, SubId, send_last}};
                {_, _} ->
                    {result, {default, pending, SubId}}
            end;
        {error, _} = Err ->
            Err
    end.

-spec access_model_to_subscription(accessModel()) -> pending | subscribed.
access_model_to_subscription(authorize) -> pending;
access_model_to_subscription(_) -> subscribed.

-spec authorize_subscription(SenderMatchesSubscriber :: boolean(),
                            Affiliation :: affiliation(),
                            PendingSubscription :: boolean(),
                            AccessModel :: accessModel(),
                            PresenceSubscription :: boolean(),
                            RosterGroup :: boolean(),
                            Whitelisted :: boolean()) -> ok | {error, exml:element()}.
authorize_subscription(false, _Affiliation, _PendingSubscription, _AccessModel,
                       _PresenceSubscription, _RosterGroup, _Whitelisted) ->
    {error, ?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"invalid-jid">>)};
authorize_subscription(_SenderMatchesSubscriber, Affiliation, _PendingSubscription, _AccessModel,
                       _PresenceSubscription, _RosterGroup, _Whitelisted)
  when (Affiliation == outcast) or (Affiliation == publish_only) ->
    {error, ?ERR_FORBIDDEN};
authorize_subscription(_SenderMatchesSubscriber, _Affiliation, true, _AccessModel,
                       _PresenceSubscription, _RosterGroup, _Whitelisted) ->
    {error, ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"pending-subscription">>)};
authorize_subscription(_SenderMatchesSubscriber, Affiliation, _PendingSubscription, presence,
                       false, _RosterGroup, _Whitelisted) when Affiliation /= owner ->
    {error, ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"presence-subscription-required">>)};
authorize_subscription(_SenderMatchesSubscriber, Affiliation, _PendingSubscription, roster,
                       _PresenceSubscription, false, _Whitelisted) when Affiliation /= owner ->
    {error, ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"not-in-roster-group">>)};
authorize_subscription(_SenderMatchesSubscriber, Affiliation, _PendingSubscription, whitelist,
                       _PresenceSubscription, _RosterGroup, false) when Affiliation /= owner ->
    {error, ?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
authorize_subscription(_SenderMatchesSubscriber, _Affiliation, _PendingSubscription, _AccessModel,
                       _PresenceSubscription, _RosterGroup, _Whitelisted) ->
    ok.

%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    SubKey = jid:to_lower(Subscriber),
    GenKey = jid:to_bare(SubKey),
    SenderMatchesSubscriber = jid:to_lower(jid:to_bare(Sender)) == GenKey,
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
                   GenKey -> GenState;
                   _ -> get_state(Nidx, SubKey)
               end,
    Subscriptions = lists:filter(fun
                                     ({_Sub, _SubId}) -> true;
                                     (_SubId) -> false
                                 end,
                                 SubState#pubsub_state.subscriptions),
    SubIdExists = case SubId of
                      <<>> -> false;
                      Binary when is_binary(Binary) -> true;
                      _ -> false
                  end,
    case authenticate_unsubscribe(SenderMatchesSubscriber, Subscriptions, SubIdExists, SubId) of
        sub_id_exists ->
            Sub = first_in_list(fun ({_, S}) when S == SubId -> true;
                                    (_) -> false end,
                                SubState#pubsub_state.subscriptions),
            case Sub of
                {value, S} ->
                    delete_subscriptions(SubKey, Nidx, [S], SubState),
                    {result, default};
                false ->
                    {error,
                     ?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL), <<"not-subscribed">>)}
            end;
        remove_all_subs ->
            delete_subscriptions(SubKey, Nidx, Subscriptions, SubState),
            {result, default};
        remove_only_sub ->
            delete_subscriptions(SubKey, Nidx, Subscriptions, SubState),
            {result, default}
    end.

authenticate_unsubscribe(false, _Subscriptions, _SubIdExists, _SubId) ->
    {error, ?ERR_FORBIDDEN};
authenticate_unsubscribe(_SenderMatchesSubscriber, [], _SubIdExists, _SubId) ->
    %% Requesting entity is not a subscriber
    {error, ?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL), <<"not-subscribed">>)};
authenticate_unsubscribe(_SenderMatchesSubscriber, _Subscriptions, true, _SubId) ->
    %% Subid supplied, so use that.
    sub_id_exists;
authenticate_unsubscribe(_SenderMatchesSubscriber, _Subscriptions, _SubIdExists, all) ->
    %% Asking to remove all subscriptions to the given node
    remove_all_subs;
authenticate_unsubscribe(_SenderMatchesSubscriber, [_], _SubIdExists, _SubId) ->
    %% No subid supplied, but there's only one matching subscription
    remove_only_sub;
authenticate_unsubscribe(_SenderMatchesSubscriber, _Subscriptions, _SubIdExists, _SubId) ->
    {error, ?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"subid-required">>)}.

delete_subscriptions(SubKey, Nidx, Subscriptions, SubState) ->
    NewSubs = lists:foldl(fun ({Subscription, SubId}, Acc) ->
                    %%pubsub_subscription:delete_subscription(SubKey, Nidx, SubId),
                    Acc -- [{Subscription, SubId}]
            end, SubState#pubsub_state.subscriptions, Subscriptions),
    case {SubState#pubsub_state.affiliation, NewSubs} of
        {none, []} -> del_state(Nidx, SubKey);
        _          -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

%% @doc <p>Publishes the item passed as parameter.</p>
%% <p>The mechanism works as follow:
%% <ul>
%% <li>The main PubSub module prepares the item to publish and passes the
%% result of the preparation as a {@link mod_pubsub:pubsubItem()} record.</li>
%% <li>This function gets the prepared record and several other parameters and can decide to:<ul>
%%  <li>reject the publication;</li>
%%  <li>allow the publication as is, letting the main module perform the database persistance;</li>
%%  <li>allow the publication, modifying the record.
%%      The main module will store the modified record;</li>
%%  <li>allow it, but perform the needed persistance operations.</li></ul>
%% </li></ul></p>
%% <p>The selected behaviour depends on the return parameter:
%%  <ul>
%%   <li><tt>{error, Reason}</tt>: an iq error result will be return. No
%%   publication is actually performed.</li>
%%   <li><tt>true</tt>: Publication operation is allowed, based on the
%%   unmodified record passed in parameter <tt>Item</tt>. If the <tt>Item</tt>
%%   parameter contains an error, no subscription will actually be
%%   performed.</li>
%%   <li><tt>{true, Item}</tt>: Publication operation is allowed, but the
%%   {@link mod_pubsub:pubsubItem()} record returned replaces the value passed
%%   in parameter <tt>Item</tt>. The persistance will be performed by the main
%%   module.</li>
%%   <li><tt>{true, done}</tt>: Publication operation is allowed, but the
%%   {@link mod_pubsub:pubsubItem()} will be considered as already stored and
%%   no further persistance operation will be performed. This case is used,
%%   when the plugin module is doing the persistance by itself or when it want
%%   to completly disable persistance.</li></ul>
%% </p>
%% <p>In the default plugin module, the record is unchanged.</p>
publish_item(_ServerHost, Nidx, Publisher, PublishModel, MaxItems, ItemId, ItemPublisher,
             Payload, _PublishOptions) ->
    SubKey = jid:to_lower(Publisher),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
        GenKey -> GenState;
        _ -> get_state(Nidx, SubKey)
    end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscribed = case PublishModel of
        subscribers -> is_subscribed(GenState#pubsub_state.subscriptions) orelse
                       is_subscribed(SubState#pubsub_state.subscriptions);
        _ -> undefined
    end,
    Allowed = (PublishModel == open) or
              (PublishModel == publishers) and
              ( (Affiliation == owner) or
                (Affiliation == publisher) or
                (Affiliation == publish_only) ) or
              (Subscribed == true),
    case Allowed of
        false  ->
            {error, ?ERR_FORBIDDEN};
        true ->
            case MaxItems > 0 of
               true ->
                   Now = timestamp(),
                   Item = make_pubsub_item(Nidx, ItemId, Now, SubKey, GenKey,
                                           Payload, Publisher, ItemPublisher),
                   Items = [ItemId | GenState#pubsub_state.items -- [ItemId]],
                   {result, {NI, OI}} = remove_extra_items(Nidx, MaxItems, Items),
                   set_item(Item),
                   set_state(GenState#pubsub_state{items = NI}),
                   {result, {default, broadcast, OI}};
               false ->
                   {result, {default, broadcast, []}}
            end
    end.

make_pubsub_item(Nidx, ItemId, Now, SubKey, GenKey, Payload, Publisher, ItemPublisher) ->
    PubId = {Now, SubKey},
    case get_item(Nidx, ItemId) of
        {result, OldItem} ->
            OldItem#pubsub_item{modification = PubId,
                                payload = Payload};
        _ ->
            Publisher0 = case ItemPublisher of
                             true -> Publisher;
                             false -> undefined
                         end,
            #pubsub_item{itemid = {ItemId, Nidx},
                         creation = {Now, GenKey},
                         modification = PubId,
                         publisher = Publisher0,
                         payload = Payload}
    end.

%% @doc <p>This function is used to remove extra items, most notably when the
%% maximum number of items has been reached.</p>
%% <p>This function is used internally by the core PubSub module, as no
%% permission check is performed.</p>
%% <p>In the default plugin module, the oldest items are removed, but other
%% rules can be used.</p>
%% <p>If another PubSub plugin wants to delegate the item removal (and if the
%% plugin is using the default pubsub storage), it can implements this function like this:
%% ```remove_extra_items(Nidx, MaxItems, ItemIds) ->
%%           node_default:remove_extra_items(Nidx, MaxItems, ItemIds).'''</p>
remove_extra_items(_Nidx, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(Nidx, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    del_items(Nidx, OldItems),
    {result, {NewItems, OldItems}}.

%% @doc <p>Triggers item deletion.</p>
%% <p>Default plugin: The user performing the deletion must be the node owner
%% or a publisher, or PublishModel being open.</p>
delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    SubKey = jid:to_lower(Publisher),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    #pubsub_state{affiliation = Affiliation, items = Items} = GenState,
    Allowed = Affiliation == publisher orelse
        Affiliation == owner orelse
        PublishModel == open orelse
        case get_item(Nidx, ItemId) of
        {result, #pubsub_item{creation = {_, GenKey}}} -> true;
        _ -> false
    end,
    case Allowed of
        false ->
            {error, ?ERR_FORBIDDEN};
        true ->
            case lists:member(ItemId, Items) of
                true ->
                    del_item(Nidx, ItemId),
                    set_state(GenState#pubsub_state{items = lists:delete(ItemId, Items)}),
                    {result, {default, broadcast}};
                false ->
                    delete_foreign_item(Nidx, ItemId, Affiliation)
            end
    end.

%% Delete an item that does not belong to the user
delete_foreign_item(Nidx, ItemId, owner) ->
    {result, States} = get_states(Nidx),
    lists:foldl(fun
                    (#pubsub_state{items = PI} = S, Res) ->
                        case lists:member(ItemId, PI) of
                            true ->
                                Nitems = lists:delete(ItemId, PI),
                                del_item(Nidx, ItemId),
                                set_state(S#pubsub_state{items = Nitems}),
                                {result, {default, broadcast}};
                            false ->
                                Res
                        end;
                    (_, Res) ->
                        Res
                end,
                {error, ?ERR_ITEM_NOT_FOUND}, States);
delete_foreign_item(_Nidx, _ItemId, _Affiliation) ->
    {error, ?ERR_ITEM_NOT_FOUND}.

purge_node(Nidx, Owner) ->
    SubKey = jid:to_lower(Owner),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    case GenState of
        #pubsub_state{affiliation = owner} ->
            {result, States} = get_states(Nidx),
            lists:foreach(fun
                    (#pubsub_state{items = []}) ->
                        ok;
                    (#pubsub_state{items = Items} = S) ->
                        del_items(Nidx, Items),
                        set_state(S#pubsub_state{items = []})
                end,
                States),
            {result, {default, broadcast}};
        _ ->
            {error, ?ERR_FORBIDDEN}
    end.

%% @doc <p>Return the current affiliations for the given user</p>
%% <p>The default module reads affiliations in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_affiliations(Host, Owner) ->
    SubKey = jid:to_lower(Owner),
    GenKey = jid:to_bare(SubKey),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'}, _ = '_'}),
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun (#pubsub_state{stateid = {_, N}, affiliation = A}, Acc) ->
                                case gen_pubsub_nodetree:get_node(NodeTree, N) of
                                    #pubsub_node{nodeid = {Host, _}} = Node -> [{Node, A} | Acc];
                                    _ -> Acc
                                end
                        end,
                        [], States),
    {result, Reply}.

get_node_affiliations(Nidx) ->
    {result, States} = get_states(Nidx),
    Tr = fun (#pubsub_state{stateid = {J, _}, affiliation = A}) -> {J, A} end,
    {result, lists:map(Tr, States)}.

get_affiliation(Nidx, Owner) ->
    SubKey = jid:to_lower(Owner),
    GenKey = jid:to_bare(SubKey),
    #pubsub_state{affiliation = Affiliation} = get_state(Nidx, GenKey),
    {result, Affiliation}.

set_affiliation(Nidx, Owner, Affiliation) ->
    SubKey = jid:to_lower(Owner),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    case {Affiliation, GenState#pubsub_state.subscriptions} of
        {none, []} -> del_state(Nidx, GenKey);
        _ -> set_state(GenState#pubsub_state{affiliation = Affiliation})
    end.

%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_subscriptions(Host, Owner) ->
    {U, D, _} = SubKey = jid:to_lower(Owner),
    GenKey = jid:to_bare(SubKey),
    States = case SubKey of
        GenKey ->
            mnesia:match_object(#pubsub_state{stateid = {{U, D, '_'}, '_'}, _ = '_'});
        _ ->
            mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'}, _ = '_'})
            ++
            mnesia:match_object(#pubsub_state{stateid = {SubKey, '_'}, _ = '_'})
    end,
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun (PubSubState, Acc) ->
                                get_entity_subscriptions_loop(NodeTree, PubSubState, Acc)
                        end,
                        [], States),
    {result, Reply}.

get_entity_subscriptions_loop(NodeTree, #pubsub_state{stateid = {J, N}, subscriptions = Ss}, Acc) ->
    case gen_pubsub_nodetree:get_node(NodeTree, N) of
        #pubsub_node{} = Node ->
            lists:foldl(fun ({Sub, SubId}, Acc2) -> [{Node, Sub, SubId, J} | Acc2] end, Acc, Ss);
        _ ->
            Acc
    end.

get_node_subscriptions(Nidx) ->
    {result, States} = get_states(Nidx),
    Tr = fun (#pubsub_state{stateid = {J, _}, subscriptions = [_ | _] = Subscriptions}) ->
                 lists:foldl(fun ({S, SubId}, Acc) ->
                                     [{J, S, SubId} | Acc]
                             end,
                             [], Subscriptions);
             (#pubsub_state{stateid = {_, _}, subscriptions = []}) ->
                 [];
             (#pubsub_state{stateid = {J, _}}) ->
                 [{J, none}]
         end,
    {result, lists:flatmap(Tr, States)}.

get_subscriptions(Nidx, Owner) ->
    SubKey = jid:to_lower(Owner),
    SubState = get_state(Nidx, SubKey),
    {result, SubState#pubsub_state.subscriptions}.

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    SubKey = jid:to_lower(Owner),
    SubState = get_state(Nidx, SubKey),
    case {SubId, SubState#pubsub_state.subscriptions} of
        {_, []} ->
            case Subscription of
                none ->
                    {error,
                        ?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"not-subscribed">>)};
                _ ->
                    new_subscription(Nidx, Owner, Subscription, SubState)
            end;
        {<<>>, [{_, SID}]} ->
            case Subscription of
                none -> unsub_with_subid(Nidx, SID, SubState);
                _ -> replace_subscription({Subscription, SID}, SubState)
            end;
        {<<>>, [_ | _]} ->
            {error,
                ?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"subid-required">>)};
        _ ->
            case Subscription of
                none -> unsub_with_subid(Nidx, SubId, SubState);
                _ -> replace_subscription({Subscription, SubId}, SubState)
            end
    end.

replace_subscription(NewSub, SubState) ->
    NewSubs = replace_subscription(NewSub, SubState#pubsub_state.subscriptions, []),
    set_state(SubState#pubsub_state{subscriptions = NewSubs}).

replace_subscription(_, [], Acc) -> Acc;
replace_subscription({Sub, SubId}, [{_, SubId} | T], Acc) ->
    replace_subscription({Sub, SubId}, T, [{Sub, SubId} | Acc]).

new_subscription(_Nidx, _Owner, Sub, SubState) ->
    %%SubId = pubsub_subscription:add_subscription(Owner, Nidx, []),
    SubId = pubsub_subscription:make_subid(),
    Subs = SubState#pubsub_state.subscriptions,
    set_state(SubState#pubsub_state{subscriptions = [{Sub, SubId} | Subs]}),
    {Sub, SubId}.

unsub_with_subid(Nidx, SubId, #pubsub_state{stateid = {Entity, _}} = SubState) ->
    %%pubsub_subscription:delete_subscription(SubState#pubsub_state.stateid, Nidx, SubId),
    NewSubs = [{S, Sid}
            || {S, Sid} <- SubState#pubsub_state.subscriptions,
                SubId =/= Sid],
    case {NewSubs, SubState#pubsub_state.affiliation} of
        {[], none} -> del_state(Nidx, Entity);
        _ -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

%% @doc <p>Returns a list of Owner's nodes on Host with pending
%% subscriptions.</p>
get_pending_nodes(Host, Owner) ->
    GenKey = jid:to_bare(jid:to_lower(Owner)),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'},
                                               affiliation = owner,
                                               _ = '_'}),
    NodeIdxs = [Nidx || #pubsub_state{stateid = {_, Nidx}} <- States],
    NodeTree = mod_pubsub:tree(Host),
    Reply = mnesia:foldl(fun (#pubsub_state{stateid = {_, Nidx}} = PubSubState, Acc) ->
                                 get_node_with_pending_subs_if_in_list(Nidx, NodeIdxs, NodeTree,
                                                                       PubSubState, Acc)
                         end,
                         [], pubsub_state),
    {result, Reply}.

get_node_with_pending_subs_if_in_list(Nidx, NodeIdxs, NodeTree, PubSubState, Acc) ->
  case lists:member(Nidx, NodeIdxs) of
      true ->
          case get_node_if_has_pending_subs(NodeTree, PubSubState) of
              {value, Node} -> [Node | Acc];
              false -> Acc
          end;
      false ->
          Acc
  end.

get_node_if_has_pending_subs(NodeTree, #pubsub_state{stateid = {_, N}, subscriptions = Subs}) ->
    HasPending = fun
        ({pending, _}) -> true;
        (pending) -> true;
        (_) -> false
    end,
    case lists:any(HasPending, Subs) of
        true ->
            case gen_pubsub_nodetree:get_node(NodeTree, N) of
                #pubsub_node{nodeid = {_, Node}} -> {value, Node};
                _ -> false
            end;
        false ->
            false
    end.

%% @doc Returns the list of stored states for a given node.
%% <p>For the default PubSub module, states are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_state table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the states where they wants (for example in a
%% relational database).</p>
%% <p>If a PubSub plugin wants to delegate the states storage to the default node,
%% they can implement this function like this:
%% ```get_states(Nidx) ->
%%           node_default:get_states(Nidx).'''</p>
get_states(Nidx) ->
    States = case catch mnesia:match_object(
            #pubsub_state{stateid = {'_', Nidx}, _ = '_'}) of
        List when is_list(List) -> List;
        _ -> []
    end,
    {result, States}.

%% @doc <p>Returns a state (one state list), given its reference.</p>
get_state(Nidx, Key) ->
    StateId = {Key, Nidx},
    case catch mnesia:read({pubsub_state, StateId}) of
        [State] when is_record(State, pubsub_state) -> State;
        _ -> #pubsub_state{stateid = StateId}
    end.

%% @doc <p>Write a state into database.</p>
set_state(State) when is_record(State, pubsub_state) ->
    mnesia:write(State).
%set_state(_) -> {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @doc <p>Delete a state from database.</p>
del_state(Nidx, Key) ->
    mnesia:delete({pubsub_state, {Key, Nidx}}).

%% @doc Returns the list of stored items for a given node.
%% <p>For the default PubSub module, items are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_item table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the items where they wants (for example in a
%% relational database), or they can even decide not to persist any items.</p>
get_items(Nidx, _From, _RSM) ->
    Items = mnesia:match_object(#pubsub_item{itemid = {'_', Nidx}, _ = '_'}),
    {result, {lists:reverse(lists:keysort(#pubsub_item.modification, Items)), none}}.

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId, RSM) ->
    SubKey = jid:to_lower(JID),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    SubState = get_state(Nidx, SubKey),
    Affiliation = GenState#pubsub_state.affiliation,
    BareSubscriptions = GenState#pubsub_state.subscriptions,
    FullSubscriptions = SubState#pubsub_state.subscriptions,
    Whitelisted = can_fetch_item(Affiliation, BareSubscriptions) orelse
                  can_fetch_item(Affiliation, FullSubscriptions),
    case authorize_get_item(Affiliation, AccessModel, PresenceSubscription,
                            RosterGroup, Whitelisted) of
        ok -> get_items(Nidx, JID, RSM);
        {error, _} = Err -> Err
    end.

%% @doc <p>Returns an item (one item list), given its reference.</p>

get_item(Nidx, ItemId) ->
    case mnesia:read({pubsub_item, {ItemId, Nidx}}) of
        [Item] when is_record(Item, pubsub_item) -> {result, Item};
        _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jid:to_lower(JID),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = GenState#pubsub_state.subscriptions,
    Whitelisted = can_fetch_item(Affiliation, Subscriptions),
    case authorize_get_item(Affiliation, AccessModel, PresenceSubscription,
                           RosterGroup, Whitelisted) of
        ok -> get_item(Nidx, ItemId);
        {error, _} = Err -> Err
    end.

authorize_get_item(Affiliation, _AccessModel, _PresenceSubscription, _RosterGroup, _Whitelisted)
  when (Affiliation == outcast) or (Affiliation == publish_only) ->
    {error, ?ERR_FORBIDDEN};
authorize_get_item(_Affiliation, presence, false, _RosterGroup, _Whitelisted) ->
    {error, ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"presence-subscription-required">>)};
authorize_get_item(_Affiliation, roster, _PresenceSubscription, false, _Whitelisted) ->
    {error, ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"not-in-roster-group">>)};
authorize_get_item(_Affiliation, whitelist, _PresenceSubscription, _RosterGroup, false) ->
    {error, ?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
authorize_get_item(_Affiliation, authorize, _PresenceSubscription, _RosterGroup, false) ->
    {error, ?ERR_FORBIDDEN};
authorize_get_item(_Affiliation, _AccessModel, _PresenceSubscription, _RosterGroup, _Whitelisted) ->
    ok.

%% @doc <p>Write an item into database.</p>
set_item(Item) when is_record(Item, pubsub_item) ->
    mnesia:write(Item).
%set_item(_) -> {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @doc <p>Delete an item from database.</p>
del_item(Nidx, ItemId) ->
    mnesia:delete({pubsub_item, {ItemId, Nidx}}).

del_items(Nidx, ItemIds) ->
    lists:foreach(fun (ItemId) -> del_item(Nidx, ItemId)
        end,
        ItemIds).

get_item_name(_Host, _Node, Id) ->
    Id.

%% @doc <p>Return the path of the node. In flat it's just node id.</p>
node_to_path(Node) ->
    [(Node)].

path_to_node(Path) ->
    case Path of
        % default slot
        [Node] -> iolist_to_binary(Node);
        % handle old possible entries, used when migrating database content to new format
        [Node | _] when is_binary(Node) ->
            iolist_to_binary(str:join([<<"">> | Path], <<"/">>));
        % default case (used by PEP for example)
        _ -> iolist_to_binary(Path)
    end.

can_fetch_item(owner, _) -> true;
can_fetch_item(member, _) -> true;
can_fetch_item(publisher, _) -> true;
can_fetch_item(publish_only, _) -> false;
can_fetch_item(outcast, _) -> false;
can_fetch_item(none, Subscriptions) -> is_subscribed(Subscriptions).
%can_fetch_item(_Affiliation, _Subscription) -> false.

is_subscribed(Subscriptions) ->
    lists:any(fun
            ({subscribed, _SubId}) -> true;
            (_) -> false
        end,
        Subscriptions).

first_in_list(_Pred, []) ->
    false;
first_in_list(Pred, [H | T]) ->
    case Pred(H) of
        true -> {value, H};
        _ -> first_in_list(Pred, T)
    end.

timestamp() ->
    os:timestamp().
