%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub DB behaviour
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose_logger.hrl").

-export([db_error/3, extra_debug_fun/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-callback start() -> ok.

-callback stop() -> ok.

%% ----------------------- Fun execution ------------------------

%% `ErrorDebug` are the maps of extra data that will be added to error tuple

-callback transaction(Fun :: fun(() -> {result | error, any()}),
                      ErrorDebug :: map()) ->
    {result | error, any()}.

%% Synchronous
-callback dirty(Fun :: fun(() -> {result | error, any()}),
                ErrorDebug :: map()) ->
    {result | error, any()}.

%% ----------------------- Direct #pubsub_state access ------------------------

%% When a state is not found, returns empty state.
%% Maybe can be removed completely later?
-callback get_state(Nidx :: mod_pubsub:nodeIdx(),
                    JID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.

%% Maybe can be removed completely later?
-callback get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_lus(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_bare(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_bare_and_full(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_idxs_of_own_nodes_with_pending_subs(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:nodeIdx()]}.

%% ----------------------- Node management ------------------------
%% TODO this is not really node creation
-callback create_node(Nidx :: mod_pubsub:nodeIdx(),
                      Owner :: jid:ljid()) ->
    ok.

-callback del_node(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback set_node(Node :: mod_pubsub:pubsubNode()) -> {ok, mod_pubsub:nodeIdx()}.

-callback find_node_by_id(Nidx :: mod_pubsub:nodeIdx()) ->
    {error, not_found} | {ok, mod_pubsub:pubsubNode()}.

-callback find_node_by_name(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    mod_pubsub:pubsubNode() | false.

-callback find_nodes_by_key(Key :: mod_pubsub:hostPubsub() | jid:ljid()) ->
    [mod_pubsub:pubsubNode()].

-callback delete_node(Node :: mod_pubsub:pubsubNode()) -> ok.

-callback get_subnodes(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId() | <<>>) ->
    [mod_pubsub:pubsubNode()].

-callback get_parentnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].

-callback get_subnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].

%% ----------------------- Affiliations ------------------------

-callback set_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                          JID :: jid:ljid(),
                          Affiliation :: mod_pubsub:affiliation()) ->
    ok.

-callback get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                          JID :: jid:ljid()) ->
    {ok, mod_pubsub:affiliation()}.

%% ----------------------- Subscriptions ------------------------

-callback add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                           JID :: jid:ljid(),
                           Sub :: mod_pubsub:subscription(),
                           SubId :: mod_pubsub:subId(),
                           SubOpts :: mod_pubsub:subOptions()) ->
    ok.

-callback update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                              JID :: jid:ljid(),
                              Subscription :: mod_pubsub:subscription(),
                              SubId :: mod_pubsub:subId()) ->
    ok.

-callback set_subscription_opts(Nidx :: mod_pubsub:nodeIdx(),
                                JID :: jid:ljid(),
                                SubId :: mod_pubsub:subId(),
                                Opts :: mod_pubsub:subOptions()) ->
    ok.

-callback get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(),
           Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           SubOpts :: mod_pubsub:subOptions()}]}.

-callback get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                        JID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           SubOpts :: mod_pubsub:subOptions()}]}.

-callback delete_subscription(
            Nidx :: mod_pubsub:nodeIdx(),
            JID :: jid:ljid(),
            SubId :: mod_pubsub:subId()) ->
    ok.

-callback delete_all_subscriptions(
            Nidx :: mod_pubsub:nodeIdx(),
            JID :: jid:ljid()) ->
    ok.

%% ----------------------- Items ------------------------

%% TODO: Refactor to use MaxItems value, so separate remove_items in publishing
%% won't be necessary and the whole operation may be optimised in DB layer.
-callback add_item(Nidx :: mod_pubsub:nodeIdx(),
                   JID :: jid:ljid(),
                   PubSubItem :: mod_pubsub:pubsubItem()) ->
    ok.

-callback remove_items(Nidx :: mod_pubsub:nodeIdx(),
                       JID :: jid:ljid(),
                       ItemIds :: [mod_pubsub:itemId()]) ->
    ok.

-callback remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.

-callback get_items(Nidx :: mod_pubsub:nodeIdx(), gen_pubsub_node:get_item_options()) ->
    {ok, {[mod_pubsub:pubsubItem()], jlib:rsm_out() | none}}.

-callback get_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) ->
    {ok, mod_pubsub:pubsubItem()} | {error, item_not_found}.

-callback set_item(Item :: mod_pubsub:pubsubItem()) -> ok.

-callback del_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) -> ok.

-callback del_items(Nidx :: mod_pubsub:nodeIdx(), [ItemId :: mod_pubsub:itemId()]) -> ok.

%% ----------------------- GDPR-related ------------------------

-callback get_user_payloads(LUser :: jid:luser(), LServer :: jid:lserver()) ->
    [NodeNameItemIDAndPayload :: [binary()]].

-callback get_user_nodes(LUser :: jid:luser(), LServer :: jid:lserver()) ->
    [NodeNameAndType :: [binary()]].

-callback get_user_subscriptions(LUser :: jid:luser(), LServer :: jid:lserver()) ->
    [NodeName :: [binary()]].

-callback find_nodes_by_affiliated_user(JID :: jid:ljid()) ->
    [{mod_pubsub:pubsubNode(), mod_pubsub:affiliation()}].

-callback delete_user_subscriptions(JID :: jid:ljid()) ->
    ok.

%%====================================================================
%% API
%%====================================================================

% ReasonData may either be a debug map provided by mod_pubsub
% or some other term if the crash is serious enough to lose the debug map somewhere.
-spec db_error(ReasonData :: map() | any(), ErrorDebug :: map(), Event :: any()) ->
    {error, Details :: map()}.
db_error(ReasonData, ErrorDebug, Event) ->
    {error, maps:merge(ErrorDebug#{ event => Event }, sanitize_reason(ReasonData))}.

%% transaction and sync_dirty return very truncated error data so we add extra
%% try to gather stack trace etc.
-spec extra_debug_fun(fun()) -> fun().
extra_debug_fun(Fun) ->
    fun() ->
            try Fun() of
                Res -> Res
            catch
                C:R:S ->
                    throw(#{
                      class => C,
                      reason => R,
                      stacktrace => S})
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

sanitize_reason(Map) when is_map(Map) ->
    Map;
sanitize_reason(Other) ->
    #{ unexpected_reason => Other }.

