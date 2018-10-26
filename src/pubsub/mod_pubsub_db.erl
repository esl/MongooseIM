%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub DB behaviour
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose_logger.hrl").

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-callback start(Host :: jid:lserver(), PubSubHost :: jid:lserver()) -> ok.

-callback stop(Host :: jid:lserver(), PubSubHost :: jid:lserver()) -> ok.

-callback transaction(PubSubHost :: jid:lserver(),
                      Fun :: fun(() -> {result | error, any()})) ->
    {result | error, any()}.

%% Synchronous
-callback dirty(PubSubHost :: jid:lserver(),
                Fun :: fun(() -> {result | error, any()})) ->
    {result | error, any()}.

-callback set_state(PubSubHost :: jid:lserver(),
                    mod_pubsub:pubsubState()) -> ok.

%%====================================================================
%% API
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

