%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Implementation of MongooseIM metrics.
%%%
%%% @end
%%% Created : 23 Apr 2013 by Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module (mod_metrics).

-behaviour (gen_mod).

-export ([start/2, stop/1]).

-spec start(ejabberd:server(), list()) -> ok.
start(Host, _Opts) ->
    init_folsom(Host),
    metrics_hooks(add, Host),
    ok.


-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    metrics_hooks(delete, Host),
    ok.


-spec init_folsom(ejabberd:server()) -> 'ok'.
init_folsom(Host) ->
    folsom:start(),
    lists:foreach(fun(Name) ->
        folsom_metrics:new_spiral(Name),
        folsom_metrics:tag_metric(Name, Host)
    end, get_general_counters(Host)),

    lists:foreach(fun(Name) ->
        folsom_metrics:new_counter(Name),
        folsom_metrics:tag_metric(Name, Host)
    end, get_total_counters(Host)).


-spec metrics_hooks('add' | 'delete', ejabberd:server()) -> 'ok'.
metrics_hooks(Op, Host) ->
    lists:foreach(fun(Hook) ->
        apply(ejabberd_hooks, Op, Hook)
    end, ejabberd_metrics_hooks:get_hooks(Host)).

-define (GENERAL_COUNTERS, [
         sessionSuccessfulLogins,
         sessionAuthAnonymous,
         sessionAuthFails,
         sessionLogouts,
         xmppMessageSent,
         xmppMessageReceived,
         xmppMessageBounced,
         xmppPresenceSent,
         xmppPresenceReceived,
         xmppIqSent,
         xmppIqReceived,
         xmppStanzaSent,
         xmppStanzaReceived,
         xmppStanzaDropped,
         xmppStanzaCount,
         xmppErrorTotal,
         xmppErrorBadRequest,
         xmppErrorIq,
         xmppErrorMessage,
         xmppErrorPresence,
         xmppIqTimeouts,
         modRosterSets,
         modRosterGets,
         modPresenceSubscriptions,
         modPresenceUnsubscriptions,
         modRosterPush,
         modRegisterCount,
         modUnregisterCount,
         modPrivacySets,
         modPrivacySetsActive,
         modPrivacySetsDefault,
         modPrivacyPush,
         modPrivacyGets,
         modPrivacyStanzaBlocked,
         modPrivacyStanzaAll,
         modMamPrefsSets,
         modMamPrefsGets,
         modMamArchiveRemoved,
         modMamLookups,
         modMamForwarded,
         modMamArchived,
         modMamFlushed,
         modMamDropped,
         modMamDropped2,
         modMamDroppedIQ,
         modMamSinglePurges,
         modMamMultiplePurges,
         modMucMamPrefsSets,
         modMucMamPrefsGets,
         modMucMamArchiveRemoved,
         modMucMamLookups,
         modMucMamForwarded,
         modMucMamArchived,
         modMucMamSinglePurges,
         modMucMamMultiplePurges
         ]).


-spec get_general_counters(ejabberd:server()) -> [{ejabberd:server(), atom()}].
get_general_counters(Host) ->
    [{Host, Counter} || Counter <- ?GENERAL_COUNTERS].

-define (TOTAL_COUNTERS, [
         sessionCount
         ]).


-spec get_total_counters(ejabberd:server()) ->
                            [{ejabberd:server(),'sessionCount'}].
get_total_counters(Host) ->
    [{Host, Counter} || Counter <- ?TOTAL_COUNTERS].
