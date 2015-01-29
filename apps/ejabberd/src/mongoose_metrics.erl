%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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
-module(mongoose_metrics).

%% API
-export([update/2,
         init_predefined_metrics/1,
         create_generic_hook_metric/2,
         increment_generic_hook_metric/2,
         remove_host_metrics/1]).

-spec update(term(), term()) -> no_return().
update(Metric, Change) ->
    folsom_metrics:notify(Metric, Change).

-spec init_predefined_metrics(ejabberd:lserver()) -> no_return().
init_predefined_metrics(Host) ->
    init_folsom(Host),
    metrics_hooks(add, Host),
    ok.

-spec create_generic_hook_metric(ejabberd:lserver(), atom()) -> no_return().
create_generic_hook_metric(Host, Hook) ->
    do_create_generic_hook_metric({Host, hook_to_name(Hook)}).

-spec increment_generic_hook_metric(ejabberd:lserver(), atom()) -> no_return().
increment_generic_hook_metric(Host, Hook) ->
    do_increment_generic_hook_metric({Host, hook_to_name(Hook)}).

do_create_generic_hook_metric({_, skip}) ->
    ok;
do_create_generic_hook_metric({Host, _} = MetricName) ->
    folsom_metrics:new_spiral(MetricName),
    folsom_metrics:tag_metric(MetricName, Host),
    folsom_metrics:tag_metric(MetricName, hooks).


do_increment_generic_hook_metric({_, skip}) ->
    ok;
do_increment_generic_hook_metric(MetricName) ->
    folsom_metrics:notify(MetricName, 1).

remove_host_metrics(Host) ->
    [folsom_metrics:delete_metric(Metric) ||
     {Metric, _} <- folsom_metrics:get_metrics_value(Host)].

%% allows to set a specifc name for metric
%% or skip given hook
hook_to_name(sm_register_connection_hook) -> skip;
hook_to_name(sm_remove_connetion_hook) -> skip;
hook_to_name(auth_failed) -> skip;
hook_to_name(user_send_packet) -> skip;
hook_to_name(user_receive_packet) -> skip;
hook_to_name(xmpp_bounce_message) -> skip;
hook_to_name(xmpp_stanza_dropped) -> skip;
hook_to_name(xmpp_send_element) -> skip;
hook_to_name(roster_get) -> skip;
hook_to_name(roster_set) -> skip;
hook_to_name(roster_push) -> skip;
hook_to_name(register_user) -> skip;
hook_to_name(remove_user) -> skip;
hook_to_name(privacy_iq_get) -> skip;
hook_to_name(privacy_iq_set) -> skip;
hook_to_name(privacy_check_packet) -> skip;
hook_to_name(mam_get_prefs) -> skip;
hook_to_name(mam_set_prefs) -> skip;
hook_to_name(mam_remove_archive) -> skip;
hook_to_name(mam_archive_message) -> skip;
hook_to_name(mam_flush_messages) -> skip;
hook_to_name(mam_drop_message) -> skip;
hook_to_name(mam_drop_iq) -> skip;
hook_to_name(mam_drop_messages) -> skip;
hook_to_name(mam_purge_single_message) -> skip;
hook_to_name(mam_purge_multiple_messages) -> skip;
hook_to_name(mam_muc_get_prefs) -> skip;
hook_to_name(mam_muc_set_prefs) -> skip;
hook_to_name(mam_muc_remove_archive) -> skip;
hook_to_name(mam_muc_lookup_messages) -> skip;
hook_to_name(mam_muc_archive_message) -> skip;
hook_to_name(mam_muc_flush_messages) -> skip;
hook_to_name(mam_muc_drop_message) -> skip;
hook_to_name(mam_muc_drop_iq) -> skip;
hook_to_name(mam_muc_drop_messages) -> skip;
hook_to_name(mam_muc_purge_single_message) -> skip;
hook_to_name(mam_muc_purge_multiple_messages) -> skip;

hook_to_name(Hook) -> Hook.



-spec init_folsom(ejabberd:server()) -> 'ok'.
init_folsom(Host) ->
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
    end, mongoose_metrics_hooks:get_hooks(Host)).

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
