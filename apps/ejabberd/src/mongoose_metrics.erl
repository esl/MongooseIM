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

-define(IMPL_MODULE, mongoose_metrics_hooks).

%% API
-export([create_generic_hook_metric/2,
         increment/2]).

-spec create_generic_hook_metric(ejabberd:lserver(), atom()) -> no_return().
create_generic_hook_metric(Host, Hook) ->
    do_create_generic_hook_metric({Host, hook_to_name(Hook)}).

do_create_generic_hook_metric({_, skip}) ->
    ok;
do_create_generic_hook_metric({Host, _} = MetricName) ->
    folsom_metrics:new_spiral(MetricName),
    folsom_metrics:tag_metric(MetricName, Host),
    folsom_metrics:tag_metric(MetricName, hooks).

increment(Host, Hook) ->
    do_increment({Host, hook_to_name(Hook)}).

do_increment({_, skip}) ->
    ok;
do_increment(MetricName) ->
    folsom_metrics:notify(MetricName, 1).

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
hook_to_name(mam_muc_lookup_messages) -> skip;
hook_to_name(mam_muc_archive_message) -> skip;
hook_to_name(mam_muc_flush_messages) -> skip;
hook_to_name(mam_muc_drop_message) -> skip;
hook_to_name(mam_muc_drop_iq) -> skip;
hook_to_name(mam_muc_drop_messages) -> skip;
hook_to_name(mam_muc_purge_single_message) -> skip;
hook_to_name(mam_muc_purge_multiple_messages) -> skip;

hook_to_name(Hook) -> Hook.