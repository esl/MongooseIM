%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
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
-module(mod_event_pusher_push_plugin_enhanced).
-behavior(mod_event_pusher_push_plugin).

-include("mod_event_pusher_events.hrl").

%% API
-export([should_publish/3]).

%%--------------------------------------------------------------------
%% mod_event_pusher_push_plugin callbacks
%%--------------------------------------------------------------------
-spec should_publish(Acc :: mongooseim_acc:t(),
                     Event :: mod_event_pusher:event(),
                     Services :: [mod_event_pusher_push:publish_service()]) ->
                        [mod_event_pusher_push:publish_service()].
should_publish(Acc, #unack_msg_event{to = Jid}, _Services) ->
    PublishedServices = mongoose_acc:get(event_pusher, published_services, [], Acc),
    case ejabberd_sm:get_info(Jid, publish_service) of
        {ok, Service} -> [Service] -- PublishedServices;
        {error, _} -> []
    end;
should_publish(Acc, Event, Services) ->
    mod_event_pusher_push_plugin_defaults:should_publish(Acc, Event, Services).
