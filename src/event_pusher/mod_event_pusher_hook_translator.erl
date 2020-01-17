%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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

-module(mod_event_pusher_hook_translator).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("jlib.hrl").
-include("mod_event_pusher_events.hrl").

-export([start/2, stop/1]).
-export([user_send_packet/4,
         filter_local_packet/1,
         user_present/2,
         user_not_present/5,
         unacknowledged_message/2]).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> any().
start(Host, _Opts) ->
    ejabberd_hooks:add(hooks(Host)).

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)).

%%--------------------------------------------------------------------
%% Hook callbacks
%%--------------------------------------------------------------------
-type routing_data() :: {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}.
-spec filter_local_packet(drop) -> drop;
                         (routing_data()) -> routing_data().
filter_local_packet(drop) ->
    drop;
filter_local_packet({From, To = #jid{lserver = Host}, Acc0, Packet}) ->
    Acc = case chat_type(Acc0) of
              false -> Acc0;
              Type ->
                  Event = #chat_event{type = Type, direction = out,
                                      from = From, to = To, packet = Packet},
                  NewAcc = mod_event_pusher:push_event(Acc0, Host, Event),
                  merge_acc(Acc0, NewAcc)
          end,
    {From, To, Acc, Packet}.

-spec user_send_packet(mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                       Packet :: exml:element()) -> mongoose_acc:t().
user_send_packet(Acc, From, To, Packet = #xmlel{name = <<"message">>}) ->
    case chat_type(Acc) of
        false -> Acc;
        Type ->
            Event = #chat_event{type = Type, direction = in,
                                from = From, to = To, packet = Packet},
            NewAcc = mod_event_pusher:push_event(Acc, From#jid.lserver, Event),
            merge_acc(Acc, NewAcc)
    end;
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

-spec user_present(mongoose_acc:t(), UserJID :: jid:jid()) -> mongoose_acc:t().
user_present(Acc, #jid{} = UserJID) ->
    Event = #user_status_event{jid = UserJID, status = online},
    NewAcc = mod_event_pusher:push_event(Acc, UserJID#jid.lserver, Event),
    merge_acc(Acc, NewAcc).

-spec user_not_present(mongoose_acc:t(), User :: jid:luser(), Server :: jid:lserver(),
                       Resource :: jid:lresource(), Status :: any()) -> mongoose_acc:t().
user_not_present(Acc, LUser, LHost, LResource, _Status) ->
    UserJID = jid:make_noprep(LUser, LHost, LResource),
    Event = #user_status_event{jid = UserJID, status = offline},
    NewAcc = mod_event_pusher:push_event(Acc, LHost, Event),
    merge_acc(Acc, NewAcc).

unacknowledged_message(Acc, #jid{lserver = Server} = Jid) ->
    Event = #unack_msg_event{to = Jid},
    NewAcc = mod_event_pusher:push_event(Acc, Server, Event),
    merge_acc(Acc, NewAcc).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec chat_type(mongoose_acc:t()) -> chat | groupchat | headline | normal | false.
chat_type(Acc) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"chat">> -> chat;
        <<"groupchat">> -> groupchat;
        <<"headline">> -> headline;
        <<"normal">> -> normal;
        undefined -> normal;
        _ -> false
    end.

-spec merge_acc(mongoose_acc:t(), mongoose_acc:t()) -> mongoose_acc:t().
merge_acc(Acc, EventPusherAcc) ->
    NS = mongoose_acc:get(event_pusher, EventPusherAcc),
    mongoose_acc:set_permanent(event_pusher, NS, Acc).

-spec hooks(Host :: jid:server()) -> [ejabberd_hooks:hook()].
hooks(Host) ->
    [
        {filter_local_packet, Host, ?MODULE, filter_local_packet, 90},
        {unset_presence_hook, Host, ?MODULE, user_not_present, 90},
        {user_available_hook, Host, ?MODULE, user_present, 90},
        {user_send_packet, Host, ?MODULE, user_send_packet, 90},
        {rest_user_send_packet, Host, ?MODULE, user_send_packet, 90},
        {unacknowledged_message, Host, ?MODULE, unacknowledged_message, 90}
    ].
