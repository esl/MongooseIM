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

-include("jlib.hrl").
-include("mod_event_pusher_events.hrl").

-export([add_hooks/1, delete_hooks/1]).

-export([user_send_packet/3,
         filter_local_packet/3,
         user_present/3,
         user_not_present/3,
         unacknowledged_message/3]).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec add_hooks(mongooseim:host_type()) -> ok.
add_hooks(HostType) ->
    gen_hook:add_handlers(hooks(HostType)).

-spec delete_hooks(mongooseim:host_type()) -> ok.
delete_hooks(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)).

%%--------------------------------------------------------------------
%% Hook callbacks
%%--------------------------------------------------------------------
-type routing_data() :: {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}.
-spec filter_local_packet(drop, _, _) -> {ok, drop};
                         (routing_data(), _, _) -> {ok, routing_data()}.
filter_local_packet(drop, _, _) ->
    {ok, drop};
filter_local_packet({From, To, Acc0, Packet}, _, _) ->
    Acc = case chat_type(Acc0) of
              false -> Acc0;
              Type ->
                  Event = #chat_event{type = Type, direction = out,
                                      from = From, to = To, packet = Packet},
                  NewAcc = mod_event_pusher:push_event(Acc0, Event),
                  merge_acc(Acc0, NewAcc)
          end,
    {ok, {From, To, Acc, Packet}}.

-spec user_send_packet(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{from := jid:jid(), to := jid:jid(), packet := exml:element()},
      Extra :: map().
user_send_packet(Acc, #{from := From, to := To, packet := Packet = #xmlel{name = <<"message">>}}, _) ->
    ResultAcc = case chat_type(Acc) of
        false -> Acc;
        Type ->
            Event = #chat_event{type = Type, direction = in,
                                from = From, to = To, packet = Packet},
            NewAcc = mod_event_pusher:push_event(Acc, Event),
            merge_acc(Acc, NewAcc)
    end,
    {ok, ResultAcc};
user_send_packet(Acc, _, _) ->
    {ok, Acc}.

-spec user_present(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{jid := jid:jid()},
      Extra :: map().
user_present(Acc, #{jid := UserJID = #jid{}}, _) ->
    Event = #user_status_event{jid = UserJID, status = online},
    NewAcc = mod_event_pusher:push_event(Acc, Event),
    {ok, merge_acc(Acc, NewAcc)}.

-spec user_not_present(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{jid := jid:jid()},
      Extra :: map().
user_not_present(Acc, #{jid := UserJID}, _) ->
    Event = #user_status_event{jid = UserJID, status = offline},
    NewAcc = mod_event_pusher:push_event(Acc, Event),
    {ok, merge_acc(Acc, NewAcc)}.

-spec unacknowledged_message(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{jid := jid:jid()},
      Extra :: map().
unacknowledged_message(Acc, #{jid := Jid}, _) ->
    Event = #unack_msg_event{to = Jid},
    NewAcc = mod_event_pusher:push_event(Acc, Event),
    {ok, merge_acc(Acc, NewAcc)}.

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

-spec hooks(mongooseim:host_type()) -> [gen_hook:hook_tuple()].
hooks(HostType) ->
    [
        {filter_local_packet, HostType, fun ?MODULE:filter_local_packet/3, #{}, 80},
        {unset_presence_hook, HostType, fun ?MODULE:user_not_present/3, #{}, 90},
        {user_available_hook, HostType, fun ?MODULE:user_present/3, #{}, 90},
        {user_send_packet, HostType, fun ?MODULE:user_send_packet/3, #{}, 90},
        {rest_user_send_packet, HostType, fun ?MODULE:user_send_packet/3, #{}, 90},
        {unacknowledged_message, HostType, fun ?MODULE:unacknowledged_message/3, #{}, 90}
    ].
