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

-export([user_send_message/3,
         message_routed/3,
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
-spec message_routed(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{user_status := ejabberd_sm:user_status()},
      Extra :: gen_hook:extra().
message_routed(Acc, #{user_status := UserStatus}, _) ->
    case chat_type(Acc) of
        false ->
            {ok, Acc};
        Type ->
            {ok, push_chat_event(Acc, Type, out, UserStatus)}
    end.

-spec user_send_message(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_message(Acc, _, _) ->
    Packet = mongoose_acc:packet(Acc),
    ChatType = chat_type(Acc),
    ResultAcc =
        case {Packet, ChatType} of
            {undefined, _} ->
                Acc;
            {_, false} ->
                Acc;
            {_, Type} ->
                push_chat_event(Acc, Type, in, {online, #{}})
        end,
    {ok, ResultAcc}.

-spec user_present(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
user_present(Acc, #{jid := UserJID = #jid{}}, _) ->
    Event = #user_status_event{jid = UserJID, status = online},
    NewAcc = mod_event_pusher:push_event(Acc, Event),
    {ok, merge_acc(Acc, NewAcc)}.

-spec user_not_present(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
user_not_present(Acc, #{jid := UserJID}, _) ->
    Event = #user_status_event{jid = UserJID, status = offline},
    NewAcc = mod_event_pusher:push_event(Acc, Event),
    {ok, merge_acc(Acc, NewAcc)}.

-spec unacknowledged_message(Acc, Args, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Args :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
unacknowledged_message(Acc, #{jid := Jid}, _) ->
    Event = #unack_msg_event{to = Jid},
    NewAcc = mod_event_pusher:push_event(Acc, Event),
    {ok, merge_acc(Acc, NewAcc)}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec push_chat_event(Acc, Type, Direction, UserStatus) -> Acc when
      Acc :: mongoose_acc:t(),
      Type :: chat | groupchat | headline | normal | false,
      Direction :: in | out,
      UserStatus :: ejabberd_sm:user_status().
push_chat_event(Acc, Type, Direction, UserStatus) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    Event = #chat_event{type = Type, direction = Direction,
                        from = From, to = To, packet = Packet,
                        user_status = UserStatus},
    NewAcc = mod_event_pusher:push_event(Acc, Event),
    merge_acc(Acc, NewAcc).

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
        {message_routed, HostType, fun ?MODULE:message_routed/3, #{}, 80},
        {unset_presence, HostType, fun ?MODULE:user_not_present/3, #{}, 90},
        {user_available, HostType, fun ?MODULE:user_present/3, #{}, 90},
        {user_send_message, HostType, fun ?MODULE:user_send_message/3, #{}, 90},
        {unacknowledged_message, HostType, fun ?MODULE:unacknowledged_message/3, #{}, 90}
    ].
