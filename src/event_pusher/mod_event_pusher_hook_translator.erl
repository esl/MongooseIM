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

-include("jlib.hrl").
-include("mod_event_pusher_events.hrl").

-export([start/2, stop/1]).
-export([user_send_packet/4, filter_local_packet/1, user_present/2, user_not_present/5]).

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

-spec filter_local_packet(drop) -> drop;
                         (RoutingData) -> RoutingData
                                              when RoutingData :: {jid:jid(), jid:jid(), mongoose_acc:t()}.
filter_local_packet(drop) ->
    drop;
filter_local_packet({From, To = #jid{lserver = Host}, Acc, Packet}) ->
    case chat_type(Acc) of
        false -> skip;
        Type ->
            mod_event_pusher:push_event(Acc, Host,
                                        #chat_event{type = Type, direction = out,
                                                    from = From, to = To, packet = Packet})
    end,
    {From, To, Acc, Packet}.

-spec user_send_packet(mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                       Packet :: exml:element()) -> mongoose_acc:t().
user_send_packet(Acc, From, To, Packet = #xmlel{name = <<"message">>}) ->
    case chat_type(Acc) of
        false -> skip;
        Type ->
            mod_event_pusher:push_event(Acc, From#jid.lserver,
                                        #chat_event{type = Type, direction = in,
                                                    from = From, to = To, packet = Packet})
    end,
    Acc;
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

-spec user_present(mongoose_acc:t(), UserJID :: jid:jid()) -> mongoose_acc:t().
user_present(Acc, #jid{} = UserJID) ->
    mod_event_pusher:push_event(Acc, UserJID#jid.lserver,
                                #user_status_event{jid = UserJID, status = online}),
    Acc.

-spec user_not_present(mongoose_acc:t(), User :: jid:luser(), Server :: jid:lserver(),
                       Resource :: jid:lresource(), Status :: any()) -> mongoose_acc:t().
user_not_present(Acc, User, Host, Resource, _Status) ->
    UserJID = jid:make_noprep(User, Host, Resource),
    mod_event_pusher:push_event(Acc, Host, #user_status_event{jid = UserJID, status = offline}),
    Acc.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec chat_type(mongoose_acc:t()) -> chat | groupchat | headline | normal | false.
chat_type(Acc) ->
    case mongoose_acc:get(type, Acc, <<"normal">>) of
        <<"chat">> -> chat;
        <<"groupchat">> -> groupchat;
        <<"headline">> -> headline;
        <<"normal">> -> normal;
        _ -> false
    end.

-spec hooks(Host :: jid:server()) -> [ejabberd_hooks:hook()].
hooks(Host) ->
    [
     {filter_local_packet, Host, ?MODULE, filter_local_packet, 90},
     {unset_presence_hook, Host, ?MODULE, user_not_present, 90},
     {user_available_hook, Host, ?MODULE, user_present, 90},
     {user_send_packet, Host, ?MODULE, user_send_packet, 90},
     {rest_user_send_packet, Host, ?MODULE, user_send_packet, 90}
    ].
