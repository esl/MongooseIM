%%==============================================================================
%% Copyright 2026 Erlang Solutions Ltd.
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

-module(mod_stanzaid).
-behaviour(gen_mod).
-xep([{xep, 0359}, {version, "0.7.0"}]).
-export([start/2, stop/1, hooks/1, supported_features/0, instrumentation/1]).
-export([user_send_message/3, filter_room_packet/3, user_receive_message/3]).

-include_lib("exml/include/exml.hrl").
-include_lib("jlib.hrl").

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
        {user_send_message, HostType, fun ?MODULE:user_send_message/3, #{}, 1},
        {user_receive_message, HostType, fun ?MODULE:user_receive_message/3, #{}, 100},
        {filter_room_packet, HostType, fun ?MODULE:filter_room_packet/3, #{}, 100}
    ].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(_HostType) ->
    [].

%%%%%%%%%%%%%%%%%%%
%% Handlers
-spec user_send_message(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_message(Acc, _, _) ->
    Acc1 =  mongoose_acc:set_permanent(stable_stanza_id, value, generate_id(Acc), Acc),
    {ok, Acc1}.

%% This is needed to handle a situation where MUC is enabled but with
%% no archiving. MUC routes bare message so user_receive_message
%% does not help, while there is no stanza-id element from MAM.
-spec filter_room_packet(Packet, EventData, Extra) -> {ok, Packet} when
    Packet :: exml:element(),
    EventData :: mod_muc:room_event_data(),
    Extra :: gen_hook:extra().
filter_room_packet(Packet,
                   #{stable_stanza_id := StableId, room_jid := RoomJID},
                   _Extra) ->
    {ok, ensure_stanza_id(Packet, StableId, RoomJID)};
filter_room_packet(Packet, _, _) ->
    {ok, Packet}.

-spec user_receive_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
user_receive_message(Acc, _, _) ->
    El1 = ensure_stanza_id(mongoose_acc:element(Acc),
                           mongoose_acc:get(stable_stanza_id, value, 0, Acc),
                           mongoose_acc:to_jid(Acc)),
    {ok, mongoose_acc:update_stanza(#{element => El1}, Acc)}.

ensure_stanza_id(Packet, 0, _) ->
    Packet;
ensure_stanza_id(Packet, StableId, Jid) ->
    case has_stanza_id(Packet) of
        true -> Packet;
        false -> add_stanza_id(StableId, Jid, Packet)
    end.

generate_id(Acc) ->
    CandidateStamp = mongoose_acc:timestamp(Acc),
    mod_mam_utils:generate_message_id(CandidateStamp).

has_stanza_id(#xmlel{children = Ch}) ->
    has_stanza_id(Ch);
has_stanza_id([]) ->
    false;
has_stanza_id([#xmlel{name = <<"stanza-id">>, attrs = #{<<"id">> := _,
                                                        <<"xmlns">> := ?NS_STANZAID}} | _]) ->
    true;
has_stanza_id([_ | Tail]) ->
    has_stanza_id(Tail).

add_stanza_id(StableId, To, #xmlel{children = Ch} = El) ->
    S = #xmlel{name = <<"stanza-id">>,
               attrs = #{<<"id">> => mod_mam_utils:mess_id_to_external_binary(StableId),
                         <<"by">> => jid:to_bare_binary(To),
                         <<"xmlns">> => ?NS_STANZAID}},
    El#xmlel{children = [S | Ch]}.