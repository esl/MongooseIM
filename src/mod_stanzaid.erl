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
-export([user_send_message/3, filter_local_packet/3]).

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
        {filter_local_packet, HostType, fun ?MODULE:filter_local_packet/3, #{}, 100}
    ].

-spec supported_features() -> [atom()].
supported_features() ->
    [].

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(_HostType) ->
    [].

%%%%%%%%%%%%%%%%%%%
%% Handlers
-spec user_send_message(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_message(Acc, _, _) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> ->
            Acc1 =  mongoose_acc:set_permanent(stable_stanza_id, value, generate_id(Acc), Acc),
            {ok, Acc1};
        _ ->
            {ok, Acc}
    end.

-spec filter_local_packet(FPacketAcc, Params, Extra) -> {ok, FPacketAcc} when
    FPacketAcc :: mongoose_hooks:filter_packet_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
filter_local_packet({From, To, Acc, #xmlel{name = <<"message">>} = Packet} = FPacket, _, _) ->
    case mongoose_acc:get(stable_stanza_id, value, 0, Acc) of
        0 ->
            {ok, FPacket};
        StableId ->
            case has_stanza_id(Packet) of
                true -> {ok, FPacket};
                false -> {ok, {From, To, Acc, add_stanza_id(StableId, To, Packet)}}
            end
    end;
filter_local_packet(FPacket, _, _) ->
    {ok, FPacket}.

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