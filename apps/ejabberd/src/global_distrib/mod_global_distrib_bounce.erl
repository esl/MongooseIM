%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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

-module(mod_global_distrib_bounce).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1]).
-export([remove_message5/5, remove_message4/4, maybe_store_message/1, maybe_resend_message/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts0) ->
    Opts = [{resend_after_ms, 200}, {max_retries, 4} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

remove_message5(Acc, _Jid, From, To, Packet) ->
    remove_message4(Acc, From, To, Packet).

remove_message4(Acc, From, _To, Packet) ->
    ?DEBUG("Removing message from=~s, to=~s from buffer: successful delivery",
           [jid:to_binary(From), jid:to_binary(_To)]),
    ets:delete(?MODULE, storage_key(From, Packet)),
    Acc.

maybe_resend_message(drop) -> drop;
maybe_resend_message({_From, To, Acc} = FPacket) ->
    Packet = mongoose_acc:get(to_send, Acc),
    case exml_query:attr(Packet, <<"type">>) of
        <<"error">> ->
            StorageKey = storage_key(To, Packet),
            case ets:lookup(?MODULE, StorageKey) of
                [{_, _StoredPacket, _TTL = 0}] ->
                    ets:delete(?MODULE, StorageKey),
                    FPacket;
                [{_, StoredPacket, _TTL}] ->
                    resend_message(Acc, StorageKey, StoredPacket),
                    drop;
                [] ->
                    FPacket
            end;
        _ ->
            FPacket
    end.

maybe_store_message(drop) -> drop;
maybe_store_message({_From, _To, Acc} = FPacket) ->
    Packet = mongoose_acc:get(to_send, Acc),
    case exml_query:attr(Packet, <<"distrib_ttl">>) of
        undefined -> store_message(FPacket);
        _ -> FPacket
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start() ->
    Host = opt(global_host),
    LocalHost = opt(local_host),
    ejabberd_hooks:add(filter_packet, global, ?MODULE, maybe_resend_message, 1),
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, maybe_store_message, 80),
    ejabberd_hooks:add(user_receive_packet, LocalHost, ?MODULE, remove_message5, 99),
    ejabberd_hooks:add(s2s_send_packet, LocalHost, ?MODULE, remove_message4, 99).

stop() ->
    Host = opt(global_host),
    LocalHost = opt(local_host),
    ejabberd_hooks:delete(s2s_send_packet, LocalHost, ?MODULE, remove_message4, 99),
    ejabberd_hooks:delete(user_receive_packet, LocalHost, ?MODULE, remove_message5, 99),
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, maybe_store_message, 80),
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, maybe_resend_message, 1).

resend_message(Acc, StorageKey, StoredPacket) ->
    TTL = ets:update_counter(?MODULE, StorageKey, {3, -1}),
    FromBin = exml_query:attr(StoredPacket, <<"from">>),
    ToBin = exml_query:attr(StoredPacket, <<"to">>),
    From = jid:from_binary(FromBin),
    To = jid:from_binary(ToBin),
    ?DEBUG("Scheduling resend of message from=~s to=~s retries_left=~B", [FromBin, ToBin, TTL]),
    mod_global_distrib_mapping:clear_cache_for_jid(ToBin),
    NewAcc = mongoose_acc:update_element(Acc, StoredPacket, From, To),
    timer:apply_after(opt(resend_after_ms), ejabberd_router, route, [From, To, NewAcc]).

store_message({From, To, Acc}) ->
    Packet0 = mongoose_acc:get(to_send, Acc),
    Packet = jlib:replace_from_to(From, To, maybe_set_id(Packet0)),
    NewAcc = mongoose_acc:update_element(Acc, Packet, From, To),
    ets:insert_new(?MODULE, {storage_key(From, Packet), Packet, opt(max_retries)}),
    {From, To, NewAcc}.

maybe_set_id(Packet) ->
    case exml_query:attr(Packet, <<"id">>) of
        undefined -> set_id(Packet);
        _ -> Packet
    end.

set_id(Packet) ->
    NewId = uuid:uuid_to_string(uuid:get_v4(), binary_nodash),
    Packet#xmlel{attrs = [{<<"id">>, NewId} | Packet#xmlel.attrs]}.

storage_key(From, Packet) when is_binary(From) ->
    {From, exml_query:attr(Packet, <<"id">>)};
storage_key(From, Packet) ->
    storage_key(jid:to_binary(From), Packet).

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
