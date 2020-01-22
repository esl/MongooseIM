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
-behaviour(gen_server).
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-define(MESSAGE_STORE, mod_global_distrib_bounce_message_store).
-define(MS_BY_TARGET, mod_global_distrib_bounce_message_store_by_target).

-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([maybe_store_message/1, reroute_messages/4]).
-export([bounce_queue_size/0]).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: jid:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    ResendAfterMs = proplists:get_value(resend_after_ms, Opts0, 200),
    ResendAfter = erlang:convert_time_unit(ResendAfterMs, millisecond, native),
    Opts = [{resend_after, ResendAfter}, {max_retries, 4} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: jid:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% gen_server API
%%--------------------------------------------------------------------

init(_) ->
    self() ! resend,
    {ok, no_state}.

handle_info(resend, State) ->
    erlang:send_after(100, self(), resend),
    Now = erlang:monotonic_time(),
    resend_messages(Now),
    {noreply, State}.

handle_cast(_Message, _State) ->
    exit(bad_cast).

handle_call(_Message, _From, _State) ->
    exit(bad_call).

code_change(_Version, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ignore.

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec maybe_store_message(drop) -> drop;
                         ({jid:jid(), jid:jid(), mongoose_acc:t(), exml:packet()}) ->
                                 drop | {jid:jid(), jid:jid(), mongoose_acc:t(), exml:packet()}.
maybe_store_message(drop) -> drop;
maybe_store_message({From, To, Acc0, Packet} = FPacket) ->
    LocalHost = opt(local_host),
    {ok, ID} = mod_global_distrib:find_metadata(Acc0, id),
    case mod_global_distrib:get_metadata(Acc0, {bounce_ttl, LocalHost}, opt(max_retries)) of
        0 ->
            FromBin = jid:to_binary(From),
            ToBin = jid:to_binary(To),
            ?DEBUG("Not storing global message id=~s from=~s to=~s as bounce_ttl=0",
                   [ID, FromBin, ToBin]),
            ?ERROR_MSG_IF(To#jid.luser == <<>>,
                          "event=message_to_component_ttl_zero,id=~s,from=~s,to=~s",
                          [ID, FromBin, ToBin]),
            mongoose_metrics:update(global, ?GLOBAL_DISTRIB_STOP_TTL_ZERO, 1),
            FPacket;
        OldTTL ->
            ?DEBUG("Storing global message id=~s from=~s to=~s to "
                   "resend after ~B ms (bounce_ttl=~B)",
                   [ID, jid:to_binary(From), jid:to_binary(To),
                    erlang:convert_time_unit(opt(resend_after), native, millisecond),
                    OldTTL]),
            Acc = mod_global_distrib:put_metadata(Acc0, {bounce_ttl, LocalHost}, OldTTL - 1),
            ResendAt = erlang:monotonic_time() + opt(resend_after),
            do_insert_in_store(ResendAt, {From, To, Acc, Packet}),
            drop
    end.

-spec reroute_messages(SomeAcc :: mongoose_acc:t(),
                       From :: jid:jid(),
                       To :: jid:jid(),
                       TargetHost :: binary()) -> mongoose_acc:t().
reroute_messages(Acc, From, To, TargetHost) ->
    Key = get_index_key(From, To),
    StoredMessages =
        lists:filtermap(
          fun({_, {ResendAt, FPacket}}) ->
                  case ets:take(?MESSAGE_STORE, ResendAt) of
                      [_] -> {true, FPacket};
                      [] -> false
                  end
          end,
          ets:take(?MS_BY_TARGET, Key)),
    ?DEBUG_IF(StoredMessages =/= [],
              "Routing ~B previously stored messages addressed from ~s to ~s",
              [length(StoredMessages), jid:to_binary(From), jid:to_binary(To)]),
    lists:foreach(pa:bind(fun reroute_message/2, TargetHost), StoredMessages),
    Acc.

%%--------------------------------------------------------------------
%% API for metrics
%%--------------------------------------------------------------------

-spec bounce_queue_size() -> non_neg_integer().
bounce_queue_size() ->
    case ets:info(?MESSAGE_STORE, size) of
        undefined -> 0;
        Value -> Value
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec reroute_message(TargetHost :: binary(),
                      FPacket :: {jid:jid(), jid:jid(), mongoose_acc:t(), exml:packet()}) ->
                             any().
reroute_message(TargetHost, {From, To, Acc0, Packet}) ->
    Acc = mod_global_distrib:put_metadata(Acc0, target_host_override, TargetHost),
    ejabberd_router:route(From, To, Acc, Packet).

-spec start() -> any().
start() ->
    Host = opt(global_host),
    mod_global_distrib_utils:create_ets(?MESSAGE_STORE, ordered_set),
    mod_global_distrib_utils:create_ets(?MS_BY_TARGET, bag),
    EvalDef = {[{l, [{t, [value, {v, 'Value'}]}]}],[value]},
    QueueSizeDef = {function, ?MODULE, bounce_queue_size, [], eval, EvalDef},
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_BOUNCE_QUEUE_SIZE, QueueSizeDef),
    ejabberd_hooks:add(mod_global_distrib_unknown_recipient, Host, ?MODULE, maybe_store_message, 80),
    ejabberd_hooks:add(mod_global_distrib_known_recipient, Host, ?MODULE, reroute_messages, 80),
    ChildSpec = {?MODULE, {?MODULE, start_link, []}, permanent, 1000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

-spec stop() -> any().
stop() ->
    Host = opt(global_host),
    ejabberd_sup:stop_child(?MODULE),
    ejabberd_hooks:delete(mod_global_distrib_known_recipient, Host, ?MODULE, reroute_messages, 80),
    ejabberd_hooks:delete(mod_global_distrib_unknown_recipient, Host, ?MODULE, maybe_store_message, 80),
    ets:delete(?MS_BY_TARGET),
    ets:delete(?MESSAGE_STORE).

add_index(ResendAt, {From, To, _Acc, _Packet} = FPacket) ->
    Key = get_index_key(From, To),
    ets:insert(?MS_BY_TARGET, {Key, {ResendAt, FPacket}}).

delete_index(ResendAt, {From, To, _Acc, _Packet} = FPacket) ->
    Key = get_index_key(From, To),
    ets:delete_object(?MS_BY_TARGET, {Key, {ResendAt, FPacket}}).

get_index_key(From, To) ->
    {jid:to_lower(From), jid:to_lower(To)}.

-spec do_insert_in_store(ResendAt :: integer(), {jid:jid(), jid:jid(), mongoose_acc:t(), exml:packet()}) -> any().
do_insert_in_store(ResendAt, FPacket) ->
    case ets:insert_new(?MESSAGE_STORE, {ResendAt, FPacket}) of
        true -> add_index(ResendAt, FPacket);
        false -> do_insert_in_store(ResendAt + 1, FPacket)
    end.

-spec resend_messages(Now :: integer()) -> ok.
resend_messages(Now) ->
    case ets:first(?MESSAGE_STORE) of
        Key when is_integer(Key) andalso Key < Now ->
            case ets:take(?MESSAGE_STORE, Key) of
                [{Key, {From, To, _Acc, _Packet} = FPacket}] ->
                    delete_index(Key, FPacket),
                    mod_global_distrib_mapping:clear_cache(To),
                    WorkerKey = mod_global_distrib_utils:recipient_to_worker_key(From, opt(global_host)),
                    Worker = mod_global_distrib_worker_sup:get_worker(WorkerKey),
                    gen_server:cast(Worker, {route, FPacket});
                _ ->
                    ok
            end,
            resend_messages(Now);
        _ ->
            ok
    end.

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
