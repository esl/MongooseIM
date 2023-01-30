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

-export([start_link/0, start/2, stop/1, hooks/1, deps/2]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([maybe_store_message/3, reroute_messages/3]).
-export([bounce_queue_size/0]).

-ignore_xref([bounce_queue_size/0, start_link/0]).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(_HostType, _Opts) ->
    mod_global_distrib_utils:create_ets(?MESSAGE_STORE, ordered_set),
    mod_global_distrib_utils:create_ets(?MS_BY_TARGET, bag),
    EvalDef = {[{l, [{t, [value, {v, 'Value'}]}]}], [value]},
    QueueSizeDef = {function, ?MODULE, bounce_queue_size, [], eval, EvalDef},
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_BOUNCE_QUEUE_SIZE, QueueSizeDef),
    ChildSpec = {?MODULE, {?MODULE, start_link, []}, permanent, 1000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

-spec stop(mongooseim:host_type()) -> any().
stop(_HostType) ->
    ejabberd_sup:stop_child(?MODULE),
    ets:delete(?MS_BY_TARGET),
    ets:delete(?MESSAGE_STORE).

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    [{mod_global_distrib_utils, Opts, hard}].

hooks(HostType) ->
    [{mod_global_distrib_unknown_recipient, HostType, fun ?MODULE:maybe_store_message/3, #{}, 80},
     {mod_global_distrib_known_recipient, HostType, fun ?MODULE:reroute_messages/3, #{}, 80}].

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

-spec maybe_store_message(FPacket, Params, Extra) -> {ok, FPacket} | {stop, drop} when
                        FPacket :: mongoose_hooks:filter_packet_acc(),
                        Params :: map(),
                        Extra :: map().
maybe_store_message({From, To, Acc0, Packet} = FPacket, _, _) ->
    LocalHost = opt(local_host),
    {ok, ID} = mod_global_distrib:find_metadata(Acc0, id),
    case mod_global_distrib:get_metadata(Acc0, {bounce_ttl, LocalHost},
                                         opt([bounce, max_retries])) of
        0 ->
            ?LOG_DEBUG(#{what => gd_skip_store_message,
                         text => <<"Not storing global message">>,
                         gd_id => ID, acc => Acc0, bounce_ttl => 0}),
            ?LOG_IF(error, To#jid.luser == <<>>,
                    #{what => gd_message_to_component_ttl_zero,
                      gd_id => ID, acc => Acc0}),
            mongoose_metrics:update(global, ?GLOBAL_DISTRIB_STOP_TTL_ZERO, 1),
            {ok, FPacket};
        OldTTL ->
            ResendAfterMs = opt([bounce, resend_after_ms]),
            ?LOG_DEBUG(#{what => gd_store_message,
                         text => <<"Storing global message">>,
                         gd_id => ID, acc => Acc0, bounce_ttl => OldTTL,
                         resend_after_ms => ResendAfterMs}),
            Acc = mod_global_distrib:put_metadata(Acc0, {bounce_ttl, LocalHost}, OldTTL - 1),
            ResendAfter = erlang:convert_time_unit(ResendAfterMs, millisecond, native),
            ResendAt = erlang:monotonic_time() + ResendAfter,
            do_insert_in_store(ResendAt, {From, To, Acc, Packet}),
            {stop, drop}
    end.

-spec reroute_messages(Acc, Params, Extra) -> {ok, Acc} when
                    Acc :: mongoose_acc:t(),
                    Params :: #{from := jid:jid(), to := jid:jid(), target_host := binary()},
                    Extra :: map().
reroute_messages(Acc, #{from := From, to := To, target_host := TargetHost}, _) ->
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
    ?LOG_IF(debug, StoredMessages =/= [],
            #{what => gd_route_stored,
              text => <<"Routing multiple previously stored messages">>,
              stored_messages_length => length(StoredMessages), acc => Acc}),
    lists:foreach(pa:bind(fun reroute_message/2, TargetHost), StoredMessages),
    {ok, Acc}.

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
                      FPacket :: {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}) ->
                             any().
reroute_message(TargetHost, {From, To, Acc0, Packet}) ->
    Acc = mod_global_distrib:put_metadata(Acc0, target_host_override, TargetHost),
    ejabberd_router:route(From, To, Acc, Packet).

add_index(ResendAt, {From, To, _Acc, _Packet} = FPacket) ->
    Key = get_index_key(From, To),
    ets:insert(?MS_BY_TARGET, {Key, {ResendAt, FPacket}}).

delete_index(ResendAt, {From, To, _Acc, _Packet} = FPacket) ->
    Key = get_index_key(From, To),
    ets:delete_object(?MS_BY_TARGET, {Key, {ResendAt, FPacket}}).

get_index_key(From, To) ->
    {jid:to_lower(From), jid:to_lower(To)}.

-spec do_insert_in_store(ResendAt :: integer(),
                         {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}) -> any().
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
                    WorkerKey = mod_global_distrib_utils:recipient_to_worker_key(
                                  From, opt(global_host)),
                    Worker = mod_global_distrib_worker_sup:get_worker(WorkerKey),
                    gen_server:cast(Worker, {route, FPacket});
                _ ->
                    ok
            end,
            resend_messages(Now);
        _ ->
            ok
    end.

-spec opt(gen_mod:opt_key() | gen_mod:key_path()) -> gen_mod:opt_value().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
