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

-module(mod_global_distrib).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([deps/2, start/2, stop/1, get_metadata/2, get_metadata/3, remove_metadata/2, put_metadata/3,
         maybe_reroute/1]).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec deps(Host :: jid:server(), Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Host, Opts) ->
    mod_global_distrib_utils:deps(?MODULE, Host, Opts, fun deps/1).

-spec start(Host :: jid:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    Opts = [{message_ttl, 4} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: jid:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

-spec get_metadata(mongoose_acc:t(), Key :: term(), Default :: term()) -> Value :: term().
get_metadata(Acc, Key, Default) ->
    GD = mongoose_acc:get(global_distrib, Acc),
    maps:get(Key, GD, Default).

-spec get_metadata(mongoose_acc:t(), Key :: term()) -> Value :: term().
get_metadata(Acc, Key) ->
    GD = mongoose_acc:get(global_distrib, Acc),
    maps:get(Key, GD).

-spec put_metadata(mongoose_acc:t(), Key :: term(), Value :: term()) -> mongoose_acc:t().
put_metadata(Acc, Key, Value) ->
    GD0 = mongoose_acc:get(global_distrib, Acc),
    GD = maps:put(Key, Value, GD0),
    mongoose_acc:put(global_distrib, GD, Acc).

-spec remove_metadata(mongoose_acc:t(), Key :: term()) -> mongoose_acc:t().
remove_metadata(Acc, Key) ->
    GD0 = mongoose_acc:get(global_distrib, Acc),
    GD = maps:remove(Key, GD0),
    mongoose_acc:put(global_distrib, GD, Acc).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec maybe_reroute(drop) -> drop;
                   ({jid:jid(), jid:jid(), mongoose_acc:t()}) -> drop | {jid:jid(), jid:jid(), mongoose_acc:t()}.
maybe_reroute(drop) -> drop;
maybe_reroute({From, To, Acc0, Packet} = FPacket) ->
    Acc = maybe_initialize_metadata(Acc0),
    LocalHost = opt(local_host),
    GlobalHost = opt(global_host),
    case lookup_recipients_host(To, LocalHost, GlobalHost) of
        {ok, LocalHost} ->
            ejabberd_hooks:run(mod_global_distrib_known_recipient, GlobalHost, [From, To]),
            ?DEBUG("Routing global message id=~s from=~s to=~s to local datacenter",
                   [get_metadata(Acc, id), jid:to_binary(From), jid:to_binary(To)]),
            mongoose_metrics:update(global, ?GLOBAL_DISTRIB_DELIVERED_WITH_TTL,
                                    get_metadata(Acc, ttl)),
            {From, To, Acc, Packet};

        {ok, TargetHost} ->
            ejabberd_hooks:run(mod_global_distrib_known_recipient, GlobalHost, [From, To]),
            case get_metadata(Acc, ttl) of
                0 ->
                    ?INFO_MSG("event=ttl_zero,id=~s,from=~s,to=~s,found_at=~s",
                           [get_metadata(Acc, id), jid:to_binary(From),
                            jid:to_binary(To), TargetHost]),
                    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_STOP_TTL_ZERO, 1),
                    FPacket;
                TTL ->
                    ?DEBUG("Rerouting global message id=~s from=~s to=~s to ~s (TTL: ~B)",
                           [get_metadata(Acc, id), jid:to_binary(From),
                            jid:to_binary(To), TargetHost, TTL]),
                    Acc1 = put_metadata(Acc, ttl, TTL - 1),
                    Worker = get_bound_connection(TargetHost),
                    mod_global_distrib_sender:send(Worker, {From, To, Acc1, Packet}),
                    drop
            end;

        error ->
            ?DEBUG("Unable to route global message id=~s from=~s to=~s: "
                   "user not found in the routing table",
                   [get_metadata(Acc, id), jid:to_binary(From), jid:to_binary(To)]),
            ejabberd_hooks:run_fold(mod_global_distrib_unknown_recipient,
                                    GlobalHost, {From, To, Acc, Packet}, [])
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec maybe_initialize_metadata(mongoose_acc:t()) -> mongoose_acc:t().
maybe_initialize_metadata(Acc) ->
    case mongoose_acc:get(global_distrib, Acc, undefined) of
        #{} -> Acc;
        undefined ->
            Metadata = #{ttl => opt(message_ttl),
                         id => uuid:uuid_to_string(uuid:get_v4(), binary_standard),
                         origin => opt(local_host)},
            mongoose_acc:put(global_distrib, Metadata, Acc)
    end.

-spec get_bound_connection(Server :: jid:lserver()) -> pid().
get_bound_connection(Server) ->
    get_bound_connection(Server, get({connection, Server})).

-spec get_bound_connection(Server :: jid:lserver(), pid() | undefined) -> pid().
get_bound_connection(Server, undefined) ->
    Pid = mod_global_distrib_sender:get_process_for(Server),
    put({connection, Server}, Pid),
    Pid;
get_bound_connection(Server, Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false -> get_bound_connection(Server, undefined);
        true -> Pid
    end.

-spec deps(Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Opts) ->
    ConnectionsOpts = proplists:get_value(connections, Opts, []),
    CacheOpts = proplists:get_value(cache, Opts, []),
    BounceOpts = proplists:get_value(bounce, Opts, []),

    Deps0 = [{mod_global_distrib_mapping, CacheOpts ++ Opts, hard},
             {mod_global_distrib_disco, Opts, hard},
             {mod_global_distrib_receiver, ConnectionsOpts ++ Opts, hard},
             {mod_global_distrib_sender, ConnectionsOpts ++ Opts, hard}],
    case BounceOpts of
        false -> Deps0;
        _ -> [{mod_global_distrib_bounce, BounceOpts ++ Opts, hard} | Deps0]
    end.

-spec start() -> any().
start() ->
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_DELIVERED_WITH_TTL, histogram),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_STOP_TTL_ZERO, spiral),
    ejabberd_hooks:add(filter_packet, global, ?MODULE, maybe_reroute, 99).

-spec stop() -> any().
stop() ->
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, maybe_reroute, 99).

-spec lookup_recipients_host(jid:jid(), binary(), binary()) -> {ok, binary()} | error.
lookup_recipients_host(#jid{luser = <<>>, lserver = LServer}, LocalHost, GlobalHost)
  when LServer == LocalHost; LServer == GlobalHost ->
    {ok, LocalHost};
lookup_recipients_host(#jid{luser = <<>>, lserver = HostAddressedTo}, _LocalHost, _GlobalHost) ->
    mod_global_distrib_mapping:for_domain(HostAddressedTo);
lookup_recipients_host(#jid{lserver = HostAddressedTo} = To, LocalHost, GlobalHost) ->
    case HostAddressedTo of
        LocalHost -> {ok, LocalHost};
        GlobalHost -> mod_global_distrib_mapping:for_jid(To);
        _ -> mod_global_distrib_mapping:for_domain(HostAddressedTo)
    end.

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
