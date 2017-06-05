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

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([deps/2, start/2, stop/1, maybe_reroute/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

deps(Host, Opts) ->
    mod_global_distrib_utils:deps(?MODULE, Host, Opts, fun deps/1).

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

maybe_reroute(drop) -> drop;
maybe_reroute({From, To, Acc} = FPacket) ->
    LocalHost = opt(local_host),
    GlobalHost = opt(global_host),
    case lookup_recipients_host(To, LocalHost, GlobalHost) of
        {ok, TargetHost} when TargetHost =/= LocalHost ->
            case mongoose_acc:get(distrib_ttl, Acc, 5) of %% TODO: 5
                0 -> FPacket;
                TTL ->
                    Acc1 = mongoose_acc:put(distrib_ttl, TTL - 1, Acc),
                    mod_global_distrib_sender:send(TargetHost, {From, To, Acc1}),
                    drop
            end;
        _ ->
            FPacket
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

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

start() ->
    ejabberd_hooks:add(filter_packet, global, ?MODULE, maybe_reroute, 99).

stop() ->
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, maybe_reroute, 99).

-spec lookup_recipients_host(jid(), binary(), binary()) -> {ok, binary()} | error.
lookup_recipients_host(#jid{lserver = HostAddressedTo} = To, LocalHost, GlobalHost) ->
    case HostAddressedTo of
        LocalHost -> undefined;
        GlobalHost -> mod_global_distrib_mapping:for_jid(To);
        _ -> mod_global_distrib_mapping:for_domain(HostAddressedTo)
    end.

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
