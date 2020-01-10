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

%% Intercepts filter_packet hook with maybe_reroute callback.
-module(mod_global_distrib).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([deps/2, start/2, stop/1]).
-export([find_metadata/2, get_metadata/3, remove_metadata/2, put_metadata/3]).
-export([maybe_reroute/1]).

%%--------------------------------------------------------------------
%% gen_mod API
%% See "gen_mod logic" block below in this file
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


%%--------------------------------------------------------------------
%% public functions
%%--------------------------------------------------------------------

-spec get_metadata(mongoose_acc:t(), Key :: term(), Default :: term()) -> Value :: term().
get_metadata(Acc, Key, Default) ->
    mongoose_acc:get(global_distrib, Key, Default, Acc).

-spec find_metadata(mongoose_acc:t(), Key :: term()) ->
    {ok, Value :: term()} | {error, undefined}.
find_metadata(Acc, Key) ->
    try mongoose_acc:get(global_distrib, Key, Acc) of
        Value -> {ok, Value}
    catch
        _:_ ->
            {error, undefined}
    end.

-spec put_metadata(mongoose_acc:t(), Key :: term(), Value :: term()) -> mongoose_acc:t().
put_metadata(Acc, Key, Value) ->
    mongoose_acc:set_permanent(global_distrib, Key, Value, Acc).

-spec remove_metadata(mongoose_acc:t(), Key :: term()) -> mongoose_acc:t().
remove_metadata(Acc, Key) ->
    mongoose_acc:delete(global_distrib, Key, Acc).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec maybe_reroute(drop) -> drop;
                   ({jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}) ->
    drop | {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}.
maybe_reroute(drop) -> drop;
maybe_reroute({#jid{ luser = SameUser, lserver = SameServer } = _From,
               #jid{ luser = SameUser, lserver = SameServer } = _To,
               _Acc, _Packet} = FPacket) ->
    %% GD is not designed to support two user sessions existing in distinct clusters
    %% and here we explicitly block routing stanzas between them.
    %% Without this clause, test_pm_with_ungraceful_reconnection_to_different_server test
    %% was randomly failing because sometimes 'unavailable' presence from a dead session
    %% was poisoning reg1 cache. In such case, reg1 tried to route locally stanzas
    %% from unacked SM buffer, leading to an error, while a brand new, shiny Eve
    %% on mim1 was waiting.
    FPacket;
maybe_reroute({From, To, _, Packet} = FPacket) ->
    Acc = maybe_initialize_metadata(FPacket),
    {ok, ID} = find_metadata(Acc, id),
    LocalHost = opt(local_host),
    GlobalHost = opt(global_host),
    %% If target_host_override is set (typically when routed out of bounce storage),
    %% host lookup is skipped and messages are routed to target_host_override value.
    TargetHostOverride = get_metadata(Acc, target_host_override, undefined),
    case lookup_recipients_host(TargetHostOverride, To, LocalHost, GlobalHost) of
        {ok, LocalHost} ->
            %% Continue routing with initalized metadata
            ejabberd_hooks:run(mod_global_distrib_known_recipient, GlobalHost,
                               [From, To, LocalHost]),
            ?DEBUG("Routing global message id=~s from=~s to=~s to local datacenter local_host=~ts",
                   [ID, jid:to_binary(From), jid:to_binary(To), LocalHost]),
            {ok, TTL} = find_metadata(Acc, ttl),
            mongoose_metrics:update(global, ?GLOBAL_DISTRIB_DELIVERED_WITH_TTL, TTL),
            {From, To, Acc, Packet};

        {ok, TargetHost} ->
            ejabberd_hooks:run(mod_global_distrib_known_recipient,
                               GlobalHost, [From, To, TargetHost]),
            case find_metadata(Acc, ttl) of
                {ok, 0} ->
                    %% Just continue routing
                    ?INFO_MSG("event=ttl_zero gd_id=~s from=~s to=~s found_at=~s",
                           [ID, jid:to_binary(From), jid:to_binary(To), TargetHost]),
                    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_STOP_TTL_ZERO, 1),
                    FPacket;
                {ok, TTL} ->
                    %% Forward stanza to remote cluster using global distribution
                    ?DEBUG("event=rerouting_global_message gd_id=~s from=~s to=~s to target_host=~s ttl=~B",
                           [ID, jid:to_binary(From), jid:to_binary(To), TargetHost, TTL]),
                    Acc1 = put_metadata(Acc, ttl, TTL - 1),
                    Acc2 = remove_metadata(Acc1, target_host_override),
                    %% KNOWN ISSUE: will crash loudly if there are no connections available
                    %% TODO: Discuss behaviour in such scenario
                    Worker = get_bound_connection_noisy(TargetHost, ID, FPacket),
                    mod_global_distrib_sender:send(Worker, {From, To, Acc2, Packet}),
                    drop
            end;

        error ->
            ?DEBUG("Unable to route global message gd_id=~s from=~s to=~s: "
                   "user not found in the routing table",
                   [ID, jid:to_binary(From), jid:to_binary(To)]),
            ejabberd_hooks:run_fold(mod_global_distrib_unknown_recipient,
                                    GlobalHost, {From, To, Acc, Packet}, [])
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec maybe_initialize_metadata({jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}) -> mongoose_acc:t().
maybe_initialize_metadata({From, To, Acc, Packet}) ->
    case find_metadata(Acc, origin) of
        {error, undefined} ->
            Acc1 = put_metadata(Acc, ttl, opt(message_ttl)),
            ID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
            Acc2 = put_metadata(Acc1, id, ID),
            ?DEBUG("event=gd_init_metadata gd_id=~s from=~p to=~p stanza=~p", [ID, From, To, Packet]),
            put_metadata(Acc2, origin, opt(local_host));
        _ ->
            Acc
    end.

get_bound_connection_noisy(TargetHost, GDID, FPacket) ->
    try get_bound_connection(TargetHost, GDID)
    catch Class:Reason:Stacktrace ->
              ?ERROR_MSG("event=gd_get_process_for_failed "
                         "server=~ts gd_id=~s reason=~p:~1000p  packet=~1000p stacktrace=~1000p",
                           [TargetHost, GDID, Class, Reason, FPacket, Stacktrace]),
              erlang:raise(Class, Reason, Stacktrace)
    end.

-spec get_bound_connection(Server :: jid:lserver(), binary()) -> pid().
get_bound_connection(Server, GDID) when is_binary(GDID) ->
    get_bound_connection(Server, GDID, get({connection, Server})).

-spec get_bound_connection(Server :: jid:lserver(), term(), pid() | undefined) -> pid().
get_bound_connection(Server, GDID, undefined) ->
    Pid = mod_global_distrib_sender:get_process_for(Server),
    put({connection, Server}, Pid),
    ?DEBUG("event=new_bound_connection server=~ts gd_id=~s pid=~p", [Server, GDID, Pid]),
    Pid;
get_bound_connection(Server, GDID, Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ?DEBUG("event=dead_bound_connection server=~ts gd_id=~s pid=~p", [Server, GDID, Pid]),
            get_bound_connection(Server, GDID, undefined);
        true ->
            ?DEBUG("event=reuse_bound_connection server=~ts gd_id=~s pid=~p", [Server, GDID, Pid]),
            Pid
    end.

%%--------------------------------------------------------------------
%% gen_mod logic
%%--------------------------------------------------------------------

-spec deps(Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Opts) ->
    ConnectionsOpts = proplists:get_value(connections, Opts, []),
    CacheOpts = proplists:get_value(cache, Opts, []),
    BounceOpts = proplists:get_value(bounce, Opts, []),

    Deps0 = [{mod_global_distrib_mapping, CacheOpts ++ Opts, hard},
             {mod_global_distrib_disco, Opts, hard},
             {mod_global_distrib_receiver, ConnectionsOpts ++ Opts, hard},
             {mod_global_distrib_sender, ConnectionsOpts ++ Opts, hard},
             {mod_global_distrib_hosts_refresher, Opts, hard}],
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

%%--------------------------------------------------------------------

-spec lookup_recipients_host(TargetHost :: binary() | undefined,
                             To :: jid:jid(),
                             LocalHost :: binary(),
                             GlobalHost :: binary()) ->
    {ok, binary()} | error.
lookup_recipients_host(undefined, To, LocalHost, GlobalHost) ->
    lookup_recipients_host(To, LocalHost, GlobalHost);
lookup_recipients_host(TargetHost, _To, _LocalHost, _GlobalHost) ->
    {ok, TargetHost}.

-spec lookup_recipients_host(To :: jid:jid(),
                             LocalHost :: binary(),
                             GlobalHost :: binary()) -> {ok, binary()} | error.
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
