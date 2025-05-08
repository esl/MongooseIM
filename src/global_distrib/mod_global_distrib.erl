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

%% Intercepts filter_packet hook.
-module(mod_global_distrib).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("global_distrib_metrics.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-export([deps/2, start/2, stop/1, config_spec/0, instrumentation/0]).
-export([find_metadata/2, get_metadata/3, remove_metadata/2, put_metadata/3]).
-export([filter_packet/3]).
-export([process_opts/1, process_endpoint/1]).

-ignore_xref([remove_metadata/2, instrumentation/0]).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

%% Note: while this module should be enabled for all hosts,
%% it needs to be started only once - this is why deps/2 and start/2
%% do nothing for hosts other than global_host

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(HostType, Opts = #{global_host := HostType, bounce := BounceOpts}) ->
    %% Start each required module with the same opts for simplicity
    [{Mod, Opts, hard} || Mod <- dep_modules() ++ bounce_modules(BounceOpts)];
deps(_HostType, #{}) ->
    [].

dep_modules() ->
    [mod_global_distrib_utils, mod_global_distrib_mapping, mod_global_distrib_disco,
     mod_global_distrib_receiver, mod_global_distrib_hosts_refresher].

bounce_modules(#{enabled := true}) -> [mod_global_distrib_bounce];
bounce_modules(#{enabled := false}) -> [].

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, #{global_host := HostType}) ->
    {ok, _} = application:ensure_all_started([cpool, backoff], permanent),
    mongoose_instrument:set_up(instrumentation()),
    gen_hook:add_handlers(hooks());
start(_HostType, #{}) ->
    ok.

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, global_host) of
        HostType ->
            mongoose_instrument:tear_down(instrumentation()),
            gen_hook:delete_handlers(hooks());
        _ -> ok
    end.

-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    [{?GLOBAL_DISTRIB_DELIVERED_WITH_TTL, #{}, #{metrics => #{value => histogram}}},
     {?GLOBAL_DISTRIB_STOP_TTL_ZERO, #{}, #{metrics => #{count => spiral}}}].

hooks() ->
    %% filter_packet is called in mongoose_router_global as a first
    %% element of the routing chain.
    %% mongoose_router_localdomain is called next.
    [{filter_packet, global, fun ?MODULE:filter_packet/3, #{}, 99}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"global_host">> => #option{type = binary,
                                               validate = domain},
                  <<"local_host">> => #option{type = binary,
                                              validate = domain},
                  <<"message_ttl">> => #option{type = integer,
                                               validate = non_negative},
                  <<"hosts_refresh_interval">> => #option{type = integer,
                                                          validate = non_negative},
                  <<"connections">> => connections_spec(),
                  <<"redis">> => redis_spec(),
                  <<"cache">> => cache_spec(),
                  <<"bounce">> => bounce_spec()
        },
        required = [<<"global_host">>, <<"local_host">>],
        defaults = #{<<"message_ttl">> => 4,
                     <<"hosts_refresh_interval">> => 3000},
        process = fun ?MODULE:process_opts/1
    }.

connections_spec() ->
    #section{
        items = #{<<"endpoints">> => #list{items = endpoint_spec()},
                  <<"advertised_endpoints">> => #list{items = endpoint_spec()},
                  <<"connections_per_endpoint">> => #option{type = integer,
                                                            validate = non_negative},
                  <<"endpoint_refresh_interval">> => #option{type = integer,
                                                             validate = positive},
                  <<"endpoint_refresh_interval_when_empty">> => #option{type = integer,
                                                                        validate = positive},
                  <<"disabled_gc_interval">> => #option{type = integer,
                                                        validate = positive},
                  <<"tls">> => tls_spec()
                 },
        defaults = #{<<"connections_per_endpoint">> => 1,
                     <<"endpoint_refresh_interval">> => 60,
                     <<"endpoint_refresh_interval_when_empty">> => 3,
                     <<"disabled_gc_interval">> => 60},
        include = always
    }.

endpoint_spec() ->
    #section{
        items = #{<<"host">> => #option{type = string,
                                        validate = network_address},
                  <<"port">> => #option{type = integer,
                                        validate = port}
        },
        required = all,
        process = fun ?MODULE:process_endpoint/1
    }.

tls_spec() ->
    mongoose_config_spec:tls([client, server]).

redis_spec() ->
    #section{
        items = #{<<"pool">> => #option{type = atom,
                                        validate = pool_name},
                  <<"expire_after">> => #option{type = integer,
                                                validate = positive},
                  <<"refresh_after">> => #option{type = integer,
                                                 validate = non_negative}
                 },
        defaults = #{<<"pool">> => global_distrib,
                     <<"expire_after">> => 120,
                     <<"refresh_after">> => 60},
        include = always
    }.

cache_spec() ->
    #section{
        items = #{<<"cache_missed">> => #option{type = boolean},
                  <<"domain_lifetime_seconds">> => #option{type = integer,
                                                           validate = non_negative},
                  <<"jid_lifetime_seconds">> => #option{type = integer,
                                                        validate = non_negative},
                  <<"max_jids">> => #option{type = integer,
                                            validate = non_negative}
                 },
        defaults = #{<<"cache_missed">> => true,
                     <<"domain_lifetime_seconds">> => 600,
                     <<"jid_lifetime_seconds">> => 5,
                     <<"max_jids">> => 10000
                    },
        include = always
    }.

bounce_spec() ->
    #section{
        items = #{<<"enabled">> => #option{type = boolean},
                  <<"resend_after_ms">> => #option{type = integer,
                                                   validate = non_negative},
                  <<"max_retries">> => #option{type = integer,
                                               validate = non_negative}
                 },
        defaults = #{<<"enabled">> => true,
                     <<"resend_after_ms">> => 200,
                     <<"max_retries">> => 4},
        include = always
    }.

-spec process_opts(gen_mod:module_opts()) -> gen_mod:module_opts().
process_opts(Opts = #{local_host := LocalHost, connections := Connections}) ->
    Opts#{connections := process_connections(LocalHost, Connections)}.

process_connections(_LocalHost, Opts = #{advertised_endpoints := _,
                                         endpoints := Endpoints}) ->
    Opts#{resolved_endpoints => mod_global_distrib_utils:resolve_endpoints(Endpoints)};
process_connections(LocalHost, Opts = #{endpoints := Endpoints}) ->
    process_connections(LocalHost, Opts#{advertised_endpoints => Endpoints});
process_connections(LocalHost, Opts) ->
    process_connections(LocalHost, Opts#{endpoints => [{binary_to_list(LocalHost), 5555}]}).

-spec process_endpoint(map()) -> mod_global_distrib_utils:endpoint().
process_endpoint(#{host := Host, port := Port}) ->
    {Host, Port}.

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

-spec filter_packet(FPacket, Params, Extra) -> {ok, FPacket} | {stop, drop} when
                FPacket :: mongoose_hooks:filter_packet_acc(),
                Params :: map(),
                Extra :: map().
filter_packet({#jid{ luser = SameUser, lserver = SameServer } = _From,
               #jid{ luser = SameUser, lserver = SameServer } = _To,
               _Acc, _Packet} = FPacket, _, _) ->
    %% GD is not designed to support two user sessions existing in distinct clusters
    %% and here we explicitly block routing stanzas between them.
    %% Without this clause, test_pm_with_ungraceful_reconnection_to_different_server test
    %% was randomly failing because sometimes 'unavailable' presence from a dead session
    %% was poisoning reg1 cache. In such case, reg1 tried to route locally stanzas
    %% from unacked SM buffer, leading to an error, while a brand new, shiny Eve
    %% on mim1 was waiting.
    {ok, FPacket};
filter_packet({From, To, _, Packet} = FPacket, _, _) ->
    Acc = maybe_initialize_metadata(FPacket),
    {ok, ID} = find_metadata(Acc, id),
    LocalHost = opt(local_host),
    GlobalHost = opt(global_host),
    %% If target_host_override is set (typically when routed out of bounce storage),
    %% host lookup is skipped and messages are routed to target_host_override value.
    TargetHostOverride = get_metadata(Acc, target_host_override, undefined),
    ResultFPacket = case lookup_recipients_host(TargetHostOverride, To, LocalHost, GlobalHost) of
        {ok, LocalHost} ->
            {ok, TTL} = find_metadata(Acc, ttl),
            mongoose_instrument:execute(?GLOBAL_DISTRIB_DELIVERED_WITH_TTL, #{}, #{value => TTL, from => From}),

            %% Continue routing with initialized metadata
            mongoose_hooks:mod_global_distrib_known_recipient(GlobalHost,
                                                              From, To, LocalHost),
            ?LOG_DEBUG(#{what => gd_route_local,
                         text => <<"Routing global message to local datacenter">>,
                         gd_id => ID, local_host => LocalHost, acc => Acc}),
            {From, To, Acc, Packet};

        {ok, TargetHost} ->
            mongoose_hooks:mod_global_distrib_known_recipient(GlobalHost,
                                                              From, To, TargetHost),
            case find_metadata(Acc, ttl) of
                {ok, 0} ->
                    %% Just continue routing
                    ?LOG_INFO(#{what => gd_route_ttl_zero,
                                text => <<"Skip global distribution">>,
                                gd_id => ID, acc => Acc, target_host => TargetHost}),
                    mongoose_instrument:execute(?GLOBAL_DISTRIB_STOP_TTL_ZERO, #{}, #{count => 1}),
                    FPacket;
                {ok, TTL} ->
                    ?LOG_DEBUG(#{what => gd_reroute, ttl => TTL,
                                 text => <<"Forward stanza to remote cluster "
                                           "using global distribution">>,
                                 gd_id => ID, acc => Acc, target_host => TargetHost}),
                    Acc1 = put_metadata(Acc, ttl, TTL - 1),
                    Acc2 = remove_metadata(Acc1, target_host_override),
                    %% KNOWN ISSUE: will crash loudly if there are no connections available
                    %% TODO: Discuss behaviour in such scenario
                    Worker = get_bound_connection_noisy(TargetHost, ID, Acc2),
                    mod_global_distrib_sender:send(Worker, {From, To, Acc2, Packet}),
                    drop
            end;

        error ->
            ?LOG_DEBUG(#{what => gd_route_failed, gd_id => ID, acc => Acc,
                         text => <<"Unable to route global: user not found in the routing table">>}),
            mongoose_hooks:mod_global_distrib_unknown_recipient(GlobalHost, {From, To, Acc, Packet})
    end,
    case ResultFPacket of
        drop ->
            {stop, drop};
        ResultFPacket ->
            {ok, ResultFPacket}
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec maybe_initialize_metadata({jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}) -> mongoose_acc:t().
maybe_initialize_metadata({_From, _To, Acc, _Packet}) ->
    case find_metadata(Acc, origin) of
        {error, undefined} ->
            Acc1 = put_metadata(Acc, ttl, opt(message_ttl)),
            ID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
            Acc2 = put_metadata(Acc1, id, ID),
            ?LOG_DEBUG(#{what => gd_init_metadata, gd_id => ID, acc => Acc}),
            put_metadata(Acc2, origin, opt(local_host));
        _ ->
            Acc
    end.

get_bound_connection_noisy(TargetHost, GDID, Acc) ->
    try get_bound_connection(TargetHost, GDID)
    catch Class:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => gd_get_process_for_failed,
                           gd_id => GDID, acc => Acc, target_host => TargetHost,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
              erlang:raise(Class, Reason, Stacktrace)
    end.

-spec get_bound_connection(Server :: jid:lserver(), binary()) -> pid().
get_bound_connection(Server, GDID) when is_binary(GDID) ->
    get_bound_connection(Server, GDID, get({connection, Server})).

-spec get_bound_connection(Server :: jid:lserver(), term(), pid() | undefined) -> pid().
get_bound_connection(Server, GDID, undefined) ->
    Pid = mod_global_distrib_sender:get_process_for(Server),
    put({connection, Server}, Pid),
    ?LOG_DEBUG(#{what => gd_new_bound_connection,
                 server => Server, gd_id => GDID, gd_pid => Pid}),
    Pid;
get_bound_connection(Server, GDID, Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ?LOG_DEBUG(#{what => gd_dead_bound_connection,
                         server => Server, gd_id => GDID, gd_pid => Pid}),
            get_bound_connection(Server, GDID, undefined);
        true ->
            ?LOG_DEBUG(#{what => gd_reuse_bound_connection,
                         server => Server, gd_id => GDID, gd_pid => Pid}),
            Pid
    end.

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
