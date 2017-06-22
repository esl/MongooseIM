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

-module(mod_global_distrib_mapping_redis).
-author('konrad.zemek@erlang-solutions.com').

-include("ejabberd.hrl").

-define(JIDS_ETS, mod_global_distrib_mapping_redis_jids).
-define(DOMAINS_ETS, mod_global_distrib_mapping_redis_domains).

-behaviour(mod_global_distrib_mapping).

-export([start/1, stop/0, refresh/0,
         put_session/1, get_session/1, delete_session/1,
         put_domain/1, get_domain/1, delete_domain/1,
         get_endpoints/1, get_domains/0]).

%% TODO: This module could use some cleanup, e.g. in some places the Host argument
%% is unneeded. Saving domains is probably unneeded too, as we simply refresh using
%% ejabberd_router. Figure out a way to also expire specific entries from endpoints and
%% domains; probably using a separate process similar to refresher.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Opts) ->
    Server = proplists:get_value(server, Opts, "127.0.0.1"),
    Port = proplists:get_value(port, Opts, 8102),
    Password = proplists:get_value(password, Opts, ""),
    PoolSize = proplists:get_value(pool_size, Opts, 1),
    RefreshAfter = proplists:get_value(refresh_after, Opts, 60),

    mod_global_distrib_utils:create_opts_ets(?MODULE),
    mod_global_distrib_utils:create_opts_ets(?JIDS_ETS),
    mod_global_distrib_utils:create_opts_ets(?DOMAINS_ETS),
    ExpireAfter = proplists:get_value(expire_after, Opts, 120),
    ets:insert(?MODULE, {expire_after, ExpireAfter}),

    EredisArgs = [Server, Port, 0, Password, 100, 5000],
    WorkerPool = {?MODULE, {wpool, start_pool, [?MODULE, [{workers, PoolSize},
                                                          {worker, {eredis_client, EredisArgs}}]]},
                  permanent, 10000, supervisor, dynamic},
    {ok, _} = supervisor:start_child(ejabberd_sup, WorkerPool),
    refresh(),
    timer:apply_interval(timer:seconds(RefreshAfter), ?MODULE, refresh, []),
    ok.

stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    [ets:delete(Tab) || Tab <- [?MODULE, ?JIDS_ETS, ?DOMAINS_ETS]].

put_session(Jid) ->
    ets:insert(?JIDS_ETS, {Jid}),
    do_put(Jid).

get_session(Jid) ->
    do_get(Jid).

delete_session(Jid) ->
    ets:delete(?JIDS_ETS, Jid),
    do_delete(Jid).

do_put(Key) ->
    LocalHost = opt(local_host),
    {ok, _} = q([<<"SET">>, Key, LocalHost, <<"EX">>, expire_after()]),
    {ok, LocalHost}.

do_get(Key) ->
    case q([<<"GET">>, Key]) of
        {ok, undefined} -> error;
        {ok, Host} -> {ok, Host}
    end.

do_delete(Key) ->
    LocalHost = opt(local_host),
    case q([<<"GET">>, Key]) of
        {ok, LocalHost} -> {ok, _} = q([<<"DEL">>, Key]);
        _ -> ok
    end.

get_endpoints(Host) ->
    Nodes = [_ | _] = get_nodes(Host), %% TODO: error: unknown host
    EndpointKeys = [endpoints_key(Host, Node) || Node <- Nodes],
    {ok, BinEndpoints} = q([<<"SUNION">> | EndpointKeys]),
    {ok, [binary_to_term(BinEndpoint) || BinEndpoint <- BinEndpoints]}.

refresh() ->
    refresh_hosts(),
    refresh_nodes(),
    refresh_jids(),
    refresh_endpoints(),
    refresh_domains().

refresh_hosts() ->
    q([<<"SADD">>, <<"hosts">>, opt(local_host)]).

get_hosts() ->
    {ok, _} = q([<<"SMEMBERS">>, <<"hosts">>]).

refresh_nodes() ->
    NodesKey = nodes_key(),
    Now = p1_time_compat:system_time(seconds),
    case get_expired_nodes(Now) of
        [] -> ok;
        ExpiredNodes -> q([<<"HDEL">>, NodesKey | ExpiredNodes])
    end,
    q([<<"HSET">>, NodesKey, atom_to_binary(node(), latin1), Now]).

get_nodes(Host) ->
    {ok, Nodes} = q([<<"HKEYS">>, nodes_key(Host)]),
    Nodes.

get_expired_nodes(Now) ->
    {ok, Results} = q([<<"HGETALL">>, nodes_key()]),
    lists:foldl(
      fun
          (Val, [{Key} | Acc]) ->
              Stamp = binary_to_integer(Val),
              case Stamp + expire_after() < Now of
                  true -> Acc;
                  false -> [Key | Acc]
              end;
          (Key, Acc) ->
              [{Key} | Acc]
      end,
      [],
      Results).

refresh_jids() ->
    ets:foldl(fun({Jid}, _) -> refresh_jid(Jid) end, [], ?JIDS_ETS).

refresh_jid(Jid) ->
    LocalHost = opt(local_host),
    q([<<"SET">>, Jid, LocalHost, <<"EX">>, expire_after()]). %% TODO: log error

refresh_endpoints() ->
    EndpointsKey = endpoints_key(),
    BinEndpoints = [term_to_binary(E) || E <- mod_global_distrib_receiver:endpoints()],
    refresh_set(EndpointsKey, BinEndpoints).

refresh_domains() ->
    LocalHost = opt(local_host),
    DomainsKey = domains_key(),
    Domains = [Domain || {Domain} <- ets:tab2list(?DOMAINS_ETS)],
    refresh_set(DomainsKey, Domains),
    lists:foreach(fun(Domain) ->
                          q([<<"SET">>, Domain, LocalHost, <<"EX">>, expire_after()])
                  end,
                  Domains).

refresh_set(Key, Members) ->
    q([<<"PERSIST">>, Key]),

    ToDelete =
        case q([<<"SMEMBERS">>, Key]) of
            {ok, ExistingMembers} ->
                ordsets:subtract(ordsets:from_list(ExistingMembers),
                                 ordsets:from_list(Members));
            _ ->
                []
        end,

    case ToDelete of
        [] -> ok;
        _ -> q([<<"SREM">>, Key | ToDelete])
    end,

    case Members of
        [] -> ok;
        _ -> q([<<"SADD">>, Key | Members])
    end,
    q([<<"EXPIRE">>, Key, expire_after()]).

put_domain(Domain) ->
    ets:insert(?DOMAINS_ETS, {Domain}),
    {ok, _} = q([<<"SADD">>, domains_key(), Domain]),
    do_put(Domain).

get_domain(Domain) ->
    do_get(Domain).

delete_domain(Domain) ->
    ets:delete(?DOMAINS_ETS, Domain),
    do_delete(Domain),
    {ok, _} = q([<<"SREM">>, domains_key(), Domain]),
    ok.

get_domains() ->
    Hosts = get_hosts(),
    Nodes = lists:flatmap(fun(Host) -> [{Host, Node} || Node <- get_nodes(Host)] end, Hosts),
    Keys = [domains_key(Host, Node) || {Host, Node} <- Nodes],
    {ok, _Domains} = q([<<"SUNION">> | Keys]).

q(Args) ->
    {ok, _} = eredis:q(wpool_pool:best_worker(?MODULE), Args).

nodes_key() ->
    LocalHost = opt(local_host),
    nodes_key(LocalHost).

nodes_key(Host) ->
    <<Host/binary, "#{nodes}">>.

endpoints_key() ->
    key(<<"endpoints">>).

endpoints_key(Host, Node) ->
    key(Host, Node, <<"endpoints">>).

domains_key() ->
    key(<<"domains">>).

domains_key(Host, Node) ->
    key(Host, Node, <<"domains">>).

key(Type) ->
    LocalHost = opt(local_host),
    Node = atom_to_binary(node(), latin1),
    key(LocalHost, Node, Type).

key(Host, Node, Type) ->
    <<Host/binary, "#", Node/binary, "#{", Type/binary, "}">>.

opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib_mapping, Key).

expire_after() ->
    ets:lookup_element(?MODULE, expire_after, 2).
