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

-behaviour(mod_global_distrib_mapping).

-include("mongoose.hrl").

-define(JIDS_ETS, mod_global_distrib_mapping_redis_jids).
-define(DOMAINS_ETS, mod_global_distrib_mapping_redis_domains).

-export([start/1, stop/0,
         put_session/1, get_session/1, delete_session/1,
         put_domain/1, get_domain/1, delete_domain/1,
         get_endpoints/1, get_domains/0]).

-export([init/1, handle_info/2]).

%% Only for debug & tests!
-export([nodes_key/0, set_endpoints/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(proplists:proplist()) -> any().
start(Opts) ->
    Server = proplists:get_value(server, Opts, "127.0.0.1"),
    Port = proplists:get_value(port, Opts, 8102),
    Password = proplists:get_value(password, Opts, ""),
    PoolSize = proplists:get_value(pool_size, Opts, 1),
    RefreshAfter = proplists:get_value(refresh_after, Opts, 60),

    mod_global_distrib_utils:create_ets([?MODULE, ?JIDS_ETS, ?DOMAINS_ETS]),
    ExpireAfter = proplists:get_value(expire_after, Opts, 120),
    ets:insert(?MODULE, {expire_after, ExpireAfter}),

    EredisArgs = [Server, Port, 0, Password, 100, 5000],
    WorkerPool = {?MODULE,
                  {wpool, start_pool, [?MODULE, [{workers, PoolSize},
                                                 {worker, {eredis_client, EredisArgs}}]]},
                  permanent, 10000, supervisor, dynamic},
    Refresher = {mod_global_distrib_redis_refresher,
                 {gen_server, start_link, [?MODULE, RefreshAfter, []]},
                 permanent, 1000, supervisor, [?MODULE]},
    {ok, _} = supervisor:start_child(ejabberd_sup, WorkerPool),
    {ok, _} = supervisor:start_child(ejabberd_sup, Refresher),
    ok.

-spec stop() -> any().
stop() ->
    lists:foreach(
     fun(Id) ->
             supervisor:terminate_child(ejabberd_sup, Id),
             supervisor:delete_child(ejabberd_sup, Id)
     end,
      [?MODULE, mod_global_distrib_redis_refresher]),
    [ets:delete(Tab) || Tab <- [?MODULE, ?JIDS_ETS, ?DOMAINS_ETS]].

-spec put_session(Jid :: binary()) -> ok.
put_session(Jid) ->
    ets:insert(?JIDS_ETS, {Jid}),
    do_put(Jid, opt(local_host)).

-spec get_session(Jid :: binary()) -> {ok, Host :: jid:lserver()} | error.
get_session(Jid) ->
    do_get(Jid).

-spec delete_session(Jid :: binary()) -> ok.
delete_session(Jid) ->
    ets:delete(?JIDS_ETS, Jid),
    do_delete(Jid).

put_domain(Domain) ->
    ets:insert(?DOMAINS_ETS, {Domain}),
    {ok, _} = q([<<"SADD">>, domains_key(), Domain]),
    do_put(Domain, opt(local_host)).

-spec get_domain(Domain :: binary()) -> {ok, Host :: jid:lserver()} | error.
get_domain(Domain) ->
    do_get(Domain).

-spec delete_domain(Domain :: binary()) -> ok.
delete_domain(Domain) ->
    ets:delete(?DOMAINS_ETS, Domain),
    do_delete(Domain),
    {ok, _} = q([<<"SREM">>, domains_key(), Domain]),
    ok.

-spec get_domains() -> {ok, [Domain :: binary()]}.
get_domains() ->
    Hosts = get_hosts(),
    Nodes = lists:flatmap(fun(Host) -> [{Host, Node} || Node <- get_nodes(Host)] end, Hosts),
    Keys = [domains_key(Host, Node) || {Host, Node} <- Nodes],
    {ok, _Domains} = q([<<"SUNION">> | Keys]).

-spec get_endpoints(Host :: jid:lserver()) -> {ok, [mod_global_distrib_utils:endpoint()]}.
get_endpoints(Host) ->
    Nodes = [_ | _] = get_nodes(Host), %% TODO: error: unknown host
    EndpointKeys = [endpoints_key(Host, Node) || Node <- Nodes],
    {ok, BinEndpoints} = q([<<"SUNION">> | EndpointKeys]),
    {ok, lists:map(fun binary_to_endpoint/1, BinEndpoints)}.

%%--------------------------------------------------------------------
%% gen_server API
%%--------------------------------------------------------------------

init(RefreshAfter) ->
    handle_info(refresh, RefreshAfter),
    {ok, RefreshAfter}.

handle_info(refresh, RefreshAfter) ->
    ?INFO_MSG("event=refreshing_hosts", []),
    refresh_hosts(),
    ?INFO_MSG("event=refreshing_nodes", []),
    refresh_nodes(),
    ?INFO_MSG("event=refreshing_jids", []),
    refresh_jids(),
    ?INFO_MSG("event=refreshing_endpoints", []),
    refresh_endpoints(),
    ?INFO_MSG("event=refreshing_domains", []),
    refresh_domains(),
    ?INFO_MSG("event=refreshing_done,next_refresh_in=~p", [RefreshAfter]),
    erlang:send_after(timer:seconds(RefreshAfter), self(), refresh),
    {noreply, RefreshAfter}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec q(Args :: list()) -> {ok, term()}.
q(Args) ->
    case eredis:q(wpool_pool:best_worker(?MODULE), Args) of
        {ok, _} = OKRes ->
            OKRes;
        Error ->
            ?ERROR_MSG("event=redis_query_error,query='~p',error='~p'",
                       [Args, Error]),
            error(Error)
    end.

-spec nodes_key() -> binary().
nodes_key() ->
    LocalHost = opt(local_host),
    nodes_key(LocalHost).

-spec nodes_key(Host :: jid:lserver()) -> binary().
nodes_key(Host) ->
    <<Host/binary, "#{nodes}">>.

-spec endpoints_key() -> binary().
endpoints_key() ->
    key(<<"endpoints">>).

-spec endpoints_key(Host :: jid:lserver(), Node :: binary()) -> binary().
endpoints_key(Host, Node) ->
    key(Host, Node, <<"endpoints">>).

-spec domains_key() -> binary().
domains_key() ->
    key(<<"domains">>).

-spec domains_key(Host :: jid:lserver(), Node :: binary()) -> binary().
domains_key(Host, Node) ->
    key(Host, Node, <<"domains">>).

-spec key(Type :: binary()) -> binary().
key(Type) ->
    LocalHost = opt(local_host),
    Node = atom_to_binary(node(), latin1),
    key(LocalHost, Node, Type).

-spec key(Host :: jid:lserver(), Node :: binary(), Type :: binary()) -> binary().
key(Host, Node, Type) ->
    <<Host/binary, "#", Node/binary, "#{", Type/binary, "}">>.

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib_mapping, Key).

-spec expire_after() -> pos_integer().
expire_after() ->
    ets:lookup_element(?MODULE, expire_after, 2).

-spec do_put(Key :: binary(), Host :: binary()) -> ok.
do_put(Key, Host) ->
    {ok, _} = q([<<"SET">>, Key, Host, <<"EX">>, expire_after()]),
    ok.

-spec do_get(Key :: binary()) -> {ok, Host :: jid:lserver()} | error.
do_get(Key) ->
    case q([<<"GET">>, Key]) of
        {ok, undefined} -> error;
        {ok, Host} -> {ok, Host}
    end.

-spec do_delete(Key :: binary()) -> ok.
do_delete(Key) ->
    LocalHost = opt(local_host),
    case q([<<"GET">>, Key]) of
        {ok, LocalHost} -> {ok, _} = q([<<"DEL">>, Key]);
        _ -> ok
    end,
    ok.

-spec refresh_hosts() -> any().
refresh_hosts() ->
    q([<<"SADD">>, <<"hosts">>, opt(local_host)]).

-spec get_hosts() -> [Host :: jid:lserver()].
get_hosts() ->
    {ok, Hosts} = q([<<"SMEMBERS">>, <<"hosts">>]),
    Hosts.

-spec refresh_nodes() -> any().
refresh_nodes() ->
    NodesKey = nodes_key(),
    Now = p1_time_compat:system_time(seconds),
    case get_expired_nodes(Now) of
        [] -> ok;
        ExpiredNodes -> q([<<"HDEL">>, NodesKey | ExpiredNodes])
    end,
    q([<<"HSET">>, NodesKey, atom_to_binary(node(), latin1), Now]).

-spec get_nodes(Host :: jid:lserver()) -> [Node :: binary()].
get_nodes(Host) ->
    {ok, Nodes} = q([<<"HKEYS">>, nodes_key(Host)]),
    Nodes.

-spec get_expired_nodes(Now :: integer()) -> [Node :: binary()].
get_expired_nodes(Now) ->
    {ok, Results} = q([<<"HGETALL">>, nodes_key()]),
    lists:foldl(
      fun
          (Val, [{Key} | Acc]) ->
              Stamp = binary_to_integer(Val),
              case Stamp + expire_after() > Now of
                  true -> Acc;
                  false -> [Key | Acc]
              end;
          (Key, Acc) ->
              [{Key} | Acc]
      end,
      [],
      Results).

-spec refresh_jids() -> any().
refresh_jids() ->
    ets:foldl(fun({Jid}, _) -> refresh_jid(Jid) end, [], ?JIDS_ETS).

-spec refresh_jid(Jid :: binary()) -> any().
refresh_jid(Jid) ->
    LocalHost = opt(local_host),
    q([<<"SET">>, Jid, LocalHost, <<"EX">>, expire_after()]). %% TODO: log error

-spec refresh_endpoints() -> any().
refresh_endpoints() ->
    set_endpoints(mod_global_distrib_receiver:endpoints()).

-spec set_endpoints(Endpoints :: [mod_global_distrib_utils:endpoint()]) -> any().
set_endpoints(Endpoints) ->
    EndpointsKey = endpoints_key(),
    BinEndpoints = lists:map(fun endpoint_to_binary/1, Endpoints),
    refresh_set(EndpointsKey, BinEndpoints).

-spec endpoint_to_binary(mod_global_distrib_utils:endpoint()) -> binary().
endpoint_to_binary({IpAddr, Port}) ->
    iolist_to_binary([inet:ntoa(IpAddr), "#", integer_to_binary(Port)]).

-spec binary_to_endpoint(binary()) -> mod_global_distrib_utils:endpoint().
binary_to_endpoint(Bin) ->
    [Addr, BinPort] = binary:split(Bin, <<"#">>),
    {ok, IpAddr} = inet:parse_address(binary_to_list(Addr)),
    Port = binary_to_integer(BinPort),
    {IpAddr, Port}.

-spec refresh_domains() -> any().
refresh_domains() ->
    LocalHost = opt(local_host),
    DomainsKey = domains_key(),
    Domains = [Domain || {Domain} <- ets:tab2list(?DOMAINS_ETS)],
    refresh_set(DomainsKey, Domains),
    lists:foreach(fun(Domain) ->
                          q([<<"SET">>, Domain, LocalHost, <<"EX">>, expire_after()])
                  end,
                  Domains).

-spec refresh_set(Key :: binary(), Members :: [binary()]) -> any().
refresh_set(Key, Members) ->
    q([<<"PERSIST">>, Key]),

    ToDelete =
        case catch q([<<"SMEMBERS">>, Key]) of
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

