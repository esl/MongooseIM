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
-define(PUBLIC_DOMAINS_ETS, mod_global_distrib_mapping_redis_public_domains).

-export([start/1, stop/0,
         put_session/1, get_session/1, delete_session/1,
         put_domain/2, get_domain/1, delete_domain/1,
         get_endpoints/1, get_domains/0, get_public_domains/0, get_hosts/0]).

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

    mod_global_distrib_utils:create_ets([?MODULE, ?JIDS_ETS, ?DOMAINS_ETS, ?PUBLIC_DOMAINS_ETS]),
    ExpireAfter = proplists:get_value(expire_after, Opts, 120),
    ets:insert(?MODULE, {expire_after, ExpireAfter}),

    ConnectionOpts = [{host, Server}, {port, Port}, {database, 0}, {password, Password}],
    mongoose_wpool:start(redis, global, distrib, [{workers, PoolSize}], ConnectionOpts),

    Refresher = {mod_global_distrib_redis_refresher,
                 {gen_server, start_link, [?MODULE, RefreshAfter, []]},
                 permanent, 1000, supervisor, [?MODULE]},
    ejabberd_sup:start_child(Refresher),
    ok.

-spec stop() -> any().
stop() ->
    lists:foreach(
     fun(Id) ->
             ejabberd_sup:stop_child(Id)
     end,
      [?MODULE, mod_global_distrib_redis_refresher]),
    [ets:delete(Tab) || Tab <- [?MODULE, ?JIDS_ETS, ?DOMAINS_ETS, ?PUBLIC_DOMAINS_ETS]].

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

-spec put_domain(Domain :: binary(), IsHidden :: boolean()) -> ok.
put_domain(Domain, IsHidden) ->
    ets:insert(?DOMAINS_ETS, {Domain}),
    {ok, _} = q([<<"SADD">>, domains_key(), Domain]),
    do_put(Domain, opt(local_host)),
    case IsHidden of
        false ->
            ets:insert(?PUBLIC_DOMAINS_ETS, {Domain}),
            {ok, _} = q([<<"SADD">>, public_domains_key(), Domain]),
            ok;
        true ->
            ok
    end.

-spec get_domain(Domain :: binary()) -> {ok, Host :: jid:lserver()} | error.
get_domain(Domain) ->
    do_get(Domain).

-spec delete_domain(Domain :: binary()) -> ok.
delete_domain(Domain) ->
    ets:delete(?DOMAINS_ETS, Domain),
    ets:delete(?PUBLIC_DOMAINS_ETS, Domain),
    do_delete(Domain),
    {ok, _} = q([<<"SREM">>, domains_key(), Domain]),
    ok.

-spec get_domains() -> {ok, [Domain :: binary()]}.
get_domains() ->
    get_domains(fun domains_key/2).

-spec get_public_domains() -> {ok, [Domain :: binary()]}.
get_public_domains() ->
    get_domains(fun public_domains_key/2).

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
    ?DEBUG("event=refreshing_own_hosts", []),
    refresh_hosts(),
    ?DEBUG("event=refreshing_own_nodes", []),
    refresh_nodes(),
    ?DEBUG("event=refreshing_own_jids", []),
    refresh_jids(),
    ?DEBUG("event=refreshing_own_endpoints", []),
    refresh_endpoints(),
    ?DEBUG("event=refreshing_own_domains", []),
    refresh_domains(),
    ?DEBUG("event=refreshing_own_public_domains", []),
    refresh_public_domains(),
    ?DEBUG("event=refreshing_own_data_done,next_refresh_in=~p", [RefreshAfter]),
    erlang:send_after(timer:seconds(RefreshAfter), self(), refresh),
    {noreply, RefreshAfter}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec q(Args :: list()) -> {ok, term()}.
q(Args) ->
    {ok, Worker} = mongoose_wpool:get_worker(redis, global, distrib),
    case eredis:q(Worker, Args) of
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

-spec public_domains_key() -> binary().
public_domains_key() ->
    key(<<"public_domains">>).

-spec public_domains_key(Host :: jid:lserver(), Node :: binary()) -> binary().
public_domains_key(Host, Node) ->
    key(Host, Node, <<"public_domains">>).

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

-spec get_domains(KeyFun :: fun((Host :: binary(), Node :: binary()) -> Key :: binary())) ->
    {ok, Domains :: [binary()]}.
get_domains(KeyFun) ->
    Hosts = get_hosts(),
    Nodes = lists:flatmap(fun(Host) -> [{Host, Node} || Node <- get_nodes(Host)] end, Hosts),
    Keys = [KeyFun(Host, Node) || {Host, Node} <- Nodes],
    {ok, _Domains} = q([<<"SUNION">> | Keys]).

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
    AdvertisedEndpoints = opt(advertised_endpoints),
    LocalEndpoints = mod_global_distrib_receiver:endpoints(),
    FinalEndpoints = case AdvertisedEndpoints of
                         false -> LocalEndpoints;
                         Endpoints -> Endpoints
                     end,
    set_endpoints(FinalEndpoints).

-spec set_endpoints(Endpoints :: [mod_global_distrib_utils:endpoint()]) -> any().
set_endpoints(Endpoints) ->
    EndpointsKey = endpoints_key(),
    BinEndpoints = lists:map(fun endpoint_to_binary/1, Endpoints),
    refresh_set(EndpointsKey, BinEndpoints).

-spec endpoint_to_binary(mod_global_distrib_utils:endpoint()) -> binary().
endpoint_to_binary({IpAddr, Port}) when is_tuple(IpAddr) ->
    iolist_to_binary([inet:ntoa(IpAddr), "#", integer_to_binary(Port)]);
endpoint_to_binary({Domain, Port}) when is_list(Domain) ->
    iolist_to_binary([unicode:characters_to_binary(Domain), "#", integer_to_binary(Port)]).

-spec binary_to_endpoint(binary()) -> mod_global_distrib_utils:endpoint().
binary_to_endpoint(Bin) ->
    [Addr, BinPort] = binary:split(Bin, <<"#">>),
    {_, IpAddrOrDomain} = mod_global_distrib_utils:parse_address(Addr),
    Port = binary_to_integer(BinPort),
    {IpAddrOrDomain, Port}.

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

-spec refresh_public_domains() -> any().
refresh_public_domains() ->
    DomainsKey = public_domains_key(),
    Domains = [Domain || {Domain} <- ets:tab2list(?PUBLIC_DOMAINS_ETS)],
    refresh_set(DomainsKey, Domains).

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
