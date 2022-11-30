-module(mongoose_wpool_cassandra).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

-ifdef(TEST).
-export([prepare_cqerl_opts/1]).
-endif.

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    {ok, []} = application:ensure_all_started(cqerl),
    application:set_env(cqerl, maps, true).

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOptsIn, ConnOpts) ->
    PoolSize = proplists:get_value(workers, WpoolOptsIn),
    application:set_env(cqerl, num_clients, PoolSize),
    Servers = prepare_cqerl_servers(ConnOpts),
    CqerlOpts = prepare_cqerl_opts(ConnOpts),
    set_cluster_config(Tag, Servers, CqerlOpts),
    Res = cqerl_cluster:add_nodes(Tag, Servers, CqerlOpts),
    case lists:keyfind(error, 1, Res) of
        false ->
            ok;
        _ ->
            erlang:throw({not_all_nodes_added, Res})
    end,
    Name = mongoose_wpool:make_pool_name(cassandra, HostType, Tag),
    Worker = {mongoose_cassandra_worker, [Tag]},
    WpoolOpts = [{worker, Worker} | WpoolOptsIn],
    mongoose_wpool:start_sup_pool(cassandra, Name, WpoolOpts).

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(_, _) ->
    ok.

%% --------------------------------------------------------------
%% Internal functions
prepare_cqerl_servers(#{servers := Servers}) ->
    [cqerl_server(Server) || Server <- Servers].

cqerl_server(#{host := Host, port := Port}) -> {Host, Port};
cqerl_server(#{host := Host}) -> Host.

prepare_cqerl_opts(ConnOpts) ->
    lists:flatmap(fun(Opt) -> cqerl_opts(Opt, ConnOpts) end, [keyspace, auth, tcp, tls]).

cqerl_opts(keyspace, #{keyspace := Keyspace}) ->
    [{keyspace, Keyspace}];
cqerl_opts(auth, #{auth := #{plain := #{username := UserName, password := Password}}}) ->
    [{auth, {cqerl_auth_plain_handler, [{UserName, Password}]}}];
cqerl_opts(tcp, #{}) ->
    [{tcp_opts, [{keepalive, true}]}]; % always set
cqerl_opts(tls, #{tls := TLSOpts}) ->
    [{ssl, just_tls:make_ssl_opts(TLSOpts)}];
cqerl_opts(_Opt, #{}) ->
    [].

%% make the config survive the restart of 'cqerl_cluster' in case of a network failure
set_cluster_config(Tag, Servers, ExtConfig) ->
    Clusters = application:get_env(cqerl, cassandra_clusters, []),
    ClusterConfig = {Tag, {Servers, ExtConfig}},
    NewClusters = lists:keystore(Tag, 1, Clusters, ClusterConfig),
    application:set_env(cqerl, cassandra_clusters, NewClusters).
