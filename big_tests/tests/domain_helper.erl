-module(domain_helper).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [get_or_fail/1, rpc/4, mim/0]).

host_types() ->
    host_types(mim).

host_types(NodeKey) ->
    lists:usort([host_type(NodeKey), secondary_host_type(NodeKey)]).

host_type() ->
    host_type(mim).

domain() ->
    domain(mim).

secondary_domain() ->
    get_or_fail({hosts, mim, secondary_domain}).

host_type(NodeKey) ->
    get_or_fail({hosts, NodeKey, host_type}).

domain_to_host_type(Node, Domain) ->
    {ok, HostType} = rpc(Node, mongoose_domain_core, get_host_type, [Domain]),
    HostType.

restart_domain_core(Node) ->
    {ok, _} = rpc(Node, mongoose_domain_sup, restart_core, [[]]).

restart_domain_core(Node, Pairs, AllowedHostTypes) ->
    {ok, _} = rpc(Node, mongoose_domain_sup, restart_core, [[Pairs, AllowedHostTypes]]).

domain(NodeKey) ->
    get_or_fail({hosts, NodeKey, domain}).

secondary_host_type() ->
    secondary_host_type(mim).

secondary_host_type(NodeKey) ->
    get_or_fail({hosts, NodeKey, secondary_host_type}).

anonymous_host_type() ->
    anonymous_host_type(mim).

anonymous_host_type(NodeKey) ->
    get_or_fail({hosts, NodeKey, anonymous_host_type}).

make_metrics_prefix(HostType) ->
    metrics_helper:make_host_type_name(HostType).

insert_configured_domains() ->
    for_each_configured_domain(fun insert_persistent_domain/3).

delete_configured_domains() ->
    for_each_configured_domain(fun delete_persistent_domain/3).

insert_domain(Node, Domain, HostType) ->
    ok = rpc(Node, mongoose_domain_core, insert, [Domain, HostType, dummy_source]).

delete_domain(Node, Domain) ->
    ok = rpc(Node, mongoose_domain_core, delete, [Domain]).

insert_persistent_domain(Node, Domain, HostType) ->
    {ok, _} = rpc(Node, mongoose_domain_api, insert_domain, [Domain, HostType]).

delete_persistent_domain(Node, Domain, HostType) ->
    {ok, _} = rpc(Node, mongoose_domain_api, delete_domain, [Domain, HostType]).

set_domain_password(Node, Domain, Password) ->
    {ok, _} = rpc(Node, mongoose_domain_api, set_domain_password, [Domain, Password]).

delete_domain_password(Node, Domain) ->
    {ok, _} = rpc(Node, mongoose_domain_api, delete_domain_password, [Domain]).

for_each_configured_domain(F) ->
    %% Skip nodes not specified in `--test-hosts'
    Keys = distributed_helper:get_node_keys(),
    [for_each_configured_domain(F, Opts)
     || {Key, Opts} <- ct:get_config(hosts),
        lists:member(Key, Keys)],
    ok.

for_each_configured_domain(F, Opts) ->
    case proplists:get_value(dynamic_domains, Opts) of
        undefined ->
            ok; % no dynamic domains required
        DomainsByHostType ->
            Node = #{node => proplists:get_value(node, Opts)},
            [F(Node, Domain, HostType) || {HostType, Domains} <- DomainsByHostType,
                                          Domain <- Domains],
            rpc(Node, service_domain_db, force_check_for_updates, []),
            rpc(Node, service_domain_db, sync_local, [])
    end.
