-module(domain_helper).

-export([insert_configured_domains/0,
         delete_configured_domains/0,
         insert_domain/3,
         delete_domain/2,
         make_metrics_prefix/1,
         host_types/0,
         host_types/1,
         host_type/0,
         host_type/1,
         domain_to_host_type/2,
         domain/0,
         domain/1,
         secondary_host_type/0,
         secondary_host_type/1]).

-import(distributed_helper, [get_or_fail/1, rpc/4, mim/0]).

host_types() ->
    host_types(mim).

host_types(NodeKey) ->
    lists:usort([host_type(NodeKey), secondary_host_type(NodeKey)]).

host_type() ->
    host_type(mim).

domain() ->
    domain(mim).

host_type(NodeKey) ->
    get_or_fail({hosts, NodeKey, host_type}).

domain_to_host_type(Node, Domain) ->
    {ok, HostType} = rpc(Node, mongoose_domain_core, get_host_type, [Domain]),
    HostType.

domain(NodeKey) ->
    get_or_fail({hosts, NodeKey, domain}).

secondary_host_type() ->
    secondary_host_type(mim).

secondary_host_type(NodeKey) ->
    get_or_fail({hosts, NodeKey, secondary_host_type}).

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
    ok = rpc(Node, mongoose_domain_api, insert_domain, [Domain, HostType]).

delete_persistent_domain(Node, Domain, HostType) ->
    ok = rpc(Node, mongoose_domain_api, delete_domain, [Domain, HostType]).

for_each_configured_domain(F) ->
    [for_each_configured_domain(F, Opts) || {_, Opts} <- ct:get_config(hosts)],
    ok.

for_each_configured_domain(F, Opts) ->
    case proplists:get_value(dynamic_domains, Opts, []) of
        [] ->
            ok;
        DomainsByHostType ->
            Node = #{node => proplists:get_value(node, Opts)},
            [F(Node, Domain, HostType) || {HostType, Domains} <- DomainsByHostType,
                                          Domain <- Domains],
            rpc(Node, service_domain_db, sync_local, [])
    end.
