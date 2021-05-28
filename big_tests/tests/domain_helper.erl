-module(domain_helper).

-export([insert_configured_domains/0,
         delete_configured_domains/0,
         insert_domain/3,
         delete_domain/2,
         host_type/0,
         host_type/1,
         host_type/2]).

-import(distributed_helper, [get_or_fail/1, rpc/4]).

host_type() ->
    ct:get_config({hosts, mim, host_type}).

host_type(NodeKey) ->
    host_type(NodeKey, domain).

host_type(NodeKey, DomainKey) ->
    Node = #{node => get_or_fail({hosts, NodeKey, node})},
    Domain = get_or_fail({hosts, NodeKey, DomainKey}),
    {ok, HostType} = rpc(Node, mongoose_domain_core, get_host_type, [Domain]),
    HostType.

insert_configured_domains() ->
    for_each_configured_domain(fun insert_domain/3).

delete_configured_domains() ->
    for_each_configured_domain(fun(Node, Domain, _) -> delete_domain(Node, Domain) end).

insert_domain(Node, Domain, HostType) ->
    ok = rpc(Node, mongoose_domain_core, insert, [Domain, HostType, dummy_source]).

delete_domain(Node, Domain) ->
    ok = rpc(Node, mongoose_domain_core, delete, [Domain]).

for_each_configured_domain(F) ->
    [F(#{node => proplists:get_value(node, Opts)}, Domain, proplists:get_value(host_type, Opts)) ||
        {_, Opts} <- ct:get_config(hosts),
        Domain <- proplists:get_value(dynamic_domains, Opts, [])].
