-module(domain_helper).

-export([insert_configured_domains/0,
         delete_configured_domains/0,
         insert_domain/2,
         delete_domain/2]).

-import(distributed_helper, [get_or_fail/1, rpc/4]).

insert_configured_domains() ->
    for_each_configured_domain(fun insert_domain/2).

delete_configured_domains() ->
    for_each_configured_domain(fun delete_domain/2).

insert_domain(Node, Domain) ->
    ok = rpc(Node, mongoose_domain_core, insert, [Domain, host_type(), dummy_source]).

delete_domain(Node, Domain) ->
    ok = rpc(Node, mongoose_domain_core, delete, [Domain]).

for_each_configured_domain(F) ->
    [F(#{node => proplists:get_value(node, Opts)}, Domain) ||
        {_, Opts} <- ct:get_config(hosts),
        Domain <- proplists:get_value(dynamic_domains, Opts, [])].

host_type() ->
    <<"test type">>. %% preconfigured in the toml file
