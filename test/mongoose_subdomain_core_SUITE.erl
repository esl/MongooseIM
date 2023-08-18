-module(mongoose_subdomain_core_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(STATIC_HOST_TYPE, <<"static type">>).
-define(STATIC_DOMAIN, <<"example.com">>).
-define(DYNAMIC_HOST_TYPE1, <<"dynamic type #1">>).
-define(DYNAMIC_HOST_TYPE2, <<"dynamic type #2">>).
-define(DYNAMIC_DOMAINS, [<<"localhost">>, <<"local.host">>]).
-define(STATIC_PAIRS, [{?STATIC_DOMAIN, ?STATIC_HOST_TYPE}]).
-define(ALLOWED_HOST_TYPES, [?DYNAMIC_HOST_TYPE1, ?DYNAMIC_HOST_TYPE2]).

-define(assertEqualLists(L1, L2), ?assertEqual(lists:sort(L1), lists:sort(L2))).

all() ->
    [can_register_and_unregister_subdomain_for_static_host_type,
     can_register_and_unregister_subdomain_for_dynamic_host_type_with_domains,
     can_register_and_unregister_subdomain_for_dynamic_host_type_without_domains,
     can_register_and_unregister_fqdn_for_static_host_type,
     can_register_and_unregister_fqdn_for_dynamic_host_type_with_domains,
     can_register_and_unregister_fqdn_for_dynamic_host_type_without_domains,
     can_add_and_remove_domain,
     can_get_host_type_and_subdomain_details,
     handles_domain_removal_during_subdomain_registration,
     prevents_double_subdomain_registration,
     prevents_prefix_subdomain_overriding_by_prefix_subdomain,
     prevents_fqdn_subdomain_overriding_by_prefix_subdomain,
     prevents_prefix_subdomain_overriding_by_fqdn_subdomain,
     prevents_fqdn_subdomain_overriding_by_fqdn_subdomain,
     detects_domain_conflict_with_prefix_subdomain,
     detects_domain_conflict_with_fqdn_subdomain].

init_per_testcase(TestCase, Config) ->
    %% mongoose_domain_core preconditions:
    %%   - one "static" host type with only one configured domain name
    %%   - one "dynamic" host type without any configured domain names
    %%   - one "dynamic" host type with two configured domain names
    %% initial mongoose_subdomain_core conditions:
    %%   - no subdomains configured for any host type
    mongooseim_helper:start_link_loaded_hooks(),
    mongoose_domain_sup:start_link(?STATIC_PAIRS, ?ALLOWED_HOST_TYPES),
    [mongoose_domain_core:insert(Domain, ?DYNAMIC_HOST_TYPE2, dummy_source)
     || Domain <- ?DYNAMIC_DOMAINS],
    setup_meck(TestCase),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

%%-------------------------------------------------------------------
%% normal test cases
%%-------------------------------------------------------------------
can_register_and_unregister_subdomain_for_static_host_type(_Config) ->
    Handler = mongoose_packet_handler:new(?MODULE),
    Pattern = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Subdomain = mongoose_subdomain_utils:get_fqdn(Pattern, ?STATIC_DOMAIN),
    %% register one "prefix" subdomain for static host type.
    %% check that ETS table contains expected subdomain and nothing else.
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?STATIC_HOST_TYPE,
                                                                Pattern, Handler)),
    ?assertEqual([Subdomain], get_all_subdomains()),
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?STATIC_HOST_TYPE,
                                                                  Pattern)),
    ?assertEqual([], get_all_subdomains()),
    ?assertEqual([Subdomain], get_list_of_disabled_subdomains()),
    no_collisions().

can_register_and_unregister_subdomain_for_dynamic_host_type_with_domains(_Config) ->
    Handler = mongoose_packet_handler:new(?MODULE),
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain2.@HOST@"),
    Subdomains1 = [mongoose_subdomain_utils:get_fqdn(Pattern1, Domain)
                   || Domain <- ?DYNAMIC_DOMAINS],
    Subdomains2 = [mongoose_subdomain_utils:get_fqdn(Pattern2, Domain)
                   || Domain <- ?DYNAMIC_DOMAINS],
    %% register one "prefix" subdomain for dynamic host type with 2 domains.
    %% check that ETS table contains all the expected subdomains and nothing else.
    %% make a snapshot of subdomains ETS table and check its size.
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern1, Handler)),
    ?assertEqualLists(Subdomains1, get_all_subdomains()),
    %% register one more "prefix" subdomain for dynamic host type with 2 domains.
    %% check that ETS table contains all the expected subdomains and nothing else.
    %% check ETS table size.
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern2, Handler)),
    ?assertEqualLists(Subdomains1 ++ Subdomains2, get_all_subdomains()),
    %% check mongoose_subdomain_core:get_all_subdomains_for_domain/1 interface.
    [DynamicDomain | _] = ?DYNAMIC_DOMAINS,
    HostTypeExtra = #{host_type => ?DYNAMIC_HOST_TYPE2},
    HandlerWithHostType = mongoose_packet_handler:add_extra(Handler, HostTypeExtra),
    ?assertEqualLists(
        [#{host_type => ?DYNAMIC_HOST_TYPE2, subdomain_pattern => Pattern1,
           parent_domain => DynamicDomain, packet_handler => HandlerWithHostType,
           subdomain => mongoose_subdomain_utils:get_fqdn(Pattern1, DynamicDomain)},
         #{host_type => ?DYNAMIC_HOST_TYPE2, subdomain_pattern => Pattern2,
           parent_domain => DynamicDomain, packet_handler => HandlerWithHostType,
           subdomain => mongoose_subdomain_utils:get_fqdn(Pattern2, DynamicDomain)}],
        mongoose_subdomain_core:get_all_subdomains_for_domain(DynamicDomain)),
    %% unregister (previously registered) subdomains one by one.
    %% check that ETS table rolls back to the previously made snapshot.
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                  Pattern2)),
    ?assertEqualLists(Subdomains1, get_all_subdomains()),
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                  Pattern1)),
    ?assertEqual([], get_all_subdomains()),
    ?assertEqualLists(Subdomains1 ++ Subdomains2, get_list_of_disabled_subdomains()),
    no_collisions().

can_register_and_unregister_subdomain_for_dynamic_host_type_without_domains(_Config) ->
    Handler = mongoose_packet_handler:new(?MODULE),
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain2.@HOST@"),
    %% register two "prefix" subdomains for dynamic host type with 0 domains.
    %% check that ETS table doesn't contain any subdomains.
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern1, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern2, Handler)),
    ?assertEqual([], get_all_subdomains()),
    %% unregister (previously registered) subdomains one by one.
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                  Pattern1)),
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                  Pattern2)),
    ?assertEqual([], get_all_subdomains()),
    ?assertEqual([], get_list_of_disabled_subdomains()),
    no_collisions().

can_register_and_unregister_fqdn_for_static_host_type(_Config) ->
    Pattern = mongoose_subdomain_utils:make_subdomain_pattern("some.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE),
    %% register one FQDN subdomain for static host type.
    %% check that ETS table contains the only expected subdomain.
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?STATIC_HOST_TYPE,
                                                                Pattern, Handler)),
    ?assertEqual([<<"some.fqdn">>], get_all_subdomains()),
    %% unregister subdomain.
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?STATIC_HOST_TYPE,
                                                                  Pattern)),
    ?assertEqual([], get_all_subdomains()),
    ?assertEqual([<<"some.fqdn">>], get_list_of_disabled_subdomains()),
    no_collisions().

can_register_and_unregister_fqdn_for_dynamic_host_type_without_domains(_Config) ->
    Pattern = mongoose_subdomain_utils:make_subdomain_pattern("some.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE),
    %% register one FQDN subdomain for dynamic host type with 0 domains.
    %% check that ETS table contains the only expected subdomain.
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern, Handler)),
    ?assertEqual([<<"some.fqdn">>], get_all_subdomains()),
    %% unregister subdomain.
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                  Pattern)),
    ?assertEqual([], get_all_subdomains()),
    ?assertEqual([<<"some.fqdn">>], get_list_of_disabled_subdomains()),
    no_collisions().

can_register_and_unregister_fqdn_for_dynamic_host_type_with_domains(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("some.fqdn"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("another.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE),
    %% register one FQDN subdomain for dynamic host type with 2 domains.
    %% check that ETS table contains all the expected subdomains and nothing else.
    %% make a snapshot of subdomains ETS table.
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern1, Handler)),
    ?assertEqual([<<"some.fqdn">>], get_all_subdomains()),
    %% register one more FQDN subdomain for dynamic host type with 2 domains.
    %% check mongoose_subdomain_core:get_all_subdomains_for_domain/1 interface
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern2, Handler)),
    ?assertEqualLists([<<"some.fqdn">>, <<"another.fqdn">>], get_all_subdomains()),
    HostTypeExtra = #{host_type => ?DYNAMIC_HOST_TYPE2},
    HandlerWithHostType = mongoose_packet_handler:add_extra(Handler, HostTypeExtra),
    ?assertEqualLists(
        [#{host_type => ?DYNAMIC_HOST_TYPE2, parent_domain => no_parent_domain,
           subdomain_pattern => Pattern1, packet_handler => HandlerWithHostType,
           subdomain => <<"some.fqdn">>},
         #{host_type => ?DYNAMIC_HOST_TYPE2, parent_domain => no_parent_domain,
           subdomain_pattern => Pattern2, packet_handler => HandlerWithHostType,
           subdomain => <<"another.fqdn">>}],
        mongoose_subdomain_core:get_all_subdomains_for_domain(no_parent_domain)),
    %% unregister (previously registered) subdomains one by one.
    %% check that ETS table rolls back to the previously made snapshot.
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                  Pattern2)),
    ?assertEqual([<<"some.fqdn">>], get_all_subdomains()),
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                  Pattern1)),
    ?assertEqual([], get_all_subdomains()),
    ?assertEqualLists([<<"some.fqdn">>, <<"another.fqdn">>],
                      get_list_of_disabled_subdomains()),
    no_collisions().

can_add_and_remove_domain(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain2.@HOST@"),
    Pattern3 = mongoose_subdomain_utils:make_subdomain_pattern("some.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE),
    Subdomains1 = [mongoose_subdomain_utils:get_fqdn(Pattern1, Domain)
                   || Domain <- ?DYNAMIC_DOMAINS],
    Subdomains2 = [mongoose_subdomain_utils:get_fqdn(Pattern2, Domain)
                   || Domain <- ?DYNAMIC_DOMAINS],
    ?assertEqual([], get_all_subdomains()),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern1, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern2, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern3, Handler)),
    ?assertEqualLists([<<"some.fqdn">> | Subdomains1 ++ Subdomains2],
                      get_all_subdomains()),
    [DynamicDomain | _] = ?DYNAMIC_DOMAINS,
    mongoose_domain_core:delete(DynamicDomain),
    ?assertEqualLists([<<"some.fqdn">> | tl(Subdomains1) ++ tl(Subdomains2)],
                      get_all_subdomains()),
    ?assertEqualLists([hd(Subdomains1), hd(Subdomains2)],
                      get_list_of_disabled_subdomains()),
    mongoose_domain_core:insert(DynamicDomain, ?DYNAMIC_HOST_TYPE2, dummy_source),
    ?assertEqualLists([<<"some.fqdn">> | Subdomains1 ++ Subdomains2],
                      get_all_subdomains()),
    no_collisions().

can_get_host_type_and_subdomain_details(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("some.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE),
    Subdomain1 = mongoose_subdomain_utils:get_fqdn(Pattern1, ?STATIC_DOMAIN),
    Subdomain2 = mongoose_subdomain_utils:get_fqdn(Pattern1, hd(?DYNAMIC_DOMAINS)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?STATIC_HOST_TYPE,
                                                                Pattern1, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                Pattern1, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern2, Handler)),
    mongoose_subdomain_core:sync(),
    ?assertEqual({ok, ?STATIC_HOST_TYPE},
                 mongoose_subdomain_core:get_host_type(Subdomain1)),
    ?assertEqual({ok, ?DYNAMIC_HOST_TYPE1},
                 mongoose_subdomain_core:get_host_type(<<"some.fqdn">>)),
    ?assertEqual({ok, ?DYNAMIC_HOST_TYPE2},
                 mongoose_subdomain_core:get_host_type(Subdomain2)),
    ?assertEqual({error, not_found},
                 mongoose_subdomain_core:get_host_type(<<"unknown.subdomain">>)),
    HostTypeExtra1 = #{host_type => ?STATIC_HOST_TYPE},
    HandlerWithHostType1 = mongoose_packet_handler:add_extra(Handler, HostTypeExtra1),
    ?assertEqual({ok, #{host_type => ?STATIC_HOST_TYPE, subdomain_pattern => Pattern1,
                        parent_domain => ?STATIC_DOMAIN, subdomain => Subdomain1,
                        packet_handler => HandlerWithHostType1}},
                 mongoose_subdomain_core:get_subdomain_info(Subdomain1)),
    HostTypeExtra2 = #{host_type => ?DYNAMIC_HOST_TYPE1},
    HandlerWithHostType2 = mongoose_packet_handler:add_extra(Handler, HostTypeExtra2),
    ?assertEqual({ok, #{host_type => ?DYNAMIC_HOST_TYPE1, subdomain_pattern => Pattern2,
                        parent_domain => no_parent_domain, subdomain => <<"some.fqdn">>,
                        packet_handler => HandlerWithHostType2}},
                 mongoose_subdomain_core:get_subdomain_info(<<"some.fqdn">>)),
    HostTypeExtra3 = #{host_type => ?DYNAMIC_HOST_TYPE2},
    HandlerWithHostType3 = mongoose_packet_handler:add_extra(Handler, HostTypeExtra3),
    ?assertEqual({ok, #{host_type => ?DYNAMIC_HOST_TYPE2, subdomain_pattern => Pattern1,
                        parent_domain => hd(?DYNAMIC_DOMAINS), subdomain => Subdomain2,
                        packet_handler => HandlerWithHostType3}},
                 mongoose_subdomain_core:get_subdomain_info(Subdomain2)),
    ?assertEqual({error, not_found},
                 mongoose_subdomain_core:get_subdomain_info(<<"unknown.subdomain">>)),
    ok.

handles_domain_removal_during_subdomain_registration(_Config) ->
    %% NumOfDomains is just some big non-round number to ensure that more than 2 ets
    %% selections are done during the call to mongoose_domain_core:for_each_domain/2.
    %% currently max selection size is 100 domains.
    NumOfDomains = 1234,
    NumOfDomainsToRemove = 1234 div 4,
    NewDomains = [<<"dummy_domain_", (integer_to_binary(N))/binary, ".localhost">>
                  || N <- lists:seq(1, NumOfDomains)],
    [mongoose_domain_core:insert(Domain, ?DYNAMIC_HOST_TYPE1, dummy_src)
     || Domain <- NewDomains],
    meck:new(mongoose_domain_core, [passthrough]),
    WrapperFn = make_wrapper_fn(NumOfDomainsToRemove * 2, NumOfDomainsToRemove),
    meck:expect(mongoose_domain_core, for_each_domain,
                fun(HostType, Fn) ->
                    meck:passthrough([HostType, WrapperFn(Fn)])
                end),
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Handler = mongoose_packet_handler:new(?MODULE),
    %% Note that mongoose_domain_core:for_each_domain/2 is used to register subdomain.
    %% some domains are removed during subdomain registration, see make_wrapper_fn/2
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern1, Handler)),
    mongoose_subdomain_core:sync(),
    %% try to add some domains second time, as this is also possible during
    %% subdomain registration
    AllDomains = mongoose_domain_core:get_domains_by_host_type(?DYNAMIC_HOST_TYPE1),
    [RegisteredDomain1, RegisteredDomain2 | _] = AllDomains,
    mongoose_subdomain_core:add_domain(?DYNAMIC_HOST_TYPE1, RegisteredDomain1),
    mongoose_subdomain_core:add_domain(?DYNAMIC_HOST_TYPE1, RegisteredDomain2),
    %% and finally try to remove some domains second time
    RemovedDomains = NewDomains -- AllDomains,
    [RemovedDomain1, RemovedDomain2 | _] = RemovedDomains,
    mongoose_subdomain_core:remove_domain(?DYNAMIC_HOST_TYPE1, RemovedDomain1),
    mongoose_subdomain_core:remove_domain(?DYNAMIC_HOST_TYPE1, RemovedDomain2),
    Subdomains = get_all_subdomains(),
    ?assertEqual(NumOfDomains - NumOfDomainsToRemove, length(Subdomains)),
    AllExpectedSubDomains = [mongoose_subdomain_utils:get_fqdn(Pattern1, Domain)
                             || Domain <- AllDomains],
    ?assertEqualLists(AllExpectedSubDomains, Subdomains),
    ?assertEqual(NumOfDomainsToRemove,
                 meck:num_calls(mongoose_lazy_routing, maybe_remove_subdomain, 1)),
    RemovedSubdomains = [mongoose_subdomain_utils:get_fqdn(Pattern1, Domain)
                         || Domain <- RemovedDomains],
    ?assertEqualLists(RemovedSubdomains, get_list_of_disabled_subdomains()),
    no_collisions(),
    meck:unload(mongoose_domain_core).

prevents_double_subdomain_registration(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern1, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern2, Handler)),
    ?assertEqual({error, already_registered},
                 mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                            Pattern1, Handler)),
    ?assertEqual({error, already_registered},
                 mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                            Pattern2, Handler)).

%%-------------------------------------------------------------------------------------
%% test cases for subdomain names collisions.
%%-------------------------------------------------------------------------------------
%% There are three possible subdomain names collisions:
%%   1) Different domain/subdomain_pattern pairs produce one and the same subdomain.
%%   2) Attempt to register the same FQDN subdomain for 2 different host types.
%%   3) Domain/subdomain_pattern pair produces the same subdomain name as another
%%      FQDN subdomain.
%%
%% Collisions of the first type can eliminated by allowing only one level subdomains,
%% e.g. ensuring that subdomain template corresponds to this regex "^[^.]*\.@HOST@$".
%%
%% Collisions of the second type are less critical as they can be detected during
%% init phase - they result in {error, subdomain_already_exists} return code, so
%% modules can detect it and crash at ?MODULE:start/2.
%%
%% Third type is hard to resolve in automatic way. One of the options is to ensure
%% that FQDN subdomains don't start with the same "prefix" as subdomain patterns.
%%
%% It's good idea to create a metric for such collisions, so devops can set some
%% alarm and react on it.
%%
%% The current behaviour rejects insertion of the conflicting subdomain, the original
%% subdomain must remain unchanged
%%-------------------------------------------------------------------------------------
prevents_prefix_subdomain_overriding_by_prefix_subdomain(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("sub.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("sub.domain.@HOST@"),
    Handler = mongoose_packet_handler:new(?MODULE, #{host_type => dummy_type}),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern1, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern2, Handler)),
    %% one prefix subdomain conflicts with another prefix subdomain
    mongoose_domain_core:insert(<<"test">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    mongoose_domain_core:insert(<<"domain.test">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    ?assertEqualLists(
        [<<"sub.domain.domain.test">>, <<"sub.domain.test">>, <<"sub.test">>],
        get_all_subdomains()),
    ?assertEqualLists([<<"test">>, <<"domain.test">>],
                      mongoose_domain_core:get_domains_by_host_type(?DYNAMIC_HOST_TYPE1)),
    %% "test" domain is added first, so subdomain for this domain must remain unchanged
    ExpectedSubdomainInfo =
        #{host_type => ?DYNAMIC_HOST_TYPE1, subdomain_pattern => Pattern2,
          parent_domain => <<"test">>, packet_handler => Handler,
          subdomain => <<"sub.domain.test">>},
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"sub.domain.test">>)),
    ?assertEqual([#{what => subdomains_collision, subdomain => <<"sub.domain.test">>}],
                 get_list_of_subdomain_collisions()),
    no_domain_collisions(),
    meck:reset(mongoose_subdomain_core),
    %% check that removal of "domain.test" domain doesn't affect
    %% "sub.domain.test" subdomain
    mongoose_domain_core:delete(<<"domain.test">>),
    ?assertEqual([<<"test">>],
                 mongoose_domain_core:get_domains_by_host_type(?DYNAMIC_HOST_TYPE1)),
    ?assertEqualLists([<<"sub.domain.test">>, <<"sub.test">>], get_all_subdomains()),
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"sub.domain.test">>)),
    ?assertEqual([<<"sub.domain.domain.test">>], get_list_of_disabled_subdomains()),
    no_collisions().

prevents_fqdn_subdomain_overriding_by_prefix_subdomain(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE, #{host_type => dummy_type}),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern1, Handler)),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern2, Handler)),
    %% FQDN subdomain conflicts with prefix subdomain
    mongoose_domain_core:insert(<<"fqdn">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    ?assertEqual([<<"subdomain.fqdn">>], get_all_subdomains()),
    ?assertEqual([<<"fqdn">>],
                 mongoose_domain_core:get_domains_by_host_type(?DYNAMIC_HOST_TYPE1)),
    %% FQDN subdomain is added first, so it must remain unchanged
    ExpectedSubdomainInfo =
        #{host_type => ?DYNAMIC_HOST_TYPE1, subdomain_pattern => Pattern2,
          parent_domain => no_parent_domain, packet_handler => Handler,
          subdomain => <<"subdomain.fqdn">>},
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"subdomain.fqdn">>)),
    ?assertEqual([#{what => subdomains_collision, subdomain => <<"subdomain.fqdn">>}],
                 get_list_of_subdomain_collisions()),
    no_domain_collisions(),
    meck:reset(mongoose_subdomain_core),
    %% check that removal of "fqdn" domain doesn't affect FQDN subdomain
    mongoose_domain_core:delete(<<"fqdn">>),
    ?assertEqual([], mongoose_domain_core:get_domains_by_host_type(?DYNAMIC_HOST_TYPE1)),
    ?assertEqual([<<"subdomain.fqdn">>], get_all_subdomains()),
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"subdomain.fqdn">>)),
    no_collisions().

prevents_fqdn_subdomain_overriding_by_fqdn_subdomain(_Config) ->
    Pattern = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE, #{host_type => dummy_type}),
    %% FQDN subdomain conflicts with another FQDN subdomain
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern, Handler)),
    ?assertEqual({error, subdomain_already_exists},
                 mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE2,
                                                            Pattern, Handler)),
    %% FQDN subdomain for ?DYNAMIC_HOST_TYPE1 is registered first, so it must
    %% remain unchanged
    ?assertEqual([<<"subdomain.fqdn">>], get_all_subdomains()),
    ExpectedSubdomainInfo =
        #{host_type => ?DYNAMIC_HOST_TYPE1, subdomain_pattern => Pattern,
          parent_domain => no_parent_domain, packet_handler => Handler,
          subdomain => <<"subdomain.fqdn">>},
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"subdomain.fqdn">>)),
    ?assertEqual([#{what => subdomains_collision, subdomain => <<"subdomain.fqdn">>}],
                 get_list_of_subdomain_collisions()),
    no_domain_collisions(),
    meck:reset(mongoose_subdomain_core),
    %% check that unregistering FQDN subdomain for ?DYNAMIC_HOST_TYPE2 doesn't
    %% affect FQDN subdomain for ?DYNAMIC_HOST_TYPE1
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE2,
                                                                  Pattern)),
    ?assertEqual([<<"subdomain.fqdn">>], get_all_subdomains()),
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"subdomain.fqdn">>)),
    no_collisions().

prevents_prefix_subdomain_overriding_by_fqdn_subdomain(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE, #{host_type => dummy_type}),
    %% FQDN subdomain conflicts with another FQDN subdomain
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern1, Handler)),
    mongoose_domain_core:insert(<<"fqdn">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    ?assertEqual({error, subdomain_already_exists},
                 mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                            Pattern2, Handler)),
    %% FQDN subdomain for ?DYNAMIC_HOST_TYPE1 is registered first, so it must
    %% remain unchanged
    ?assertEqual([<<"subdomain.fqdn">>], get_all_subdomains()),
    ExpectedSubdomainInfo =
        #{host_type => ?DYNAMIC_HOST_TYPE1, subdomain_pattern => Pattern1,
          parent_domain => <<"fqdn">>, packet_handler => Handler,
          subdomain => <<"subdomain.fqdn">>},
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"subdomain.fqdn">>)),
    ?assertEqual([#{what => subdomains_collision, subdomain => <<"subdomain.fqdn">>}],
                 get_list_of_subdomain_collisions()),
    no_domain_collisions(),
    meck:reset(mongoose_subdomain_core),
    %% check that unregistering FQDN subdomain for ?DYNAMIC_HOST_TYPE2 doesn't
    %% affect FQDN subdomain for ?DYNAMIC_HOST_TYPE1
    ?assertEqual(ok, mongoose_subdomain_core:unregister_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                  Pattern2)),
    ?assertEqual([<<"subdomain.fqdn">>], get_all_subdomains()),
    ?assertEqual({ok, ExpectedSubdomainInfo},
                 mongoose_subdomain_core:get_subdomain_info(<<"subdomain.fqdn">>)),
    no_collisions().


%%-------------------------------------------------------------------------------------
%% test cases for domain/subdomain names collisions.
%%-------------------------------------------------------------------------------------
%% There are two possible domain/subdomain names collisions:
%%   1) Domain/subdomain_pattern pair produces the same subdomain name as another
%%      existing top level domain
%%   2) FQDN subdomain is the same as some registered top level domain
%%
%% The naive domain/subdomain registration rejection is probably a bad option:
%%   * Domains and subdomains ETS tables are managed asynchronously, in addition to
%%     that subdomains patterns registration is done async as well. This all leaves
%%     room for various race conditions if we try to just make a verification and
%%     prohibit domain/subdomain registration in case of any collisions.
%%   * The only way to avoid such race conditions is to block all async. ETSs
%%     editing during the validation process, but this can result in big delays
%%     during the MIM initialisation phase.
%%   * Also it's not clear how to interpret registration of the "prefix" based
%%     subdomain patterns, should we block the registration of the whole pattern
%%     or just only conflicting subdomains registration. Blocking of the whole
%%     pattern requires generation and verification of all the subdomains (with
%%     ETS blocking during that process), which depends on domains ETS size and
%%     might take too long.
%%   * And the last big issue with simple registration rejection approach, different
%%     nodes in the cluster might have different registration sequence. So we may
%%     end up in a situation when some nodes registered domain name as a subdomain,
%%     while other nodes registered it as a top level domain.
%%
%% The better way is to prohibit registration of a top level domain if it is equal
%% to any of the FQDN subdomains or if beginning of domain name matches the prefix
%% of any subdomain template. In this case we don't need to verify subdomains at all,
%% verification of domain names against some limited number of subdomains patterns is
%% enough. And the only problem that we need to solve - mongooseim_domain_core must
%% be aware of all the subdomain patterns before it registers the first dynamic
%% domain. This would require minor configuration rework, e.g. tracking of subdomain
%% templates preprocessing (mongoose_subdomain_utils:make_subdomain_pattern/1 calls)
%% during TOML config parsing.
%%
%% It's good idea to create a metric for such collisions, so devops can set some
%% alarm and react on it.
%%
%% The current behaviour just ensures detection of the domain/subdomain names
%% collision, both (domain and subdomain) records remain unchanged in the
%% corresponding ETS tables
%%-------------------------------------------------------------------------------------
detects_domain_conflict_with_prefix_subdomain(_Config) ->
    Pattern = mongoose_subdomain_utils:make_subdomain_pattern("subdomain.@HOST@"),
    Handler = mongoose_packet_handler:new(?MODULE),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern, Handler)),
    mongoose_domain_core:insert(<<"test.net">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    %% without this sync call "subdomain.example.net" collision can be detected
    %% twice, one time by check_subdomain_name/1 function and then second time
    %% by check_domain_name/2.
    mongoose_subdomain_core:sync(),
    mongoose_domain_core:insert(<<"subdomain.test.net">>, ?DYNAMIC_HOST_TYPE2, dummy_src),
    mongoose_domain_core:insert(<<"subdomain.test.org">>, ?DYNAMIC_HOST_TYPE2, dummy_src),
    mongoose_domain_core:insert(<<"test.org">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    ?assertEqualLists([<<"subdomain.test.org">>, <<"subdomain.test.net">>],
                      get_all_subdomains()),
    ?assertEqualLists(
        [<<"subdomain.test.org">>, <<"subdomain.test.net">> | ?DYNAMIC_DOMAINS],
        mongoose_domain_core:get_domains_by_host_type(?DYNAMIC_HOST_TYPE2)),
    no_subdomain_collisions(),
    ?assertEqual(
        [#{what => check_domain_name_failed, domain => <<"subdomain.test.net">>},
         #{what => check_subdomain_name_failed, subdomain => <<"subdomain.test.org">>}],
        get_list_of_domain_collisions()),
    ?assertEqual([<<"subdomain.test.net">>], get_list_of_disabled_subdomains()).

detects_domain_conflict_with_fqdn_subdomain(_Config) ->
    Pattern1 = mongoose_subdomain_utils:make_subdomain_pattern("some.fqdn"),
    Pattern2 = mongoose_subdomain_utils:make_subdomain_pattern("another.fqdn"),
    Handler = mongoose_packet_handler:new(?MODULE),

    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern1, Handler)),
    mongoose_domain_core:insert(<<"some.fqdn">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    mongoose_domain_core:insert(<<"another.fqdn">>, ?DYNAMIC_HOST_TYPE1, dummy_src),
    ?assertEqual(ok, mongoose_subdomain_core:register_subdomain(?DYNAMIC_HOST_TYPE1,
                                                                Pattern2, Handler)),
    ?assertEqualLists([<<"some.fqdn">>, <<"another.fqdn">>], get_all_subdomains()),
    ?assertEqualLists([<<"some.fqdn">>, <<"another.fqdn">>],
                      mongoose_domain_core:get_domains_by_host_type(?DYNAMIC_HOST_TYPE1)),
    no_subdomain_collisions(),
    ?assertEqual(
        [#{what => check_domain_name_failed, domain => <<"some.fqdn">>},
         #{what => check_subdomain_name_failed, subdomain => <<"another.fqdn">>}],
        get_list_of_domain_collisions()),
    ?assertEqual([<<"some.fqdn">>], get_list_of_disabled_subdomains()).

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
setup_meck(TestCase) ->
    meck:new(mongoose_lazy_routing, [no_link]),
    meck:new(mongoose_subdomain_core, [no_link, passthrough]),
    meck:expect(mongoose_lazy_routing, maybe_remove_domain, fun(_, _) -> ok end),
    RemoveSubdomainFn =
    if
        detects_domain_conflict_with_prefix_subdomain =:= TestCase;
        detects_domain_conflict_with_fqdn_subdomain =:= TestCase ->
            %% Subdomain should never overshadow top level domain name.
            %% In case of conflict with domain name, we want to remove
            %% subdomain routing and IQ handling, but keep ETS record
            %% of that subdomain for troubleshooting.
            fun(_) -> ?assertEqual(whereis(mongoose_subdomain_core), self()) end;
        true ->
            fun(SubdomainInfo) ->
                %% For all other cases ensure that subdomain is removed from
                %% the ETS table before mongoose_lazy_routing module notified
                %% about it
                Subdomain = maps:get(subdomain, SubdomainInfo),
                ?assertEqual({error, not_found},
                             mongoose_subdomain_core:get_host_type(Subdomain)),
                ?assertEqual(whereis(mongoose_subdomain_core), self())
            end
    end,
    meck:expect(mongoose_lazy_routing, maybe_remove_subdomain, RemoveSubdomainFn).

get_all_subdomains() ->
    mongoose_subdomain_core:sync(),
    get_subdomains().

get_subdomains() ->
    %% mongoose_subdomain_core table is indexed by subdomain name field
    KeyPos = ets:info(mongoose_subdomain_core, keypos),
    [element(KeyPos, Item) || Item <- ets:tab2list(mongoose_subdomain_core)].

make_wrapper_fn(N, M) when N > M ->
    %% the wrapper function generates a new loop processing function
    %% that pauses after after processing N domains, removes M of the
    %% already processed domains and resumes after that.
    fun(Fn) ->
        put(number_of_iterations, 0),
        fun(HostType, DomainName) ->
            NumberOfIterations = get(number_of_iterations),
            if
                NumberOfIterations =:= N -> remove_some_domains(M);
                true -> ok
            end,
            put(number_of_iterations, NumberOfIterations + 1),
            Fn(HostType, DomainName)
        end
    end.

remove_some_domains(N) ->
    AllSubdomains = get_subdomains(),
    [begin
         {ok, Info} = mongoose_subdomain_core:get_subdomain_info(Subdomain),
         ParentDomain = maps:get(parent_domain, Info),
         mongoose_domain_core:delete(ParentDomain)
     end || Subdomain <- lists:sublist(AllSubdomains, N)].

no_collisions() ->
    no_domain_collisions(),
    no_subdomain_collisions().

no_domain_collisions() ->
    Hist = meck:history(mongoose_subdomain_core),
    Errors = [Call || {_P, {_M, log_error = _F, [From, _] = _A}, _R} = Call <- Hist,
                      From =:= check_subdomain_name orelse From =:= check_domain_name],
    ?assertEqual([], Errors).

get_list_of_domain_collisions() ->
    Hist = meck:history(mongoose_subdomain_core),
    [Error || {_Pid, {_Mod, log_error = _Func, [From, Error] = _Args}, _Result} <- Hist,
              From =:= check_subdomain_name orelse From =:= check_domain_name].

no_subdomain_collisions() ->
    Hist = meck:history(mongoose_subdomain_core),
    Errors = [Call || {_P, {_M, log_error = _F, [From, _] = _A}, _R} = Call <- Hist,
                      From =:= report_subdomains_collision],
    ?assertEqual([], Errors).

get_list_of_subdomain_collisions() ->
    Hist = meck:history(mongoose_subdomain_core),
    [Error || {_Pid, {_Mod, log_error = _Func, [From, Error] = _Args}, _Result} <- Hist,
              From =:= report_subdomains_collision].

get_list_of_disabled_subdomains() ->
    History = meck:history(mongoose_lazy_routing),
    [maps:get(subdomain, Info)
     || {_Pid, {_Mod, Func, [Info] = _Args}, _Result} <- History,
        Func =:= maybe_remove_subdomain].
