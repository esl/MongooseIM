-module(mongoose_lazy_routing_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(assertEqualLists(L1, L2), ?assertEqual(lists:sort(L1), lists:sort(L2))).

-define(HOST_TYPE_1, <<"host type #1">>).
-define(HOST_TYPE_2, <<"host type #2">>).

-define(DOMAIN_1, <<"domain1.test">>).
-define(DOMAIN_2, <<"domain2.test">>).
-define(DOMAIN_3, <<"domain3.test">>).
-define(SUBDOMAIN_1, <<"sub.", (?DOMAIN_1)/binary>>).
-define(SUBDOMAIN_2, <<"sub.", (?DOMAIN_2)/binary>>).
-define(SUBDOMAIN_3, <<"sub.", (?DOMAIN_3)/binary>>).

%% this domain is required for testing domain/subdomain conflict resolution.
-define(DOMAIN_X, <<"domain_x.test">>).

%% required for handles_missing_domain_or_subdomain test case
-define(MISSING_DOMAIN, <<"missing.domain.test">>).
-define(MISSING_DOMAIN_2, <<"missing.domain2.test">>).

-define(NAMESPACE_1, <<"dummy:namespace:1">>).
-define(NAMESPACE_2, <<"dummy:namespace:2">>).
-define(COMPONENT, dummy_component).

-import(mongoose_lazy_routing, [maybe_add_domain_or_subdomain/1,
                                maybe_remove_domain/2,
                                maybe_remove_subdomain/1,
                                register_iq_handler_for_domain/4,
                                register_iq_handler_for_subdomain/5,
                                unregister_iq_handler_for_domain/3,
                                unregister_iq_handler_for_subdomain/4]).

all() ->
    [can_add_and_remove_domain_or_subdomain,
     handles_missing_domain_or_subdomain,
     registers_top_level_domain_in_case_domain_subdomain_conflicts,
     can_register_and_unregister_iq_handler_for_two_domains,
     can_add_domain_for_a_registered_iq_handler,
     can_register_and_unregister_iq_handler_for_two_subdomains,
     can_add_subdomain_for_a_registered_iq_handler,
     handles_double_iq_handler_registration_deregistration_for_domain,
     handles_double_iq_handler_registration_deregistration_for_subdomain].

init_per_testcase(_, Config) ->
    mongoose_lazy_routing:start_link(),
    setup_meck(),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

%%-------------------------------------------------------------------
%% test cases
%%-------------------------------------------------------------------
can_add_and_remove_domain_or_subdomain(_Config) ->
    %% add 2 domains, one of them add twice
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_1)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_2)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_2)),
    %% add 2 subdomains, one of them add twice
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_1)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_2)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_2)),
    %% check that 2 domains and 2 subdomains added properly
    ?assertEqualLists([?DOMAIN_1, ?DOMAIN_2], get_all_registered_domains()),
    ?assertEqualLists([{?SUBDOMAIN_1, packet_handler(?SUBDOMAIN_1)},
                       {?SUBDOMAIN_2, packet_handler(?SUBDOMAIN_2)}],
                      get_all_registered_subdomains()),
    ?assertEqual([], get_all_unregistered_domains()),
    ?assertEqual([], get_all_unregistered_subdomains()),
    [meck:reset(M) || M <- [ejabberd_local, mongoose_router]],
    %% remove 2 domains, one of them remove twice
    maybe_remove_domain(domain_host_type(?DOMAIN_1), ?DOMAIN_1),
    maybe_remove_domain(domain_host_type(?DOMAIN_2), ?DOMAIN_2),
    maybe_remove_domain(domain_host_type(?DOMAIN_2), ?DOMAIN_2),
    %% remove 2 subdomains, one of them remove twice
    maybe_remove_subdomain(subdomain_info(?SUBDOMAIN_1)),
    maybe_remove_subdomain(subdomain_info(?SUBDOMAIN_2)),
    maybe_remove_subdomain(subdomain_info(?SUBDOMAIN_2)),
    %% check that 2 domains and 2 subdomains removed properly
    mongoose_lazy_routing:sync(),
    ?assertEqualLists([], get_all_registered_domains()),
    ?assertEqualLists([], get_all_registered_subdomains()),
    ?assertEqual([?SUBDOMAIN_1, ?SUBDOMAIN_2], get_all_unregistered_subdomains()),
    ?assertEqual([?DOMAIN_1, ?DOMAIN_2], get_all_unregistered_domains()).

handles_missing_domain_or_subdomain(_Config) ->
    %% ?MISSING_DOMAIN is used to emulate domain/subdomain removal
    %% before mongoose_lazy_routing processes domain registration
    ?assertEqual(false, maybe_add_domain_or_subdomain(?MISSING_DOMAIN)),
    ?assertEqual(false, maybe_add_domain_or_subdomain(?MISSING_DOMAIN_2)),
    ?assertEqual(1, meck:num_calls(mongoose_lazy_routing, handle_call,
                                   [{maybe_add_domain_or_subdomain, ?MISSING_DOMAIN},
                                    '_', '_'])),
    ?assertEqual(0, meck:num_calls(mongoose_lazy_routing, handle_call,
                                   [{maybe_add_domain_or_subdomain, ?MISSING_DOMAIN_2},
                                    '_', '_'])).

registers_top_level_domain_in_case_domain_subdomain_conflicts(_Config) ->
    %% add 2 times domain which has name collision with subdomain
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_X)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_X)),
    %% check that only top level domain is added
    ?assertEqualLists([?DOMAIN_X], get_all_registered_domains()),
    ?assertEqualLists([], get_all_registered_subdomains()),
    ?assertEqual([], get_all_unregistered_domains()),
    ?assertEqual([], get_all_unregistered_subdomains()),
    [meck:reset(M) || M <- [ejabberd_local, mongoose_router]],
    %% try to remove that domain and subdomain
    maybe_remove_domain(domain_host_type(?DOMAIN_X), ?DOMAIN_X),
    maybe_remove_subdomain(subdomain_info(?DOMAIN_X)),
    mongoose_lazy_routing:sync(),
    %% check that only top level domain is removed
    ?assertEqualLists([], get_all_registered_domains()),
    ?assertEqualLists([], get_all_registered_subdomains()),
    ?assertEqual([], get_all_unregistered_subdomains()),
    ?assertEqual([?DOMAIN_X], get_all_unregistered_domains()).

can_register_and_unregister_iq_handler_for_two_domains(_Config) ->
    %% add 2 domains for ?HOST_TYPE_2 and one subdomain (just to ensure that subdomain
    %% doesn't affect anything)
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_2)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_3)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_2)),
    [begin %% repeat twice
         %% register IQ handler for ?HOST_TYPE_2 domains
         IQHandlerWithHostType = create_iq_handler_and_register(<<"IQH">>, ?HOST_TYPE_2,
                                                                ?NAMESPACE_1, ?COMPONENT),
         ?assertEqualLists([{?COMPONENT, ?DOMAIN_2, ?NAMESPACE_1, IQHandlerWithHostType},
                            {?COMPONENT, ?DOMAIN_3, ?NAMESPACE_1, IQHandlerWithHostType}],
                           get_all_registered_iqs()),
         ?assertEqual([], get_all_unregistered_iqs()),
         meck:reset(gen_iq_component),
         %% unregister IQ handler for ?HOST_TYPE_2 domains
         ?assertEqual({ok, IQHandlerWithHostType},
                      unregister_iq_handler_for_domain(?HOST_TYPE_2, ?NAMESPACE_1,
                                                       ?COMPONENT)),
         ?assertEqual([], get_all_registered_iqs()),
         ?assertEqualLists([{?COMPONENT, ?DOMAIN_2, ?NAMESPACE_1},
                            {?COMPONENT, ?DOMAIN_3, ?NAMESPACE_1}],
                           get_all_unregistered_iqs()),
         meck:reset(gen_iq_component)
     end || _ <- lists:seq(1, 2)],
    %% remove 2 domains and one subdomain for ?HOST_TYPE_2
    maybe_remove_domain(domain_host_type(?DOMAIN_2), ?DOMAIN_2),
    maybe_remove_domain(domain_host_type(?DOMAIN_3), ?DOMAIN_3),
    maybe_remove_subdomain(subdomain_info(?SUBDOMAIN_2)),
    ?assertEqual([], get_all_registered_iqs()),
    ?assertEqual([], get_all_unregistered_iqs()).

can_add_domain_for_a_registered_iq_handler(_Config) ->
    %%-------------------------------------------------------------------------------
    %% this test case consists of the following steps:
    %% 1) register 2 IQ handlers for ?HOST_TYPE_1 domains
    %% 2) add ?SUBDOMAIN_1 (?HOST_TYPE_1, just to ensure that subdomain adding
    %%    doesn't affect anything)
    %% 3) add and then remove ?DOMAIN_1, check that it leads to the execution of
    %%    the proper gen_iq_component interfaces (run this step twice)
    %% 4) unregister 2 IQ handlers ?HOST_TYPE_1 domains
    %%-------------------------------------------------------------------------------

    %% register 2 IQ handlers
    IQHandlerWithHostType1 = create_iq_handler_and_register(<<"IQH1">>, ?HOST_TYPE_1,
                                                            ?NAMESPACE_1, ?COMPONENT),
    IQHandlerWithHostType2 = create_iq_handler_and_register(<<"IQH2">>, ?HOST_TYPE_1,
                                                            ?NAMESPACE_2, ?COMPONENT),
    %% add ?SUBDOMAIN_1
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_1)),
    ?assertEqual([], get_all_registered_iqs()),
    [begin %% repeat twice
         %% add ?DOMAIN_1
         ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_1)),
         ?assertEqualLists([{?COMPONENT, ?DOMAIN_1, ?NAMESPACE_1,
                             IQHandlerWithHostType1},
                            {?COMPONENT, ?DOMAIN_1, ?NAMESPACE_2,
                             IQHandlerWithHostType2}],
                           get_all_registered_iqs()),
         ?assertEqual([], get_all_unregistered_iqs()),
         meck:reset(gen_iq_component),
         %% remove ?DOMAIN_1
         maybe_remove_domain(?HOST_TYPE_1, ?DOMAIN_1),
         mongoose_lazy_routing:sync(),
         ?assertEqual([], get_all_registered_iqs()),
         ?assertEqualLists([{?COMPONENT, ?DOMAIN_1, ?NAMESPACE_1},
                            {?COMPONENT, ?DOMAIN_1, ?NAMESPACE_2}],
                           get_all_unregistered_iqs()),
         meck:reset(gen_iq_component)
     end || _ <- lists:seq(1, 2)],
    %% unregister 2 IQ handlers
    ?assertEqual({ok, IQHandlerWithHostType1},
                 unregister_iq_handler_for_domain(?HOST_TYPE_1, ?NAMESPACE_1,
                                                  ?COMPONENT)),
    ?assertEqual({ok, IQHandlerWithHostType2},
                 unregister_iq_handler_for_domain(?HOST_TYPE_1, ?NAMESPACE_2,
                                                  ?COMPONENT)),
    ?assertEqual([], get_all_unregistered_iqs()),
    ?assertEqual([], get_all_registered_iqs()).

can_register_and_unregister_iq_handler_for_two_subdomains(_Config) ->
    %% add 2 subdomains for ?HOST_TYPE_2 and one domain (just to ensure that domain
    %% doesn't affect anything)
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_2)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_3)),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_2)),
    Pattern = subdomain_pattern(?SUBDOMAIN_2),
    [begin %% repeat twice
         %% register IQ handler for ?HOST_TYPE_2 subdomains
         IQHandlerWithHostType = create_iq_handler_and_register(<<"IQH">>, ?HOST_TYPE_2,
                                                                Pattern, ?NAMESPACE_1,
                                                                ?COMPONENT),
         ?assertEqualLists([{?COMPONENT, ?SUBDOMAIN_2, ?NAMESPACE_1,
                             IQHandlerWithHostType},
                            {?COMPONENT, ?SUBDOMAIN_3, ?NAMESPACE_1,
                             IQHandlerWithHostType}],
                           get_all_registered_iqs()),
         ?assertEqual([], get_all_unregistered_iqs()),
         meck:reset(gen_iq_component),
         %% unregister IQ handler for ?HOST_TYPE_2 subdomains
         ?assertEqual({ok, IQHandlerWithHostType},
                      unregister_iq_handler_for_subdomain(?HOST_TYPE_2, Pattern,
                                                          ?NAMESPACE_1, ?COMPONENT)),
         ?assertEqual([], get_all_registered_iqs()),
         ?assertEqualLists([{?COMPONENT, ?SUBDOMAIN_2, ?NAMESPACE_1},
                            {?COMPONENT, ?SUBDOMAIN_3, ?NAMESPACE_1}],
                           get_all_unregistered_iqs()),
         meck:reset(gen_iq_component)
     end || _ <- lists:seq(1, 2)],
    %% remove 2 subdomains and one domain for ?HOST_TYPE_2
    maybe_remove_subdomain(subdomain_info(?SUBDOMAIN_2)),
    maybe_remove_subdomain(subdomain_info(?SUBDOMAIN_3)),
    maybe_remove_domain(domain_host_type(?DOMAIN_2), ?DOMAIN_2),
    ?assertEqual([], get_all_registered_iqs()),
    ?assertEqual([], get_all_unregistered_iqs()).

can_add_subdomain_for_a_registered_iq_handler(_Config) ->
    %%-------------------------------------------------------------------------------
    %% this test case consists of the following steps:
    %% 1) register 2 IQ handlers for ?HOST_TYPE_1 subdomains
    %% 2) add ?DOMAIN_1 (?HOST_TYPE_1, just to ensure that domain adding
    %%    doesn't affect anything)
    %% 3) add and then remove ?SUBDOMAIN_1, check that it leads to the execution of
    %%    the proper gen_iq_component interfaces (run this step twice)
    %% 4) unregister 2 IQ handlers ?HOST_TYPE_1 subdomains
    %%-------------------------------------------------------------------------------

    %% register 2 IQ handlers
    Pattern = subdomain_pattern(?SUBDOMAIN_1),
    IQHandlerWithHostType1 = create_iq_handler_and_register(<<"IQH1">>, ?HOST_TYPE_1,
                                                            Pattern, ?NAMESPACE_1,
                                                            ?COMPONENT),
    IQHandlerWithHostType2 = create_iq_handler_and_register(<<"IQH2">>, ?HOST_TYPE_1,
                                                            Pattern, ?NAMESPACE_2,
                                                            ?COMPONENT),
    %% add ?SUBDOMAIN_1
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_1)),
    ?assertEqual([], get_all_registered_iqs()),
    [begin %% repeat twice
         %% add ?SUBDOMAIN_1
         ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_1)),
         ?assertEqualLists([{?COMPONENT, ?SUBDOMAIN_1, ?NAMESPACE_1,
                             IQHandlerWithHostType1},
                            {?COMPONENT, ?SUBDOMAIN_1, ?NAMESPACE_2,
                             IQHandlerWithHostType2}],
                           get_all_registered_iqs()),
         ?assertEqual([], get_all_unregistered_iqs()),
         meck:reset(gen_iq_component),
         %% remove ?SUBDOMAIN_1
         maybe_remove_subdomain(subdomain_info(?SUBDOMAIN_1)),
         mongoose_lazy_routing:sync(),
         ?assertEqual([], get_all_registered_iqs()),
         ?assertEqualLists([{?COMPONENT, ?SUBDOMAIN_1, ?NAMESPACE_1},
                            {?COMPONENT, ?SUBDOMAIN_1, ?NAMESPACE_2}],
                           get_all_unregistered_iqs()),
         meck:reset(gen_iq_component)
     end || _ <- lists:seq(1, 2)],
    %% unregister 2 IQ handlers
    ?assertEqual({ok, IQHandlerWithHostType1},
                 unregister_iq_handler_for_subdomain(?HOST_TYPE_1, Pattern,
                                                     ?NAMESPACE_1, ?COMPONENT)),
    ?assertEqual({ok, IQHandlerWithHostType2},
                 unregister_iq_handler_for_subdomain(?HOST_TYPE_1, Pattern,
                                                     ?NAMESPACE_2, ?COMPONENT)),
    ?assertEqual([], get_all_unregistered_iqs()),
    ?assertEqual([], get_all_registered_iqs()).

handles_double_iq_handler_registration_deregistration_for_domain(_Config) ->
    %% add one domain and register IQ handler for it.
    ?assertEqual(true, maybe_add_domain_or_subdomain(?DOMAIN_1)),
    IQHandlerWithHostType = create_iq_handler_and_register(<<"IQH">>, ?HOST_TYPE_1,
                                                           ?NAMESPACE_1, ?COMPONENT),
    ?assertEqual([{?COMPONENT, ?DOMAIN_1, ?NAMESPACE_1, IQHandlerWithHostType}],
                 get_all_registered_iqs()),
    meck:reset(gen_iq_component),
    %% try to register IQ handler for the same host type,
    %% component and namespace one more time.
    ?assertEqual({error, already_registered},
                 register_iq_handler_for_domain(?HOST_TYPE_1, ?NAMESPACE_1, ?COMPONENT,
                                                IQHandlerWithHostType)),
    ?assertEqual([], get_all_registered_iqs()),
    ?assertEqual([], get_all_unregistered_iqs()),
    %% unregister IQ handler.
    ?assertEqual({ok, IQHandlerWithHostType},
                 unregister_iq_handler_for_domain(?HOST_TYPE_1, ?NAMESPACE_1,
                                                  ?COMPONENT)),
    ?assertEqual([{?COMPONENT, ?DOMAIN_1, ?NAMESPACE_1}], get_all_unregistered_iqs()),
    meck:reset(gen_iq_component),
    %% try unregister IQ handler one more time.
    ?assertEqual({error, not_found},
                 unregister_iq_handler_for_domain(?HOST_TYPE_1, ?NAMESPACE_1,
                                                  ?COMPONENT)),
    ?assertEqual([], get_all_registered_iqs()),
    ?assertEqual([], get_all_unregistered_iqs()).

handles_double_iq_handler_registration_deregistration_for_subdomain(_Config) ->
    %% add one subdomain and register IQ handler for it.
    Pattern = subdomain_pattern(?SUBDOMAIN_1),
    ?assertEqual(true, maybe_add_domain_or_subdomain(?SUBDOMAIN_1)),
    IQHandlerWithHostType = create_iq_handler_and_register(<<"IQH">>, ?HOST_TYPE_1,
                                                           Pattern, ?NAMESPACE_1,
                                                           ?COMPONENT),
    ?assertEqual([{?COMPONENT, ?SUBDOMAIN_1, ?NAMESPACE_1, IQHandlerWithHostType}],
                 get_all_registered_iqs()),
    meck:reset(gen_iq_component),
    %% try to register IQ handler for the same subdomain pattern,
    %% component and namespace one more time.
    ?assertEqual({error, already_registered},
                 register_iq_handler_for_subdomain(?HOST_TYPE_1, Pattern, ?NAMESPACE_1,
                                                   ?COMPONENT, IQHandlerWithHostType)),
    ?assertEqual([], get_all_registered_iqs()),
    ?assertEqual([], get_all_unregistered_iqs()),
    %% unregister IQ handler.
    ?assertEqual({ok, IQHandlerWithHostType},
                 unregister_iq_handler_for_subdomain(?HOST_TYPE_1, Pattern,
                                                     ?NAMESPACE_1, ?COMPONENT)),
    ?assertEqual([{?COMPONENT, ?SUBDOMAIN_1, ?NAMESPACE_1}],
                 get_all_unregistered_iqs()),
    meck:reset(gen_iq_component),
    %% try unregister IQ handler one more time.
    ?assertEqual({error, not_found},
                 unregister_iq_handler_for_subdomain(?HOST_TYPE_1, Pattern,
                                                     ?NAMESPACE_1, ?COMPONENT)),
    ?assertEqual([], get_all_registered_iqs()),
    ?assertEqual([], get_all_unregistered_iqs()).

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
setup_meck() ->
    Modules = [ejabberd_local, mongoose_router, gen_iq_component,
               mongoose_domain_core, mongoose_subdomain_core],
    [meck:new(M, [no_link]) || M <- Modules],
    meck:new(mongoose_lazy_routing, [no_link, passthrough]),
    meck:expect(ejabberd_local, register_host, fun(_) -> ok end),
    meck:expect(ejabberd_local, unregister_host, fun(_) -> ok end),
    meck:expect(mongoose_router, unregister_route, fun(_) -> ok end),
    meck:expect(mongoose_router, register_route, fun(_, _) -> ok end),
    meck:expect(gen_iq_component, register_iq_handler, fun(_, _, _, _) -> ok end),
    meck:expect(gen_iq_component, sync, fun(_) -> ok end),
    meck:expect(gen_iq_component, unregister_iq_handler, fun(_, _, _) -> ok end),
    meck:expect(mongoose_domain_core, get_host_type, fun get_domain_host_type/1),
    meck:expect(mongoose_subdomain_core, get_host_type, fun get_subdomain_host_type/1),
    meck:expect(mongoose_subdomain_core, get_subdomain_info, fun get_subdomain_info/1).

predefined_domains() ->
    #{?DOMAIN_1 => ?HOST_TYPE_1,
      ?DOMAIN_2 => ?HOST_TYPE_2,
      ?DOMAIN_3 => ?HOST_TYPE_2,
      ?DOMAIN_X => ?HOST_TYPE_1}.

-spec predefined_subdomains() ->
    #{jid:lserver() => mongoose_subdomain_core:subdomain_info()}.
predefined_subdomains() ->
    SubdomainPattern = mongoose_subdomain_utils:make_subdomain_pattern(<<"sub.@HOST@">>),
    FQDNPattern = mongoose_subdomain_utils:make_subdomain_pattern(?DOMAIN_X),
    #{?SUBDOMAIN_1 =>
          #{host_type => ?HOST_TYPE_1, subdomain => ?SUBDOMAIN_1,
            subdomain_pattern => SubdomainPattern, parent_domain => ?DOMAIN_1,
            packet_handler => mongoose_packet_handler:new(packet_handler_1,
                                                          #{host_type => ?HOST_TYPE_1})},
      ?SUBDOMAIN_2 =>
          #{host_type => ?HOST_TYPE_2, subdomain => ?SUBDOMAIN_2,
            subdomain_pattern => SubdomainPattern, parent_domain => ?DOMAIN_2,
            packet_handler => mongoose_packet_handler:new(packet_handler_2,
                                                          #{host_type => ?HOST_TYPE_2})},
      ?SUBDOMAIN_3 =>
          #{host_type => ?HOST_TYPE_2, subdomain => ?SUBDOMAIN_3,
            subdomain_pattern => SubdomainPattern, parent_domain => ?DOMAIN_3,
            packet_handler => mongoose_packet_handler:new(packet_handler_3,
                                                          #{host_type => ?HOST_TYPE_2})},
      ?DOMAIN_X =>
          #{host_type => ?HOST_TYPE_1, subdomain => ?DOMAIN_X,
            subdomain_pattern => FQDNPattern, parent_domain => no_parent_domain,
            packet_handler => mongoose_packet_handler:new(packet_handler_x,
                                                          #{host_type => ?HOST_TYPE_1})}}.

get_domain_host_type(Domain) ->
    case maps:get(Domain, predefined_domains(), undefined) of
        undefined -> {error, not_found};
        HostType -> {ok, HostType}
    end.

domain_host_type(Domain) ->
    element(2, get_domain_host_type(Domain)).

get_subdomain_host_type(?MISSING_DOMAIN) ->
    %% required for handles_domain_or_subdomain_removal_while_adding test case
    {ok, ?HOST_TYPE_1};
get_subdomain_host_type(Subdomain) ->
    case maps:get(Subdomain, predefined_subdomains(), undefined) of
        undefined -> {error, not_found};
        SubdomainInfo -> {ok, maps:get(host_type, SubdomainInfo)}
    end.

get_subdomain_info(Subdomain) ->
    case maps:get(Subdomain, predefined_subdomains(), undefined) of
        undefined -> {error, not_found};
        SubdomainInfo -> {ok, SubdomainInfo}
    end.

subdomain_info(Subdomain) ->
    element(2, get_subdomain_info(Subdomain)).

packet_handler(Subdomain) ->
    maps:get(packet_handler, maps:get(Subdomain, predefined_subdomains())).

subdomain_pattern(Subdomain) ->
    maps:get(subdomain_pattern, maps:get(Subdomain, predefined_subdomains())).

iq_handler(Extra) ->
    FN = fun(Acc, _From, _To, _IQ, _Extra) -> {Acc, ignore} end,
    %% using no_queue execution type to ensure this function returns
    %% one the same IQ handler if we call it with the same parameters.
    mongoose_iq_handler:new(FN, Extra, no_queue).

get_all_registered_domains() ->
    History = meck:history(ejabberd_local),
    [Domain || {_Pid, {_Mod, Func, [Domain] = _Args}, _Result} <- History,
               Func =:= register_host].

get_all_unregistered_domains() ->
    History = meck:history(ejabberd_local),
    [Domain || {_Pid, {_Mod, Func, [Domain] = _Args}, _Result} <- History,
               Func =:= unregister_host].

get_all_registered_subdomains() ->
    History = meck:history(mongoose_router),
    [{Subdomain, PacketHandler}
     || {_Pid, {_Mod, Func, [Subdomain, PacketHandler] = _Args}, _Result} <- History,
        Func =:= register_route].

get_all_unregistered_subdomains() ->
    History = meck:history(mongoose_router),
    [Subdomain || {_Pid, {_Mod, Func, [Subdomain] = _Args}, _Result} <- History,
                  Func =:= unregister_route].

create_iq_handler_and_register(Tag, HostType, Namespace, Component) ->
    IQHandler = iq_handler(#{tag => Tag}),
    Ret = register_iq_handler_for_domain(HostType, Namespace, Component, IQHandler),
    ?assertEqual(ok, Ret),
    mongoose_iq_handler:add_extra(IQHandler, #{host_type => HostType}).

create_iq_handler_and_register(Tag, HostType, SubdomainPattern, Namespace, Component) ->
    IQHandler = iq_handler(#{tag => Tag}),
    Ret = register_iq_handler_for_subdomain(HostType, SubdomainPattern, Namespace,
                                            Component, IQHandler),
    ?assertEqual(ok, Ret),
    mongoose_iq_handler:add_extra(IQHandler, #{host_type => HostType}).

get_all_registered_iqs() ->
    History = meck:history(gen_iq_component),
    [list_to_tuple(Args)
     || {_Pid, {_Mod, register_iq_handler = _Func, Args}, _Result} <- History].

get_all_unregistered_iqs() ->
    History = meck:history(gen_iq_component),
    [list_to_tuple(Args)
     || {_Pid, {_Mod, unregister_iq_handler = _Func, Args}, _Result} <- History].
