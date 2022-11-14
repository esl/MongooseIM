-module(mod_dynamic_domains_test).

-include("mongoose_config_spec.hrl").
-include("jlib.hrl").

-behaviour(mongoose_packet_handler).
-behaviour(gen_mod).

%% API
-export([start/2, stop/1,
         config_spec/0,
         supported_features/0]).
-export([process_packet/5, process_iq/5]).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"host1">> =>
                           #option{type = string,
                                   validate = subdomain_template,
                                   process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
                       <<"host2">> =>
                           #option{type = string,
                                   validate = subdomain_template,
                                   process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
                       <<"namespace">> =>
                           #option{type = binary,
                                   validate = non_empty}}
            }.

supported_features() -> [dynamic_domains].

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    Namespace = gen_mod:get_opt(namespace, Opts),

    %% if we need to intercept traffic to some subdomain, we can create custom packet
    %% handler and register it. note that IQs will be delivered to this packet handler
    %% as well, even if an IQ handler is registered for the same subdomain. IQ handler
    %% doesn't work at all if the IQ packet is not routed to the corresponding component.
    SubdomainPattern1 = gen_mod:get_opt(host1, Opts),
    Handler1 = mongoose_packet_handler:new(?MODULE),
    mongoose_domain_api:register_subdomain(HostType, SubdomainPattern1, Handler1),
    %% the call below is added for the demo & testing purposes, all the IQs sent for
    %% SubdomainPattern1 will go only to the process_packet/5 function.
    gen_iq_handler:add_iq_handler_for_subdomain(HostType, SubdomainPattern1,
                                                Namespace, ejabberd_local,
                                                fun ?MODULE:process_iq/5,
                                                #{handler_type => subdomain},
                                                one_queue),

    %% If we want to just process IQs sent to some subdomain, and we don't care about
    %% any other messages, then we can use `ejabberd_local` packet handler for such
    %% subdomain and register IQ handler in the similar way as we do for domains.
    SubdomainPattern2 = gen_mod:get_opt(host2, Opts),
    Handler2 = mongoose_packet_handler:new(ejabberd_local),
    mongoose_domain_api:register_subdomain(HostType, SubdomainPattern2, Handler2),
    gen_iq_handler:add_iq_handler_for_subdomain(HostType, SubdomainPattern2,
                                                Namespace, ejabberd_local,
                                                fun ?MODULE:process_iq/5,
                                                #{handler_type => subdomain},
                                                one_queue),

    %% we can use the new gen_iq_handler API to register IQ handlers for dynamic domains
    gen_iq_handler:add_iq_handler_for_domain(HostType, Namespace, ejabberd_local,
                                             fun ?MODULE:process_iq/5,
                                             #{handler_type => domain},
                                             one_queue),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Opts = gen_mod:get_module_opts(HostType, ?MODULE),
    Namespace = gen_mod:get_opt(namespace, Opts),

    SubdomainPattern1 = gen_mod:get_opt(host1, Opts),
    mongoose_domain_api:unregister_subdomain(HostType, SubdomainPattern1),
    gen_iq_handler:remove_iq_handler_for_subdomain(HostType, SubdomainPattern1,
                                                   Namespace, ejabberd_local),

    SubdomainPattern2 = gen_mod:get_opt(host2, Opts),
    mongoose_domain_api:unregister_subdomain(HostType, SubdomainPattern2),
    gen_iq_handler:remove_iq_handler_for_subdomain(HostType, SubdomainPattern2,
                                                   Namespace, ejabberd_local),

    gen_iq_handler:remove_iq_handler_for_domain(HostType, Namespace, ejabberd_local),

    ok.

-spec process_packet(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                     El :: exml:element(), Extra :: gen_hook:extra()) -> mongoose_acc:t().
process_packet(Acc, _From, _To, _El, _Extra) ->
    %% do nothing, just ignore the packet
    Acc.

-spec process_iq(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                 IQ :: jlib:iq(), Extra :: gen_hook:extra()) -> {NewAcc :: mongoose_acc:t(),
                                                      IQResp :: ignore | jlib:iq()}.
process_iq(Acc, _From, _To, IQ, _Extra) ->
    %% reply with empty result IQ stanza
    {Acc, IQ#iq{type = result, sub_el = []}}.
