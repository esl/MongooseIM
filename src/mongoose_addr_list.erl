-module(mongoose_addr_list).

-feature(maybe_expr, enable).

-compile({inline, [inet_to_dns_type/1, dns_to_inet_type/1]}).

-xep([{xep, 368}, {version, "1.1.0"}]).

-include("mongoose_logger.hrl").

-export([get_addr_list/3]).

-type hostname() :: string().
-type dns_ip_type() :: a | aaaa.
-type dns_ip_types() :: [a | aaaa, ...].
-type with_tls() :: boolean().
-type srv() :: {Prio :: integer(),
                Weight :: integer(),
                Port :: inet:port_number(),
                DnsName :: hostname()}.
-type srv_tls() :: {Prio :: integer(),
                    Weight :: integer(),
                    Port :: inet:port_number(),
                    DnsName :: hostname(),
                    Tls :: with_tls()}.
-type pre_addr() :: #{ip_address := string(),
                      ip_tuple := inet:ip_address(),
                      ip_version := inet | inet6,
                      tls := with_tls(),
                      port => inet:port_number()}.
-type addr() :: #{ip_address => string(),
                  ip_tuple := inet:ip_address(),
                  ip_version := inet | inet6,
                  port := inet:port_number(),
                  tls := with_tls()}.
-export_type([addr/0]).

-spec get_addr_list(HostType :: mongooseim:host_type(),
                    LServer :: jid:lserver(),
                    EnforcedTls :: with_tls()) -> [addr()].
get_addr_list(HostType, LServer, EnforceTls) ->
    maybe
        [] ?= get_predefined_addresses(HostType, LServer, EnforceTls),
        Domain = binary_to_list(LServer),
        [] ?= lookup_services(HostType, Domain, EnforceTls),
        lookup_addrs(HostType, Domain, EnforceTls)
    else
        [_|_] = Res ->
            Res
    end.

%% @doc Get IPs predefined for a given s2s domain in the configuration
-spec get_predefined_addresses(HostType :: mongooseim:host_type(),
                               LServer :: jid:lserver(),
                               EnforceTls :: with_tls()) -> [addr()].
get_predefined_addresses(HostType, LServer, EnforceTls) ->
    case lookup_predefined_addresses(HostType, LServer) of
        {ok, M} ->
            ensure_tls_and_port(HostType, M, EnforceTls);
        _ ->
            []
    end.

%% https://datatracker.ietf.org/doc/html/rfc6120#section-3.2.1 Preferred Process: SRV Lookup
%%    The preferred process for FQDN resolution is to use [DNS-SRV] records
-spec lookup_services(HostType :: mongooseim:host_type(),
                      Domain :: hostname(),
                      EnforceTls :: with_tls()) -> [addr()].
lookup_services(HostType, Domain, EnforceTls) ->
    case mongoose_s2s_lib:domain_utf8_to_ascii(Domain, string) of
        false -> [];
        ASCIIAddr -> do_lookup_services(HostType, ASCIIAddr, EnforceTls)
    end.

%% https://datatracker.ietf.org/doc/html/rfc6120#section-3.2.2 Fallback Process
%%    The fallback process SHOULD be a normal "A" or "AAAA" address record resolution
%%    to determine the IPv4 or IPv6 address of the origin domain.
-spec lookup_addrs(HostType :: mongooseim:host_type(),
                   Domain :: hostname(),
                   EnforceTls :: with_tls()) -> [addr()].
lookup_addrs(HostType, Domain, EnforceTls) ->
    Port = outgoing_s2s_port(HostType),
    Types = outgoing_s2s_types(HostType),
    Fun = fun(Type) ->
                  MaybeHostEnt = dns_lookup(HostType, Domain, Type),
                  prepare_addr(MaybeHostEnt, Port, EnforceTls, Domain, Type)
          end,
    Expanded = lists:map(Fun, Types),
    lists:flatten(Expanded).

-spec ensure_tls_and_port(mongooseim:host_type(), pre_addr(), with_tls()) -> [addr()].
ensure_tls_and_port(_, #{tls := false}, true) ->
    [];
ensure_tls_and_port(_, #{port := _} = M, _) ->
    [M];
ensure_tls_and_port(HostType, #{} = M, _) ->
    Port = outgoing_s2s_port(HostType),
    [M#{port => Port}].

-spec do_lookup_services(HostType :: mongooseim:host_type(),
                         Domain :: hostname(),
                         EnforceTls :: with_tls()) -> [addr()].
do_lookup_services(HostType, Domain, EnforceTls) ->
    case srv_lookups(HostType, Domain, EnforceTls) of
        {error, Reason} ->
            ?LOG_ERROR(#{what => s2s_srv_lookup_failed,
                         text => <<"The DNS servers failed to lookup a SRV record."
                                   " You should check your DNS configuration.">>,
                         nameserver => inet_db:res_option(nameserver),
                         service => build_service_name(EnforceTls, Domain),
                         dns_rr_type => srv, reason => Reason}),
            [];
        TlsTaggedSrvAddrLists ->
            Addrs = order_and_prepare_addrs(HostType, TlsTaggedSrvAddrLists),
            ?LOG_DEBUG(#{what => s2s_srv_lookup_success, addresses => Addrs, server => Domain}),
            Addrs
    end.

%% https://xmpp.org/extensions/xep-0368.html SRV records for XMPP over TLS
%%    This XEP extends that to include new xmpps-client/xmpps-server SRV records pointing
%%    to direct TLS ports and combine priorities and weights as if they were a single SRV
%%    record similar to RFC-6186.
%%    Both 'xmpp-' and 'xmpps-' records SHOULD be treated as the same record with regard to
%%    connection order as specified by RFC-2782, in that all priorities and weights are mixed.
%%    This enables the server operator to decide if they would rather clients connect
%%    with STARTTLS or direct TLS. However, clients MAY choose to prefer one type of
%%    connection over the other.
-spec order_and_prepare_addrs(mongooseim:host_type(), [srv_tls()]) -> [addr()].
order_and_prepare_addrs(HostType, TlsTaggedSrvAddrLists) ->
    OrderedByPriority = order_by_priority_and_weight(TlsTaggedSrvAddrLists),
    WithIpAddresses = for_each_tagged_srv_get_ip_addresses(HostType, OrderedByPriority),
    lists:flatten(WithIpAddresses).

%% Probabilities are not exactly proportional to weights
%% for simplicity (higher weights are overvalued)
%% https://datatracker.ietf.org/doc/html/rfc2782
%%  Priority:
%%      A client MUST attempt to contact the target host with
%%      the lowest-numbered priority it can reach;
%%  Weight:
%%      The weight field specifies a relative weight for entries with the same priority.
%%      Larger weights SHOULD be given a proportionately higher probability of being selected.
-spec order_by_priority_and_weight([srv_tls()]) -> [srv_tls()].
order_by_priority_and_weight(TlsTaggedSrvAddrLists) ->
    Fun = fun({P1, _W1}, {P2, _W2}) when P1 < P2 -> true;
             ({P1, _W1}, {P2, _W2}) when P1 > P2 -> false;
             ({_P1, W1}, {_P2, W2}) -> W2 < W1 end,
    lists:sort(Fun, TlsTaggedSrvAddrLists).

-spec for_each_tagged_srv_get_ip_addresses(mongooseim:host_type(), [srv_tls()]) -> [[[addr()]]].
for_each_tagged_srv_get_ip_addresses(HostType, TlsTaggedSrvAddrLists) ->
    MapFun = fun({_, _, Port, Host, Tls}) ->
                     build_with_ip_address_list_for_host(HostType, Host, Port, Tls)
             end,
    lists:map(MapFun, TlsTaggedSrvAddrLists).

-spec build_with_ip_address_list_for_host(
        mongooseim:host_type(), hostname(), inet:port_number(), with_tls()) ->
    [[addr()]].
build_with_ip_address_list_for_host(HostType, Host, Port, Tls) ->
    Types = outgoing_s2s_types(HostType),
    FoldFun = fun(Type) -> build_with_typed_dns_lookup(HostType, Host, Port, Tls, Type) end,
    lists:map(FoldFun, Types).

-spec build_with_typed_dns_lookup(
        mongooseim:host_type(), hostname(), inet:port_number(), with_tls(), dns_ip_type()) ->
    [addr()].
build_with_typed_dns_lookup(HostType, Host, Port, Tls, Type) ->
    MaybeHostEnt = dns_lookup(HostType, Host, Type),
    prepare_addr(MaybeHostEnt, Port, Tls, Host, Type).

%% Config lookup functions
-spec outgoing_s2s_types(mongooseim:host_type()) -> dns_ip_types().
outgoing_s2s_types(HostType) ->
    [inet_to_dns_type(V) || V <- ip_versions(HostType)].

-spec inet_to_dns_type(4 | 6) -> dns_ip_type().
inet_to_dns_type(4) -> a;
inet_to_dns_type(6) -> aaaa.

-spec dns_to_inet_type(a | aaaa) -> inet | inet6.
dns_to_inet_type(a) -> inet;
dns_to_inet_type(aaaa) -> inet6.

-spec ip_versions(mongooseim:host_type()) -> [4 | 6].
ip_versions(HostType) ->
    mongoose_config:get_opt([{s2s, HostType}, outgoing, ip_versions]).

-spec lookup_predefined_addresses(mongooseim:host_type(), jid:lserver()) ->
    {ok, pre_addr()} | {error, atom()}.
lookup_predefined_addresses(HostType, LServer) ->
    mongoose_config:lookup_opt([{s2s, HostType}, outgoing, address, LServer]).

-spec outgoing_s2s_port(mongooseim:host_type()) -> inet:port_number().
outgoing_s2s_port(HostType) ->
    mongoose_config:get_opt([{s2s, HostType}, outgoing, port]).

-spec get_dns(mongooseim:host_type()) ->
    #{timeout := non_neg_integer(), retries := pos_integer()}.
get_dns(HostType) ->
    mongoose_config:get_opt([{s2s, HostType}, outgoing, dns]).

-spec prepare_addr
    ([inet:ip4_address()], inet:port_number(), with_tls(), hostname(), a) -> [addr()];
    ([inet:ip6_address()], inet:port_number(), with_tls(), hostname(), aaaa) -> [addr()].
prepare_addr([_|_] = Addrs, Port, Tls, _, Type) ->
    MapFun = fun(Addr) -> #{ip_tuple => Addr, ip_version => dns_to_inet_type(Type),
                            port => Port, tls => Tls} end,
    lists:map(MapFun, Addrs);
prepare_addr([], _, _, Domain, Type) ->
    ?LOG_ERROR(#{what => s2s_dns_lookup_failed,
                 text => <<"The DNS servers failed to lookup an A|AAAA record."
                           " You should check your DNS configuration.">>,
                 nameserver => inet_db:res_option(nameserver),
                 server => Domain, dns_rr_type => Type}),
    [].

-spec srv_lookups(HostType :: mongooseim:host_type(),
                  Domain :: hostname(),
                  EnforceTls :: with_tls()) -> [srv_tls()] | {error, atom()}.
srv_lookups(HostType, Domain, true) ->
    case dns_lookup(HostType, build_service_name(true, Domain), srv) of
        [_|_] = TlsAddrList ->
            tag_tls(TlsAddrList, true);
        [] ->
            {error, timeout}
    end;
srv_lookups(HostType, Domain, _) ->
    TlsHostEnt = dns_lookup(HostType, build_service_name(true, Domain), srv),
    HostEnt = dns_lookup(HostType, build_service_name(false, Domain), srv),
    case {TlsHostEnt, HostEnt} of
        {[_|_] = TlsAddrList, [_|_] = AddrList} ->
            tag_tls(TlsAddrList, true) ++ tag_tls(AddrList, false);
        {[_|_] = TlsAddrList, []} ->
            tag_tls(TlsAddrList, true);
        {[], [_|_] = AddrList} ->
            tag_tls(AddrList, false);
        {[], []} ->
            {error, timeout}
    end.

-spec build_service_name(with_tls(), hostname()) -> hostname().
build_service_name(true, Domain) ->
    "_xmpps-server._tcp." ++ Domain;
build_service_name(false, Domain) ->
    "_xmpp-server._tcp." ++ Domain.

tag_tls(AddrList, Tls) ->
    lists:map(fun({Prio, Weight, Port, Host}) -> {Prio, Weight, Port, Host, Tls} end, AddrList).

-spec dns_lookup(mongooseim:host_type(), hostname(), a) -> [inet:ip4_address()];
                (mongooseim:host_type(), hostname(), aaaa) -> [inet:ip6_address()];
                (mongooseim:host_type(), hostname(), srv) -> [srv()].
dns_lookup(HostType, Domain, DnsRrType) ->
    #{timeout := TimeoutSec, retries := Retries} = get_dns(HostType),
    Timeout = timer:seconds(TimeoutSec),
    dns_lookup(Domain, DnsRrType, Timeout, Retries).

-spec dns_lookup(hostname(), a, timeout(), non_neg_integer()) -> [inet:ip4_address()];
                (hostname(), aaaa, timeout(), non_neg_integer()) -> [inet:ip6_address()];
                (hostname(), srv, timeout(), non_neg_integer()) -> [srv()].
dns_lookup(_Domain, _DnsRrType, _, 0) ->
    [];
dns_lookup(Domain, DnsRrType, Timeout, Retries) ->
    case inet_res:lookup(Domain, in, DnsRrType, [], Timeout) of
        [_|_] = List ->
            List;
        _ ->
            dns_lookup(Domain, DnsRrType, Timeout, Retries - 1)
    end.
