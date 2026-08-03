-module(mongoose_addr_list_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").

-define(HOST_TYPE, <<"localhost">>).
-define(REMOTE_DOMAIN, <<"partners.example.net">>).

all() ->
    [srv_records_are_ordered_by_priority_and_weight].

init_per_suite(Config) ->
    mongoose_config:set_opts(opts()),
    ok = meck:new(inet_res, [no_link, unstick, passthrough]),
    Config.

end_per_suite(_Config) ->
    meck:unload(inet_res),
    mongoose_config:erase_opts().

srv_records_are_ordered_by_priority_and_weight(_Config) ->
    meck:expect(inet_res, lookup, fun dns_lookup/5),
    ?assertEqual(
       [#{ip_tuple => {192, 0, 2, 10}, ip_version => inet, port => 5269, tls => false},
        #{ip_tuple => {192, 0, 2, 20}, ip_version => inet, port => 5270, tls => false},
        #{ip_tuple => {192, 0, 2, 30}, ip_version => inet, port => 5269, tls => false}],
       mongoose_addr_list:get_addr_list(?HOST_TYPE, ?REMOTE_DOMAIN, false)).

opts() ->
    #{{s2s, ?HOST_TYPE} => config_parser_helper:default_config([s2s])}.

dns_lookup("_xmpps-server._tcp.partners.example.net", in, srv, _Opts, _Timeout) ->
    [];
dns_lookup("_xmpp-server._tcp.partners.example.net", in, srv, _Opts, _Timeout) ->
    [{20, 10, 5269, "s2s-backup.partners.example.net"},
     {10, 0, 5269, "s2s-primary.partners.example.net"},
     {20, 50, 5270, "s2s-east.partners.example.net"}];
dns_lookup("s2s-primary.partners.example.net", in, a, _Opts, _Timeout) ->
    [{192, 0, 2, 10}];
dns_lookup("s2s-east.partners.example.net", in, a, _Opts, _Timeout) ->
    [{192, 0, 2, 20}];
dns_lookup("s2s-backup.partners.example.net", in, a, _Opts, _Timeout) ->
    [{192, 0, 2, 30}];
dns_lookup(Name, Class, Type, Opts, Timeout) ->
    meck:passthrough([Name, Class, Type, Opts, Timeout]).
