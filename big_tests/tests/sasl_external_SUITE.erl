-module(sasl_external_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(domain_helper, [host_type/0, domain/0]).
-import(distributed_helper, [mim/0]).

all() ->
    [
     {group, self_signed},
     {group, ca_signed}
    ].

groups() ->
    [
     {standard_keep_auth, [{group, registered}, {group, not_registered}]},
     {registered, [parallel], [cert_one_xmpp_addrs_no_identity]},
     {not_registered, [parallel], [cert_one_xmpp_addrs_no_identity_not_registered]},
     {standard, [parallel], standard_test_cases()},
     {use_common_name, [parallel], use_common_name_test_cases()},
     {use_common_name_with_prefix, [parallel], use_common_name_test_cases()},
     {allow_just_user_identity, [parallel], allow_just_user_identity_test_cases()},
     {demo_verification_module, [parallel], demo_verification_module_test_cases()},
     {self_signed_certs_allowed, [parallel], self_signed_certs_allowed_test_cases()},
     {self_signed_certs_not_allowed, [parallel], self_signed_certs_not_allowed_test_cases()},
     {ca_signed, [self_signed_certs_not_allowed_group() | base_groups()]},
     {self_signed, [self_signed_certs_allowed_group() | base_groups()]}
    ].

self_signed_certs_allowed_group() ->
    {group, self_signed_certs_allowed}.
self_signed_certs_not_allowed_group() ->
    {group, self_signed_certs_not_allowed}.

base_groups() ->
    [
     {group, standard},
     {group, standard_keep_auth},
     {group, use_common_name},
     {group, use_common_name_with_prefix},
     {group, allow_just_user_identity},
     {group, demo_verification_module}
    ].

standard_test_cases() ->
    [
     cert_no_xmpp_addrs_fails,
     cert_no_xmpp_addrs_no_identity,
     cert_one_xmpp_addr_identity_correct,
     cert_one_xmpp_addrs_no_identity,
     cert_one_xmpp_addr_wrong_hostname,
     cert_more_xmpp_addrs_identity_correct,
     cert_more_xmpp_addrs_no_identity_fails,
     cert_more_xmpp_addrs_wrong_identity_fails,
     cert_with_critical_extension_connects_but_fails_to_authenticate
    ].

use_common_name_test_cases() ->
    [
     cert_with_cn_no_xmpp_addrs_identity_correct,
     cert_with_cn_no_xmpp_addrs_wrong_identity_fails,
     cert_with_cn_no_xmpp_addrs_no_identity
    ].

allow_just_user_identity_test_cases() ->
    [
     cert_no_xmpp_addrs_just_use_identity
    ].

demo_verification_module_test_cases()->
    [cert_no_xmpp_addrs_just_use_identity,
     cert_one_xmpp_addrs_no_identity,
     cert_with_jid_cn_no_xmpp_addrs_no_identity,
     cert_with_jid_cn_many_xmpp_addrs_no_identity,
     cert_more_xmpp_addrs_no_identity_fails,
     cert_no_xmpp_addrs_no_identity].

self_signed_certs_allowed_test_cases() ->
    [
     self_signed_cert_is_allowed_with_tls,
     self_signed_cert_is_allowed_with_ws,
     self_signed_cert_is_allowed_with_bosh,
     no_cert_fails_to_authenticate
    ].

self_signed_certs_not_allowed_test_cases() ->
    [self_signed_cert_fails_to_authenticate_with_tls,
     self_signed_cert_fails_to_authenticate_with_ws,
     self_signed_cert_fails_to_authenticate_with_bosh,
     ca_signed_cert_is_allowed_with_ws,
     ca_signed_cert_is_allowed_with_bosh,
     no_cert_fails_to_authenticate].

init_per_suite(Config) ->
    Config0 = escalus:init_per_suite(Config),
    Config1 = ejabberd_node_utils:init(Config0),
    generate_certs(Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(ca_signed, Config) ->
    [{signed, ca} | Config];
init_per_group(self_signed, Config) ->
    [{signed, self} | Config];
init_per_group(registered, Config) ->
    escalus:create_users(Config, [{bob, generate_user_tcp(Config, username("bob", Config))}]);
init_per_group(not_registered, Config) ->
    Config;
init_per_group(standard_keep_auth = GroupName, Config) ->
    Config1 = configure(GroupName, Config),
    %% Currently only LDAP supports SASL External
    case mongoose_helper:supports_sasl_module(cyrsasl_external) of
        false ->
            restore(Config1), % required because end_per_group is skipped
            {skip, "SASL External not supported"};
        true ->
            Config1
    end;
init_per_group(GroupName, Config) ->
    configure(GroupName, Config).

configure(GroupName, Config) ->
    AuthOpts = auth_opts(GroupName),
    Config1 = mongoose_helper:backup_and_set_config(Config, config_opts(AuthOpts)),
    TLSOpts = tls_opts(Config1),
    HTTPSOpts = maps:remove(disconnect_on_failure, TLSOpts), % there is no such option for HTTPS
    OrigListeners = [restart_listener(c2s_port, mongoose_c2s_listener, TLSOpts),
                     restart_listener(cowboy_secure_port, ejabberd_cowboy, HTTPSOpts)],
    [{orig_listeners, OrigListeners}, {auth_opts, AuthOpts} | Config1].

restart_listener(PortName, Module, NewOpts) ->
    Port = ct:get_config({hosts, mim, PortName}),
    [Listener] = mongoose_helper:get_listeners(mim(), #{port => Port, module => Module}),
    NewListener = add_tls_opts(Listener, NewOpts),
    ok = mongoose_helper:restart_listener(mim(), NewListener),
    Listener.

tls_opts(Config) ->
    #{disconnect_on_failure => false,
      verify_mode => verify_mode(proplists:get_value(signed, Config)),
      cacertfile => filename:join([path_helper:repo_dir(Config),
                                   "tools", "ssl", "ca-clients", "cacert.pem"])}.

verify_mode(ca) -> peer;
verify_mode(self) -> selfsigned_peer.

add_tls_opts(C2SListener = #{tls := TLS}, Extra) ->
    C2SListener#{tls => maps:merge(TLS, Extra)}.

config_opts(AuthOpts) ->
    #{[{auth, host_type()}, Opt] => Value || Opt := Value <- AuthOpts}.

auth_opts(standard_keep_auth) ->
    #{sasl_mechanisms => [cyrsasl_external],
      sasl_external => [standard]};
auth_opts(use_common_name) ->
    #{methods => [pki],
      sasl_mechanisms => [cyrsasl_external],
      sasl_external => [standard, common_name]};
auth_opts(use_common_name_with_prefix) ->
    Opts = auth_opts(use_common_name),
    Opts#{sasl_external_common_name => #{prefix => <<"user_">>, suffix => <<"-test">>}};
auth_opts(allow_just_user_identity) ->
    #{methods => [pki],
      sasl_mechanisms => [cyrsasl_external],
      sasl_external => [standard, auth_id]};
auth_opts(demo_verification_module) ->
    #{methods => [pki],
      sasl_mechanisms => [cyrsasl_external],
      sasl_external => [{mod, cyrsasl_external_verification}]};
auth_opts(_GroupName) ->
    #{methods => [pki],
      sasl_mechanisms => [cyrsasl_external],
      sasl_external => [standard]}.

end_per_group(ca_signed, _Config) ->
    ok;
end_per_group(self_signed, _Config) ->
    ok;
end_per_group(registered, Config) ->
    escalus:delete_users(Config, [{bob, generate_user_tcp(Config, username("bob", Config))}]);
end_per_group(not_registered, _Config) ->
    ok;
end_per_group(_, Config) ->
    restore(Config).

restore(Config) ->
    mongoose_helper:restore_config(Config),
    OrigListeners = proplists:get_value(orig_listeners, Config, []),
    [mongoose_helper:restart_listener(mim(), Listener) || Listener <- OrigListeners],
    ok.

cert_more_xmpp_addrs_identity_correct(C) ->
    User = username("kate", C),
    %% More than one xmpp_addr and specified identity, common_name not used
    UserSpec = [{requested_name, requested_name(User)} |
		generate_user_tcp(C, User)],
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_one_xmpp_addr_identity_correct(C) ->
    User = username("bob", C),
    UserSpec = [{requested_name, requested_name(User)} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

cert_no_xmpp_addrs_fails(C) ->
    User = username("john", C),
    UserSpec = [{requested_name, requested_name(User)} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

cert_no_xmpp_addrs_just_use_identity(C) ->
    User = username("not-mike", C),
    UserSpec = [{requested_name, requested_name("mike")} |
		generate_user_tcp(C, User)],
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_no_xmpp_addrs_no_identity(C) ->
    User = username("john", C),
    UserSpec = generate_user_tcp(C, User),
    cert_fails_to_authenticate(UserSpec).

cert_more_xmpp_addrs_no_identity_fails(C) ->
    User = username("not-alice", C),
    UserSpec = generate_user_tcp(C, User),
    cert_fails_to_authenticate(UserSpec).

cert_one_xmpp_addrs_no_identity(C) ->
    User = username("bob", C),
    UserSpec = generate_user_tcp(C, User),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_one_xmpp_addrs_no_identity_not_registered(C) ->
    User = username("bob", C),
    UserSpec = generate_user_tcp(C, User),
    cert_fails_to_authenticate(UserSpec).

cert_with_cn_no_xmpp_addrs_no_identity(C) ->
    User = username("john", C),
    UserSpec = generate_user_tcp(C, User),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_with_jid_cn_no_xmpp_addrs_no_identity(C) ->
    User = add_domain_str("john"),
    UserSpec = generate_user_tcp(C, User),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_with_jid_cn_many_xmpp_addrs_no_identity(C) ->
    User = add_domain_str("grace"),
    UserSpec = generate_user_tcp(C, User),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_with_cn_no_xmpp_addrs_identity_correct(C) ->
    User = username("john", C),
    UserSpec = [{requested_name, requested_name(User)} |
                generate_user_tcp(C, User)],
    {ok, Client, _} = escalus_connection:start(UserSpec),
    check_username(User, C, escalus_client:username(Client)),
    escalus_connection:stop(Client).

cert_with_cn_no_xmpp_addrs_wrong_identity_fails(C) ->
    User = username("not-mike", C),
    UserSpec = [{requested_name, requested_name("mike")} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

cert_more_xmpp_addrs_wrong_identity_fails(C) ->
    User = username("grace", C),
    UserSpec = [{requested_name, requested_name(User)} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

cert_with_critical_extension_connects_but_fails_to_authenticate(C) ->
    User = username("alice-critical", C),
    UserSpec = [{requested_name, requested_name(User)} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

cert_one_xmpp_addr_wrong_hostname(C) ->
    User = username("bob", C),
    UserSpec = [{requested_name, requested_name(User)} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

ca_signed_cert_is_allowed_with_ws(C) ->
    UserSpec = generate_user(C, "bob", escalus_ws),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

ca_signed_cert_is_allowed_with_bosh(C) ->
    UserSpec = generate_user(C, "bob", escalus_bosh),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

self_signed_cert_fails_to_authenticate_with_tls(C) ->
    self_signed_cert_fails_to_authenticate(C, escalus_tcp).

self_signed_cert_fails_to_authenticate_with_ws(C) ->
    self_signed_cert_fails_to_authenticate(C, escalus_ws).

self_signed_cert_fails_to_authenticate_with_bosh(C) ->
    self_signed_cert_fails_to_authenticate(C, escalus_bosh).

cert_fails_to_authenticate(UserSpec) ->
    Self = self(),
    F = fun() ->
		{ok, Client, _} = escalus_connection:start(UserSpec),
		Self ! escalus_connected,
		escalus_connection:stop(Client)
	end,
    receive_failed_to_authenticate(F).

self_signed_cert_fails_to_authenticate(C, EscalusTransport) ->
    Self = self(),
    F = fun() ->
		UserSpec = generate_user(C, "greg-self-signed", EscalusTransport),
		{ok, Client, _} = escalus_connection:start(UserSpec),
		Self ! escalus_connected,
		escalus_connection:stop(Client)
	end,
    receive_failed_to_authenticate(F).

receive_failed_to_authenticate(F) ->
    %% We spawn the process trying to connect because otherwise the testcase may crash
    %% due linked process crash (client's process are started with start_link)
    Pid = spawn(F),
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _Reason} ->
            ok;
        escalus_connected ->
            ct:fail(authenticated_but_should_not)
    after 10000 ->
              ct:fail(timeout_waiting_for_authentication_error)
    end.

self_signed_cert_is_allowed_with_tls(C) ->
    self_signed_cert_is_allowed_with(escalus_tcp, C).

self_signed_cert_is_allowed_with_ws(C) ->
    self_signed_cert_is_allowed_with(escalus_ws, C).

self_signed_cert_is_allowed_with_bosh(C) ->
    self_signed_cert_is_allowed_with(escalus_bosh, C).

self_signed_cert_is_allowed_with(EscalusTransport, C) ->
    UserSpec = generate_user(C, "bob-self-signed", EscalusTransport),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

no_cert_fails_to_authenticate(_C) ->
    UserSpec = [{username, <<"no_cert_user">>},
                {server, domain()},
                {host, <<"localhost">>},
                {port, ct:get_config({hosts, mim, c2s_port})},
                {password, <<"break_me">>},
                {resource, <<>>}, %% Allow the server to generate the resource
                {auth, fun escalus_auth:auth_sasl_external/2},
                {starttls, required},
                {ssl_opts, [{fail_if_no_peer_cert, false}, {verify, verify_none}]}],

    Result = escalus_connection:start(UserSpec),
    ?assertMatch({error, {connection_step_failed, _, _}}, Result),
    {error, {connection_step_failed, _Call, Details}} = Result,
    ?assertMatch({auth_failed, _, #xmlel{name = <<"failure">>}}, Details),
    ok.

generate_certs(C) ->
    CA = [#{cn => "not-alice", xmpp_addrs => [add_domain_str("alice"), "alice@fed1"]},
          #{cn => "kate", xmpp_addrs => [add_domain_str("kate"), "kate@fed1"]},
          #{cn => "bob", xmpp_addrs => [add_domain_str("bob")]},
          #{cn => "greg", xmpp_addrs => [add_domain_str("greg")]},
          #{cn => "john", xmpp_addrs => undefined},
          #{cn => add_domain_str("john"), xmpp_addrs => undefined},
          #{cn => "not-mike", xmpp_addrs => undefined},
          #{cn => "grace", xmpp_addrs => ["grace@fed1", "grace@reg1"]},
          #{cn => add_domain_str("grace"), xmpp_addrs => ["grace@fed1", "grace@reg1"]},
          #{cn => "alice-critical", xmpp_addrs => [add_domain_str("alice-critical")],
            with_critical_extension => true}],
    SelfSigned = [ M#{cn => CN ++ "-self-signed", signed => self, xmpp_addrs => replace_addrs(Addrs)}
                   || M = #{ cn := CN , xmpp_addrs := Addrs} <- CA],
    CertSpecs = CA ++ SelfSigned,
    TemplateValues = #{"xmppOids" => xmpp_oids()},
    Certs = [{maps:get(cn, CertSpec), ca_certificate_helper:generate_cert(C, CertSpec, TemplateValues)}
             || CertSpec <- CertSpecs],
    [{certs, maps:from_list(Certs)} | C].

generate_user_tcp(C, User) ->
    generate_user(C, User, escalus_tcp).

generate_user(C, User, Transport) ->
    Certs = ?config(certs, C),
    UserCert = maps:get(User, Certs),
    Common = [{username, list_to_binary(User)},
              {server, domain()},
              {host, <<"localhost">>},
              {password, <<"break_me">>},
              {resource, <<>>}, %% Allow the server to generate the resource
              {auth, fun escalus_auth:auth_sasl_external/2},
              {transport, Transport},
              {ssl_opts, [{verify, verify_none},
                          {versions, ['tlsv1.3']},
                          {certfile, maps:get(cert, UserCert)},
                          {keyfile, maps:get(key, UserCert)}]}],
    Common ++ transport_specific_options(Transport).

transport_specific_options(escalus_tcp) ->
    [{starttls, required}, {port, ct:get_config({hosts, mim, c2s_port})}];
transport_specific_options(_) ->
     [{port, ct:get_config({hosts, mim, cowboy_secure_port})},
      {ssl, true}].

xmpp_oids() ->
    case os:cmd("openssl list -objects | grep id-on-xmppAddr") of
        "id-on-xmppAddr" ++ _ -> ""; % already defined in OpenSSL 3.*
        _ -> "id-on-xmppAddr = 1.3.6.1.5.5.7.8.5\n"
    end.

requested_name(User) ->
    add_domain(list_to_binary(User)).

username(Name, Config) ->
    case escalus_config:get_config(signed, Config, ca) of
        self ->
            Name ++ "-self-signed";
        ca ->
            Name
    end.

replace_addrs(undefined) ->
    undefined;
replace_addrs(Addresses) ->
    lists:map( fun(Addr) -> [User, Hostname] = binary:split(list_to_binary(Addr), <<"@">>),
                            binary_to_list(<<User/binary, <<"-self-signed@">>/binary, Hostname/binary>>) end, Addresses).

-spec add_domain_str(User :: string()) -> string().
add_domain_str(User) ->
    User ++ "@" ++ binary:bin_to_list(domain()).

-spec add_domain(User :: binary()) -> binary().
add_domain(User) ->
    <<User/binary, "@", (domain())/binary>>.

-spec check_username(string(), ct_suite:ct_config(), jid:luser()) -> ok.
check_username(RequestedNameStr, Config, ActualName) ->
    RequestedName = list_to_binary(RequestedNameStr),
    case proplists:get_value(auth_opts, Config) of
        #{sasl_external_common_name := #{prefix := Prefix, suffix := Suffix}} ->
            ?assertEqual(<<Prefix/binary, RequestedName/binary, Suffix/binary>>, ActualName);
        _ ->
            ?assertEqual(RequestedName, ActualName)
    end.
