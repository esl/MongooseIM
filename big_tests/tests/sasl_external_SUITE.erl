-module(sasl_external_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

all() ->
    [{group, fast_tls},
     {group, just_tls}].

groups() ->
    G = [{fast_tls, [], common_test_cases()},
	 {just_tls, [], common_test_cases()}],
    ct_helper:repeat_all_until_all_ok(G).

common_test_cases() ->
    [cert_with_cn_xmpp_addrs_requested_correct_user,
     cert_with_cn_xmpp_addrs_request_name_empty,
     cert_with_cn_no_xmpp_addrs_request_name_empty,
     no_cert_fails_to_authenticate].

init_per_suite(Config) ->
    Config0 = escalus:init_per_suite(Config),
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    Config1.

end_per_suite(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(mongooseim),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    CACertFile = filename:join([path_helper:repo_dir(Config),
				"tools", "ssl", "ca-clients", "cacert.pem"]),
    NewConfigValues = [{tls_config, "{certfile, \"priv/ssl/fake_server.pem\"},"
			            "starttls, verify_peer,"
				    "{cafile, \"" ++ CACertFile ++ "\"},"},
		       {tls_module, "{tls_module, " ++ atom_to_list(GroupName) ++ "},"},
		       {auth_method, "pki"},
		       {sasl_mechanisms, "{sasl_mechanisms, [cyrsasl_external]}."}],
    ejabberd_node_utils:modify_config_file(NewConfigValues, Config),
    ejabberd_node_utils:restart_application(mongooseim),

    Config.

end_per_group(_, Config) ->
    Config.

cert_with_cn_xmpp_addrs_requested_correct_user(C) ->
    UserSpec = [{requested_name, <<"alice@localhost">>} |
		generate_user(C, "john", #{"xmppAddrs" => ["alice@localhost", "alice@fed1"]})],
    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).

cert_with_cn_xmpp_addrs_request_name_empty(C) ->
    UserSpec = generate_user(C, "bob", #{"xmppAddrs" => ["bob@localhost", "bob@fed1"]}),
    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).

cert_with_cn_no_xmpp_addrs_request_name_empty(C) ->
    UserSpec = generate_user(C, "john"),
    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).

no_cert_fails_to_authenticate(_C) ->
    UserSpec = [{username, <<"no_cert_user">>},
		{server, <<"localhost">>},
		{password, <<"break_me">>},
		{resource, <<>>}, %% Allow the server to generate the resource
		{auth, {escalus_auth, auth_sasl_external}},
		{starttls, required}],

    Result = escalus_connection:start(UserSpec),
    ?assertMatch({error, {connection_step_failed, _, _}}, Result),
    {error, {connection_step_failed, _Call, Details}} = Result,
    ?assertMatch({auth_failed, _, #xmlel{name = <<"failure">>}}, Details),
    ok.


generate_user(C, User) ->
    generate_user(C, User, #{}).

generate_user(C, User, TemplateValuesIn) ->
    SSLDir = filename:join([path_helper:repo_dir(C), "tools", "ssl"]),

    ConfigTemplate = filename:join(?config(data_dir, C), "openssl-user.cnf"),
    {ok, Template} = file:read_file(ConfigTemplate),
    TemplateValues = prepare_template_values(User, TemplateValuesIn),
    OpenSSLConfig = bbmustache:render(Template, TemplateValues),
    UserConfig = filename:join(?config(priv_dir, C), User ++ ".cfg"),
    file:write_file(UserConfig, OpenSSLConfig),
    UserKey = filename:join(?config(priv_dir, C), User ++ "_key.pem"),
    UserCsr = filename:join(?config(priv_dir, C), User ++ ".csr"),

    Cmd = ["openssl", "req", "-config", UserConfig, "-newkey", "rsa:2048", "-sha256", "-nodes",
	   "-out", UserCsr, "-keyout", UserKey, "-outform", "PEM"],
    {done, 0, _Output} = erlsh:run(Cmd),

    UserCert = filename:join(?config(priv_dir, C), User ++ "_cert.pem"),
    SignCmd = filename:join(?config(data_dir, C), "sign_cert.sh"),
    Cmd2 = [SignCmd, "--req", UserCsr, "--out", UserCert],
    LogFile = filename:join(?config(priv_dir, C), User ++ "signing.log"),
    {done, 0, _} = erlsh:run(Cmd2, LogFile, SSLDir),

    [{username, list_to_binary(User)},
     {server, <<"localhost">>},
     {password, <<"break_me">>},
     {resource, <<>>}, %% Allow the server to generate the resource
     {auth, {escalus_auth, auth_sasl_external}},
     {ssl_opts, [{certfile, UserCert},
		 {keyfile, UserKey}]},
     {starttls, required}].


prepare_template_values(User, TemplateValues) ->
    Defaults = #{"cn" => User,
		 "xmppAddrs" => ""},
    XMPPAddrs = maybe_prepare_xmpp_addresses(maps:get("xmppAddrs", TemplateValues, undefined)),
    maps:merge(Defaults, TemplateValues#{"xmppAddrs" => XMPPAddrs}).

maybe_prepare_xmpp_addresses(undefined) ->
    "";
maybe_prepare_xmpp_addresses(Addrs) when is_list(Addrs) ->
    AddrsWithSeq = lists:zip(Addrs, lists:seq(1, length(Addrs))),
    Entries = [make_xmpp_addr_entry(Addr, I) || {Addr, I} <- AddrsWithSeq],
    string:join(Entries, "\n").

make_xmpp_addr_entry(Addr, I) ->
    % id-on-xmppAddr OID is specified in the openssl-user.cnf config file
    "otherName." ++ integer_to_list(I) ++ " = id-on-xmppAddr;UTF8:" ++ Addr.

