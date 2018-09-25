-module(sasl_external_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, fast_tls}].

groups() ->
    G = [{fast_tls, [parallel],
	  [cert_with_cn_xmpp_addrs_requested_correct_user,
	   cert_with_cn_xmpp_addrs_request_name_empty,
	   cert_with_cn_no_xmpp_addrs_request_name_empty]}],
    %ct_helper:repeat_all_until_all_ok(G).
    G.

init_per_suite(Config) ->
    Config0 = escalus:init_per_suite(Config),
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    Config1.

end_per_suite(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(mongooseim),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    CACertFile = filename:join([path_helper:repo_dir(Config),
				"tools", "ssl", "ca-clients", "cacert.pem"]),
    NewConfigValues = [{tls_config, "{certfile, \"priv/ssl/fake_server.pem\"},"
			            "starttls, verify_peer,"
				    "{cafile, \"" ++ CACertFile ++ "\"},"},
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
    LogFile = filename:join(?config(priv_dir, C), User ++ "singing.log"),
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
    "otherName." ++ integer_to_list(I) ++ " = 1.3.6.1.5.5.7.8.5;UTF8:" ++ Addr.

