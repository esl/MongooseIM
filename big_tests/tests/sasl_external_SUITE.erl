-module(sasl_external_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, fast_tls}].

groups() ->
    G = [{fast_tls, [parallel],
	  [cert_with_cn_xmpp_addrs_requested_correct_user,
	   cert_with_cn_xmpp_addrs_request_name_empty]}],
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
		generate_user(C)],
    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).

cert_with_cn_xmpp_addrs_request_name_empty(C) ->
    UserSpec = generate_user(C),
    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).

generate_user(C) ->
    SSLDir = filename:join([path_helper:repo_dir(C), "tools", "ssl"]),

    AliceConfig = filename:join(?config(data_dir, C), "openssl-alice.cnf"),
    AliceKey = filename:join(?config(priv_dir, C), "alice_key.pem"),
    AliceCsr = filename:join(?config(priv_dir, C), "alice.csr"),

    Cmd = ["openssl", "req", "-config", AliceConfig, "-newkey", "rsa:2048", "-sha256", "-nodes",
	   "-out", AliceCsr, "-keyout", AliceKey, "-outform", "PEM"],
    ct:pal("Sign request"),
    {done, 0, _Output} = erlsh:run(Cmd),

    AliceCert = filename:join(?config(priv_dir, C), "alice_cert.pem"),
    SignCmd = filename:join(?config(data_dir, C), "sign_cert.sh"),
    Cmd2 = [SignCmd, "--req", AliceCsr, "--out", AliceCert],
    ct:pal("Signinig by CA"),
    LogFile = filename:join(?config(priv_dir, C), "singing.log"),
    {done, 0, _} = erlsh:run(Cmd2, LogFile, SSLDir),

    [{username, <<"alice">>},
     {server, <<"localhost">>},
     {password, <<"break_me">>},
     {resource, <<>>}, %% Allow the server to generate the resource
     {auth, {escalus_auth, auth_sasl_external}},
     {ssl_opts, [{certfile, AliceCert},
		 {keyfile, AliceKey}]},
     {starttls, required}].


