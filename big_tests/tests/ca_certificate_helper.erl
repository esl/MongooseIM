-module(ca_certificate_helper).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

generate_cert(Config, #{cn := User} = CertSpec, BasicTemplateValues) ->
    UserConfig = certificate_config(Config, CertSpec, BasicTemplateValues),
    MaybeCritical = maps:get(with_critical_extension, CertSpec, false),
    ExtraOpts = maps:get(opts, CertSpec, []),
    case maps:get(signed, CertSpec, ca) of
        ca -> generate_ca_signed_cert(Config, User, UserConfig, MaybeCritical, ExtraOpts);
        self -> generate_self_signed_cert(Config, User, UserConfig, MaybeCritical, ExtraOpts)
    end.

certificate_config(Config, #{cn := User} = CertSpec, BasicTemplateValues) ->
    ConfigTemplate = filename:join(?config(mim_data_dir, Config), "openssl-user.cnf"),
    {ok, Template} = file:read_file(ConfigTemplate),
    XMPPAddrs = maps:get(xmpp_addrs, CertSpec, undefined),
    TemplateValues = maps:merge(BasicTemplateValues, prepare_template_values(User, XMPPAddrs)),
    OpenSSLConfig = bbmustache:render(Template, TemplateValues),
    UserConfig = filename:join(?config(priv_dir, Config), User ++ ".cfg"),
    ct:log("OpenSSL config: ~ts~n~ts", [UserConfig, OpenSSLConfig]),
    ok = file:write_file(UserConfig, OpenSSLConfig),
    UserConfig.

prepare_template_values(User, XMPPAddrsIn) ->
    XMPPAddrs = maybe_prepare_xmpp_addresses(XMPPAddrsIn),
    #{"cn" => User, "xmppAddrs" => XMPPAddrs}.

maybe_prepare_xmpp_addresses(undefined) ->
    "";
maybe_prepare_xmpp_addresses(Addrs) when is_list(Addrs) ->
    AddrsWithSeq = lists:enumerate(Addrs),
    Entries = [make_xmpp_addr_entry(I, Addr) || {I, Addr} <- AddrsWithSeq],
    string:join(Entries, "\n").

make_xmpp_addr_entry(I, Addr) ->
    % id-on-xmppAddr OID is specified in the openssl-user.cnf config file
    "otherName." ++ integer_to_list(I) ++ " = id-on-xmppAddr;UTF8:" ++ Addr.

generate_ca_signed_cert(Config, User, ConfigCfg, MaybeCritical, ExtraOpts) ->
    {Csr, Key, Cert} = file_paths(Config, User),
    Args = ["-config", ConfigCfg | extensions(ca_signed, MaybeCritical)],
    generate_certificate_request(Key, Csr, Args),
    sign_certificate_request(Config, Csr, Cert, ExtraOpts),
    #{key => Key, cert => Cert}.

generate_self_signed_cert(Config, User, ConfigCfg, MaybeCritical, ExtraOpts) ->
    {_, Key, Cert} = file_paths(Config, User ++ "_self_signed"),
    openssl(["req", "-config", ConfigCfg, "-newkey", "rsa:2048", "-sha256", "-nodes",
             "-out", Cert, "-keyout", Key, "-x509", "-outform", "PEM"
             | extensions(self_signed, MaybeCritical) ++ ExtraOpts]),
    #{key => Key, cert => Cert}.

generate_intermediate_signed_cert(Config, #{cn := User} = CertSpec, TemplateValues) ->
    % Generate intermediate cert
    {ICSR, IKey, ICert} = file_paths(Config, User ++ "_intermediate"),
    CAConfig = filename:join(?config(mim_data_dir, Config), "openssl-intermediate.cnf"),
    generate_certificate_request(IKey, ICSR, ["-subj", "/CN=" ++ User ++ " intermediate"]),
    sign_certificate_request(
      Config, ICSR, ICert, ["-extfile", CAConfig, "-extensions", "intermediate_ca"]),

    % Generate user cert
    UserConfig = certificate_config(Config, CertSpec, TemplateValues),
    {CSR, Key, Cert} = file_paths(Config, User),
    generate_certificate_request(Key, CSR, ["-config", UserConfig]),
    sign_certificate_request(Config, CSR, Cert, ["-cert", ICert, "-keyfile", IKey]),
    #{key => Key, cert => Cert, intermediate_cert => ICert}.

generate_chain(Config, User, Certs) ->
    PrivDir = ?config(priv_dir, Config),
    Chain = filename:join(PrivDir, User ++ "_chain.pem"),
    Content = [PEM || File <- Certs, {ok, PEM} <:- [file:read_file(File)]],
    ok = file:write_file(Chain, Content),
    Chain.

file_paths(Config, Prefix) ->
    PrivDir = ?config(priv_dir, Config),
    CSR = filename:join(PrivDir, Prefix ++ ".csr"),
    Key = filename:join(PrivDir, Prefix ++ "_key.pem"),
    Cert = filename:join(PrivDir, Prefix ++ "_cert.pem"),
    {CSR, Key, Cert}.

generate_certificate_request(KeyFile, RequestFile, Options) ->
    openssl(["req", "-new", "-newkey", "rsa:2048", "-nodes", "-sha256",
             "-keyout", KeyFile, "-out", RequestFile, "-outform", "PEM" | Options]).

sign_certificate_request(Config, RequestFile, CertFile, Options) ->
    SSLDir = filename:join([path_helper:repo_dir(Config), "tools", "ssl"]),
    openssl(["ca", "-config", "openssl-ca-clients.cnf", "-policy", "signing_policy",
             "-extensions", "signing_req", "-in", RequestFile, "-out", CertFile, "-batch"
             | Options], [{cd, SSLDir}]).

openssl(Args) ->
    openssl(Args, []).

openssl(Args, Opts) ->
    case mongooseimctl_helper:run(os:find_executable("openssl"), Args, [stderr_to_stdout | Opts]) of
        {_Output, 0} -> ok;
        {Output, Status} -> ct:fail({openssl_failed, Args, Status, Output})
    end.

extensions(ca_signed, false) ->
    [];
extensions(ca_signed, true) ->
    ["-extensions", "critical_extensions"];
extensions(self_signed, false) ->
    ["-extensions", "client_req_extensions"];
extensions(self_signed, true) ->
    ["-extensions", "self_signed_critical_extensions"].
