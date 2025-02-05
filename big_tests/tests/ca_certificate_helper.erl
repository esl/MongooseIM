-module(ca_certificate_helper).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

generate_cert(Config, #{cn := User} = CertSpec, BasicTemplateValues) ->
    ConfigTemplate = filename:join(?config(mim_data_dir, Config), "openssl-user.cnf"),
    {ok, Template} = file:read_file(ConfigTemplate),
    XMPPAddrs = maps:get(xmpp_addrs, CertSpec, undefined),
    TemplateValues = maps:merge(BasicTemplateValues, prepare_template_values(User, XMPPAddrs)),
    OpenSSLConfig = bbmustache:render(Template, TemplateValues),
    UserConfig = filename:join(?config(priv_dir, Config), User ++ ".cfg"),
    ct:log("OpenSSL config: ~ts~n~ts", [UserConfig, OpenSSLConfig]),
    file:write_file(UserConfig, OpenSSLConfig),
    UserKey = filename:join(?config(priv_dir, Config), User ++ "_key.pem"),
    MaybeCritical = maps:get(with_critical_extension, CertSpec, false),
    case maps:get(signed, CertSpec, ca) of
	    ca -> 
	        generate_ca_signed_cert(Config, User, UserConfig, UserKey, MaybeCritical);
	    self -> 
	        generate_self_signed_cert(Config, User, UserConfig, UserKey, MaybeCritical)
    end.

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


generate_ca_signed_cert(Config, Filename, ConfigCfg, KeyFilename, MaybeCritical) ->
    Csr = filename:join(?config(priv_dir, Config), Filename ++ ".csr"),
    Cmd = ["openssl req -config ", ConfigCfg, " -newkey rsa:2048 -sha256 -nodes -out ",
           Csr, " -keyout ", KeyFilename, " -outform PEM" | extensions(ca_signed, MaybeCritical)],
    Out = os:cmd(Cmd ++ " && echo CREATED"),
    verify_created(Cmd, Out),
    ct:log("generate_ca_signed_cert 1:~nCmd ~p~nOut ~ts", [Cmd, Out]),
    Cert = filename:join(?config(priv_dir, Config), Filename ++ "_cert.pem"),
    SignCmd = filename:join(?config(mim_data_dir, Config), "sign_cert.sh"),
    Cmd2 = [SignCmd, " --req ", Csr, " --out ", Cert],
    SSLDir = filename:join([path_helper:repo_dir(Config), "tools", "ssl"]),
    OutLog = os:cmd("cd " ++ SSLDir ++ " && " ++ Cmd2),
    ct:log("generate_ca_signed_cert 2:~nCmd ~p~nOut ~ts", [Cmd2, OutLog]),
    #{key => KeyFilename,
      cert => Cert}.

generate_self_signed_cert(Config, Filename, ConfigCfg, KeyFilename, MaybeCritical) ->
    Cert = filename:join(?config(priv_dir, Config), Filename ++ "_self_signed_cert.pem"),
    Cmd = ["openssl req -config ", ConfigCfg, " -newkey rsa:2048 -sha256 -nodes -out ",
           Cert, " -keyout ", KeyFilename, " -x509 -outform PEM", extensions(self_signed, MaybeCritical)],
    OutLog = os:cmd(Cmd ++ " && echo CREATED"),
    verify_created(Cmd, OutLog),
    ct:log("generate_self_signed_cert:~nCmd ~p~nOut ~ts", [Cmd, OutLog]),
    #{key => KeyFilename,
      cert => Cert}.

verify_created(Cmd, Output) ->
    case lists:suffix("CREATED\n", Output) of
        true ->
            ok;
        false ->
            ct:fail({failed_to_create_certificate, Cmd, Output})
    end.

extensions(ca_signed, false) ->
    [];
extensions(ca_signed, true) ->
    " -extensions critical_extensions";
extensions(self_signed, false) ->
    " -extensions client_req_extensions";
extensions(self_signed, true) ->
    " -extensions self_signed_critical_extensions".
