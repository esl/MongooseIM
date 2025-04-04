-module(s2s_helper).

-export([init_s2s/1, end_s2s/1, configure_s2s/2, has_inet_errors/2, has_xmpp_server/3,
         reset_s2s_connections/0]).

-import(distributed_helper, [rpc_spec/1, rpc/4]).
-import(domain_helper, [host_type/1]).

init_s2s(Config) ->
    [{{s2s, NodeKey}, get_s2s_opts(NodeKey)} || NodeKey <- node_keys()] ++
    [{escalus_user_db, xmpp} | Config].

end_s2s(Config) ->
    [configure_and_restart_s2s(NodeKey, S2SOrig) || {{s2s, NodeKey}, S2SOrig} <- Config],
    ok.

node_keys() ->
    [mim, fed].

configure_s2s(Group, Config) ->
    TLSPreset = tls_preset(Group),
    [configure_and_restart_s2s(NodeKey, s2s_config(maps:get(NodeKey, TLSPreset), S2SOrig, Config))
     || {{s2s, NodeKey}, S2SOrig} <- Config],
    Config.

has_inet_errors(History, Server) ->
    Inet = lists:any(
        fun({_, {inet_res, lookup, [Server1, in, a, _, _]}, []})
            when Server1 =:= Server -> true;
           (_) -> false
        end, History),
    Inet6 = lists:any(
        fun({_, {inet_res, lookup, [Server1, in, aaaa, _, _]}, []})
            when Server1 =:= Server -> true;
           (_) -> false
        end, History),
    Inet andalso Inet6.

has_xmpp_server(History, Server, DnsRrType) ->
    lists:any(
        fun({_Pid, {inet_res, lookup, [Server1, in, DnsRrType1, _, _]}, [_|_]})
            when Server1 =:= Server, DnsRrType1 =:= DnsRrType -> true;
           (_) -> false
        end, History).

get_s2s_opts(NodeKey) ->
    RPCSpec = rpc_spec(NodeKey),
    S2SOpts = rpc(RPCSpec, mongoose_config, get_opt, [[{s2s, host_type(NodeKey)}, outgoing]]),
    S2SPort = ct:get_config({hosts, NodeKey, incoming_s2s_port}),
    [S2SListener] = mongoose_helper:get_listeners(RPCSpec, #{port => S2SPort,
                                                             module => mongoose_s2s_listener}),
    #{outgoing => S2SOpts, listener => S2SListener}.

s2s_config(StartTLS, S2S = #{outgoing := Outgoing, listener := Listener}, Config) ->
    NewOutgoing = tls_config(StartTLS, Outgoing, Config),
    NewIncoming = tls_config(StartTLS, Listener, Config),
    S2S#{outgoing := NewOutgoing, listener := NewIncoming}.

tls_config(required_trusted_with_cachain, #{tls := TlsOpts} = Opts, Config) ->
    CACertFile = filename:join([path_helper:repo_dir(Config), "tools", "ssl", "ca", "cacert.pem"]),
    Opts#{tls => TlsOpts#{mode => starttls_required, cacertfile => CACertFile, verify_mode => peer}};
tls_config(required_trusted, #{tls := TlsOpts} = Opts, _) ->
    Opts#{tls => TlsOpts#{mode => starttls_required, verify_mode => selfsigned_peer}};
tls_config(required, #{tls := TlsOpts} = Opts, _) ->
    Opts#{tls => TlsOpts#{mode => starttls_required, verify_mode => none}};
tls_config(enforced, #{tls := TlsOpts} = Opts, _) ->
    Opts#{tls => TlsOpts#{mode => tls, verify_mode => none}};
tls_config(optional, #{tls := TlsOpts} = Opts, _) ->
    Opts#{tls => TlsOpts#{mode => starttls, verify_mode => none}};
tls_config(plain, Opts, _) ->
    maps:remove(tls, Opts).

tls_preset(both_plain) ->
    #{mim => plain, fed => plain};
tls_preset(both_tls_optional) ->
    #{mim => optional, fed => optional};
tls_preset(both_tls_required) ->
    #{mim => required, fed => required};
tls_preset(both_tls_enforced) ->
    #{mim => enforced, fed => enforced};
tls_preset(node1_tls_optional_node2_tls_required) ->
    #{mim => optional, fed => required};
tls_preset(node1_tls_required_node2_tls_optional) ->
    #{mim => required, fed => optional};
tls_preset(node1_tls_required_trusted_node2_tls_optional) ->
    #{mim => required_trusted, fed => optional};
tls_preset(node1_tls_false_node2_tls_required) ->
    #{mim => plain, fed => required};
tls_preset(node1_tls_required_node2_tls_false) ->
    #{mim => required, fed => plain};
tls_preset(node1_tls_optional_node2_tls_required_trusted_with_cachain) ->
    #{mim => optional, fed => required_trusted_with_cachain}.

configure_and_restart_s2s(NodeKey, #{outgoing := Outgoing, listener := Listener}) ->
    set_opt(rpc_spec(NodeKey), [{s2s, host_type(NodeKey)}, outgoing], Outgoing),
    restart_s2s(rpc_spec(NodeKey), Listener).

set_opt(Spec, Opt, Value) ->
    rpc(Spec, mongoose_config, set_opt, [Opt, Value]).

restart_s2s(#{} = Spec, S2SListener) ->
    reset_s2s_connections(Spec),
    mongoose_helper:restart_listener(Spec, S2SListener).

reset_s2s_connections() ->
    [reset_s2s_connections(rpc_spec(NodeKey)) || NodeKey <- node_keys()].

reset_s2s_connections(Spec) ->
    reset_outgoing_s2s(Spec),
    reset_incoming_s2s(Spec).

reset_outgoing_s2s(Spec) ->
    Children = rpc(Spec, supervisor, which_children, [mongoose_s2s_out_sup]),
    [rpc(Spec, mongoose_s2s_out, stop_connection, [Pid, <<"closing connection">>]) ||
     {_, Pid, _, _} <- Children].

reset_incoming_s2s(Spec) ->
    Children = rpc(Spec, supervisor, which_children, [mongoose_listener_sup]),
    [reset_s2s_listener(Spec, Ref) || {Ref, _, _, [mongoose_s2s_listener | _]} <- Children].

reset_s2s_listener(Spec, Ref) ->
    Procs = rpc(Spec, ranch, procs, [Ref, connections]),
    [rpc(Spec, erlang, exit, [Pid, kill]) || {_, Pid, _, _} <- Procs].
