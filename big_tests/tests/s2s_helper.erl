-module(s2s_helper).
-export([init_s2s/1]).
-export([end_s2s/1]).
-export([configure_s2s/2]).

-import(distributed_helper, [rpc_spec/1, rpc/4]).
-import(domain_helper, [host_type/1]).

init_s2s(Config) ->
    [{{s2s, NodeKey}, get_s2s_opts(NodeKey)} || NodeKey <- node_keys()] ++
        [{escalus_user_db, xmpp} | Config].

node_keys() ->
    [mim, fed].

get_s2s_opts(NodeKey) ->
    RPCSpec = rpc_spec(NodeKey),
    S2SOpts = rpc(RPCSpec, mongoose_config, get_opt, [{s2s, host_type(NodeKey)}]),
    S2SPort = ct:get_config({hosts, NodeKey, incoming_s2s_port}),
    [S2SListener] = mongoose_helper:get_listeners(RPCSpec, #{port => S2SPort,
                                                             module => ejabberd_s2s_in}),
    #{opts => S2SOpts, listener => S2SListener}.

end_s2s(Config) ->
    [configure_and_restart_s2s(NodeKey, S2SOrig) || {{s2s, NodeKey}, S2SOrig} <- Config],
    ok.

configure_s2s(Group, Config) ->
    TLSPreset = tls_preset(Group),
    [configure_and_restart_s2s(NodeKey, s2s_config(maps:get(NodeKey, TLSPreset), S2SOrig, Config))
     || {{s2s, NodeKey}, S2SOrig} <- Config],
    Config.

s2s_config(plain, S2S = #{opts := Opts}, _) ->
    S2S#{opts := maps:remove(certfile, Opts#{use_starttls := false})};
s2s_config(required_trusted_with_cachain, S2S = #{opts := Opts, listener := Listener}, Config) ->
    #{tls := TLSOpts} = Listener,
    CACertFile = filename:join([path_helper:repo_dir(Config), "tools", "ssl", "ca", "cacert.pem"]),
    NewTLSOpts = TLSOpts#{cacertfile => CACertFile},
    S2S#{opts := Opts#{use_starttls := required_trusted}, listener := Listener#{tls := NewTLSOpts}};
s2s_config(StartTLS, S2S = #{opts := Opts}, _) ->
    S2S#{opts := Opts#{use_starttls := StartTLS}}.

tls_preset(both_plain) ->
    #{mim => plain, fed => plain};
tls_preset(both_tls_optional) ->
    #{mim => optional, fed => optional};
tls_preset(both_tls_required) ->
    #{mim => required, fed => required};
tls_preset(node1_tls_optional_node2_tls_required) ->
    #{mim => optional, fed => required};
tls_preset(node1_tls_required_node2_tls_optional) ->
    #{mim => required, fed => optional};
tls_preset(node1_tls_required_trusted_node2_tls_optional) ->
    #{mim => required_trusted, fed => optional};
tls_preset(node1_tls_false_node2_tls_optional) ->
    #{mim => false, fed => optional};
tls_preset(node1_tls_optional_node2_tls_false) ->
    #{mim => optional, fed => false};
tls_preset(node1_tls_false_node2_tls_required) ->
    #{mim => false, fed => required};
tls_preset(node1_tls_required_node2_tls_false) ->
    #{mim => required, fed => false};
tls_preset(node1_tls_optional_node2_tls_required_trusted_with_cachain) ->
    #{mim => optional, fed => required_trusted_with_cachain}.

configure_and_restart_s2s(NodeKey, #{opts := Opts, listener := Listener}) ->
    HostType = host_type(NodeKey),
    set_opt(rpc_spec(NodeKey), [{s2s, HostType}], Opts),
    restart_s2s(rpc_spec(NodeKey), Listener).

set_opt(Spec, Opt, Value) ->
    rpc(Spec, mongoose_config, set_opt, [Opt, Value]).

restart_s2s(#{} = Spec, S2SListener) ->
    Children = rpc(Spec, supervisor, which_children, [ejabberd_s2s_out_sup]),
    [rpc(Spec, ejabberd_s2s_out, stop_connection, [Pid]) ||
     {_, Pid, _, _} <- Children],

    ChildrenIn = rpc(Spec, supervisor, which_children, [ejabberd_s2s_in_sup]),
    [rpc(Spec, erlang, exit, [Pid, kill]) ||
     {_, Pid, _, _} <- ChildrenIn],

    mongoose_helper:restart_listener(Spec, S2SListener).
