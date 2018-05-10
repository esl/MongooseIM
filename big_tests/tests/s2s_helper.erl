-module(s2s_helper).
-export([suite/1]).
-export([init_s2s/2]).
-export([end_s2s/1]).
-export([configure_s2s/2]).

-import(distributed_helper, [fed/0,
                             mim/0,
                             require_rpc_nodes/1,
                             rpc/4, rpc/5]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-record(s2s_opts, {
          node1_s2s_certfile = undefined,
          node1_s2s_use_starttls = undefined,
          node2_s2s_certfile = undefined,
          node2_s2s_use_starttls = undefined
         }).

suite(Config) ->
    require_rpc_nodes([mim, fed]) ++ Config.

init_s2s(Config, CoverEnabled) when is_boolean(CoverEnabled) ->
    Node1S2SCertfile = rpc(mim(), ejabberd_config, get_local_option, [s2s_certfile]),
    Node1S2SUseStartTLS = rpc(mim(), ejabberd_config, get_local_option, [s2s_use_starttls]),
    case CoverEnabled of
        true ->
            rpc(fed(), mongoose_cover_helper, start, [[ejabberd]]);
        _ ->
            ok
    end,
    Node2S2SCertfile = rpc(fed(), ejabberd_config, get_local_option, [s2s_certfile]),
    Node2S2SUseStartTLS = rpc(fed(), ejabberd_config, get_local_option, [s2s_use_starttls]),
    S2S = #s2s_opts{node1_s2s_certfile = Node1S2SCertfile,
                    node1_s2s_use_starttls = Node1S2SUseStartTLS,
                    node2_s2s_certfile = Node2S2SCertfile,
                    node2_s2s_use_starttls = Node2S2SUseStartTLS},

    [{s2s_opts, S2S},
     {escalus_user_db, xmpp},
     {cover_enabled, CoverEnabled} | Config].

end_s2s(Config) ->
    S2SOrig = ?config(s2s_opts, Config),
    configure_s2s(S2SOrig),
    CoverEnabled = ?config(cover_enabled, Config),
    case CoverEnabled of
        true ->
            rpc(fed(), mongoose_cover_helper, analyze, []);
        _ ->
            ok
    end.

configure_s2s(both_plain, Config) ->
    configure_s2s(#s2s_opts{}),
    Config;
configure_s2s(both_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config), %The initial config assumes that both nodes are configured to use encrypted s2s
    configure_s2s(S2S),
    Config;
configure_s2s(both_tls_required, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required,
                               node2_s2s_use_starttls = required}),
    Config;
configure_s2s(node1_tls_optional_node2_tls_required, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node2_s2s_use_starttls = required}),
    Config;
configure_s2s(node1_tls_required_node2_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required}),
    Config;
configure_s2s(node1_tls_required_trusted_node2_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required_trusted}),
    Config;
configure_s2s(node1_tls_false_node2_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = false}),
    Config;
configure_s2s(node1_tls_optional_node2_tls_false, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node2_s2s_use_starttls = false}),
    Config;
configure_s2s(node1_tls_false_node2_tls_required, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = false,
                               node2_s2s_use_starttls = required}),
    Config;
configure_s2s(node1_tls_required_node2_tls_false, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required,
                               node2_s2s_use_starttls = false}),
    Config.

configure_s2s(#s2s_opts{node1_s2s_certfile = Certfile1,
                        node1_s2s_use_starttls = StartTLS1,
                        node2_s2s_certfile = Certfile2,
                        node2_s2s_use_starttls = StartTLS2}) ->
    configure_s2s(mim(), Certfile1, StartTLS1),
    configure_s2s(fed(), Certfile2, StartTLS2),
    restart_s2s().

configure_s2s(Node, Certfile, StartTLS) ->
    rpc(Node, ejabberd_config, add_local_option, [s2s_certfile, Certfile]),
    rpc(Node, ejabberd_config, add_local_option, [s2s_use_starttls, StartTLS]).

restart_s2s() ->
    restart_s2s(mim()),
    restart_s2s(fed()).

restart_s2s(Node) ->
    Children = rpc(Node, supervisor, which_children, [ejabberd_s2s_out_sup]),
    [rpc(Node, ejabberd_s2s_out, stop_connection, [Pid]) ||
     {_, Pid, _, _} <- Children],

    ChildrenIn = rpc(Node, supervisor, which_children, [ejabberd_s2s_in_sup]),
    [rpc(Node, erlang, exit, [Pid, kill]) ||
     {_, Pid, _, _} <- ChildrenIn].
