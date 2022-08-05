-module(mongoose_c2s_listener).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_ns.hrl").

-behaviour(mongoose_listener).
-export([socket_type/0, start_listener/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

-behaviour(supervisor).
-export([start_link/1, init/1]).
-ignore_xref([start_link/1]).

%% backwards compatibility, process iq-session
-export([process_iq/5]).

-type options() :: #{module := module(),
                     atom() => any()}.

%% mongoose_listener
-spec socket_type() -> mongoose_listener:socket_type().
socket_type() ->
    xml_stream.

-spec start_listener(options()) -> ok.
start_listener(Opts) ->
    ListenerId = mongoose_listener_config:listener_id(Opts),
    ChildSpec = listener_child_spec(ListenerId, Opts),
    mongoose_listener_sup:start_child(ChildSpec),
    ok.

process_iq(Acc, _From, _To, #iq{type = set, sub_el = #xmlel{name = <<"session">>}} = IQ, _) ->
    {Acc, IQ#iq{type = result}}.

%% ranch_protocol
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
	gen_statem:start_link(mongoose_c2s, {Ref, Transport, Opts}, [{hibernate_after, HibernateAfterTimeout}]).

%% supervisor
-spec start_link(options()) -> any().
start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

-spec init(options()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{module := Module} = Opts) ->
    [ gen_iq_handler:add_iq_handler_for_domain(
        HostType, ?NS_SESSION, ejabberd_sm, fun ?MODULE:process_iq/5, #{}, no_queue)
      || HostType <- ?ALL_HOST_TYPES],
    TransportOpts = prepare_socket_opts(Opts),
    ListenerId = mongoose_listener_config:listener_id(Opts),
    OptsWithTlsConfig = process_tls_opts(Opts),
    Child = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, Module, OptsWithTlsConfig),
    {ok, {#{strategy => one_for_one, intensity => 100, period => 1}, [Child]}}.

process_tls_opts(Opts = #{tls := TlsOpts}) ->
    ReadyTlsOpts = just_tls:make_ssl_opts(TlsOpts),
    Opts#{tls := TlsOpts#{opts => ReadyTlsOpts}};
process_tls_opts(Opts) ->
    Opts.

listener_child_spec(ListenerId, Opts) ->
    #{id => ListenerId,
      start => {?MODULE, start_link, [Opts]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [?MODULE]}.

prepare_socket_opts(#{port := Port,
                      ip_version := IPVersion,
                      ip_tuple := IPTuple,
                      backlog := Backlog,
                      num_acceptors := NumAcceptors,
                      max_connections := MaxConnections,
                      reuseport := ReusePort}) ->
    SocketOpts = [{nodelay, true},
                  {keepalive, true},
                  {ip, IPTuple},
                  {port, Port},
                  {backlog, Backlog},
                  mongoose_listener_config:address_family(IPVersion)
                  | maybe_reuseport(ReusePort)],
    #{max_connections => MaxConnections,
      num_acceptors => NumAcceptors,
      num_listen_sockets => num_listen_sockets(ReusePort),
      socket_opts => SocketOpts}.

maybe_reuseport(false) -> [];
maybe_reuseport(true) -> [{raw, 1, 15, <<1:32/native>>}].

num_listen_sockets(false) -> 1;
num_listen_sockets(true) -> erlang:system_info(schedulers_online).
