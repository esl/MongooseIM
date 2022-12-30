-module(mongoose_c2s_listener).

-include("mongoose.hrl").

-behaviour(mongoose_listener).
-export([socket_type/0, start_listener/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

-behaviour(supervisor).
-export([start_link/1, init/1]).
-ignore_xref([start_link/1]).

%% Hook handlers
-export([handle_user_open_session/3]).

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

%% Hooks and handlers
-spec handle_user_open_session(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
handle_user_open_session(Acc, #{c2s_data := StateData}, #{host_type := HostType, access := Access}) ->
    Jid = mongoose_c2s:get_jid(StateData),
    LServer = mongoose_c2s:get_lserver(StateData),
    case acl:match_rule(HostType, LServer, Access, Jid) of
        allow ->
            case mongoose_hooks:session_opening_allowed_for_user(HostType, Jid) of
                allow -> {ok, Acc};
                _ -> {stop, Acc}
            end;
        deny ->
            {stop, Acc}
    end.

%% ranch_protocol
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    mongoose_c2s:start_link({mongoose_c2s_ranch, {Transport, Ref}, Opts}, [{hibernate_after, HibernateAfterTimeout}]).

%% supervisor
-spec start_link(options()) -> any().
start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

-spec init(options()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{module := Module} = Opts) ->
    HostTypes = ?ALL_HOST_TYPES,
    maybe_add_access_check(HostTypes, Opts),
    TransportOpts = prepare_socket_opts(Opts),
    ListenerId = mongoose_listener_config:listener_id(Opts),
    OptsWithTlsConfig = process_tls_opts(Opts),
    Child = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, Module, OptsWithTlsConfig),
    {ok, {#{strategy => one_for_one, intensity => 100, period => 1}, [Child]}}.

maybe_add_access_check(_, #{access := all}) ->
    ok;
maybe_add_access_check(HostTypes, #{access := Access}) ->
    AclHooks = [ {user_open_session, HostType, fun ?MODULE:handle_user_open_session/3, #{access => Access}, 10}
                 || HostType <- HostTypes ],
    gen_hook:add_handlers(AclHooks).

process_tls_opts(Opts = #{tls := #{module := TlsMod} = TlsOpts}) ->
    ReadyTlsOpts = mongoose_tls:prepare_options(TlsMod, TlsOpts),
    Opts#{tls := TlsOpts#{prepare_options => ReadyTlsOpts}};
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
                      reuse_port := ReusePort}) ->
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
