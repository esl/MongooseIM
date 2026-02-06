-module(mongoose_websocket).
-moduledoc "This module allows mongoose_c2s to communicate with a WebSocket managed by Cowboy".

-behaviour(mongoose_xmpp_socket).

%% API
-export([start/3]).

%% mongoose_xmpp_socket callbacks
-export([peername/1,
         tcp_to_tls/3,
         handle_data/2,
         activate/1,
         close/1,
         send_xml/2,
         get_peer_certificate/1,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         export_key_materials/5,
         is_ssl/1]).

-record(websocket, {
          pid :: pid(),
          peername :: mongoose_transport:peer(),
          peercert :: undefined | binary()
         }).

-opaque socket() :: #websocket{}.

-export_type([socket/0]).

-spec start(mongoose_transport:peer(), undefined | binary(), mongoose_listener:options()) ->
          supervisor:startchild_ret().
start(Peer, PeerCert, #{hibernate_after := HibernateAfterTimeout} = Opts) ->
    State = #websocket{pid = self(), peername = Peer, peercert = PeerCert},
    mongoose_c2s:start({mongoose_websocket, State, Opts},
                       [{hibernate_after, HibernateAfterTimeout}]).

%% mongoose_xmpp_socket callbacks

-spec peername(socket()) -> mongoose_transport:peer().
peername(#websocket{peername = PeerName}) ->
    PeerName.

-spec tcp_to_tls(socket(), mongoose_listener:options(), mongoose_xmpp_socket:side()) ->
  {ok, socket()} | {error, term()}.
tcp_to_tls(_Socket, _LOpts, server) ->
    {error, tcp_to_tls_not_supported_for_websockets}.

-spec handle_data(socket(), {tcp | ssl, term(), term()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
handle_data(_Socket, {_Kind, _Term, Packet}) ->
    {raw, [Packet]}.

-spec activate(socket()) -> ok.
activate(_Socket) ->
    ok.

-spec close(socket()) -> ok.
close(#websocket{pid = Pid}) ->
    Pid ! stop,
    ok.

-spec send_xml(socket(), iodata() | exml:element() | [exml:element()]) ->
    ok | {error, term()}.
send_xml(#websocket{pid = Pid}, XMLs) when is_list(XMLs) ->
    [Pid ! {send_xml, XML} || XML <- XMLs],
    ok;
send_xml(#websocket{pid = Pid}, XML) ->
    Pid ! {send_xml, XML},
    ok.

-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(Socket, _) ->
    get_peer_certificate(Socket) /= no_peer_cert.

-spec get_peer_certificate(socket()) -> mongoose_xmpp_socket:peercert_return().
get_peer_certificate(#websocket{peercert = undefined}) ->
    no_peer_cert;
get_peer_certificate(#websocket{peercert = PeerCert}) ->
    Decoded = public_key:pkix_decode_cert(PeerCert, plain),
    {ok, Decoded}.

-spec is_channel_binding_supported(socket()) -> boolean().
is_channel_binding_supported(_Socket) ->
    false.

-spec export_key_materials(socket(), Labels, Contexts, WantedLengths, ConsumeSecret) ->
    {error, export_key_materials_not_supported_for_websockets}
      when
      Labels :: [binary()],
      Contexts :: [binary() | no_context],
      WantedLengths :: [non_neg_integer()],
      ConsumeSecret :: boolean().
export_key_materials(_Socket, _, _, _, _) ->
    {error, export_key_materials_not_supported_for_websockets}.

-spec is_ssl(socket()) -> boolean().
is_ssl(_Socket) ->
    false.
