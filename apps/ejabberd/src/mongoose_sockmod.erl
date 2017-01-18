%%%----------------------------------------------------------------------
%%% File    : mongoose_sockmod.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : SockMod behaviour
%%% Created : 18 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_sockmod).
-author('piotr.nosek@erlang-solutions.com').

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-type socket_state() :: any().
-type send_xml_input() :: {xmlstreamelement, exml:element()}
                        | jlib:xmlstreamstart()
                        | jlib:xmlstreamend().
-type peername_return() :: {ok, {inet:ip_address(), inet:port_number()}} | {error, inet:posix()}.

-export_type([send_xml_input/0, peername_return/0]).

%%----------------------------------------------------------------------
%% Callback declarations
%%----------------------------------------------------------------------

-callback starttls(SocketState :: socket_state(), TLSOpts :: list()) -> socket_state().

-callback monitor(SocketState :: socket_state()) -> MonitorRef :: reference().

-callback send_xml(SocketState :: socket_state(), Data :: send_xml_input()) -> ok.

-callback peername(SocketState :: socket_state()) -> peername_return().

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([starttls/3, monitor/2, send_xml/3, peername/2]).

-spec starttls(SockMod :: module(), SocketState :: socket_state(), TLSOpts :: list()) ->
    socket_state().
starttls(SockMod, SocketState, TLSOpts) -> SockMod:starttls(SocketState, TLSOpts).

-spec monitor(SockMod :: module(), SocketState :: socket_state()) -> reference().
monitor(SockMod, SocketState) -> SockMod:monitor(SocketState).

-spec send_xml(SockMod :: module(), SocketState :: socket_state(), Data :: send_xml_input()) -> ok.
send_xml(SockMod, SocketState, Data) -> SockMod:send_xml(SocketState, Data).

-spec peername(SockMod :: module(), SocketState :: socket_state()) -> peername_return().
peername(SockMod, SocketState) -> SockMod:peername(SocketState).

