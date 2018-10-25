%%%----------------------------------------------------------------------
%%% File    : mongoose_transport.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : TransportMod behaviour
%%% Created : 18 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_transport).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("public_key/include/public_key.hrl").
%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-type t() :: any().
-type send_xml_input() :: {xmlstreamelement, exml:element()}
                        | jlib:xmlstreamstart()
                        | jlib:xmlstreamend().
-type peer() :: {inet:ip_address(), inet:port_number()}.
-type peername_return() :: {ok, peer()} | {error, inet:posix()}.
-type peercert_return() :: no_peer_cert | {ok, #'Certificate'{}}.

-export_type([t/0, send_xml_input/0, peer/0, peername_return/0, peercert_return/0]).

%%----------------------------------------------------------------------
%% Callback declarations
%%----------------------------------------------------------------------

-callback starttls(Transport :: t(), TLSOpts :: list()) -> t().

-callback monitor(Transport :: t()) -> MonitorRef :: reference().

-callback send_xml(Transport :: t(), Data :: send_xml_input()) -> ok.

-callback peername(Transport :: t()) -> peername_return().

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([starttls/3, monitor/2, send_xml/3, peername/2,
         get_peer_certificate/2]).

-spec starttls(TransportMod :: module(), Transport :: t(), TLSOpts :: list()) -> t().
starttls(TransportMod, Transport, TLSOpts) -> TransportMod:starttls(Transport, TLSOpts).

-spec monitor(TransportMod :: module(), Transport :: t()) -> reference().
monitor(TransportMod, Transport) -> TransportMod:monitor(Transport).

-spec send_xml(TransportMod :: module(), Transport :: t(), Data :: send_xml_input()) -> ok.
send_xml(TransportMod, Transport, Data) -> TransportMod:send_xml(Transport, Data).

-spec peername(TransportMod :: module(), Transport :: t()) -> peername_return().
peername(TransportMod, Transport) -> TransportMod:peername(Transport).

-spec get_peer_certificate(TransportMod :: module(), Transport :: t()) -> peercert_return().
get_peer_certificate(TransportMod, Transport) -> TransportMod:get_peer_certificate(Transport).
