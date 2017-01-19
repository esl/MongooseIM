%%%----------------------------------------------------------------------
%%% File    : mongoose_transport.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : TransportMod behaviour
%%% Created : 18 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_transport).
-author('piotr.nosek@erlang-solutions.com').

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-type t() :: any().
-type send_xml_input() :: {xmlstreamelement, exml:element()}
                        | jlib:xmlstreamstart()
                        | jlib:xmlstreamend().
-type peername_return() :: {ok, {inet:ip_address(), inet:port_number()}} | {error, inet:posix()}.

-export_type([t/0, send_xml_input/0, peername_return/0]).

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

-export([starttls/3, monitor/2, send_xml/3, peername/2]).

-spec starttls(TransportMod :: module(), Transport :: t(), TLSOpts :: list()) -> t().
starttls(TransportMod, Transport, TLSOpts) -> TransportMod:starttls(Transport, TLSOpts).

-spec monitor(TransportMod :: module(), Transport :: t()) -> reference().
monitor(TransportMod, Transport) -> TransportMod:monitor(Transport).

-spec send_xml(TransportMod :: module(), Transport :: t(), Data :: send_xml_input()) -> ok.
send_xml(TransportMod, Transport, Data) -> TransportMod:send_xml(Transport, Data).

-spec peername(TransportMod :: module(), Transport :: t()) -> peername_return().
peername(TransportMod, Transport) -> TransportMod:peername(Transport).

