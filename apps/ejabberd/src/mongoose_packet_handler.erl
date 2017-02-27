%%%----------------------------------------------------------------------
%%% File    : mongoose_packet_handler.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Packet handler behaviour
%%% Created : 24 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_packet_handler).
-author('piotr.nosek@erlang-solutions.com').

-include("jlib.hrl").

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-record(packet_handler, { module, extra }).

-type t() :: #packet_handler{
                module :: module(),
                extra :: any()
               }.

-export_type([t/0]).

%%----------------------------------------------------------------------
%% Callback declarations
%%----------------------------------------------------------------------

-callback process_packet(From :: jid(), To :: jid(), Packet :: exml:element() | mongoose_acc:t(),
    Extra :: any()) -> any().

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([new/1, new/2, process/4]).

-spec new(Module :: module()) -> t().
new(Module) ->
    new(Module, undefined).

-spec new(Module :: module(), Extra :: any()) -> t().
new(Module, Extra) when is_atom(Module) ->
    #packet_handler{ module = Module, extra = Extra }.

-spec process(Handler :: t(), From :: jid(), To :: jid(),
    Packet :: exml:element() | mongoose_acc:t()) -> any().
process(#packet_handler{ module = Module, extra = Extra }, From, To, Packet) ->
    Module:process_packet(From, To, Packet, Extra).

