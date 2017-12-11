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

-callback process_packet(Acc :: mongoose_acc:t(), From :: jid(), To :: jid(),
                         El :: exml:element(), Extra :: any()) -> any().

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([new/1, new/2, process/5]).

-spec new(Module :: module()) -> t().
new(Module) ->
    new(Module, undefined).

-spec new(Module :: module(), Extra :: any()) -> t().
new(Module, Extra) when is_atom(Module) ->
    #packet_handler{ module = Module, extra = Extra }.

-spec process(Handler :: t(),
              Acc :: mongoose_acc:t(),
              From :: jid(),
              To :: jid(),
              El :: exml:element()) -> any().
process(#packet_handler{ module = Module, extra = Extra }, Acc, From, To, El) ->
    Module:process_packet(Acc, From, To, El, Extra).

