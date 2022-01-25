%%%----------------------------------------------------------------------
%%% File    : mongoose_packet_handler.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Packet handler behaviour
%%% Created : 24 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_packet_handler).
-author('piotr.nosek@erlang-solutions.com').

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-record(packet_handler, { module, extra }).

-type t() :: #packet_handler{
                module :: module(),
                extra :: map()
               }.

-export_type([t/0]).

%%----------------------------------------------------------------------
%% Callback declarations
%%----------------------------------------------------------------------

-callback process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                         El :: exml:element(), Extra :: map()) -> mongoose_acc:t().

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([new/1, new/2, process/5, add_extra/2]).
%% Getters
-export([module/1, extra/1]).

-ignore_xref([behaviour_info/1]).

-spec new(Module :: module()) -> t().
new(Module) ->
    new(Module, #{}).

-spec new(Module :: module(), Extra :: map()) -> t().
new(Module, Extra) when is_atom(Module), is_map(Extra) ->
    #packet_handler{ module = Module, extra = Extra }.

-spec process(Handler :: t(),
              Acc :: mongoose_acc:t(),
              From ::jid:jid(),
              To ::jid:jid(),
              El :: exml:element()) -> mongoose_acc:t().
process(#packet_handler{ module = Module, extra = Extra }, Acc, From, To, El) ->
    Module:process_packet(Acc, From, To, El, Extra).

module(#packet_handler{ module = Module }) ->
    Module.

extra(#packet_handler{ extra = Extra }) ->
    Extra.

add_extra(#packet_handler{ extra = OldExtra } = Handler, Extra) ->
    %% KV pairs from the OldExtra map will remain unchanged, only
    %% the new keys from Extra map will be added to the NewExtra map
    NewExtra = maps:merge(Extra, OldExtra),
    Handler#packet_handler{extra = NewExtra}.
