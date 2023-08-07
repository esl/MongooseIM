%%%----------------------------------------------------------------------
%%% File    : mongoose_packet_handler.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Packet handler behaviour
%%% Created : 24 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_packet_handler).
-author('piotr.nosek@erlang-solutions.com').

-record(packet_handler, {
          handler :: process_packet(),
          extra :: extra()
         }).
-type extra() :: map().
-type t() :: #packet_handler{}.
-export_type([t/0]).

-define(ARGS, Acc :: mongoose_acc:t(),
              From :: jid:jid(),
              To :: jid:jid(),
              El :: exml:element(),
              Extra :: extra()).
-type process_packet() :: fun((?ARGS) -> mongoose_acc:t()).
-callback process_packet(?ARGS) -> mongoose_acc:t().

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([new/1, new/2, process/5, add_extra/2]).
%% Getters
-export([module/1, extra/1]).

-spec new(Module :: module()) -> t().
new(Module) ->
    new(Module, #{}).

-spec new(Module :: module(), Extra :: extra()) -> t().
new(Module, Extra) when is_atom(Module), is_map(Extra) ->
    #packet_handler{handler = fun Module:process_packet/5, extra = Extra }.

-spec process(Handler :: t(),
              Acc :: mongoose_acc:t(),
              From ::jid:jid(),
              To ::jid:jid(),
              El :: exml:element()) -> mongoose_acc:t().
process(#packet_handler{handler = ProcessPacket, extra = Extra }, Acc, From, To, El) ->
    ProcessPacket(Acc, From, To, El, Extra).

module(#packet_handler{handler = ProcessPacket }) ->
    {_, Module} = erlang:fun_info(ProcessPacket, module),
    Module.

extra(#packet_handler{ extra = Extra }) ->
    Extra.

add_extra(#packet_handler{ extra = OldExtra } = Handler, Extra) ->
    %% KV pairs from the OldExtra map will remain unchanged, only
    %% the new keys from Extra map will be added to the NewExtra map
    NewExtra = maps:merge(Extra, OldExtra),
    Handler#packet_handler{extra = NewExtra}.
