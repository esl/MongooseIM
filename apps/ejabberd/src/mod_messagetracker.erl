%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2016 18:17
%%%-------------------------------------------------------------------
-module(mod_messagetracker).

-behaviour(gen_mod).
-export([start/2,
    msg_enter/4,
    msg_exit/3,
    stop/1
]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").
-define(BACKEND, mod_privacy_backend).
%% API
-export([]).


start(Host, _Opts) ->
    mongoose_hooks:add(user_send_packet, Host,
        ?MODULE, msg_enter, 1),
    mongoose_hooks:add(xmpp_send_element, Host,
        ?MODULE, msg_exit, 99),
    ok.

stop(Host) ->
    mongoose_hooks:delete(user_send_packet, Host,
        ?MODULE, msg_enter, 1),
    mongoose_hooks:delete(xmpp_send_element, Host,
        ?MODULE, msg_exit, 99),
    ok.


msg_enter({packet, Packet}, _From, _To, _Packet) ->
    NPacket = packet:stamp(Packet),
    NNPacket = packet:mark(NPacket, marked_by_tracker),
    {packet, NNPacket};
msg_enter(nopacket, _From, _To, _Packet) ->
    nopacket.

msg_exit({packet, #xmlel{name = <<"message">>} = Packet}, _, _Packet) ->
    M = Packet#xmlel.meta,
    ?ERROR_MSG("Uid: ~p", [Packet#xmlel.uid]),
    ?ERROR_MSG("Entered at: ~p", [proplists:get_value(timestamp, M, unknown)]),
    ?ERROR_MSG("Exit at: ~p", [os:timestamp()]),
    ?ERROR_MSG("Meta at: ~p", [M]),
    {packet, Packet};
msg_exit({packet, P}, _, _Packet) ->
    {packet, P};
msg_exit(nopacket, _, _) ->
    nopacket.
