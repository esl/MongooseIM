-module(mod_jewel).
-author('mayukh@jc.com').

-behaviour(gen_mod).

-export([start/2, stop/1]).
-export([user_send_packet/4]).

%-define(PROCNAME, ?MODULE).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("ejabberd_c2s.hrl").
-include_lib("exml/include/exml.hrl").
%-include("logger.hrl").


%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> any().
start(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 50).

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 50).




%%--------------------------------------------------------------------
%% Hook callbacks
%%--------------------------------------------------------------------

-spec user_send_packet(mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                       Packet :: exml:element()) -> mongoose_acc:t().
user_send_packet(Acc, From, To, Packet = #xmlel{name = <<"message">>}) ->
    ?DEBUG("INSIDE HOOK", []),
    JAcc = case mongoose_acc:stanza_type(Acc) of
        <<"chat">> -> get_jewel_chat(Acc, From, To, Packet);
        <<"groupchat">> -> get_jewel_groupchat(Acc, From, To, Packet);
        <<"headline">> -> Acc;
        <<"normal">> -> Acc;
        undefined -> Acc;
        _ -> Acc
    end,
    JAcc;
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec get_jewel_chat(mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                       Packet :: exml:element()) -> mongoose_acc:t().
get_jewel_chat(Acc, From, To, Packet) ->    
    R = rand:uniform(123),   
    J = if
       R >= 122 ->
        "15";
       R >= 118 ->
        "12";
       R >= 109 ->
        "9";
       R >= 82 ->
        "6";
       R < 82 ->
        "3"
    end,
    Jewel = #xmlel{ name = <<"jewel">>, attrs=[{<<"number">>, J }]},
    JPacket = xml:append_subtags(Packet, [Jewel]),
    JAcc = mongoose_acc:update_stanza(#{ element => JPacket, from_jid => From, to_jid => To }, Acc),    
    JAcc.


-spec get_jewel_groupchat(mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                       Packet :: exml:element()) -> mongoose_acc:t().
get_jewel_groupchat(Acc, From, To, Packet) ->
    R = rand:uniform(39),   
    J = if
       R >= 37 ->
        "9";
       R >= 28 ->
        "6";
       R < 28 ->
        "3"
    end,
    Jewel = #xmlel{ name = <<"jewel">>, attrs=[{<<"number">>, J }]},
    JPacket = xml:append_subtags(Packet, [Jewel]),
    JAcc = mongoose_acc:update_stanza(#{ element => JPacket, from_jid => From, to_jid => To }, Acc),    
    JAcc.    
    





