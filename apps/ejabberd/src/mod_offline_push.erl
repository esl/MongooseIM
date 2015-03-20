%%%-------------------------------------------------------------------
%%% @author Baibossynov Valeryi
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Mar 2015 8:26 AM
%%%-------------------------------------------------------------------
-module(mod_offline_push).
-author("vol").
-include("jlib.hrl").

-include("ejabberd.hrl").

%% API
-export([stop/1, start/2, send_notice/3]).

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_offline_push", []),
    {api_key, ApiKey} = gen_mod:get_opt(gcm, Opts),
    gcm:start(offline_push, ApiKey),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ok.


stop(Host)->
    ?INFO_MSG("Stoping mod_offline_push", []),
    gcm:stop(offline_push),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ok.


send_notice(_From, _To, Packet) ->
    ?DEBUG("Packet: ~p", [Packet]),
    Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
    Message = [{<<"data">>, [{<<"message">>, Body}]}],
    if (Body /= <<"">>) ->
        gcm:push(offline_push, [<<"APA91bHSpuWOr0kgUXbTjOFuu355PbYk-97_Z4pYfXatKC_1sJUwehFVnP89Mw0jHHf98rr9OaGAYntoQYpXAyak0BcSh3thTq0IiEnDrUdCpg_AFoZJiYanC9xDlbQRoe5F_CZzrq4_Xcj7sDdfWy65XuofClyUGdP-wCx_MAUFWFoDYYeu2C0">>], Message),
        ok;
        true ->
        ok
    end.
