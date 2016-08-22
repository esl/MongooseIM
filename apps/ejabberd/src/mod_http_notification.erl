%%%----------------------------------------------------------------------
%%% File    : mod_notify.erl
%%% Author  : Baibossynv Valery <baibossynov.valery@gmail.com>
%%% Purpose : Message passing via http
%%% Created : 16 Dec 2015 by Baibossynv Valery <baibossynov.valery@gmail.com>
%%%----------------------------------------------------------------------

-module(mod_http_notification).
-author("baibossynov.valery@gmail.com").
-behaviour(gen_mod).

%% API
-export([start/2, stop/1, on_user_send_packet/3, should_make_req/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DEFAULT_POOL_NAME, http_pool).
-define(DEFAULT_PATH, "").

start(Host, _Opts) ->
    Path = gen_mod:get_module_opt(Host, ?MODULE, path, ?DEFAULT_PATH),
    PoolName = gen_mod:get_module_opt(Host, ?MODULE, pool_name, ?DEFAULT_POOL_NAME),
    mod_http_notification_requestor:start(Host, PoolName, Path),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
    ok.

on_user_send_packet(From, To, Packet) ->
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    Mod = get_callback_module(),
    case Mod:should_make_req(Packet, From, To) of
        true ->
            mod_http_notification_requestor:request(From#jid.lserver, From#jid.luser, To#jid.luser, Body);
        _ ->
            ok
    end.

%% @doc This function determines whether to send http notification or not.
%% Can be reconfigured by creating a custom module implementing should_make_req/3
%% and adding it to mod_http_notification settings as {callback_module}
%% Default behaviour is to send all chat messages with non-empty body.
should_make_req(Packet, From, To) ->
    Type = exml_query:attr(Packet, <<"type">>, <<>>),
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    should_make_req(Type, Body, From, To).

should_make_req(<<"chat">>, Body, _From, _To) when Body /= <<"">> ->
    true;
should_make_req(_, _, _, _) ->
    false.

get_callback_module() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, callback_module, ?MODULE).


