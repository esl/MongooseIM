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
-export([start/2, stop/1, on_user_send_packet/3,should_make_req/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DEFAULT_HTTP_POOL_SIZE, 5).
-define(DEFAULT_HTTP_HOST, "http://localhost").
-define(DEFAULT_PREFIX_PATH, <<"/">>).

start(Host, _Opts) ->
  HttpHost = gen_mod:get_module_opt(Host, ?MODULE, host, ?DEFAULT_HTTP_HOST),
  PoolSize = gen_mod:get_module_opt(Host, ?MODULE, pool_size, ?DEFAULT_HTTP_POOL_SIZE),
  ChildMods = [fusco],
  ChildMF = {fusco, start_link},
  ChildArgs = {for_all, [HttpHost, []]},
  {ok, _} = supervisor:start_child(ejabberd_sup,
    {{http_notification_sup, Host},
      {cuesport, start_link,
        [pool_name(Host), PoolSize, ChildMods, ChildMF, ChildArgs]},
      transient, 2000, supervisor, [cuesport | ChildMods]}),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
  ok.

stop(Host) ->
  Ch = {http_notification_sup, Host},
  supervisor:terminate_child(ejabberd_sup, Ch),
  supervisor:delete_child(ejabberd_sup, Ch),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
  ok.

on_user_send_packet(From, To, Packet) ->
  Type = exml_query:attr(Packet, <<"type">>, <<>>),
  Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
  Mod = get_callback_module(),
  case Mod:should_make_req(Type, Body, From, To) of
    true ->
      make_req(From#jid.lserver, From#jid.luser, To#jid.luser, Body);
    _ ->
      ok
  end.

%% @doc This function determines whether to send http notification or not.
%% Can be reconfigured by creating a custom module implementing should_make_req/4
%% and adding it to mod_http_notification settings as {callback_module}
%% Default behaviour is to send all chat messages with non-empty body.
should_make_req(<<"chat">>, Body, _From, _To) when Body /= <<"">> ->
  true;
should_make_req(_, _, _, _) ->
  false.

get_callback_module() ->
  gen_mod:get_module_opt(?MYNAME, ?MODULE, callback_module, ?MODULE).

make_req(Host, Sender, Receiver, Message) ->
  PathPrefix = gen_mod:get_module_opt(Host, ?MODULE, prefix_path, ?DEFAULT_PREFIX_PATH),
  Connection = cuesport:get_worker(existing_pool_name(Host)),
  Query = <<"author=", Sender/binary, "&server=", Host/binary, "&reciever=", Receiver/binary, "&message=", Message/binary>>,
  ?INFO_MSG("Making request '~s' for user ~s@~s...", [PathPrefix, Sender, Host]),
  Header = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
  {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} = fusco:request(Connection, <<PathPrefix/binary>>,
       "POST", Header, Query, 2, 5000),
  ?INFO_MSG("Request result: ~s: ~p", [Code, RespBody]),
  ok.

pool_name(Host) ->
  list_to_atom("http_notification_" ++ binary_to_list(Host)).

existing_pool_name(Host) ->
  list_to_existing_atom("http_notification_" ++ binary_to_list(Host)).