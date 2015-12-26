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
-export([start/2, stop/1, on_user_send_packet/3]).

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
  %%TODO: delete children of supervisor
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
  ok.

on_user_send_packet(From, To, Packet) ->
  Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
  Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
  if (Type == <<"chat">>) and (Body /= <<"">>) ->
    make_req(post, From#jid.lserver, From#jid.luser, To#jid.luser, Body);
    ok;
    true ->
      ok
  end.

make_req(Method, Host, Sender, Receiver, Message) ->
  PathPrefix = gen_mod:get_module_opt(Host, ?MODULE, prefix_path, ?DEFAULT_PREFIX_PATH),
  Connection = cuesport:get_worker(existing_pool_name(Host)),
  Query = <<"author=", Sender/binary, "&server=", Host/binary, "&reciever=", Receiver/binary, "&message=", Message/binary>>,
  ?INFO_MSG("Making request '~s' for user ~s@~s...", [PathPrefix, Sender, Host]),
  Header = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
  {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} = case Method of
    get -> fusco:request(Connection, <<PathPrefix/binary, "?", Query/binary>>,
      "GET", Header, "", 2, 5000);
    post -> fusco:request(Connection, <<PathPrefix/binary>>,
       "POST", Header, Query, 2, 5000)
  end,
  ?INFO_MSG("Request result: ~s: ~p", [Code, RespBody]),
  ok.

pool_name(Host) ->
  list_to_atom("http_notification_" ++ binary_to_list(Host)).

existing_pool_name(Host) ->
  list_to_existing_atom("http_notification_" ++ binary_to_list(Host)).