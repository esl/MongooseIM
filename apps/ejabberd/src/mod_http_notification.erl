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
-export([start/2, stop/1, on_user_send_packet/3,should_make_req/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DEFAULT_HTTP_POOL_SIZE, 100).
-define(DEFAULT_HTTP_HOST, "http://localhost").
-define(DEFAULT_PREFIX_PATH, <<"/">>).
-define(DEFAULT_HTTP_WORKER_TIMEOUT, 5000).
-define(DEFAULT_HTTP_POOL_TIMEOUT, 200).

start(Host, _Opts) ->
  HttpHost = gen_mod:get_module_opt(Host, ?MODULE, host, ?DEFAULT_HTTP_HOST),
  PoolSize = gen_mod:get_module_opt(Host, ?MODULE, pool_size, ?DEFAULT_HTTP_POOL_SIZE),
  Timeout = gen_mod:get_module_opt(Host, ?MODULE, worker_timeout, ?DEFAULT_HTTP_WORKER_TIMEOUT),
  PathPrefix = gen_mod:get_module_opt(Host, ?MODULE, prefix_path, ?DEFAULT_PREFIX_PATH),
  PoolName = pool_name(Host),
  PoolOpts = [
    {name, {local, PoolName}},
    {size, PoolSize},
    {max_overflow, 5},
    {worker_module, mod_http_notification_requestor}
  ],
  WorkerOpts = [
    {http_host, HttpHost},
    {timeout, Timeout},
    {path_prefix, PathPrefix}
  ],
  {ok, _} = supervisor:start_child(ejabberd_sup,
    {PoolName,
      {poolboy, start_link,
        [PoolOpts, WorkerOpts]},
      transient, 2000, supervisor, [mod_http_notification_requestor]}),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
  ensure_metrics(Host),
  ok.

stop(Host) ->
  Ch = existing_pool_name(Host),
  supervisor:terminate_child(ejabberd_sup, Ch),
  supervisor:delete_child(ejabberd_sup, Ch),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
  ok.

on_user_send_packet(From, To, Packet) ->
  Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
  Mod = get_callback_module(),
  case Mod:should_make_req(Packet, From, To) of
    true ->
      make_req(From#jid.lserver, From#jid.luser, To#jid.luser, Body);
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

make_req(Host, Sender, Receiver, Message) ->
  Req = {Host, Sender, Receiver, Message},
  PoolName = existing_pool_name(Host),
  PoolTimeout = gen_mod:get_module_opt(Host, ?MODULE, pool_timeout, ?DEFAULT_HTTP_POOL_TIMEOUT),
  Res = case catch poolboy:transaction(PoolName, fun(W) -> gen_server:call(W, Req) end, PoolTimeout) of
          {'EXIT', {timeout, _}} ->
            {error, poolbusy};
          {error, HttpError} ->
            {error, HttpError};
          ok ->
            ok
        end,
  record_result(Host, Res),
  ok.

pool_name(Host) ->
  list_to_atom("http_notification_" ++ binary_to_list(Host)).

existing_pool_name(Host) ->
  list_to_existing_atom("http_notification_" ++ binary_to_list(Host)).

ensure_metrics(Host) ->
  mongoose_metrics:ensure_metric([Host, http_notifications_sent], spiral),
  mongoose_metrics:ensure_metric([Host, http_notifications_failed], spiral),
  ok.

record_result(Host, ok) ->
  mongoose_metrics:update([Host, http_notifications_sent], 1),
  ok;
record_result(Host, {error, Reason}) ->
  mongoose_metrics:update([Host, http_notifications_failed], 1),
  ?WARNING_MSG("Sending http notification failed: ~p", [Reason]),
  ok.
