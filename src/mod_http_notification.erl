%%%----------------------------------------------------------------------
%%% File    : mod_http_notification.erl
%%% Author  : Baibossynv Valery <baibossynov.valery@gmail.com>
%%% Purpose : Message passing via http
%%% Created : 16 Dec 2015 by Baibossynv Valery <baibossynov.valery@gmail.com>
%%%----------------------------------------------------------------------

-module(mod_http_notification).
-author("baibossynov.valery@gmail.com").

-behaviour(gen_mod).

-callback should_make_req(Packet :: exml:element(), From :: jid(), To :: jid()) -> boolean().

%% API
-export([start/2, stop/1, on_user_send_packet/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DEFAULT_POOL_NAME, http_pool).
-define(DEFAULT_PATH, "").

-define(SENT_METRIC, [mod_http_notifications, sent]).
-define(FAILED_METRIC, [mod_http_notifications, failed]).
-define(RESPONSE_METRIC, [mod_http_notifications, response_time]).

start(Host, _Opts) ->
    ensure_metrics(Host),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 100),
    ok.

on_user_send_packet(Acc, From, To, Packet) ->
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    Mod = get_callback_module(),
    case Mod:should_make_req(Packet, From, To) of
        true ->
            make_req(From#jid.lserver, From#jid.luser, To#jid.luser, Body);
        _ ->
            ok
    end,
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_callback_module() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, callback_module, mod_http_notification_default).

make_req(Host, Sender, Receiver, Message) ->
    Path = fix_path(list_to_binary(gen_mod:get_module_opt(Host, ?MODULE, path, ?DEFAULT_PATH))),
    PoolName = gen_mod:get_module_opt(Host, ?MODULE, pool_name, ?DEFAULT_POOL_NAME),
    Pool = mongoose_http_client:get_pool(PoolName),
    Query = <<"author=", Sender/binary, "&server=", Host/binary,
              "&receiver=", Receiver/binary, "&message=", Message/binary>>,
    ?INFO_MSG("Making request '~p' for user ~s@~s...", [Path, Sender, Host]),
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    T0 = os:timestamp(),
    {Res, Elapsed} = case mongoose_http_client:post(Pool, Path, Headers, Query) of
                         {ok, _} ->
                             {ok, timer:now_diff(os:timestamp(), T0)};
                         {error, Reason} ->
                             {{error, Reason}, 0}
                     end,
    record_result(Host, Res, Elapsed),
    ok.

ensure_metrics(Host) ->
    mongoose_metrics:ensure_metric(Host, ?SENT_METRIC, spiral),
    mongoose_metrics:ensure_metric(Host, ?FAILED_METRIC, spiral),
    mongoose_metrics:ensure_metric(Host, ?RESPONSE_METRIC, histogram),
    ok.
record_result(Host, ok, Elapsed) ->
    mongoose_metrics:update(Host, ?SENT_METRIC, 1),
    mongoose_metrics:update(Host, ?RESPONSE_METRIC, Elapsed),
    ok;
record_result(Host, {error, Reason}, _) ->
    mongoose_metrics:update(Host, ?FAILED_METRIC, 1),
    ?WARNING_MSG("Sending http notification failed: ~p", [Reason]),
    ok.

%% @doc Strip initial slash (it is added by mongoose_http_client)
fix_path(<<"/", R/binary>>) ->
    R;
fix_path(R) ->
    R.
