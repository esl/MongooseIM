%%%----------------------------------------------------------------------
%%% File    : mod_event_pusher_http.erl
%%% Author  : Baibossynv Valery <baibossynov.valery@gmail.com>
%%% Purpose : Message passing via http
%%% Created : 16 Dec 2015 by Baibossynv Valery <baibossynov.valery@gmail.com>
%%%----------------------------------------------------------------------

-module(mod_event_pusher_http).
-author("baibossynov.valery@gmail.com").

-behaviour(gen_mod).
-behaviour(mod_event_pusher).

-callback should_make_req(Acc :: mongoose_acc:t(),
                          Packet :: exml:element(),
                          From :: jid:jid(),
                          To :: jid:jid()) -> boolean().
-callback prepare_headers(Acc :: mongoose_acc:t(),
                          Host :: jid:lserver(),
                          Message :: binary(),
                          Sender :: jid:luser(),
                          Receiver :: jid:luser()) -> [{binary(), binary()}].
-callback prepare_body(Acc :: mongoose_acc:t(),
                       Host :: jid:lserver(),
                       Message :: binary(),
                       Sender :: jid:luser(),
                       Receiver :: jid:luser()) -> binary().

-include("mod_event_pusher_events.hrl").
-include("jlib.hrl").

%% API
-export([start/2, stop/1, push_event/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-define(DEFAULT_POOL_NAME, http_pool).
-define(DEFAULT_PATH, "").

-define(SENT_METRIC, [mod_http_notifications, sent]).
-define(FAILED_METRIC, [mod_http_notifications, failed]).
-define(RESPONSE_METRIC, [mod_http_notifications, response_time]).

start(Host, _Opts) ->
    ensure_metrics(Host),
    ok.

stop(_Host) ->
    ok.

push_event(Acc, _Host, #chat_event{direction = in, from = From, to = To, packet = Packet}) ->
    lists:map(fun(Opts) -> push_event(Acc, From, To, Packet, Opts) end,
              gen_mod:get_module_opt(From#jid.lserver, ?MODULE, configs, [])),
    Acc;
push_event(Acc, _Host, _Event) ->
    Acc.

push_event(Acc, From, To, Packet, Opts) ->
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    Mod = get_callback_module(Opts),
    case Mod:should_make_req(Acc, Packet, From, To) of
        true ->
            make_req(Acc, From#jid.lserver, From#jid.luser, To#jid.luser, Body, Opts);
        _ ->
            ok
    end,
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_callback_module(Opts) ->
    proplists:get_value(callback_module, Opts, mod_event_pusher_http_defaults).

make_req(Acc, Host, Sender, Receiver, Message, Opts) ->
    Path = fix_path(list_to_binary(proplists:get_value(path, Opts, ?DEFAULT_PATH))),
    PoolName = proplists:get_value(pool_name, Opts, ?DEFAULT_POOL_NAME),
    Pool = mongoose_http_client:get_pool(PoolName),
    Mod = get_callback_module(Opts),
    Body = Mod:prepare_body(Acc, Host, Message, Sender, Receiver),
    Headers = Mod:prepare_headers(Acc, Host, Message, Sender, Receiver),
    ?INFO_MSG("Making request '~p' for user ~s@~s...", [Path, Sender, Host]),
    T0 = os:timestamp(),
    {Res, Elapsed} = case mongoose_http_client:post(Pool, Path, Headers, Body) of
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
