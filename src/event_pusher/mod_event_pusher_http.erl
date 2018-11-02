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
                          Dir :: in | out,
                          Packet :: exml:element(),
                          From :: jid:jid(),
                          To :: jid:jid(),
                          Opts :: [{atom(), term()}]) -> boolean().
-callback prepare_headers(Acc :: mongoose_acc:t(),
                          Dir :: in | out,
                          Host :: jid:lserver(),
                          Message :: binary(),
                          Sender :: jid:luser(),
                          Receiver :: jid:luser(),
                          Opts :: [{atom(), term()}]) -> [{binary(), binary()}].
-callback prepare_body(Acc :: mongoose_acc:t(),
                       Dir :: in | out,
                       Host :: jid:lserver(),
                       Message :: binary(),
                       Sender :: jid:luser(),
                       Receiver :: jid:luser(),
                       Opts :: [{atom(), term()}]) -> binary().

-include("mod_event_pusher_events.hrl").
-include("jlib.hrl").

%% API
-export([start/2, stop/1, push_event/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-define(DEFAULT_POOL_NAME, http_pool).
-define(DEFAULT_PATH, "").

start(Host, _Opts) ->
    lists:map(fun(Conf) -> ensure_metrics(Host, proplists:get_value(callback_module, Conf)) end,
                          gen_mod:get_module_opt(Host, ?MODULE, configs, [])),
    ok.

stop(_Host) ->
    ok.

push_event(Acc, _Host, #chat_event{direction = Dir, from = From, to = To, packet = Packet}) ->
    lists:map(fun(Opts) -> push_event(Acc, Dir, From, To, Packet, Opts) end,
              gen_mod:get_module_opt(From#jid.lserver, ?MODULE, configs, [])),
    Acc;
push_event(Acc, _Host, _Event) ->
    Acc.

push_event(Acc, Dir, From, To, Packet, Opts) ->
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    Mod = get_callback_module(Opts),
    case Mod:should_make_req(Acc, Dir, Packet, From, To, Opts) of
        true ->
            make_req(Acc, Dir, From#jid.lserver, From#jid.luser, To#jid.luser, Body, Opts);
        _ ->
            ok
    end,
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_callback_module(Opts) ->
    proplists:get_value(callback_module, Opts, mod_event_pusher_http_defaults).

make_req(Acc, Dir, Host, Sender, Receiver, Message, Opts) ->
    Path = fix_path(list_to_binary(proplists:get_value(path, Opts, ?DEFAULT_PATH))),
    PoolName = proplists:get_value(pool_name, Opts, ?DEFAULT_POOL_NAME),
    Mod = get_callback_module(Opts),
    Body = Mod:prepare_body(Acc, Dir, Host, Message, Sender, Receiver, Opts),
    Headers = Mod:prepare_headers(Acc, Dir, Host, Message, Sender, Receiver, Opts),
    ?INFO_MSG("Making request '~p' for user ~s@~s...", [Path, Sender, Host]),
    T0 = os:timestamp(),
    {Res, Elapsed} = case mongoose_http_client:post(Host, PoolName, Path, Headers, Body) of
                         {ok, _} ->
                             {ok, timer:now_diff(os:timestamp(), T0)};
                         {error, Reason} ->
                             {{error, Reason}, 0}
                     end,
    record_result(Host, Mod, Res, Elapsed),
    ok.

ensure_metrics(Host, Mod) ->
    mongoose_metrics:ensure_metric(Host, [http_push, Mod, sent], spiral),
    mongoose_metrics:ensure_metric(Host, [http_push, Mod, failed], spiral),
    mongoose_metrics:ensure_metric(Host, [http_push, Mod, response_time], histogram),
    ok.
record_result(Host, Mod, ok, Elapsed) ->
    mongoose_metrics:update(Host, [http_push, Mod, sent], 1),
    mongoose_metrics:update(Host, [http_push, Mod, response_time], Elapsed),
    ok;
record_result(Host, Mod, {error, Reason}, _) ->
    mongoose_metrics:update(Host, [http_push, Mod, failed], 1),
    ?WARNING_MSG("Sending http notification via ~p failed: ~p", [Mod, Reason]),
    ok.

%% @doc Strip initial slash (it is added by mongoose_http_client)
fix_path(<<"/", R/binary>>) ->
    R;
fix_path(R) ->
    R.
