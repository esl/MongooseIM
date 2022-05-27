%%%----------------------------------------------------------------------
%%% File    : mod_event_pusher_http.erl
%%% Author  : Baibossynv Valery <baibossynov.valery@gmail.com>
%%% Purpose : Message passing via http
%%% Created : 16 Dec 2015 by Baibossynv Valery <baibossynov.valery@gmail.com>
%%%----------------------------------------------------------------------

-module(mod_event_pusher_http).
-author("baibossynov.valery@gmail.com").

-ignore_xref([behaviour_info/1]).

-behaviour(gen_mod).
-behaviour(mod_event_pusher).
-behaviour(mongoose_module_metrics).

-callback should_make_req(Acc :: mongoose_acc:t(),
                          Dir :: in | out,
                          Packet :: exml:element(),
                          From :: jid:jid(),
                          To :: jid:jid(),
                          Opts :: [{atom(), term()}]) -> boolean().
-callback prepare_headers(Acc :: mongoose_acc:t(),
                          Dir :: in | out,
                          Domain :: jid:lserver(),
                          Message :: binary(),
                          Sender :: jid:luser(),
                          Receiver :: jid:luser(),
                          Opts :: [{atom(), term()}]) -> [{binary(), binary()}].
-callback prepare_body(Acc :: mongoose_acc:t(),
                       Dir :: in | out,
                       Domain :: jid:lserver(),
                       Message :: binary(),
                       Sender :: jid:luser(),
                       Receiver :: jid:luser(),
                       Opts :: [{atom(), term()}]) -> binary().

-include("mod_event_pusher_events.hrl").
-include("jlib.hrl").

%% API
-export([start/2, stop/1, config_spec/0, push_event/2]).

%% config spec callbacks
-export([fix_path/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-define(SENT_METRIC, [mod_event_pusher_http, sent]).
-define(FAILED_METRIC, [mod_event_pusher_http, failed]).
-define(RESPONSE_METRIC, [mod_event_pusher_http, response_time]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    ensure_metrics(HostType),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"handlers">> => #list{items = handler_config_spec(),
                                              validate = unique}},
             defaults = #{<<"handlers">> => []}}.

handler_config_spec() ->
    #section{items = #{<<"pool_name">> => #option{type = atom,
                                                  validate = pool_name},
                       <<"path">> => #option{type = binary,
                                             process = fun ?MODULE:fix_path/1},
                       <<"callback_module">> => #option{type = atom,
                                                        validate = module}
                },
             defaults = #{<<"pool_name">> => http_pool,
                          <<"path">> => <<>>,
                          <<"callback_module">> => mod_event_pusher_http_defaults}
            }.

push_event(Acc, #chat_event{direction = Dir, from = From, to = To, packet = Packet}) ->
    HostType = mongoose_acc:host_type(Acc),
    lists:map(fun(Opts) -> push_event(Acc, Dir, From, To, Packet, Opts) end,
              gen_mod:get_module_opt(HostType, ?MODULE, handlers)),
    Acc;
push_event(Acc, _Event) ->
    Acc.

push_event(Acc, Dir, From, To, Packet, Opts = #{callback_module := Mod}) ->
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
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

make_req(Acc, Dir, Domain, Sender, Receiver, Message, Opts) ->
    #{pool_name := PoolName, path := Path, callback_module := Mod} = Opts,
    HostType = mongoose_acc:host_type(Acc),
    Body = Mod:prepare_body(Acc, Dir, Domain, Message, Sender, Receiver, Opts),
    Headers = Mod:prepare_headers(Acc, Dir, Domain, Message, Sender, Receiver, Opts),
    LogMeta = #{what => event_pusher_http_req,
                text => <<"mod_event_pusher_http makes an external HTTP call">>,
                path => Path, body => Body, headers => Headers,
                sender => Sender, receiver => Receiver, direction => Dir,
                server => Domain, pool_name => PoolName, acc => Acc},
    ?LOG_INFO(LogMeta),
    T0 = os:timestamp(),
    {Res, Elapsed} = case mongoose_http_client:post(HostType, PoolName, Path, Headers, Body) of
                         {ok, _} ->
                             {ok, timer:now_diff(os:timestamp(), T0)};
                         {error, Reason} ->
                             {{error, Reason}, 0}
                     end,
    record_result(HostType, Res, Elapsed, LogMeta),
    ok.

ensure_metrics(HostType) ->
    mongoose_metrics:ensure_metric(HostType, ?SENT_METRIC, spiral),
    mongoose_metrics:ensure_metric(HostType, ?FAILED_METRIC, spiral),
    mongoose_metrics:ensure_metric(HostType, ?RESPONSE_METRIC, histogram),
    ok.

record_result(HostType, ok, Elapsed, _LogMeta) ->
    mongoose_metrics:update(HostType, ?SENT_METRIC, 1),
    mongoose_metrics:update(HostType, ?RESPONSE_METRIC, Elapsed),
    ok;
record_result(HostType, {error, Reason}, _, LogMeta) ->
    mongoose_metrics:update(HostType, ?FAILED_METRIC, 1),
    ?LOG_WARNING(LogMeta#{what => event_pusher_http_req_failed,
                          text => <<"mod_event_pusher_http HTTP call failed">>,
                          reason => Reason
                  }),
    ok.

%% @doc Strip initial slash (it is added by mongoose_http_client)
fix_path(<<"/", R/binary>>) ->
    R;
fix_path(R) ->
    R.
