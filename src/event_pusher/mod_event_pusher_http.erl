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
-export([start/2, stop/1, hooks/1, config_spec/0, instrumentation/1]).

%% hook handlers
-export([push_event/3]).

%% config spec callbacks
-export([fix_path/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-define(SENT_METRIC, mod_event_pusher_http_sent).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{push_event, HostType, fun ?MODULE:push_event/3, #{}, 50}].

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

instrumentation(HostType) ->
    [{?SENT_METRIC, #{host_type => HostType},
      #{metrics => #{count => spiral, response_time => histogram, failure_count => spiral}}}].

-spec push_event(mod_event_pusher:push_event_acc(), mod_event_pusher:push_event_params(),
                 gen_hook:extra()) -> {ok, mod_event_pusher:push_event_acc()}.
push_event(HookAcc, #{event := #chat_event{direction = Dir, from = From, to = To, packet = Packet}},
           #{host_type := HostType}) ->
    #{acc := Acc} = HookAcc,
    lists:map(fun(Opts) -> push_event(Acc, Dir, From, To, Packet, Opts) end,
              gen_mod:get_module_opt(HostType, ?MODULE, handlers)),
    {ok, HookAcc};
push_event(HookAcc, _Params, _Extra) ->
    {ok, HookAcc}.

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
    mongoose_instrument:span(?SENT_METRIC, #{host_type => HostType},
                             fun mongoose_http_client:post/5, [HostType, PoolName, Path, Headers, Body],
                             fun(Time, Result) -> handle_post_result(Time, Result, LogMeta) end).

handle_post_result(Time, {ok, {Code, _Body}}, #{sender := Sender}) ->
    #{count => 1, response_time => Time, sender => Sender, response_code => Code};
handle_post_result(_Time, {error, Reason}, LogMeta = #{sender := Sender}) ->
    ?LOG_WARNING(LogMeta#{what => event_pusher_http_req_failed,
                          text => <<"mod_event_pusher_http HTTP call failed">>,
                          reason => Reason}),
    #{failure_count => 1, sender => Sender}.

%% @doc Strip initial slash (it is added by mongoose_http_client)
fix_path(<<"/", R/binary>>) ->
    R;
fix_path(R) ->
    R.
