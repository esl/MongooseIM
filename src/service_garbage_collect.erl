-module(service_garbage_collect).

-behaviour(mongoose_service).
-behaviour(gen_server).

-include("mongoose_config_spec.hrl").

-define(DEFAULT_INITIAL_REPORT, timer:minutes(5)).
-define(DEFAULT_REPORT_AFTER, timer:hours(3)).

-include("mongoose.hrl").

-export([start/1, stop/0, config_spec/0]).
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-ignore_xref([start_link/1]).

-record(state, {interval, ref}).

-spec start(proplists:proplist()) -> {ok, pid()}.
start(Args) ->
    Spec = {?MODULE, {?MODULE, start_link, [Args]},
            temporary, brutal_kill, worker, [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(Spec).

-spec stop() -> ok.
stop() ->
    ejabberd_sup:stop_child(?MODULE).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"interval">> => #option{type = integer,
                                           validate = non_negative}
                }
      }.

-spec start_link(proplists:proplist()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec init(proplists:proplist()) -> {ok, #state{}}.
init(Args) ->
    Interval = proplists:get_value(interval, Args, 60000),
    Ref = erlang:send_after(Interval, self(), gc_timeout),
    {ok, #state{interval = Interval, ref = Ref}}.

handle_info(gc_timeout, #state{interval = Interval, ref = Ref} = State) ->
    erlang:cancel_timer(Ref),
    do_gc(),
    Ref2 = erlang:send_after(Interval, self(), gc_timeout),
    %% Do hibernate to clean our process
    {noreply, State#state{ref = Ref2}, hibernate};
handle_info(_Message, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
   ok.

do_gc() ->
    do_gc(self(), erlang:processes()).

do_gc(Me, [Me|T]) ->
    %% Do not collect our process, because we will do GC, once we don't have
    %% a list of processes in our heap
    do_gc(Me, T);
do_gc(Me, [H|T]) ->
    erlang:garbage_collect(H),
    do_gc(Me, T);
do_gc(_Me, []) ->
    ok.
