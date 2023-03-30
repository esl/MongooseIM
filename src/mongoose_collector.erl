-module(mongoose_collector).

-include("mongoose_logger.hrl").

%% gen_server callbacks
-behaviour(gen_server).
-export([start_common/3, stop_common/2]).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-ignore_xref([start_link/2]).

-record(watchdog, {
          host_type :: mongooseim:host_type(),
          action :: fun((mongooseim:host_type(), map()) -> term()),
          opts :: term(),
          interval :: pos_integer(), %% milliseconds
          timer_ref :: undefined | reference()
         }).

-spec start_common(atom(), mongooseim:host_type(), map()) -> term().
start_common(Module, HostType, WOpts) ->
    Name = gen_mod:get_module_proc(HostType, Module),
    MFA = {mongoose_collector, start_link, [Name, WOpts]},
    ChildSpec = {Name, MFA, permanent, 5000, worker, [Module, mongoose_collector]},
    ejabberd_sup:start_child(ChildSpec).

stop_common(Module, HostType) ->
    Name = gen_mod:get_module_proc(HostType, Module),
    ejabberd_sup:stop_child(Name).

start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, Opts, []).

init(#{host_type := HostType,
       action := Fun,
       opts := Opts,
       interval := Interval}) when is_function(Fun, 2) ->
    State = #watchdog{host_type = HostType,
                      action = Fun,
                      opts = Opts,
                      interval = Interval,
                      timer_ref = undefined},
    {ok, schedule_check(State)}.

handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info({timeout, Ref, run_action},
            #watchdog{timer_ref = Ref} = State) ->
    run_action(State),
    {noreply, schedule_check(State)};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

schedule_check(State = #watchdog{interval = Interval}) ->
    State#watchdog{timer_ref = erlang:start_timer(Interval, self(), run_action)}.

run_action(#watchdog{host_type = HostType, action = Fun, opts = Opts}) ->
    Fun(HostType, Opts).
