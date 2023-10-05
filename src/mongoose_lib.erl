-module(mongoose_lib).

-export([log_if_backend_error/4]).
%% Maps
-export([maps_append/3]).
-export([maps_foreach/2]).
-export([pairs_foreach/2]).
-export([maps_or_pairs_foreach/2]).
%% Busy Wait
-export([wait_until/2, wait_until/3]).
-export([parse_ip_netmask/1]).

-export([get_message_type/1, does_local_user_exist/3]).

%% Private, just for warning
-export([deprecated_logging/1]).
-deprecated({deprecated_logging, 1, eventually}).

-ignore_xref([pairs_foreach/2, wait_until/3]).

-export_type([microseconds/0]).
-export([pmap/2, pmap/3]).
-ignore_xref([pmap/3]).

-export([is_exported/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type microseconds() :: integer().
-type message_type() :: one2one | groupchat.

%% ------------------------------------------------------------------
%% Logging
%% ------------------------------------------------------------------

%% @doc Database backends for various modules return ok, {atomic, ok}
%% or {atomic, []} on success, and usually {error, ...} on failure.
%% All we need is to log an error if such occurred, and proceed normally.
-spec log_if_backend_error(V :: any(), % value return by called backend fun
                           Module :: atom(), % caller
                           Line :: integer(),
                           Args :: any() ) -> ok.
log_if_backend_error(ok, _Module, _Line, _Args) -> ok;
log_if_backend_error({ok, _}, _Module, _Line, _Args) -> ok;
log_if_backend_error({atomic, _}, _Module, _Line, _Args) -> ok;
log_if_backend_error({updated, _}, _Module, _Line, _Args) -> ok;
log_if_backend_error({error, E}, Module, Line, Args) ->
    ?LOG_ERROR(#{what => backend_error,
                 text => <<"Error calling backend module">>,
                 caller_module => Module, caller_line => Line,
                 reason => E, args => Args}),
    ok;
log_if_backend_error(E, Module, Line, Args) ->
    ?LOG_ERROR(#{what => backend_error,
                 text => <<"Unexpected return from backend">>,
                 caller_module => Module, caller_line => Line,
                 reason => E, args => Args}),
    ok.

%% ------------------------------------------------------------------
%% Maps
%% ------------------------------------------------------------------

%% Appends a new Value to the current list of values associated with Key.
maps_append(Key, Value, Map) ->
    Values = maps:get(Key, Map, []),
    maps:put(Key, Values ++ [Value], Map).

-spec maps_foreach(fun(), map()) -> ok.
maps_foreach(Fun, Map) when is_function(Fun, 1) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun({Key, Value}), Acc
              end, ok, Map);
maps_foreach(Fun, Map) when is_function(Fun, 2) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun(Key, Value), Acc
              end, ok, Map).

-spec pairs_foreach(Fun, [{Key, Value}]) -> ok
    when
      Fun :: fun((Key, Value) -> term())
           | fun(({Key, Value}) -> term()),
      Key :: term(),
      Value :: term().
pairs_foreach(Fun, List) when is_function(Fun, 1) ->
    lists:foreach(Fun, List);
pairs_foreach(Fun, List) when is_function(Fun, 2) ->
    lists:foreach(fun({K,V}) -> Fun(K,V) end, List).

maps_or_pairs_foreach(Fun, Map) when is_map(Map) ->
    maps_foreach(Fun, Map);
maps_or_pairs_foreach(Fun, List) when is_list(List) ->
    pairs_foreach(Fun, List).


%% Busy wait
wait_until(Fun, ExpectedValue) ->
    wait_until(Fun, ExpectedValue, #{}).

wait_until(Fun, ExpectedValue, Opts) ->
    Defaults = #{time_left => timer:seconds(5), sleep_time => 100},
    do_wait_until(Fun, ExpectedValue, maps:merge(Defaults, Opts)).

do_wait_until(_, _, #{time_left := TimeLeft}) when TimeLeft =< 0 ->
    ok;
do_wait_until(Fun, ExpectedValue, Opts) ->
    case Fun() of
        ExpectedValue -> {ok, ExpectedValue};
        _OtherValue -> wait_and_continue(Fun, ExpectedValue, Opts)
    end.

wait_and_continue(Fun, ExpectedValue, #{time_left := TimeLeft, sleep_time := SleepTime} = Opts) ->
    timer:sleep(SleepTime),
    do_wait_until(Fun, ExpectedValue, Opts#{time_left => TimeLeft - SleepTime}).


deprecated_logging(Location) ->
    Map = #{what => deprecated_logging_macro,
            text => <<"Deprecated logging macro is used in your code">>},
    mongoose_deprecations:log(Location, Map, [{log_level, warning}]).

%% ------------------------------------------------------------------
%% Parse IP
%% ------------------------------------------------------------------
parse_ip_netmask(S) ->
    case string:tokens(S, "/") of
        [IPStr] -> parse_ip_netmask(IPStr, undefined);
        [IPStr, MaskStr] -> parse_ip_netmask(IPStr, MaskStr);
        _ -> error
    end.

parse_ip_netmask(IPStr, undefined) ->
    case inet_parse:address(IPStr) of
        {ok, {_, _, _, _} = IP} ->
            {ok, {IP, 32}};
        {ok, {_, _, _, _, _, _, _, _} = IP} ->
            {ok, {IP, 128}};
        _ ->
            error
    end;
parse_ip_netmask(IPStr, MaskStr) ->
    case catch list_to_integer(MaskStr) of
        Mask when is_integer(Mask),
                  Mask >= 0 ->
            case inet_parse:address(IPStr) of
                {ok, {_, _, _, _} = IP} when Mask =< 32 ->
                    {ok, {IP, Mask}};
                {ok, {_, _, _, _, _, _, _, _} = IP} when Mask =< 128 ->
                    {ok, {IP, Mask}};
                _ ->
                    error
            end;
        _ ->
            error
    end.

%% ------------------------------------------------------------------
%% does_local_user_exist
%% ------------------------------------------------------------------
-spec get_message_type(mongoose_acc:t()) -> message_type().
get_message_type(Acc) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"groupchat">> -> groupchat;
        _ -> one2one
    end.

-spec does_local_user_exist(mongooseim:host_type(), jid:jid(), message_type()) -> boolean().
does_local_user_exist(HostType, To, groupchat) ->
    (not is_to_room(To)) andalso ejabberd_auth:does_user_exist(HostType, To, stored);
does_local_user_exist(HostType, To, _) ->
    ejabberd_auth:does_user_exist(HostType, To, stored).

%% WHY: filter_local_packet is executed twice in the pipeline of muc messages. in two routing steps:
%%  - From the sender to the room: runs filter_local_packet with From=Sender, To=Room
%%  - For each member of the room:
%%      From the room to each member: runs with From=Room/Sender, To=Member
%% So, as inbox is a per-user concept, it is on the second routing step only when we want to do act.
%% NOTE: ideally for groupchats, we could instead act on `filter_room_packet`, like MAM.
-spec is_to_room(jid:jid()) -> boolean().
is_to_room(Jid) ->
    {error, not_found} =:= mongoose_domain_api:get_domain_host_type(Jid#jid.lserver).

%% ------------------------------------------------------------------
%% parallel map
%% ------------------------------------------------------------------

%% Runs a function for each element on the same node
pmap(F, Es) ->
    pmap(F, Es, 5000).

pmap(F, Es, Timeout) ->
    TimerRef = erlang:start_timer(Timeout, self(), pmap_timeout),
    Running = 
        [spawn_monitor(fun() -> exit({pmap_result, F(E)}) end)
            || E <- Es],
    Result = collect(Running, TimerRef),
    cancel_and_flush_timer(TimerRef),
    Result.

collect([], _TimerRef) -> [];
collect([{Pid, MRef} | Next] = In, TimerRef) ->
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            [reason_to_result(Reason) | collect(Next, TimerRef)];
        {timeout, TimerRef, pmap_timeout} ->
            stop_processes(In),
            collect(In, TimerRef)
    end.

stop_processes(In) ->
    [erlang:exit(Pid, timeout) || {Pid, _} <- In].

reason_to_result({pmap_result, Result}) ->
    {ok, Result};
reason_to_result(Reason) ->
    {error, Reason}.

cancel_and_flush_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    receive
        {timeout, TimerRef, _} -> ok
    after 0 -> ok
    end.

-spec is_exported(Module :: module(), Function :: atom(),
                  Arity :: integer()) -> boolean().
is_exported(Module, Function, Arity) ->
    code:ensure_loaded(Module),
    erlang:function_exported(Module, Function, Arity).
