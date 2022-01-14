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

-export([term_to_readable_binary/1]).

%% Private, just for warning
-export([deprecated_logging/1]).
-deprecated({deprecated_logging, 1, eventually}).

-ignore_xref([pairs_foreach/2, wait_until/3]).

-export_type([microseconds/0]).

-include("mongoose.hrl").

-type microseconds() :: integer().

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
log_if_backend_error(V, Module, Line, Args) ->
    case V of
        ok -> ok;
        {atomic, _} -> ok;
        {updated, _} -> ok; % rdbms
        L when is_list(L) -> ok; % riak
        {error, E} ->
            ?LOG_ERROR(#{what => backend_error,
                         text => <<"Error calling backend module">>,
                         caller_module => Module, caller_line => Line,
                         reason => E, args => Args});
        E ->
            ?LOG_ERROR(#{what => backend_error,
                         text => <<"Unexpected return from backend">>,
                         caller_module => Module, caller_line => Line,
                         reason => E, args => Args})
    end,
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

term_to_readable_binary(X) ->
    iolist_to_binary(io_lib:format("~0p", [X])).
