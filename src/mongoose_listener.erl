%% @doc Manage starting and stopping of configured listeners

-module(mongoose_listener).

-include("mongoose.hrl").

%% Only for tests
-export([start_listener/1, stop_listener/1]).
-ignore_xref([start_listener/1, stop_listener/1]).

%% API
-export([start/0, stop/0]).

-callback start_listener(options()) -> ok.
-callback listener_instrumentation(options()) -> [mongoose_instrument:spec()].
-optional_callbacks([listener_instrumentation/1]).

-type options() :: #{port := inet:port_number(),
                     ip_tuple := inet:ip_address(),
                     ip_address := string(),
                     ip_version := 4 | 6,
                     proto := proto(),
                     any() => any()}.
-type id() :: {inet:port_number(), inet:ip_address(), proto()}.
-type proto() :: tcp.

-export_type([options/0, id/0, proto/0]).

%% API

start() ->
    %% Each listener module could be started more than on different ports.
    Listeners = mongoose_config:get_opt(listen),
    mongoose_instrument:set_up(instrumentation(Listeners)),
    lists:foreach(fun start_listener/1, Listeners).

stop() ->
    Listeners = mongoose_config:get_opt(listen),
    lists:foreach(fun stop_listener/1, Listeners),
    mongoose_instrument:tear_down(instrumentation(Listeners)).

%% Internal functions

start_listener(Opts = #{module := Module}) ->
    try
        Module:start_listener(Opts) % This function should call mongoose_listener_sup:start_child/1
    catch
        Class:Reason:Stacktrace ->
            ?LOG_CRITICAL(#{what => listener_failed_to_start,
                            text => <<"Failed to start a listener">>,
                            module => Module, opts => Opts,
                            class => Class, reason => Reason, stacktrace => Stacktrace}),
            erlang:raise(Class, Reason, Stacktrace)
    end.

stop_listener(Opts) ->
    ListenerId = mongoose_listener_config:listener_id(Opts),
    supervisor:terminate_child(mongoose_listener_sup, ListenerId),
    supervisor:delete_child(mongoose_listener_sup, ListenerId).

-spec instrumentation([options()]) -> [mongoose_instrument:spec()].
instrumentation(Listeners) ->
    lists:usort([Spec || Listener <- Listeners, Spec <- listener_instrumentation(Listener)]).

-spec listener_instrumentation(options()) -> [mongoose_instrument:spec()].
listener_instrumentation(Opts = #{module := Module}) ->
    case erlang:function_exported(Module, listener_instrumentation, 1) of
        true ->
            Module:listener_instrumentation(Opts);
        false ->
            []
    end.
