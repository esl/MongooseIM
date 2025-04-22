-module(mongoose_component_listener).

-behaviour(mongoose_listener).
-export([listener_spec/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

%% mongoose_listener
-spec instrumentation(_) -> [mongoose_instrument:spec()].
instrumentation(_) ->
    [{component_auth_failed, #{}, #{metrics => #{count => spiral}}} |
     mongoose_instrument_xmpp:instrumentation(component)].

%% mongoose_listener
-spec listener_spec(mongoose_listener:options()) -> supervisor:child_spec().
listener_spec(Opts) ->
    mongoose_listener:child_spec(Opts).

%% ranch_protocol
-spec start_link(ranch:ref(), mongoose_listener:transport_module(), mongoose_listener:options()) ->
    {ok, pid()}.
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    ProcessOpts = [{hibernate_after, HibernateAfterTimeout}],
    mongoose_component_connection:start_link({Transport, Ref, Opts}, ProcessOpts).
