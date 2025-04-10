-module(mongoose_component_listener).

-behaviour(mongoose_listener).
-export([listener_spec/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

%% mongoose_listener
-spec instrumentation(_) -> [mongoose_instrument:spec()].
instrumentation(_) ->
    [{tcp_data_in, #{connection_type => component}, #{metrics => #{byte_size => spiral}}},
     {tcp_data_out, #{connection_type => component}, #{metrics => #{byte_size => spiral}}},
     {tls_data_in, #{connection_type => component}, #{metrics => #{byte_size => spiral}}},
     {tls_data_out, #{connection_type => component}, #{metrics => #{byte_size => spiral}}},
     {xmpp_element_size_out, #{connection_type => component}, #{metrics => #{byte_size => histogram}}},
     {xmpp_element_size_in, #{connection_type => component}, #{metrics => #{byte_size => histogram}}},
     {component_auth_failed, #{}, #{metrics => #{count => spiral}}},
     {component_element_in, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}},
     {component_element_out, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}}].

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
