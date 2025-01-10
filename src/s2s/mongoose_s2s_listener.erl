-module(mongoose_s2s_listener).

-behaviour(mongoose_listener).
-export([start_listener/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

%% mongoose_listener
-spec instrumentation(_) -> [mongoose_instrument:spec()].
instrumentation(_) ->
    [{s2s_tcp_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {s2s_tcp_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {s2s_tls_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {s2s_tls_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {s2s_xmpp_element_size_out, #{}, #{metrics => #{byte_size => histogram}}},
     {s2s_xmpp_element_size_in, #{}, #{metrics => #{byte_size => histogram}}},
     {s2s_auth_failed, #{}, #{metrics => #{count => spiral}}},
     {s2s_element_in, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}},
     {s2s_element_out, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}}].

%% mongoose_listener
-spec start_listener(mongoose_listener:options()) -> {ok, supervisor:child_spec()}.
start_listener(Opts) ->
    {ok, mongoose_listener:child_spec(Opts)}.

%% ranch_protocol
-spec start_link(ranch:ref(), mongoose_listener:transport_module(), mongoose_listener:options()) ->
    {ok, pid()}.
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    ProcessOpts = [{hibernate_after, HibernateAfterTimeout}],
    mongoose_s2s_socket:start_link({Ref, Transport, Opts}, ProcessOpts).
