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
    TransportOpts0 = mongoose_listener:prepare_socket_opts(Opts),
    TransportOpts = TransportOpts0#{connection_type => supervisor},
    ListenerId = mongoose_listener_config:listener_id(Opts),
    ChildSpec0 = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, ?MODULE, Opts),
    ChildSpec1 = ChildSpec0#{id := ListenerId, modules => [?MODULE, ranch_embedded_sup]},
    {ok, ChildSpec1}.

%% ranch_protocol
-spec start_link(ranch:ref(), mongoose_listener:transport_module(), mongoose_listener:options()) ->
    {ok, pid()}.
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    ProcessOpts = [{hibernate_after, HibernateAfterTimeout}],
    mongoose_s2s_socket:start_link({Ref, Transport, Opts}, ProcessOpts).
