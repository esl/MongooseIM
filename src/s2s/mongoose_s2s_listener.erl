-module(mongoose_s2s_listener).

-behaviour(mongoose_listener).
-export([start_listener/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

-type options() :: #{module := ?MODULE,
                     atom() => any()}.

-export_type([options/0]).

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

-spec start_listener(options()) -> ok.
start_listener(#{module := ?MODULE} = Opts) ->
    TransportOpts0 = mongoose_listener:prepare_socket_opts(Opts),
    TransportOpts = TransportOpts0#{connection_type => supervisor},
    ListenerId = mongoose_listener_config:listener_id(Opts),
    ChildSpec0 = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, ?MODULE, Opts),
    ChildSpec1 = ChildSpec0#{id := ListenerId, modules => [?MODULE, ranch_embedded_sup]},
    mongoose_listener_sup:start_child(ChildSpec1).

%% ranch_protocol
-spec start_link(ranch:ref(), module(), map()) -> {ok, pid()}.
start_link(Ref, Transport, Opts) ->
    mongoose_s2s_socket:start_link(Ref, Transport, Opts).
