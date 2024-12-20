-module(mongoose_component_listener).

-behaviour(mongoose_listener).
-export([start_listener/1, instrumentation/0]).

-behaviour(ranch_protocol).
-export([start_link/3]).

-type conflict_behaviour() :: disconnect | kick_old.

-type options() :: #{access := atom(),
                     shaper := atom(),
                     password := binary(),
                     check_from := boolean(),
                     hidden_components := boolean(),
                     conflict_behaviour := conflict_behaviour(),
                     atom() => any()}.

-export_type([conflict_behaviour/0, options/0]).

%% mongoose_listener

-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    [{component_tcp_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tcp_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {component_xmpp_element_size_out, #{}, #{metrics => #{byte_size => histogram}}},
     {component_xmpp_element_size_in, #{}, #{metrics => #{byte_size => histogram}}},
     {component_auth_failed, #{}, #{metrics => #{count => spiral}}},
     {component_element_in, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- element_spirals()])}},
     {component_element_out, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- element_spirals()])}}].

element_spirals() ->
    [count, stanza_count, message_count, iq_count, presence_count,
     error_count, message_error_count, iq_error_count, presence_error_count].

-spec start_listener(options()) -> ok.
start_listener(#{module := Module} = Opts) when is_atom(Module) ->
    TransportOpts = mongoose_listener:prepare_socket_opts(Opts),
    ListenerId = mongoose_listener_config:listener_id(Opts),
    ChildSpec0 = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, Module, Opts),
    ChildSpec1 = ChildSpec0#{id := ListenerId, modules => [?MODULE, ranch_embedded_sup]},
    mongoose_listener_sup:start_child(ChildSpec1).

%% ranch_protocol
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    ProcessOpts = [{hibernate_after, HibernateAfterTimeout}],
    mongoose_component_connection:start_link({mongoose_component_ranch, {Transport, Ref}, Opts}, ProcessOpts).
