-module(mongoose_component_listener).

-behaviour(mongoose_listener).
-export([start_listener/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

-type conflict_behaviour() :: disconnect | kick_old.

-type options() :: #{access := atom(),
                     shaper_rule := atom(),
                     password := binary(),
                     check_from := boolean(),
                     hidden_components := boolean(),
                     conflict_behaviour := conflict_behaviour(),
                     atom() => any()}.

-export_type([conflict_behaviour/0, options/0]).

%% mongoose_listener
-spec instrumentation(options()) -> [mongoose_instrument:spec()].
instrumentation(#{connection_type := component} = _Opts) ->
    [{component_xmpp_element_size_in, #{}, #{metrics => #{byte_size => histogram}}},
     {component_xmpp_element_size_out, #{}, #{metrics => #{byte_size => histogram}}},
     {component_tcp_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tls_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tcp_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tls_data_out, #{}, #{metrics => #{byte_size => spiral}}}].

-spec start_listener(options()) -> ok.
start_listener(#{module := Module} = Opts) when is_atom(Module) ->
    TransportOpts = mongoose_listener:prepare_socket_opts(Opts),
    ListenerId = mongoose_listener_config:listener_id(Opts),
    ChildSpec = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, Module, Opts),
    mongoose_listener_sup:start_child(ChildSpec).

%% ranch_protocol
start_link(Ref, Transport, Opts) ->
    ejabberd_service:start_link({Transport, Ref, Opts}, Opts).
