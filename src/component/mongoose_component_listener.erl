-module(mongoose_component_listener).

-behaviour(mongoose_listener).

-type conflict_behaviour() :: disconnect | kick_old.

-type options() :: #{access := atom(),
                     shaper_rule := atom(),
                     password := binary(),
                     check_from := boolean(),
                     hidden_components := boolean(),
                     conflict_behaviour := conflict_behaviour(),
                     atom() => any()}.

-export_type([conflict_behaviour/0, options/0]).

-export([start_listener/1, instrumentation/1]).

-spec start_listener(options()) -> ok.
start_listener(Opts) ->
    mongoose_tcp_listener:start_listener(Opts).

-spec instrumentation(options()) -> [mongoose_instrument:spec()].
instrumentation(#{connection_type := component} = _Opts) ->
    [{component_xmpp_element_size_in, #{}, #{metrics => #{byte_size => histogram}}},
     {component_xmpp_element_size_out, #{}, #{metrics => #{byte_size => histogram}}},
     {component_tcp_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tls_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tcp_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tls_data_out, #{}, #{metrics => #{byte_size => spiral}}}].
