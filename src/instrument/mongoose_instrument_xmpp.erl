%%% @doc Helpers for instrumenting XMPP traffic in c2s, s2s and component connections

-module(mongoose_instrument_xmpp).

-export([execute_element_event/5, instrumentation/1]).

-include_lib("exml/include/exml.hrl").
-include("mongoose.hrl").

-type connection_type() :: c2s | s2s | component.

%%% API

-spec execute_element_event(mongoose_instrument:event_name(), connection_type(),
                            mongooseim:host_type() | undefined, exml:element(),
                            mongoose_instrument:measurements()) -> ok.
execute_element_event(EventName, ConnectionType, HostType, Element, Metadata) ->
    Measurements = measure_element(Element),
    mongoose_instrument:execute(EventName, element_labels(ConnectionType, HostType),
                                maps:merge(Measurements, Metadata)).

-spec instrumentation(connection_type()) -> [mongoose_instrument:spec()].
instrumentation(ConnectionType) ->
    element_instrumentation(ConnectionType) ++ data_instrumentation(ConnectionType).

%%% Internal functions

-spec measure_element(exml:element()) -> mongoose_instrument:measurements().
measure_element(#xmlel{name = Name} = Element) ->
    Metrics = measure_element(Name, exml_query:attr(Element, <<"type">>)),
    Metrics#{byte_size => exml:xml_size(Element), element => Element}.

-spec measure_element(binary(), binary() | undefined) -> mongoose_instrument:measurements().
measure_element(<<"message">>, <<"error">>) ->
    #{count => 1, stanza_count => 1, error_count => 1, message_error_count => 1};
measure_element(<<"iq">>, <<"error">>) ->
    #{count => 1, stanza_count => 1, error_count => 1, iq_error_count => 1};
measure_element(<<"presence">>, <<"error">>) ->
    #{count => 1, stanza_count => 1, error_count => 1, presence_error_count => 1};
measure_element(<<"message">>, _Type) ->
    #{count => 1, stanza_count => 1, message_count => 1};
measure_element(<<"iq">>, _Type) ->
    #{count => 1, stanza_count => 1, iq_count => 1};
measure_element(<<"presence">>, _Type) ->
    #{count => 1, stanza_count => 1, presence_count => 1};
measure_element(<<"stream:error">>, _Type) ->
    #{count => 1, error_count => 1};
measure_element(_Name, _Type) ->
    #{count => 1}.

-spec element_instrumentation(connection_type()) -> [mongoose_instrument:spec()].
element_instrumentation(ConnectionType) ->
    Config = #{metrics => element_metrics()},
    [{Event, element_labels(ConnectionType, HostType), Config}
     || HostType <- [undefined | host_types(ConnectionType)],
        Event <- [xmpp_element_in, xmpp_element_out]].

-spec host_types(connection_type()) -> [mongooseim:host_type()].
host_types(component) -> []; % components don't belong to host types
host_types(_) -> ?ALL_HOST_TYPES.

-spec data_instrumentation(connection_type()) -> [mongoose_instrument:spec()].
data_instrumentation(ConnectionType) ->
    Config = #{metrics => #{byte_size => spiral}},
    [{Event, #{connection_type => ConnectionType}, Config}
     || Event <- [tcp_data_in, tcp_data_out, tls_data_in, tls_data_out]].

%% Each event comes from a particular XMPP connection type: c2s, s2s or component
%% Host type has to be a non-empty binary or undefined if not known yet (e.g. stream start error)
-spec element_labels(connection_type(), mongooseim:host_type() | undefined) ->
          mongoose_instrument:labels().
element_labels(ConnectionType, undefined) ->
    #{connection_type => ConnectionType, host_type => <<>>};
element_labels(ConnectionType, HostType) when byte_size(HostType) > 0 ->
    #{connection_type => ConnectionType, host_type => HostType}.

-spec element_metrics() -> mongoose_instrument:metrics().
element_metrics() ->
    Spirals = maps:from_keys(element_spirals(), spiral),
    Spirals#{byte_size => histogram}.

-spec element_spirals() -> [mongoose_instrument:metric_name()].
element_spirals() ->
    [count, stanza_count, message_count, iq_count, presence_count,
     error_count, message_error_count, iq_error_count, presence_error_count].
