-module(mongoose_measurements).

-export([measure_element/1, element_metrics/0]).

-include_lib("exml/include/exml.hrl").

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

-spec element_metrics() -> mongoose_instrument:metrics().
element_metrics() ->
    Spirals = maps:from_keys(element_spirals(), spiral),
    Spirals#{byte_size => histogram}.

-spec element_spirals() -> [mongoose_instrument:metric_name()].
element_spirals() ->
    [count, stanza_count, message_count, iq_count, presence_count,
     error_count, message_error_count, iq_error_count, presence_error_count].
