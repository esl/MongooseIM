-module(mongoose_measurements).

-export([measure_element/1]).

-include_lib("exml/include/exml.hrl").

-spec measure_element(exml:element()) -> mongoose_instrument:measurements().
measure_element(#xmlel{name = Name} = Element) ->
    Metrics = measure_element(Name, exml_query:attr(Element, <<"type">>)),
    Metrics#{element => Element}.

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
