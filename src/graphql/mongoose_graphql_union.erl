-module(mongoose_graphql_union).
-export([execute/1]).

-ignore_xref([execute/1]).

-include("mongoose_logger.hrl").

execute(#{<<"result">> := Result}) when is_binary(Result) -> {ok, <<"MnesiaStringResponse">>};
execute(#{<<"result">> := Result}) when is_list(Result) -> {ok, <<"MnesiaListResponse">>};
execute(#{<<"result">> := Result}) when is_integer(Result) -> {ok, <<"MnesiaIntResponse">>};
execute(#{<<"type">> := _, <<"binValue">> := _}) -> {ok, <<"ImageData">>};
execute(#{<<"extValue">> := _}) -> {ok, <<"External">>};
execute(#{<<"phonetic">> := _}) -> {ok, <<"Phonetic">>};
execute(#{<<"binValue">> := _}) -> {ok, <<"BinValue">>};
execute(#{<<"vcard">> := _}) -> {ok, <<"AgentVcard">>};
execute(#{<<"type">> := <<"histogram">>, <<"name">> := _, <<"p50">> := _}) ->
    {ok, <<"HistogramMetric">>};
execute(#{<<"type">> := <<"spiral">>, <<"name">> := _, <<"one">> := _}) ->
    {ok, <<"SpiralMetric">>};
execute(#{<<"type">> := <<"counter">>, <<"name">> := _, <<"ms_since_reset">> := _}) ->
    {ok, <<"CounterMetric">>};
execute(#{<<"type">> := <<"gauge">>, <<"name">> := _, <<"value">> := _}) ->
    {ok, <<"GaugeMetric">>}.
