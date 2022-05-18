-module(mongoose_graphql_union).

-export([execute/1]).

-ignore_xref([execute/1]).

execute(#{<<"type">> := _, <<"binValue">> := _}) -> {ok, <<"ImageData">>};
execute(#{<<"extValue">> := _}) -> {ok, <<"External">>};
execute(#{<<"phonetic">> := _}) -> {ok, <<"Phonetic">>};
execute(#{<<"binValue">> := _}) -> {ok, <<"BinValue">>};
execute(#{<<"vcard">> := _}) -> {ok, <<"AgentVcard">>};
execute(_Otherwise) -> {error, unknown_type}.