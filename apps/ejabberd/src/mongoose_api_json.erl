-module(mongoose_api_json).

-export([serialize/1,
         deserialize/1]).

deserialize(Json) ->
    try mochijson2:decode(Json) of
        Data ->
            {ok, do_deserialize(Data)}
    catch _:_ ->
        {error, unprocessable}
    end.

serialize(Data) ->
    do_serialize(Data).

do_deserialize({ElementName, {struct, [{_Key, _Value}|_Rest]=Proplist}}) ->
    {ElementName, do_deserialize(Proplist)};
do_deserialize({struct, Proplist}) ->
    [do_deserialize(Element) || Element <- Proplist];
do_deserialize(List) when is_list(List) ->
    [do_deserialize(Element) || Element <- List];
do_deserialize(Other) ->
    Other.

do_serialize(Data) ->
    mochijson2:encode(prepare_struct(Data)).

prepare_struct({ElementName, Value}) ->
    prepare_struct2([{ElementName, Value}]);
prepare_struct(Other) ->
    prepare_struct2(Other).

prepare_struct2({ElementName, [{_Key, _Value}|_Rest]=Proplist}) ->
    {ElementName, prepare_struct2(Proplist)};
prepare_struct2([{_,_}|_]=Proplist) ->
    {struct, [prepare_struct2(Element) || Element <- Proplist]};
prepare_struct2(List) when is_list(List) ->
    [prepare_struct2(Element) || Element <- List];
prepare_struct2(Other) ->
    Other.
