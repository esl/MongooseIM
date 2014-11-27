%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongoose_api_json).

-behaviour(mongoose_api_format).

%% mongoose_api_format callbacks
-export([serialize/1,
         deserialize/1]).

%%--------------------------------------------------------------------
%% mongoose_api_format callbacks
%%--------------------------------------------------------------------
deserialize(Json) ->
    try mochijson2:decode(Json) of
        Data ->
            {ok, do_deserialize(Data)}
    catch _:_ ->
        {error, unprocessable}
    end.

serialize(Data) ->
    do_serialize(Data).

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
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

prepare_struct({Key, Value}) ->
    {struct, [{Key, prepare_struct(Value)}]};
prepare_struct(List) when is_list(List) ->
    case is_proplist(List) of
        true  ->
            Fields = [{K, prepare_struct(V)} || {K, V} <- List],
            {struct, Fields};
        false ->
            [prepare_struct(Element) || Element <- List]
    end;
prepare_struct(Other) ->
    Other.

is_proplist(List) ->
    is_proplist(List, sets:new()).

is_proplist([], _Keys) ->
    true;
is_proplist([{Key, _}|Tail], Keys) ->
    case sets:is_element(Key, Keys) of
        true ->
            false;
        false ->
            is_proplist(Tail, sets:add_element(Key, Keys))
    end;
is_proplist(_Other, _Keys) ->
    false.
