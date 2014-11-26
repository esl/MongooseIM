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
