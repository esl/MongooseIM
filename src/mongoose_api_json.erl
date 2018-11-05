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
    try jiffy:decode(Json, [return_maps]) of
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
do_deserialize(#{} = Map) ->
    maps:to_list(maps:map(fun(_K, V) -> do_deserialize(V) end, Map));
do_deserialize(NotAMap) ->
    NotAMap.

do_serialize(Data) ->
    jiffy:encode(prepare_struct(Data)).

prepare_struct({Key, Value}) ->
    #{prepare_key(Key) => prepare_struct(Value)};
prepare_struct([]) ->
    [];
prepare_struct(List) when is_list(List) ->
    case is_proplist(List) of
        true  ->
            maps:from_list([{prepare_key(K), prepare_struct(V)} || {K, V} <- List]);
        false ->
            [prepare_struct(Element) || Element <- List]
    end;
prepare_struct(List) when is_list(List) ->
    try unicode:characters_to_binary(List) of
        Bin when is_binary(Bin) -> Bin;
        _ -> List %% Items in List are not valid unicode codepoints
    catch
        error:badarg -> List %% List is not a list of characters
    end;
prepare_struct(Other) ->
    Other.

prepare_key(Key) when is_integer(Key) ->
    integer_to_binary(Key);
prepare_key(Key) when is_list(Key); is_binary(Key) ->
    case unicode:characters_to_binary(Key) of
        Bin when is_binary(Bin) -> Bin
    end;
prepare_key(Key) ->
    Key.

is_proplist(List) ->
    is_proplist(List, sets:new()).

is_proplist([], _Keys) ->
    true;
is_proplist([{Key, _} | Tail], Keys) ->
    case sets:is_element(Key, Keys) of
        true ->
            false;
        false ->
            is_proplist(Tail, sets:add_element(Key, Keys))
    end;
is_proplist(_Other, _Keys) ->
    false.
