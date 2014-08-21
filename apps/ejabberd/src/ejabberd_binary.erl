-module(ejabberd_binary).

-export([string_to_binary/1]).

-ifdef(no_binary_to_integer).

-export([binary_to_integer/1,
         integer_to_binary/1]).

binary_to_integer(B) ->
    catch list_to_integer(binary_to_list(B)).

integer_to_binary(I) ->
    catch list_to_binary(integer_to_list(I)).

-endif.

-spec string_to_binary(binary() | list()) -> binary().
string_to_binary(S) when is_list(S) ->
    % If list is in Erlang representation of Unicode, we must use `unicode` module
    % If it's not or is already converted, we must use list_to_binary
    % since input can be from `file:consult/1` and prior to 17.0
    % this function returned bytes in a list instead of proper unicode string
    % so it is already like after a call to `unicode`.
    case lists:any(fun(C) -> C > 255 end, S) of
        true -> unicode:characters_to_binary(S);
        false -> list_to_binary(S)
    end;
string_to_binary(B) when is_binary(B) ->
    B.
