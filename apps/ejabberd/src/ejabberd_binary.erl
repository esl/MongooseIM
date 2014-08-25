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
    list_to_binary(S);
string_to_binary(B) when is_binary(B) ->
    B.

