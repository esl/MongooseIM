-module(ejabberd_binary).

-export([string_to_binary/1]).
-export([join/2]).


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


join([], _) ->
    <<>>;
join([Part], _) ->
    Part;
join(Parts, Sep) ->
    iolist_to_binary(do_join(Parts, Sep, [])).

do_join([Part], _, Acc) ->
    lists:reverse([Part | Acc]);
do_join([Part | Rest], Sep, Acc) ->
    do_join(Rest, Sep, [Sep, Part | Acc]).
