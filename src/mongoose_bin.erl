%%%----------------------------------------------------------------------
%%% File    : mongoose_bin.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Binaries manipulation and generation
%%% Created : 24 Jul 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mongoose_bin).
-author('piotr.nosek@erlang-solutions.com').

-export([tokens/2,
         join/2,
         gen_from_crypto/0,
         gen_from_timestamp/0,
         string_to_binary/1]).

%% ---------------------------------------------------
%% API
%% ---------------------------------------------------

%% tokens/2 and join/2 functions from original str.erl module
%% (C) Evgeniy Khramtsov <ekhramtsov@process-one.net>

%% Each byte in second argument is treated as distinct, one-character separator
-spec tokens(Subject :: binary(), BinaryWithSeparators :: binary()) -> [binary()].
tokens(B1, B2) ->
    [list_to_binary(T) ||
     T <- string:tokens(binary_to_list(B1), binary_to_list(B2))].

-spec join(BinsToJoin :: [binary()], Separator :: binary() | char()) -> binary().
join(L, Sep) ->
    iolist_to_binary(join_s(L, Sep)).

-spec gen_from_crypto() -> binary().
gen_from_crypto() ->
    bin_to_hex:bin_to_hex(crypto:strong_rand_bytes(8)).

-spec gen_from_timestamp() -> binary().
gen_from_timestamp() ->
    {Mega, Secs, Micro} = os:timestamp(),
    MegaB = integer_to_binary(Mega),
    SecsB = integer_to_binary(Secs),
    MicroB = integer_to_binary(Micro),
    <<MegaB/binary, $-, SecsB/binary, $-, MicroB/binary>>.

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


%% ---------------------------------------------------
%% Internal functions
%% ---------------------------------------------------

join_s([], _Sep) ->
    [];
join_s([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

