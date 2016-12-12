-module(mongoose_lib).
-export([bin_to_int/1]).

%% @doc string:to_integer/1 for binaries
bin_to_int(Bin) ->
    bin_to_int(Bin, 0).

bin_to_int(<<H, T/binary>>, X) when $0 =< H, H =< $9 ->
    bin_to_int(T, (X*10)+(H-$0));
bin_to_int(Bin, X) ->
    {X, Bin}.
