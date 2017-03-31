-module(mongoose_lib).
-export([bin_to_int/1, log_if_backend_error/1]).

-include("ejabberd.hrl").

%% @doc string:to_integer/1 for binaries
bin_to_int(Bin) ->
    bin_to_int(Bin, 0).

bin_to_int(<<H, T/binary>>, X) when $0 =< H, H =< $9 ->
    bin_to_int(T, (X*10)+(H-$0));
bin_to_int(Bin, X) ->
    {X, Bin}.


%% @doc Database backends for various modules return ok, {atomic, ok}
%% or {atomic, []} on success, and usually {error, ...} on failure.
%% All we need is to log an error if such occurred, and proceed normally.
-spec log_if_backend_error(any()) -> ok.
log_if_backend_error(V) ->
    case V of
        ok -> ok;
        {atomic, _} -> ok;
        {error, E} -> ?ERROR_MSG("Error calling backend - got '~p'", [E]);
        E -> ?ERROR_MSG("Unexpected return from backend - got '~p'", [E])
    end,
    ok.

