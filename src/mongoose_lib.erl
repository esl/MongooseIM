-module(mongoose_lib).
-export([bin_to_int/1, log_if_backend_error/4]).

-include("mongoose.hrl").

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
-spec log_if_backend_error(V :: any(), % value return by called backend fun
                           Module :: atom(), % caller
                           Line :: integer(),
                           Args :: any() ) -> ok.
log_if_backend_error(V, Module, Line, Args) ->
    case V of
        ok -> ok;
        {atomic, _} -> ok;
        {updated, _} -> ok; % odbc
        L when is_list(L) -> ok; % riak
        {error, E} ->
            make_msg("Error calling backend", E, Module, Line, Args);
        E ->
            make_msg("Unexpected return from backend", E, Module, Line, Args)
    end,
    ok.

make_msg(Msg, Error, Module, Line, Args) ->
    ?ERROR_MSG("~p:~p module=~p line=~p arguments=~p",
                  [Msg, Error, Module, Line, Args]).
