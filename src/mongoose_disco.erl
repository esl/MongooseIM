-module(mongoose_disco).

-export([add_features/2]).

-type acc() :: empty | {error, any()} | {result, [exml:element()]}.
-type feature() :: binary().

-spec add_features([feature()], acc())  -> acc().
add_features(Features, Acc) ->
    case Acc of
        {error, _} = Error ->
            Error;
        empty ->
            {result, Features};
        {result, InitialFeatures} ->
            {result, Features ++ InitialFeatures}
    end.
