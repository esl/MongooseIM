-module(mongoose_graphql_permission).

-export([if_permitted/3]).

if_permitted(Ctx, ExpectedRole, Fun) ->
    case maps:get(role, Ctx, none) of
        ExpectedRole ->
            Fun();
        _ ->
            {error, no_permission}
    end.
