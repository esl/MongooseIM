-module(mongoose_graphql_permission).

-export([if_permitted/3]).
-export_type([role/0]).

-type role() :: none | user | admin.

-spec if_permitted(Ctx, Role, Fun) -> Ret when
      Ctx :: #{role => role()},
      Role :: role(),
      Fun :: fun(() -> any()),
      Ret :: {ok, any()} | {error, any()}.
if_permitted(Ctx, ExpectedRole, Fun) ->
    case maps:get(role, Ctx, none) of
        ExpectedRole ->
            Fun();
        _ ->
            {error, no_permission}
    end.
