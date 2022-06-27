-module(mongoose_graphql_offline_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, offline, <<"deleteExpiredMessages">>, #{<<"domain">> := Domain} = Input) ->
    case mod_offline_api:delete_expired_messages(Domain) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, Input)
    end;

execute(_Ctx, offline, <<"deleteOldMessages">>, #{<<"domain">> := Domain, <<"days">> := Days} = Input) ->
    case mod_offline_api:delete_old_messages(Domain, Days) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, Input)
    end.
