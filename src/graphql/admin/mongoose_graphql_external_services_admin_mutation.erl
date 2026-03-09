-module(mongoose_graphql_external_services_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, externalServices, <<"getServices">>, #{<<"domain">> := Domain,
                                                     <<"type">> := Type} = Data) ->
    case mod_extdisco_api:get_services(Domain, Type) of
        {ok, _} = Result -> Result;
        Error ->
            make_error(Error, Data)
    end.
