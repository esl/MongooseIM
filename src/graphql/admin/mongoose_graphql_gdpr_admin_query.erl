-module(mongoose_graphql_gdpr_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, gdpr, <<"retrievePersonalData">>, #{<<"username">> := User, <<"domain">> := Domain,
                                                  <<"resultFilepath">> := FilePath}) ->
    try gdpr_api:retrieve_all(User, Domain, FilePath) of
        ok -> {ok, "Data retrieved"};
        Error -> make_error(Error, #{user => User, domain => Domain})
    catch
        _ -> make_error({internal_server_error, "Internal server error"},
                        #{user => User, domain => Domain, filePath => FilePath})
    end.
