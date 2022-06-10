-module(mongoose_graphql_http_upload_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, httpUpload, <<"getUrl">>, #{<<"domain">> := Domain,
                                          <<"size">> := FileSize,
                                          <<"contentType">> := ContentType,
                                          <<"timeout">> := Timeout,
                                          <<"filename">> := FileName} = Data) ->
    case mod_http_upload_api:get_urls(Domain, FileName, FileSize, ContentType, Timeout) of
        {ok, _} = Result -> Result;
        Error ->
            make_error(Error, Data)
    end.
