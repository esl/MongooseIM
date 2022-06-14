-module(mongoose_graphql_http_upload_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(#{user := #jid{lserver = Domain}}, httpUpload, <<"getUrl">>,
        #{<<"filename">> := FileName, <<"size">> := FileSize,
          <<"contentType">> := ContentType, <<"timeout">> := Timeout} = Data) ->
    case mod_http_upload_api:get_urls(Domain, FileName, FileSize, ContentType, Timeout) of
        {ok, _} = Result ->
            Result;
        Error ->
            make_error(Error, Data)
    end.
