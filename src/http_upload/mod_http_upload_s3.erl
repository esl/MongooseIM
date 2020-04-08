%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_http_upload_s3).
-author('konrad.zemek@erlang-solutions.com').
-behaviour(mod_http_upload).

-export([create_slot/6]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec create_slot(UTCDateTime :: calendar:datetime(), Token :: binary(),
                  Filename :: unicode:unicode_binary(), ContentType :: binary() | undefined,
                  Size :: pos_integer(), Opts :: proplists:proplist()) ->
                         {PUTURL :: binary(), GETURL :: binary(),
                          Headers :: #{binary() => binary()}}.
create_slot(UTCDateTime, Token, Filename, ContentType, Size, Opts) ->
    S3Opts = gen_mod:get_opt(s3, Opts),
    ExpirationTime = gen_mod:get_opt(expiration_time, Opts, 60),
    AddACL = proplists:get_value(add_acl, S3Opts, false),
    BucketURL = unicode:characters_to_binary(gen_mod:get_opt(bucket_url, S3Opts)),
    Region = list_to_binary(gen_mod:get_opt(region, S3Opts)),
    AccessKeyId = list_to_binary(gen_mod:get_opt(access_key_id, S3Opts)),
    SecretAccessKey = list_to_binary(gen_mod:get_opt(secret_access_key, S3Opts)),

    {Scheme, Host, Port, Path} = extract_uri_params(BucketURL, Token, Filename),

    ExpectedHeaders = get_expected_headers(Scheme, Host, Port, Size,
                                           ContentType, AddACL),
    UnsignedQueries = create_queries(UTCDateTime, AccessKeyId, Region,
                                     ExpirationTime, ExpectedHeaders),

    Signature = aws_signature_v4:sign(<<"PUT">>, Path, UnsignedQueries, ExpectedHeaders,
                                      UTCDateTime, Region, <<"s3">>, SecretAccessKey),

    Queries = maps:put(<<"X-Amz-Signature">>, Signature, UnsignedQueries),

    {
      compose_url(Scheme, Host, Port, Path, Queries),
      compose_url(Scheme, Host, Port, Path, #{}),
      #{}
    }.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec create_queries(UTCDateTime :: calendar:datetime(), AccessKeyId :: binary(),
                     Region :: binary(), ExpirationTime :: pos_integer(),
                     ExpectedHeaders :: #{binary() => binary()}) ->
                            Queries :: #{binary() => binary()}.
create_queries(UTCDateTime, AccessKeyId, Region, ExpirationTime, ExpectedHeaders) ->
    Scope = aws_signature_v4:compose_scope(UTCDateTime, Region, <<"s3">>),
    SignedHeadersSemi = << <<H/binary, ";">> || H <- maps:keys(ExpectedHeaders) >>,
    SignedHeaders = binary_part(SignedHeadersSemi, 0, byte_size(SignedHeadersSemi) - 1),
    #{
       <<"X-Amz-Algorithm">> => <<"AWS4-HMAC-SHA256">>,
       <<"X-Amz-Credential">> => <<AccessKeyId/binary, "/", Scope/binary>>,
       <<"X-Amz-Date">> => aws_signature_v4:datetime_iso8601(UTCDateTime),
       <<"X-Amz-Expires">> => integer_to_binary(ExpirationTime),
       <<"X-Amz-SignedHeaders">> => SignedHeaders
     }.


-spec get_expected_headers(Scheme :: http | https | atom(),
                           Host :: unicode:unicode_binary(),
                           Port :: inet:port_number(),
                           Size :: pos_integer(),
                           ContentType :: binary() | undefined,
                           AddACL :: boolean()) ->
                              Headers :: #{binary() => binary()}.
get_expected_headers(Scheme, Host, Port, Size, ContentType, AddACL) ->
    Headers = #{<<"host">> => with_port_component(Scheme, Host, Port),
                <<"content-length">> => integer_to_binary(Size)},
    WithContentType = maybe_add_content_type(ContentType, Headers),
    maybe_add_acl(AddACL, WithContentType).

maybe_add_content_type(undefined, Headers) ->
    Headers;
maybe_add_content_type(ContentType, Headers) ->
    maps:put(<<"content-type">>, ContentType, Headers).

maybe_add_acl(false, Headers) ->
    Headers;
maybe_add_acl(true, Headers) ->
    maps:put(<<"x-amz-acl">>, <<"public-read">>, Headers).


-spec extract_uri_params(BucketURL :: unicode:unicode_binary(), Token :: binary(),
                         Filename :: unicode:unicode_binary()) ->
                                {Scheme :: http | https | atom(), Host :: unicode:unicode_binary(),
                                 Port :: inet:port_number(), Path :: unicode:unicode_binary()}.
extract_uri_params(BucketURL, Token, Filename) ->
    {ok, {Scheme, [], Host, Port, Path0, []}} = http_uri:parse(binary_to_list(BucketURL)),
    KeylessPath = trim_slash(list_to_binary(Path0)),
    EscapedFilename = aws_signature_v4:uri_encode(Filename),
    Path = <<KeylessPath/binary, "/", Token/binary, "/", EscapedFilename/binary>>,
    {Scheme, list_to_binary(Host), Port, Path}.


-spec compose_url(Scheme :: http | https | atom(), Host :: unicode:unicode_binary(),
                  Port :: inet:port_number(), Path :: unicode:unicode_binary(),
                  Queries :: #{binary() => binary()}) ->
                         URL :: unicode:unicode_binary().
compose_url(Scheme, Host, Port, Path, Queries) ->
    SchemeBin = atom_to_binary(Scheme, latin1),
    <<SchemeBin/binary, "://", (with_port_component(Scheme, Host, Port))/binary,
      Path/binary, (query_string(Queries))/binary>>.


-spec query_string(Queries :: #{binary() => binary()}) -> QueryString :: binary().
query_string(Queries) ->
    query_string(maps:to_list(Queries), []).


-spec query_string(Queries :: [binary()], Acc :: [binary()]) -> binary().
query_string([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
query_string([Query | Queries], []) ->
    query_string(Queries, [<<"?", (query_encode(Query))/binary>>]);
query_string([Query | Queries], Acc) ->
    query_string(Queries, [<<"&", (query_encode(Query))/binary>> | Acc]).


-spec query_encode({Key :: binary(), Value :: binary()}) -> QueryComponent :: binary().
query_encode({Key, Value}) ->
    <<(aws_signature_v4:uri_encode(Key))/binary, "=",
      (aws_signature_v4:uri_encode(Value))/binary>>.


-spec with_port_component(Scheme :: http | https | atom(),
                          Host :: unicode:unicode_binary(),
                          Port :: inet:port_number()) -> binary().
with_port_component(Scheme, Host, Port) ->
    case lists:keyfind(Scheme, 1, http_uri:scheme_defaults()) of
        {Scheme, Port} -> Host;
        _ -> <<Host/binary, ":", (integer_to_binary(Port))/binary>>
    end.


%% Path has always at least one byte ("/")
-spec trim_slash(binary()) -> binary().
trim_slash(Data) ->
    case binary:last(Data) of
        $/ -> erlang:binary_part(Data, 0, byte_size(Data) - 1);
        _ -> Data
    end.
