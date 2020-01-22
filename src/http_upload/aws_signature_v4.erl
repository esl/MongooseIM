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

-module(aws_signature_v4).
-author('konrad.zemek@erlang-solutions.com').

-export([sign/8]).
-export([datetime_iso8601/1, date_iso8601/1, compose_scope/3, uri_encode/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc
%% Signs AWS request with version 4 signature according to
%% https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
%% @end
-spec sign(Method :: binary(), URI :: binary(), Queries :: #{binary() => binary()},
           Headers :: #{binary() => binary()}, UTCDateTime :: calendar:datetime(),
           Region :: binary(), Service :: binary(), SecretAccessKey :: binary()) ->
                  Signature :: binary().
sign(Method, URI, Queries, Headers, UTCDateTime, Region, Service, SecretAccessKey) ->
    CanonicalRequest = create_canonical_request(Method, URI, Queries, Headers),
    StringToSign = create_string_to_sign(UTCDateTime, Region, Service, CanonicalRequest),
    Signature = calculate_signature(UTCDateTime, Region, Service, SecretAccessKey, StringToSign),
    hex(Signature).


%% @doc
%% Composes an AWS scope string in the form of `<date>/<region>/<service>/aws4_request'
%% @end
-spec compose_scope(UTCDateTime :: calendar:datetime(), Region :: binary(), Service :: binary())
                   -> Scope :: binary().
compose_scope(UTCDateTime, Region, Service) ->
    <<(date_iso8601(UTCDateTime))/binary, "/", Region/binary, "/",
      Service/binary, "/", "aws4_request">>.


%% @doc
%% Formats timestamp as `[YYYYMMDD]T[hhmmss]Z' (brackets added for readability).
%% @end
-spec datetime_iso8601(UTCDateTime :: calendar:datetime()) -> ISO8601DateTime :: binary().
datetime_iso8601(UTCDateTime) ->
    {_, {H, M, S}} = UTCDateTime,
    DateComponent = date_iso8601(UTCDateTime),
    TimeComponent = list_to_binary(io_lib:format("~2..0B~2..0B~2..0B", [H, M, S])),
    <<DateComponent/binary, "T", TimeComponent/binary, "Z">>.


%% @doc
%% Formats timestamp as `YYYYMMDD'.
%% @end
-spec date_iso8601(UTCDateTime :: calendar:datetime()) -> ISO8601Date :: binary().
date_iso8601(UTCDateTime) ->
    {{Y, MM, D}, _} = UTCDateTime,
    Str = io_lib:format("~B~2..0B~2..0B", [Y, MM, D]),
    list_to_binary(Str).


%% @doc
%% Encodes data according to RFC 3986. Handles utf-8 encoded data, as opposed
%% to http_uri:encode/1 .
%% @end
-spec uri_encode(Data :: binary()) -> URIEncodedData :: binary().
uri_encode(Data) ->
    uri_encode(Data, true).

%%--------------------------------------------------------------------
%% Signing steps
%%--------------------------------------------------------------------

%% Task 1: Create a Canonical Request
%% https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html#canonical-request
%% With a caveat that payload is unknown and thus unsigned, as in
%% https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
-spec create_canonical_request(Method :: binary(), URI :: binary(),
                               Queries :: #{binary() => binary()},
                               Headers :: #{binary() => binary()}) ->
                                      CanonicalRequest :: binary().
create_canonical_request(Method, URI, Queries, Headers) ->
    <<
      Method/binary, "\n",
      (uri_encode(URI, false))/binary, "\n",
      (canonical_query_string(Queries))/binary, "\n",
      (canonical_headers(Headers))/binary, "\n",
      (signed_headers(Headers))/binary, "\n",
      "UNSIGNED-PAYLOAD"
    >>.

%% Task 2: Create a String to Sign
%% https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html#request-string
-spec create_string_to_sign(UTCDateTime :: calendar:datetime(), Region :: binary(),
                            Service :: binary(), CanonicalRequest :: binary()) ->
                                   StringToSign :: binary().
create_string_to_sign(UTCDateTime, Region, Service, CanonicalRequest) ->
    <<
      "AWS4-HMAC-SHA256\n",
      (datetime_iso8601(UTCDateTime))/binary, "\n",
      (compose_scope(UTCDateTime, Region, Service))/binary, "\n",
      (hex(sha256_hash(CanonicalRequest)))/binary
    >>.

%% Task 3: Calculate Signature
%% https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html#signing-key
-spec calculate_signature(UTCDateTime :: calendar:datetime(), Region :: binary(),
                          Service :: binary(), SecretAccessKey :: binary(),
                          StringToSign :: binary()) ->
                                 Signature :: binary().
calculate_signature(UTCDateTime, Region, Service, SecretAccessKey, StringToSign) ->
    DateKey = hmac_sha256(<<"AWS4", SecretAccessKey/binary>>, date_iso8601(UTCDateTime)),
    DateRegionKey = hmac_sha256(DateKey, Region),
    DateRegionServiceKey = hmac_sha256(DateRegionKey, Service),
    SigningKey = hmac_sha256(DateRegionServiceKey, <<"aws4_request">>),
    hmac_sha256(SigningKey, StringToSign).


-spec canonical_query_string(Queries :: #{binary() => binary()}) -> binary().
canonical_query_string(Queries) ->
    EncodedQueries = [{uri_encode(Key), uri_encode(Val)} || {Key, Val} <- maps:to_list(Queries)],
    SortedQueries = lists:keysort(1, EncodedQueries),
    WithAmp = << <<Key/binary, "=", Val/binary, "&">> || {Key, Val} <- SortedQueries >>,
    binary_part(WithAmp, 0, byte_size(WithAmp) - 1).


-spec canonical_headers(Headers :: #{binary() => binary()}) -> binary().
canonical_headers(Headers) ->
    SortedHeaders = lists:keysort(1, maps:to_list(Headers)),
    << <<Key/binary, ":", Value/binary, "\n">> || {Key, Value} <- SortedHeaders >>.


-spec signed_headers(Headers :: #{binary() => binary()}) -> binary().
signed_headers(Headers) ->
    SortedHeaders = lists:sort(maps:keys(Headers)),
    WithColon = << <<Key/binary, ";">> || Key <- SortedHeaders >>,
    binary_part(WithColon, 0, byte_size(WithColon) - 1).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec uri_encode(Data :: binary(), EncodeSlash :: boolean()) -> URIEncodedData :: binary().
uri_encode(Data, EncodeSlash) ->
    << <<(uri_encode_char(C, EncodeSlash))/binary>> || <<C>> <= Data >>.


-spec uri_encode_char(C :: integer(), EncodeSlash :: boolean()) -> EncodedChar :: binary().
uri_encode_char(C, _) when C >= $A, C =< $Z -> <<C>>;
uri_encode_char(C, _) when C >= $a, C =< $z -> <<C>>;
uri_encode_char(C, _) when C >= $0, C =< $9 -> <<C>>;
uri_encode_char(C, _) when C == $_; C == $-; C == $~; C == $. -> <<C>>;
uri_encode_char($/, false) -> <<$/>>;
uri_encode_char(C, _) -> list_to_binary([$% | httpd_util:integer_to_hexlist(C)]).


-spec hex(Data :: binary()) -> HexEncoded :: binary().
hex(Data) ->
    base16:encode(Data).


-spec sha256_hash(Data :: binary()) -> Hash :: binary().
sha256_hash(Data) ->
    crypto:hash(sha256, Data).


-spec hmac_sha256(Key :: binary(), Data :: binary()) -> Hash :: binary().
hmac_sha256(Key, Data) ->
    crypto:hmac(sha256, Key, Data).
