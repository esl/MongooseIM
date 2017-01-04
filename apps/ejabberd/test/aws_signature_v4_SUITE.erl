-module(aws_signature_v4_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ACCESS_KEY_ID, "AKIAIAOAONIULXQGMOUA").
-define(SECRET_ACCESS_KEY, "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").
-define(REGION, "eu-west-1").
-define(HOST, "s3-eu-west-1.amazonaws.com").
-define(DATE, {{2014, 1, 2}, {0, 0, 0}}).
-define(BUCKET, "testbucket").
-define(EXPIRATION_TIME, 3600).

all() -> [
          signs_basic_url,
          signs_urls_with_additional_headers,
          handles_unicode_path,
          formats_datetime_timestamp,
          pads_datetime_timestamp,
          formats_date_timestamp,
          pads_date_timestamp,
          composes_scope_from_values,
          does_not_encode_unreserved_set,
          does_encode_reserved_set,
          does_encode_utf8_characters
         ].

%% Tests

%% sign

signs_basic_url(Config) ->
    Path = <<"/", ?BUCKET, "/test/key">>,
    Signature = sign(Path, ?config(queries, Config), ?config(headers, Config)),
    ?assertEqual(<<"85bf825e9b67d7838f354b74d97253317584919d8a77b7b50675c2005f003f1f">>,
                 Signature).

signs_urls_with_additional_headers(Config) ->
    Headers = maps:put(<<"content-length">>, <<"12">>, ?config(headers, Config)),
    Queries = queries(Headers, ?DATE, ?EXPIRATION_TIME),
    Path = <<"/", ?BUCKET, "/test/key">>,
    Signature = sign(Path, Queries, Headers),
    ?assertEqual(<<"0ac4c8569e7a9e50af1a34db59678b4f23b98ff9734719b2727fe2e971626d72">>,
                 Signature).

handles_unicode_path(Config) ->
    Datetime = {{2017, 1, 3}, {14, 47, 32}},
    Queries = queries(headers(), Datetime, 900),
    Path = <<"/♠♣♥♦/test/key"/utf8>>,
    Signature = sign(Path, Queries, ?config(headers, Config), Datetime),
    ?assertEqual(<<"384a2f2a8f9f799abdd633971e48f9e100236d6be25a66513b59ea9dea43e76b">>,
                 Signature).

%% datetime_iso8601

formats_datetime_timestamp(_Config) ->
    Timestamp = {{2016, 12, 10}, {21, 13, 20}},
    ?assertEqual(<<"20161210T211320Z">>, aws_signature_v4:datetime_iso8601(Timestamp)).

pads_datetime_timestamp(_Config) ->
    Timestamp = {{2016, 2, 1}, {1, 3, 0}},
    ?assertEqual(<<"20160201T010300Z">>, aws_signature_v4:datetime_iso8601(Timestamp)).

%% date_iso8601

formats_date_timestamp(_Config) ->
    Timestamp = {{1234, 12, 31}, {0, 0, 0}},
    ?assertEqual(<<"12341231">>, aws_signature_v4:date_iso8601(Timestamp)).

pads_date_timestamp(_Config) ->
    Timestamp = {{1234, 2, 3}, {0, 0, 0}},
    ?assertEqual(<<"12340203">>, aws_signature_v4:date_iso8601(Timestamp)).

%% compose_scope

composes_scope_from_values(_Config) ->
    Timestamp = {{1234, 2, 3}, {0, 0, 0}},
    Scope = aws_signature_v4:compose_scope(Timestamp, <<"Region">>, <<"Service">>),
    ?assertEqual(<<"12340203/Region/Service/aws4_request">>, Scope).

%% uri_encode

does_not_encode_unreserved_set(_Config) ->
    Uncoded = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~~">>,
    ?assertEqual(Uncoded, aws_signature_v4:uri_encode(Uncoded)).

does_encode_reserved_set(_Config) ->
    Uncoded = <<"!*'();:@&=+$,/?%#[] ">>,
    Encoded = <<"%21%2A%27%28%29%3B%3A%40%26%3D%2B%24%2C%2F%3F%25%23%5B%5D%20">>,
    ?assertEqual(Encoded, aws_signature_v4:uri_encode(Uncoded)).

does_encode_utf8_characters(_Config) ->
    Uncoded = <<"zażółć gęślą jaźń"/utf8>>,
    Encoded = <<"za%C5%BC%C3%B3%C5%82%C4%87%20g%C4%99%C5%9Bl%C4%85%20ja%C5%BA%C5%84">>,
    ?assertEqual(Encoded, aws_signature_v4:uri_encode(Uncoded)).

%% Fixtures

init_per_testcase(_, Config) ->
    [{headers, headers()}, {queries, queries(headers(), ?DATE, ?EXPIRATION_TIME)} | Config].

%% Helpers

headers() ->
    #{<<"host">> => <<?HOST>>}.

queries(Headers, UTCDateTime, ExpirationTime) ->
    Scope = aws_signature_v4:compose_scope(UTCDateTime, <<?REGION>>, <<"s3">>),
    #{
       <<"X-Amz-Algorithm">> => <<"AWS4-HMAC-SHA256">>,
       <<"X-Amz-Credential">> => <<?ACCESS_KEY_ID, "/", Scope/binary>>,
       <<"X-Amz-Date">> => aws_signature_v4:datetime_iso8601(UTCDateTime),
       <<"X-Amz-Expires">> => integer_to_binary(ExpirationTime),
       <<"X-Amz-SignedHeaders">> => join_headers(Headers)
     }.

join_headers(Headers) ->
    maps:fold(
      fun
          (Key, _, <<>>) -> Key;
          (Key, _, Acc) -> <<Acc/binary, ";", Key/binary>>
      end, <<>>, Headers).

sign(URI, Queries, Headers) ->
    sign(URI, Queries, Headers, ?DATE).

sign(URI, Queries, Headers, UTCDateTime) ->
    aws_signature_v4:sign(<<"PUT">>, URI, Queries, Headers, UTCDateTime,
                          <<?REGION>>, <<"s3">>, <<?SECRET_ACCESS_KEY>>).
