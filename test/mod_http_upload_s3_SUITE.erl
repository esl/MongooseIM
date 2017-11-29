-module(mod_http_upload_s3_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TOKEN, <<"TOKEN">>).
-define(FILENAME, <<"filename.jpg">>).
-define(CONTENT_TYPE, <<"image/jpeg">>).
-define(SIZE, 1234).
-define(TIMESTAMP, {{1234, 5, 6}, {7, 8, 9}}).
-define(OPTS,
        [
         {s3, [
               {bucket_url, "http://bucket.s3-eu-east-25.example.com"},
               {region, "eu-east-25"},
               {access_key_id, "AKIAIAOAONIULXQGMOUA"},
               {secret_access_key, "CG5fGqG0/n6NCPJ10FylpdgRnuV52j8IZvU7BSj8"}
              ]}
        ]).

all() -> [
          creates_slot_with_given_timestamp,
          cretes_slot_with_aws_v4_auth_queries,
          signs_url_with_expected_size,
          creates_slot_with_given_expiration_time,
          signs_url_with_expected_content_type_if_given,
          provides_and_signs_acl,
          does_not_provide_acl_when_disabled,
          parses_bucket_url_with_custom_port,
          parses_unicode_bucket_url,
          parses_bucket_url_with_path,
          parse_bucket_url_with_slashful_path,
          includes_token_in_url,
          creates_get_url_to_the_resource
         ].

%% Tests

creates_slot_with_given_timestamp(_Config) ->
    Timestamp = calendar:universal_time(),
    {PutUrl, _} = create_slot(#{timestamp => Timestamp}),
    Queries = parse_url(PutUrl, queries),

    {_, BinTimestamp} = lists:keyfind(<<"X-Amz-Date">>, 1, Queries),
    ?assertEqual(Timestamp, binary_to_timestamp(BinTimestamp)),

    {_, Credential} = lists:keyfind(<<"X-Amz-Credential">>, 1, Queries),
    [_, BinDate | _] = binary:split(Credential, <<"/">>, [global]),
    {Datestamp, _} = Timestamp,
    ?assertEqual(Datestamp, binary_to_timestamp(BinDate)).

cretes_slot_with_aws_v4_auth_queries(_Config) ->
    {PutUrl, _} = create_slot(#{}),
    Queries = parse_url(PutUrl, queries),
    ?assert(lists:keymember(<<"X-Amz-Credential">>, 1, Queries)),
    ?assert(lists:keymember(<<"X-Amz-Date">>, 1, Queries)),
    ?assert(lists:keymember(<<"X-Amz-Expires">>, 1, Queries)),
    ?assert(lists:keymember(<<"X-Amz-SignedHeaders">>, 1, Queries)),
    ?assert(lists:keymember(<<"X-Amz-Signature">>, 1, Queries)),
    ?assertEqual({<<"X-Amz-Algorithm">>, <<"AWS4-HMAC-SHA256">>},
                 lists:keyfind(<<"X-Amz-Algorithm">>, 1, Queries)).

creates_slot_with_given_expiration_time(_Config) ->
    Opts = [{expiration_time, 1234} | ?OPTS],
    {PutUrl, _} = create_slot(#{opts => Opts}),
    Queries = parse_url(PutUrl, queries),
    {_, BinExpires} = lists:keyfind(<<"X-Amz-Expires">>, 1, Queries),
    ?assertEqual(1234, binary_to_integer(BinExpires)).

signs_url_with_expected_size(_Config) ->
    meck:new(aws_signature_v4, [passthrough]),
    meck:expect(aws_signature_v4, sign,
                fun
                    (_, _, _, Headers, _, _, _, _) ->
                        maps:get(<<"content-length">>, Headers, <<"noheader">>)
                end),

    {PutUrl, _} = create_slot(#{size => 4321}),
    Queries = parse_url(PutUrl, queries),
    ?assertEqual({<<"X-Amz-Signature">>, <<"4321">>},
                 lists:keyfind(<<"X-Amz-Signature">>, 1, Queries)),

    meck:unload(aws_signature_v4).

signs_url_with_expected_content_type_if_given(_Config) ->
    meck:new(aws_signature_v4, [passthrough]),
    meck:expect(aws_signature_v4, sign,
                fun
                    (_, _, _, Headers, _, _, _, _) ->
                        maps:get(<<"content-type">>, Headers, <<"noheader">>)
                end),

    {PutUrl, _} = create_slot(#{content_type => <<"content/type">>}),
    Queries = parse_url(PutUrl, queries),
    ?assertEqual({<<"X-Amz-Signature">>, <<"content/type">>},
                 lists:keyfind(<<"X-Amz-Signature">>, 1, Queries)),

    {PutUrlNoCT, _} = create_slot(#{content_type => undefined}),
    QueriesNoCT = parse_url(PutUrlNoCT, queries),
    ?assertEqual({<<"X-Amz-Signature">>, <<"noheader">>},
                 lists:keyfind(<<"X-Amz-Signature">>, 1, QueriesNoCT)),

    meck:unload(aws_signature_v4).

provides_and_signs_acl(_Config) ->
    meck:new(aws_signature_v4, [passthrough]),
    meck:expect(aws_signature_v4, sign,
                fun
                    (_, _, QS, _, _, _, _, _) ->
                        maps:get(<<"x-amz-acl">>, QS, <<"noquery">>)
                end),

    {PutUrl, _} = create_slot(#{}),
    Queries = parse_url(PutUrl, queries),
    ?assertEqual({<<"x-amz-acl">>, <<"public-read">>},
                 lists:keyfind(<<"x-amz-acl">>, 1, Queries)),

    ?assertEqual({<<"X-Amz-Signature">>, <<"public-read">>},
                 lists:keyfind(<<"X-Amz-Signature">>, 1, Queries)),

    meck:unload(aws_signature_v4).

does_not_provide_acl_when_disabled(_Config) ->
    meck:expect(aws_signature_v4, sign,
                fun
                    (_, _, QS, _, _, _, _, _) ->
                       maps:get(<<"x-amz-acl">>, QS, <<"noquery">>)
               end),

    Opts = with_s3_opts(#{add_acl => false}),
    {PutUrl, _} = create_slot(#{opts => Opts}),
    Queries = parse_url(PutUrl, queries),
    ?assertEqual(false, lists:keyfind(<<"x-amz-acl">>, 1, Queries)),
    ?assertEqual({<<"X-Amz-Signature">>, <<"noquery">>},
                 lists:keyfind(<<"X-Amz-Signature">>, 1, Queries)),

    meck:unload(aws_signature_v4).

parses_bucket_url_with_custom_port(_Config) ->
    Opts = with_s3_opts(#{bucket_url => <<"http://localhost:1234">>}),
    {PutUrl, _} = create_slot(#{opts => Opts}),
    ?assertEqual(1234, parse_url(PutUrl, port)).

parses_unicode_bucket_url(_Config) ->
    Opts = with_s3_opts(#{bucket_url => <<"http://example.com/❤☀☆☂☻♞"/utf8>>}),
    {PutUrl, _} = create_slot(#{opts => Opts}),
    ?assertMatch(<<"/❤☀☆☂☻♞"/utf8, _/binary>>, parse_url(PutUrl, path)).

parses_bucket_url_with_path(_Config) ->
    Opts = with_s3_opts(#{bucket_url => <<"http://example.com/a/path">>}),
    {PutUrl, _} = create_slot(#{opts => Opts}),
    Path = parse_url(PutUrl, path),
    ?assertMatch(<<"/a/path/", _/binary>>, Path),
    ?assertNotMatch(<<"/a/path//", _/binary>>, Path).

parse_bucket_url_with_slashful_path(_Config) ->
    Opts = with_s3_opts(#{bucket_url => <<"http://example.com/p/">>}),
    {PutUrl, _} = create_slot(#{opts => Opts}),
    Path = parse_url(PutUrl, path),
    ?assertMatch(<<"/p/", _/binary>>, Path),
    ?assertNotMatch(<<"/p//", _/binary>>, Path).

includes_token_in_url(_Config) ->
    {PutUrl, _} = create_slot(#{token => <<"1234token">>}),
    ?assertMatch(<<"/1234token/", _/binary>>, parse_url(PutUrl, path)).

creates_get_url_to_the_resource(_Config) ->
    {PutUrl, GetUrl} = create_slot(#{}),
    GetUrlSize = byte_size(GetUrl),
    ?assertMatch(<<GetUrl:GetUrlSize/binary, _/binary>>, PutUrl),
    ?assertEqual([], parse_url(GetUrl, queries)).

%% Helpers

create_slot(Args) ->
    {PutUrl, GetUrl, #{}} = mod_http_upload_s3:create_slot(
                              maps:get(timestamp, Args, ?TIMESTAMP),
                              maps:get(token, Args, ?TOKEN),
                              maps:get(filename, Args, ?FILENAME),
                              maps:get(content_type, Args, ?CONTENT_TYPE),
                              maps:get(size, Args, ?SIZE),
                              maps:get(opts, Args, ?OPTS)),
    {PutUrl, GetUrl}.

with_s3_opts(Opts) ->
    [{s3, S3Opts}] = ?OPTS,
    NewS3Opts = maps:to_list(maps:merge(maps:from_list(S3Opts), Opts)),
    [{s3, NewS3Opts}].

parse_url(URL) ->
    {ok, {Scheme, _, HostList, Port, PathList, QuerySList}} = http_uri:parse(binary_to_list(URL)),
    Host = list_to_binary(HostList),
    Path = list_to_binary(PathList),
    Queries =
        case QuerySList of
            [$? | QueryTail] -> cow_qs:parse_qs(list_to_binary(QueryTail));
            _ -> []
        end,
    #{scheme => Scheme, host => Host, path => Path, port => Port, queries => Queries}.

parse_url(URL, Element) -> maps:get(Element, parse_url(URL)).

binary_to_timestamp(<<Y:4/binary, M:2/binary, D:2/binary, "T",
                      HH:2/binary, MM:2/binary, SS:2/binary, "Z">>) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
     {binary_to_integer(HH), binary_to_integer(MM), binary_to_integer(SS)}};
binary_to_timestamp(<<Y:4/binary, M:2/binary, D:2/binary>>) ->
    {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}.
