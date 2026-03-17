-module(mod_caps_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").

-define(HOST_TYPE, ~"test type").

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(module, Config) ->
    mongoose_config:set_opts(opts()),
    Config1 = async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                          {gen_hook, start_link, []}]),
    mongoose_modules:start(),
    Config1;
init_per_group(hash, Config) ->
    Responses = maps:map(fun(_, File) -> parse_response(Config, File) end, response_files()),
    [{responses, Responses} | Config];
init_per_group(hash_v1, Config) ->
    [{version, v1} | Config];
init_per_group(hash_v2, Config) ->
    [{version, v2} | Config].

end_per_group(module, Config) ->
    mongoose_modules:stop(),
    async_helper:stop_all(Config),
    mongoose_config:erase_opts();
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [{group, hash},
     {group, module}].

groups() ->
    [{hash, [], [{group, hash_v1}, {group, hash_v2}]},
     {hash_v1, [parallel], hash_alg_tests() ++ special_case_tests()},
     {hash_v2, [parallel], hash_alg_tests() ++ special_case_tests()},
     {module, [parallel], module_tests()}].

hash_alg_tests() ->
    [generate_hash_md5,
     generate_hash_sha1, % this test checks all files
     generate_hash_sha224,
     generate_hash_sha256,
     generate_hash_sha384,
     generate_hash_sha512,
     generate_hash_shake128,
     generate_hash_shake256,
     generate_hash_sha3_256,
     generate_hash_sha3_512,
     generate_hash_blake2b_512].

special_case_tests() ->
    [generate_empty_hash,
     duplicate_elements,
     invalid_elements,
     invalid_forms_accepted_by_v1,
     invalid_forms_skipped_by_v1,
     invalid_forms,
     unsupported_algorithm].

module_tests() ->
    [get_set_and_delete_jid_features,
     get_set_and_delete_hash_features].

%% Test cases

generate_hash_md5(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"md5")),
    assert_hash(Config, complex2, Version, ~"md5").

-doc "This test checks all file/version combinations".
generate_hash_sha1(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"sha-1")),
    assert_hash(Config, simple, Version, ~"sha-1"),
    assert_hash(Config, simple2, Version, ~"sha-1"),
    assert_hash(Config, complex, Version, ~"sha-1"),
    assert_hash(Config, complex2, Version, ~"sha-1").

generate_hash_sha224(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"sha-224")),
    assert_hash(Config, complex2, Version, ~"sha-224").

generate_hash_sha256(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"sha-256")),
    assert_hash(Config, simple2, Version, ~"sha-256"),
    assert_hash(Config, complex2, Version, ~"sha-256").

generate_hash_sha384(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"sha-384")),
    assert_hash(Config, complex2, Version, ~"sha-384").

generate_hash_sha512(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"sha-512")),
    assert_hash(Config, complex2, Version, ~"sha-512").

generate_hash_shake128(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"shake128")),
    assert_hash(Config, complex2, Version, ~"shake128").

generate_hash_shake256(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"shake256")),
    assert_hash(Config, complex2, Version, ~"shake256").

generate_hash_sha3_256(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"sha3-256")),
    assert_hash(Config, simple2, Version, ~"sha3-256"),
    assert_hash(Config, complex2, Version, ~"sha3-256").

generate_hash_sha3_512(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"sha3-512")),
    assert_hash(Config, complex2, Version, ~"sha3-512").

generate_hash_blake2b_512(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(true, mod_caps_hash:is_alg_supported(~"blake2b-512")),
    assert_hash(Config, complex2, Version, ~"blake2b-512").

generate_empty_hash(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(empty_hash(Version), mod_caps_hash:generate([], Version, ~"sha-1")).

-doc """
XEP-0390 4.1 1.1 requires aborting on elements not adhering to XEP-0030 or XEP-0128
XEP-0115 does not specify such a rule, so v1 skips such elements
""".
invalid_elements(Config) ->
    Version = proplists:get_value(version, Config),
    #xmlel{children = Els} = get_response(Config, complex2),

    %% These elements don't follow the schema from XEP-0030
    BrokenEls = [#xmlel{name = ~"feature", attrs = #{}}, % no 'var'
                 #xmlel{name = ~"identity", attrs = #{~"name" => ~"blabber"}}, % no 'category'
                 #xmlel{name = ~"futility"}],
    F = fun() -> mod_caps_hash:generate(BrokenEls ++ Els, Version, ~"sha-1") end,
    case Version of
        v1 -> ?assertEqual(mod_caps_hash:generate(Els, v1, ~"sha-1"), F());
        v2 -> ?assertError({invalid_elements, BrokenEls}, F())
    end.

-doc """
XEP-0390 4.1 1.2 requires aborting on forms containing <reported/> or <item/>
XEP-0115 does not specify such a rule, so v1 ignores extra elements
""".
invalid_forms_accepted_by_v1(Config) ->
    Version = proplists:get_value(version, Config),
    QueryEl = #xmlel{children = Els} = get_response(Config, complex2),
    BaseForm = #xmlel{children = Fields} = exml_query:subelement(QueryEl, ~"x"),
    OtherEls = Els -- [BaseForm],
    BadForms = [BaseForm#xmlel{children = [#xmlel{name = ~"reported"} | Fields]},
                BaseForm#xmlel{children = [#xmlel{name = ~"item"} | Fields]}],
    Hash = mod_caps_hash:generate(Els, Version, ~"sha-1"),
    F = fun(Form) -> mod_caps_hash:generate([Form | OtherEls], Version, ~"sha-1") end,
    [case Version of
         v1 -> ?assertEqual(Hash, F(Form));
         v2 -> ?assertError({invalid_elements, [Form]}, F(Form))
     end || Form <- BadForms],
    ok.

-doc """
XEP-0390 4.1 1.3 requires aborting forms not adhering to the FORM_TYPE protocol (XEP-0068)
XEP-0115 5.4 3.f requires skipping forms with non-hidden/missing FORM_TYPE
""".
invalid_forms_skipped_by_v1(Config) ->
    Version = proplists:get_value(version, Config),
    QueryEl = #xmlel{children = Els} = get_response(Config, complex2),
    BaseForm = #xmlel{children = Fields} = exml_query:subelement(QueryEl, ~"x"),
    OtherEls = Els -- [BaseForm],
    BadForms = [exml:remove_attr(BaseForm, ~"xmlns"),
                exml:remove_attr(BaseForm, ~"type"),
                BaseForm#xmlel{children = lists:map(fun unhide_form_type/1, Fields)}],
    Hash = mod_caps_hash:generate(OtherEls, Version, ~"sha-1"),
    F = fun(Form) -> mod_caps_hash:generate([Form | OtherEls], Version, ~"sha-1") end,
    [case Version of
         v1 -> ?assertEqual(Hash, F(Form));
         v2 -> ?assertError({invalid_elements, [Form]}, F(Form))
     end || Form <- BadForms],
    ok.

-doc """
XEP-0390 4.1 1.3 requires aborting on forms not adhering to the FORM_TYPE protocol (XEP-0068)
XEP-0115 5.4 3.e requires aborting on forms with multiple values of FORM_TYPE
""".
invalid_forms(Config) ->
    Version = proplists:get_value(version, Config),
    QueryEl = #xmlel{children = Els} = get_response(Config, complex2),
    BaseForm = #xmlel{children = Fields} = exml_query:subelement(QueryEl, ~"x"),
    OtherEls = Els -- [BaseForm],
    Form = BaseForm#xmlel{children = lists:map(fun add_form_type_value/1, Fields)},
    ?assertError({invalid_elements, [Form]},
                 mod_caps_hash:generate([Form | OtherEls], Version, ~"sha-1")).

-doc """
XEP-0115 5.4 3.c/d requires aborting on any duplicate identity/feature
             3.e requires aborting or more than one form with the same FORM_TYPE
XEP-0390 does not specify this explicitly, but the same rule is applied as duplicate elements
         don't make sense in disco#info responses, and the code is more consistent this way
""".
duplicate_elements(Config) ->
    Version = proplists:get_value(version, Config),
    QueryEl = #xmlel{children = Els} = get_response(Config, complex2),

    Feature = exml_query:subelement(QueryEl, ~"feature"),
    ?assertError({duplicate_elements, [_]},
                 mod_caps_hash:generate(Els ++ [Feature], Version, ~"sha-256")),

    Identity = exml_query:subelement(QueryEl, ~"identity"),
    ?assertError({duplicate_elements, [_]},
                 mod_caps_hash:generate(Els ++ [Identity], Version, ~"sha-256")),

    Form = exml_query:subelement(QueryEl, ~"x"),
    ?assertError({duplicate_elements, [_]},
                 mod_caps_hash:generate(Els ++ [Form], Version, ~"sha-256")),

    Form1 = exml:filter_children(Form, fun(Field) -> exml_query:attr(Field, ~"var") =/= ~"os" end),
    ?assertError({duplicate_elements, [_]},
                 mod_caps_hash:generate(Els ++ [Form1], Version, ~"sha-256")).

unsupported_algorithm(Config) ->
    Version = proplists:get_value(version, Config),
    ?assertEqual(false, mod_caps_hash:is_alg_supported(~"sha3-224")),
    ?assertError({badmatch, {error, unknown_alg}},
                 generate_hash(Config, simple2, Version, ~"sha3-224")).

get_set_and_delete_jid_features(_Config) ->
    Alice1 = jid:from_binary_noprep(~"alice@domain/res1"),
    Alice2 = jid:from_binary_noprep(~"alice@domain/res2"),
    Bob = jid:from_binary_noprep(~"bob@domain/res1"),
    Features1 = [~"feature1", ~"feature2"],
    Features2 = [~"feature2"],

    %% get
    ?assertEqual([], mod_caps:get_features(?HOST_TYPE, Alice1)),
    ?assertEqual([], mod_caps:get_resources_with_features(?HOST_TYPE, jid:to_bare(Alice1))),

    %% set + get
    ?assertEqual(ok, mod_caps_backend:set_features(?HOST_TYPE, Alice1, Features1)),
    ?assertEqual(ok, mod_caps_backend:set_features(?HOST_TYPE, Alice2, Features2)),
    ?assertEqual(ok, mod_caps_backend:set_features(?HOST_TYPE, Bob, Features2)),
    ?assertEqual(Features1, mod_caps:get_features(?HOST_TYPE, Alice1)),
    ?assertEqual(Features2, mod_caps:get_features(?HOST_TYPE, Alice2)),
    ?assertEqual(Features2, mod_caps:get_features(?HOST_TYPE, Bob)),
    ?assertEqual([{~"res1", Features1}, {~"res2", Features2}],
                 mod_caps:get_resources_with_features(?HOST_TYPE, jid:to_bare(Alice1))),
    ?assertEqual([{~"res1", Features2}],
                 mod_caps:get_resources_with_features(?HOST_TYPE, jid:to_bare(Bob))),

    %% delete + get
    ?assertEqual(ok, mod_caps_backend:delete_features(?HOST_TYPE, Alice1)),
    ?assertEqual(ok, mod_caps_backend:delete_features(?HOST_TYPE, Bob)),
    ?assertEqual([{~"res2", Features2}],
                 mod_caps:get_resources_with_features(?HOST_TYPE, jid:to_bare(Alice1))),
    ?assertEqual([], mod_caps:get_resources_with_features(?HOST_TYPE, jid:to_bare(Bob))).

get_set_and_delete_hash_features(_Config) ->
    Hash1 = {~"alg1", ~"value1"},
    Hash2 = {~"alg2", ~"value2"},
    Features1 = [~"feature1", ~"feature2"],
    Features2 = [~"feature2"],

    %% get
    ?assertEqual({error, not_found}, mod_caps_backend:get_hash_features(?HOST_TYPE, Hash1)),

    %% set + get
    ?assertEqual(ok, mod_caps_backend:set_hash_features(?HOST_TYPE, Hash1, Features1)),
    ?assertEqual(ok, mod_caps_backend:set_hash_features(?HOST_TYPE, Hash2, Features2)),
    ?assertEqual({ok, Features1}, mod_caps_backend:get_hash_features(?HOST_TYPE, Hash1)),

    %% delete + get
    ?assertEqual(ok, mod_caps_backend:delete_hash_features(?HOST_TYPE, Hash1)),
    ?assertEqual({error, not_found}, mod_caps_backend:get_hash_features(?HOST_TYPE, Hash1)),
    ?assertEqual({ok, Features2}, mod_caps_backend:get_hash_features(?HOST_TYPE, Hash2)).

%% Helpers

opts() ->
    #{hosts => [],
      host_types => [?HOST_TYPE],
      default_server_domain => ~"localhost",
      language => ~"en",
      instrumentation => config_parser_helper:default_config([instrumentation]),
      {modules, ?HOST_TYPE} => config_parser_helper:config([modules], #{mod_caps => #{}})}.

generate_hash(Config, FileKey, Version, Alg) ->
    #xmlel{children = Children} = get_response(Config, FileKey),
    mod_caps_hash:generate(Children, Version, Alg).

get_response(Config, FileKey) ->
    #{FileKey := Response} = proplists:get_value(responses, Config),
    Response.

parse_response(Config, FileName) ->
    {ok, XML} = file:read_file(ejabberd_helper:data(Config, FileName)),
    {ok, QueryEl} = exml:parse(XML),
    QueryEl.

presence(Attrs, Children) ->
    #xmlel{name = ~"presence", attrs = Attrs, children = Children}.

unhide_form_type(Field = #xmlel{name = ~"field", attrs = #{~"var" := ~"FORM_TYPE"}}) ->
    exml:remove_attr(Field, ~"type");
unhide_form_type(Field) ->
    Field.

add_form_type_value(Field = #xmlel{name = ~"field", attrs = #{~"var" := ~"FORM_TYPE"}}) ->
    exml:append_children(Field, [#xmlel{name = ~"value",
                                        children = [#xmlcdata{content = ~"bad"}]}]);
add_form_type_value(Field) ->
    Field.

response_files() ->
    #{simple => "simple_response.xml",
      simple2 => "simple_response_2.xml",
      complex => "complex_response.xml",
      complex2 => "complex_response_2.xml"}.

assert_hash(Config, FileKey, Version, Alg) ->
    ?assertEqual(hash(FileKey, Version, Alg), generate_hash(Config, FileKey, Version, Alg)).

%% XEP-0115 examples (v1): https://xmpp.org/extensions/xep-0115.html
hash(simple, v1, ~"sha-1") -> ~"QgayPKawpkPSDYmwT/WM94uAlu0=";
hash(complex, v1, ~"sha-1") -> ~"q07IKJEyjvHSyhy//CH0CxmKi8w=";

%% XEP-0390 examples (v2): https://xmpp.org/extensions/xep-0390.html#algorithm-example
hash(simple2, v2, ~"sha-256") -> ~"kzBZbkqJ3ADrj7v08reD1qcWUwNGHaidNUgD7nHpiw8=";
hash(complex2, v2, ~"sha-256") -> ~"u79ZroNJbdSWhdSp311mddz44oHHPsEBntQ5b1jqBSY=";
hash(simple2, v2, ~"sha3-256") -> ~"79mdYAfU9rEdTOcWDO7UEAt6E56SUzk/g6TnqUeuD9Q=";
hash(complex2, v2, ~"sha3-256") -> ~"XpUJzLAc93258sMECZ3FJpebkzuyNXDzRNwQog8eycg=";

%% Precomputed by: mod_caps_hash:encode(Version, Children) and then
%%   openssl dgst -<alg> -binary | openssl base64 -A
%% For SHAKE: openssl dgst -shake128 -xoflen 32 / -shake256 -xoflen 64
hash(complex2, v1, ~"md5") -> ~"QLPiimcy6MD9yjkTTcD6ng==";
hash(complex2, v2, ~"md5") -> ~"i4jAgF/hAqE2lqmc0+V43Q==";
hash(simple2, v1, ~"sha-1") -> ~"GRREviyyjLzK2wK4QLX5NNF9FmQ=";
hash(complex2, v1, ~"sha-1") -> ~"cePxJUNNZuDoNDbCMqs2VNEcJeY=";
hash(simple, v2, ~"sha-1") -> ~"utogC9qlkongH5tx05hvJ22ioWQ=";
hash(simple2, v2, ~"sha-1") -> ~"zkwogI8zTfQzkDxVOTYYX6IA80g=";
hash(complex, v2, ~"sha-1") -> ~"H5wcqqkaf95ZGaO9mfEah16hooA=";
hash(complex2, v2, ~"sha-1") -> ~"aMMr2Ibe1aN4cS0aa62sTohLVfQ=";
hash(complex2, v1, ~"sha-224") -> ~"ovBBM00BK4+9e/4WsjvQFZMoAmmT8suN9l1VAw==";
hash(complex2, v2, ~"sha-224") -> ~"grFEaVAB0VyL8hZsPRfoAojl5+sFJW34hJx7Hw==";
hash(simple2, v1, ~"sha-256") -> ~"A+o8Orx0KgVcW+7NuFZFjRHlY5owcon1c92ENpkLJ00=";
hash(complex2, v1, ~"sha-256") -> ~"U1s9Z5JSeF5FinatM8JzroaiBowuKMzQU/v5VG4NAYE=";
hash(complex2, v1, ~"sha-384") ->
    ~"DK24d06J1y/R6p/CpE4WYMx8HqZ5EUdOBWsr99H1lhTp0HyQSpf6XhYESDZ7sFPU";
hash(complex2, v2, ~"sha-384") ->
    ~"RI4e5naUlOsGS4QN/GCEiZFqUCuYqpSLShyOzV+3AT4F1DU6+xxwMriwoVPgXlax";
hash(complex2, v1, ~"sha-512") ->
    ~"kfYDC9DohECqZqmBvxjKyQndWLvA1HUP0qqA5FzP/z4kYqNaLWER3Ml1eSQY4hHdFpsdmaMAApvQslXlAiqHCg==";
hash(complex2, v2, ~"sha-512") ->
    ~"wIbFhIiq0e6IDudjhlAhnkQ/lCWpdDl5srNSBeog88oAJ5L6QzujTzNTskPuYmUNEgCaJLq0rvKgbL1ufVfEzw==";
hash(complex2, v1, ~"shake128") -> ~"uscTWqzYSqOGP5zstCYtS+e0lVtKWpKwZckXYSeT4DY=";
hash(complex2, v2, ~"shake128") -> ~"S4Qi8OIiphus44kbH6mT+Yl/DGWTT5V6h6Zvuv0WZCU=";
hash(complex2, v1, ~"shake256") ->
    ~"MfwgAzTVHT86HVBhhdU/XYvlKp+rRctRT1uxZkXLpCBcrlV72tkH7134/OldWUKk5Dm3InebmivPlMT2tJyQ9A==";
hash(complex2, v2, ~"shake256") ->
    ~"rKS2cO8tPD2AfHOA35oBLb5tUV+3kulBjGQbamOUXeEFuLQyrMKGIcW8j46ws3mLAqPG1dDcuO0Nd8Z3tnq6tg==";
hash(simple2, v1, ~"sha3-256") -> ~"Q9fh1OmwzpEIf+Wg1T59306eTy1m8aAWPfrZRVw+i1A=";
hash(complex2, v1, ~"sha3-256") -> ~"O3suP+/FfEB+bmsVDRvQAZOoTxmMv4gch+6WieVrtYs=";
hash(complex2, v1, ~"sha3-512") ->
    ~"9hm/VTruOAlAb4IPMKUNK3ykhQ0udXq8WgG2xVwi7c7D1xjnwE3JumNQWZR7PrSJxHNSChGlbZAu+RpfFvnKdg==";
hash(complex2, v2, ~"sha3-512") ->
    ~"8NpB8tVC37s8baJng+PChUHPjB0DEIKJJtei35JYfQsaSw4lY9e0JQ+S8Qgvc2hgNOxbtm4cIX9VV1O+iU67Ug==";
hash(complex2, v1, ~"blake2b-512") ->
    ~"JT+PpgNFzxvEVDqKS/IqAuQ1C4+NDKR7gmSdSbO4gz7nUpGX07YOmLEYxxgfDtXPDZJPTBE+D8CJZKunhtr/Qw==";
hash(complex2, v2, ~"blake2b-512") ->
    ~"2luBJJE760PpkKFBfQznLjNIVIfEls0dUS3tQnHknvaOhmzY7hA0NX8OOSgqCRl6hzuwEhAru4A5pSh6ZsOhLg==".

empty_hash(v1) -> ~"2jmj7l5rSw0yVb/vlWAYkK/YBwk="; % hash of an empty string
empty_hash(v2) -> ~"pYjuVoBDlg976gbNTdOYXMw0xBI=". % hash of '\x1c\x1c\x1c'
