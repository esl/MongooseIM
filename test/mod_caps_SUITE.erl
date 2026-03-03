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
init_per_group(_GroupName, Config) ->
    Config.

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
    [{hash, [parallel], hash_tests()},
     {module, [parallel], module_tests()}].

hash_tests() ->
    [generate_simple_hash_sha1,
     generate_complex_hash_md5,
     generate_complex_hash_sha1,
     generate_complex_hash_sha224,
     generate_complex_hash_sha256,
     generate_complex_hash_sha384,
     generate_complex_hash_sha512,
     generate_empty_hash_sha1].

module_tests() ->
    [get_set_and_delete_jid_features,
     get_set_and_delete_hash_features].

%% Test cases

generate_simple_hash_sha1(Config) ->
    test_hash_generation(
      Config, "simple_response.xml", ~"sha-1", ~"QgayPKawpkPSDYmwT/WM94uAlu0=").

generate_complex_hash_md5(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~"md5", ~"28MfZZzsz3dCwWpb8JHPBA==").

generate_complex_hash_sha1(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~"sha-1", ~"q07IKJEyjvHSyhy//CH0CxmKi8w=").

generate_complex_hash_sha224(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~"sha-224", ~"hVeEOlwa1XV+Ey5pOJflr60mWtkRVzVNkc8/FA==").

generate_complex_hash_sha256(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~"sha-256", ~"VyRoCfkwN7Q9lxZhqOI+mxfSpo/MsaCF4hBufCzfCpI=").

generate_complex_hash_sha384(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~"sha-384",
      ~"ZwpmMk+bCM0ZTwRORt/BCd+WEocOBHUmMKNeaODqb1uiUlQ19DuRNPvz9ttfv49q").

generate_complex_hash_sha512(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~"sha-512",
      ~"D2YKKKjx1pTqnV8eCvkyhkdcBe4lPrf8Rp/Ss0zmEut0XEkfTIVEk7zByVMifWpJeb9cTdufU+k47oKIkQ3UUQ==").

generate_empty_hash_sha1(_Config) ->
    EmptyHash = ~"2jmj7l5rSw0yVb/vlWAYkK/YBwk=", % hash of an empty string
    ?assertEqual(EmptyHash, mod_caps_hash:generate([], ~"sha-1")),
    EmptyFeature = #xmlel{name = ~"feature"}, % this doesn't follow XEP-0030 and should be skipped
    ?assertEqual(EmptyHash, mod_caps_hash:generate([EmptyFeature], ~"sha-1")).

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

test_hash_generation(Config, FileName, Alg, ExpectedHash) ->
    ?assertEqual(ExpectedHash, generate_hash(Config, FileName, Alg)).

generate_hash(Config, FileName, Alg) ->
    #xmlel{children = Children} = parse_response(Config, FileName),
    mod_caps_hash:generate(Children, Alg).

parse_response(Config, FileName) ->
    {ok, XML} = file:read_file(ejabberd_helper:data(Config, FileName)),
    {ok, #xmlel{children = [QueryEl]}} = exml:parse(XML),
    QueryEl.

presence(Attrs, Children) ->
    #xmlel{name = ~"presence", attrs = Attrs, children = Children}.
