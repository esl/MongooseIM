-module(mod_caps_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("stdlib/include/assert.hrl").

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [{group, hash}].

groups() ->
    [{hash, [], hash_tests()}].

hash_tests() ->
    [generate_simple_hash_sha1,
     generate_complex_hash_md5,
     generate_complex_hash_sha1,
     generate_complex_hash_sha224,
     generate_complex_hash_sha256,
     generate_complex_hash_sha384,
     generate_complex_hash_sha512].

%% Test cases

generate_simple_hash_sha1(Config) ->
    test_hash_generation(
      Config, "simple_response.xml", ~b"sha-1", ~b"QgayPKawpkPSDYmwT/WM94uAlu0=").

generate_complex_hash_md5(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~b"md5", ~b"28MfZZzsz3dCwWpb8JHPBA==").

generate_complex_hash_sha1(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~b"sha-1", ~b"q07IKJEyjvHSyhy//CH0CxmKi8w=").

generate_complex_hash_sha224(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~b"sha-224", ~b"hVeEOlwa1XV+Ey5pOJflr60mWtkRVzVNkc8/FA==").

generate_complex_hash_sha256(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~b"sha-256",
      ~b"VyRoCfkwN7Q9lxZhqOI+mxfSpo/MsaCF4hBufCzfCpI=").

generate_complex_hash_sha384(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~b"sha-384",
      ~b"ZwpmMk+bCM0ZTwRORt/BCd+WEocOBHUmMKNeaODqb1uiUlQ19DuRNPvz9ttfv49q").

generate_complex_hash_sha512(Config) ->
    test_hash_generation(
      Config, "complex_response.xml", ~b"sha-512",
      ~b"D2YKKKjx1pTqnV8eCvkyhkdcBe4lPrf8Rp/Ss0zmEut0XEkfTIVEk7zByVMifWpJeb9cTdufU+k47oKIkQ3UUQ==").

%% Helpers

test_hash_generation(Config, FileName, Alg, ExpectedHash) ->
    {ok, XML} = file:read_file(ejabberd_helper:data(Config, FileName)),
    {ok, #xmlel{children = [#xmlel{children = Children}]}} = exml:parse(XML),
    ct:pal("Children: ~p", [Children]),
    Caps = mod_caps_hash:encode(Children),
    ct:pal("Encoded caps: ~p", [Caps]),
    Hash = mod_caps_hash:generate(Children, Alg),
    ?assertEqual(ExpectedHash, Hash).
