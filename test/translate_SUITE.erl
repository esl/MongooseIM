-module(translate_SUITE).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     test_undefined_translation,
     test_english_translation,
     test_polish_translation,
     test_portuguese_translation
    ].


end_per_testcase(_, C) ->
    catch meck:unload(ejabberd_config),
    C.

test_undefined_translation(_Config) ->
    %% given - if undefined it should be english(pass through)
    given_default_language(undefined),
    given_loaded_translations(),
    %% then
    ?assertEqual(<<"undef">>, translate:translate(<<"en">>, <<"undef">>)),
    ?assertEqual(<<"undef2">>, translate:translate(<<"klingon">>, <<"undef2">>)),

    ok.

test_english_translation(_Config) ->
    %% given
    given_default_language(<<"en">>),
    given_loaded_translations(),

    %% then
    ?assertEqual(<<"cat">>, translate:translate(<<"en">>, <<"cat">>)),
    ?assertEqual(<<"dog">>, translate:translate(<<"en-us">>, <<"dog">>)),
    ?assertEqual(<<"rabbit">>, translate:translate(<<"en-br">>, <<"rabbit">>)),
    ?assertEqual(<<"kangaroo">>, translate:translate(<<"en-au">>, <<"kangaroo">>)),
    ?assertEqual(<<"wombat">>, translate:translate(<<"klingon">>, <<"wombat">>)),

    %% tear down mocks
    meck:unload(),
    ok.

test_polish_translation(_Config) ->
    %% given
    given_default_language(<<"pl">>),
    given_loaded_translations(),

    ?assertEqual(<<"Dodaj nowe">>, translate:translate(<<"pl">>, <<"Add New">>)),
    %% check if the languages in the form of en-us are handled correctly in case
    %% of other languages
    ?assertEqual(<<"Dodaj nowe">>, translate:translate(<<"pl-gr">>, <<"Add New">>)),
    %% not existing key is not translated
    ?assertEqual(<<"undef_test">>, translate:translate(<<"pl">>, <<"undef_test">>)),
    %% in case of non-existing languege it will chouuse polish tranlation
    ?assertEqual(<<"Dodaj nowe">>, translate:translate(<<"klingon">>, <<"Add New">>)),

    ok.

test_portuguese_translation(_Config)->
    %% given
    given_default_language(<<"pt">>),
    given_loaded_translations(),

    ?assertEqual(<<"Adicionar usuário"/utf8>>, translate:translate(<<"pt-br">>, <<"Add User">>)),
    %% check brasilian
    ?assertEqual(<<"Adicionar utilizador"/utf8>>, translate:translate(<<"pt">>, <<"Add User">>)),

    ok.

given_loaded_translations() ->
    translate:start().

given_default_language(Lang) ->
    (catch meck:unload()),
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_global_option, fun(language) -> Lang end),
    ok.
