-module(translate_SUITE).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     test_english_translation,
     test_polish_translation,
     test_portuguese_translation
    ].

end_per_testcase(_, _C) ->
    mongoose_config:erase_opts().

test_english_translation(_Config) ->
    %% given
    given_default_language(<<"en">>),
    given_loaded_translations(),

    %% then
    ?assertEqual(<<"cat">>, service_translations:do(<<"en">>, <<"cat">>)),
    ?assertEqual(<<"dog">>, service_translations:do(<<"en-us">>, <<"dog">>)),
    ?assertEqual(<<"rabbit">>, service_translations:do(<<"en-br">>, <<"rabbit">>)),
    ?assertEqual(<<"kangaroo">>, service_translations:do(<<"en-au">>, <<"kangaroo">>)),
    ?assertEqual(<<"wombat">>, service_translations:do(<<"klingon">>, <<"wombat">>)),

    %% tear down mocks
    meck:unload(),
    ok.

test_polish_translation(_Config) ->
    %% given
    given_default_language(<<"pl">>),
    given_loaded_translations(),

    ?assertEqual(<<"Dodaj nowe">>, service_translations:do(<<"pl">>, <<"Add New">>)),
    %% check if the languages in the form of en-us are handled correctly in case
    %% of other languages
    ?assertEqual(<<"Dodaj nowe">>, service_translations:do(<<"pl-gr">>, <<"Add New">>)),
    %% not existing key is not translated
    ?assertEqual(<<"undef_test">>, service_translations:do(<<"pl">>, <<"undef_test">>)),
    %% in case of non-existing languege it will chouuse polish tranlation
    ?assertEqual(<<"Dodaj nowe">>, service_translations:do(<<"klingon">>, <<"Add New">>)),

    ok.

test_portuguese_translation(_Config)->
    %% given
    given_default_language(<<"pt">>),
    given_loaded_translations(),

    ?assertEqual(<<"Adicionar usuário"/utf8>>, service_translations:do(<<"pt-br">>, <<"Add User">>)),
    %% check brasilian
    ?assertEqual(<<"Adicionar utilizador"/utf8>>, service_translations:do(<<"pt">>, <<"Add User">>)),

    ok.

given_loaded_translations() ->
    service_translations:start(#{}).

given_default_language(Language) ->
    mongoose_config:set_opts(#{language => Language,
                               services => #{service_translations => #{}}}).
