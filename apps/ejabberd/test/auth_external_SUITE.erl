-module(auth_external_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, no_cache}].

groups() ->
    [{no_cache, [], all_tests()}].

all_tests() ->
    [try_register].

init_per_suite(C) ->
    application:start(p1_stringprep),
    C.

end_per_suite(C) ->
    application:stop(p1_stringprep),
    C.

init_per_group(G, Config) ->
    setup_meck(G, Config),
    ejabberd_auth_external:start(domain()),
    Config.

end_per_group(G, Config) ->
    ejabberd_auth_external:stop(domain()),
    unload_meck(G),
    Config.

try_register(_C) ->
    ok.


domain() ->
    <<"mim1.esl.com">>.


setup_meck(_G, Config) ->
    DataDir = ?config(data_dir, Config),
    ct:print("~p", [DataDir]),
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_local_option,
                fun(auth_opts, _Host) ->
                        [{extauth_program, DataDir ++ "sample_external_auth.py"}]
                end),
    meck:expect(ejabberd_config, get_local_option,
                fun({extauth_instances, _Host}) -> undefined;
                   ({extauth_cache, _Host}) -> undefined
                end).



unload_meck(_G) ->
    meck:unload(ejabberd_config).
