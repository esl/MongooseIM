-module(keystore_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

all() ->
    [
     module_startup_no_opts,
     module_startup_read_key_from_file,
     module_startup_create_ram_key,
     module_startup_create_ram_key_of_given_size
    ].

init_per_suite(C) ->
    stringprep:start(),
    xml:start(),
    {ok, _} = application:ensure_all_started(mnesia),
    C.

init_per_testcase(_, Config) ->
    mock_mongoose_metrics(),
    {ok, P} = start_async(ejabberd_hooks, start_link, []),
    %ct:pal("t2l hooks: ~p", [ets:tab2list(hooks)]),
    Helpers = [P | proplists:get_value(async_helpers, Config, [])],
    lists:keystore(async_helpers, 1, Config, {async_helpers, Helpers}).

end_per_testcase(_, C) ->
    meck:unload(mongoose_metrics),
    ok = mod_keystore:stop(<<"localhost">>),
    [ P ! stop || P <- proplists:get_value(async_helpers, C, []) ],
    mnesia:delete_table(key),
    C.

%%
%% Tests
%%

module_startup_no_opts(_) ->
    ok = mod_keystore:start(<<"localhost">>, []).

module_startup_read_key_from_file(_) ->
    %% given
    RawKey = <<"qwe123">>,
    {ok, KeyFile} = key_at("/tmp/key-from-file", RawKey),
    %% when
    ok = mod_keystore:start(<<"localhost">>, key_from_file(KeyFile)),
    %% then
    [{key_from_file, RawKey}] = ejabberd_hooks:run_fold(get_key, <<"localhost">>,
                                                        [], [key_from_file]).

module_startup_create_ram_key(Config) ->
    module_startup_create_ram_key(Config, ram_key()),
    %% then we can access the key
    [{ram_key, Key}] = ejabberd_hooks:run_fold(get_key, <<"localhost">>, [], [ram_key]),
    true = is_binary(Key).

module_startup_create_ram_key_of_given_size(Config) ->
    KeySize = 4,
    module_startup_create_ram_key(Config, sized_ram_key(KeySize)),
    %% then
    [{ram_key, Key}] = ejabberd_hooks:run_fold(get_key, <<"localhost">>, [], [ram_key]),
    true = is_binary(Key),
    KeySize = byte_size(Key).

module_startup_create_ram_key(_, ModKeystoreOpts) ->
    %% given no key
    [] = ejabberd_hooks:run_fold(get_key, <<"localhost">>, [], [ram_key]),
    %% when keystore starts with config to generate a memory-only key
    ok = mod_keystore:start(<<"localhost">>, ModKeystoreOpts).

%%
%% Helpers
%%

start_async(M, F, A) ->
    Self = self(),
    P = spawn(fun () ->
                      erlang:apply(M, F, A),
                      Self ! started,
                      helper_loop()
              end),
    receive
        started ->
            %ct:pal("started", []),
            {ok, P}
        after timer:seconds(1) ->
            ct:fail("async start timeout")
    end.

helper_loop() ->
    receive
        stop -> exit(normal);
        _    -> helper_loop()
    end.

key_at(Path, Data) ->
    ok = file:write_file(Path, Data),
    {ok, Path}.

key_from_file(KeyFile) ->
    [{keys, [{key_from_file, {file, KeyFile}}]}].

ram_key() ->
    [{keys, [{ram_key, ram}]}].

sized_ram_key(Size) ->
    [{keys, [{ram_key, ram}]},
     {ram_key_size, Size}].

mock_mongoose_metrics() ->
    meck:new(mongoose_metrics, []),
    meck:expect(mongoose_metrics, ensure_metric, fun (_, _) -> ok end),
    meck:expect(mongoose_metrics, create_generic_hook_metric, fun (_, _) -> ok end),
    meck:expect(mongoose_metrics, increment_generic_hook_metric, fun (_, _) -> ok end),
    ok.

%%{mod_keystore, [{keys, [{asdqwe_access_secret, ram},
%%                        {asdqwe_access_psk,    {file, "priv/asdqwe_access_psk"}},
%%                        {asdqwe_provision_psk, {file, "priv/asdqwe_access_psk"}}]}]},
