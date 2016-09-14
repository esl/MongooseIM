-module(keystore_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [
     module_startup_no_opts,
     module_startup_read_key_from_file,
     module_startup_create_ram_key,
     module_startup_create_ram_key_of_given_size,
     module_startup_for_multiple_domains,
     module_startup_non_unique_key_ids,
     multiple_domains_one_stopped
    ].

init_per_suite(C) ->
    ok = stringprep:start(),
    {ok, _} = application:ensure_all_started(mnesia),
    C.

end_per_suite(C) -> C.

init_per_testcase(_, Config) ->
    mock_mongoose_metrics(),
    async_helper:start(Config, ejabberd_hooks, start_link, []).

end_per_testcase(module_startup_non_unique_key_ids, C) ->
    clean_after_testcase(C);
end_per_testcase(module_startup_for_multiple_domains, C) ->
    mod_keystore:stop(<<"first.com">>),
    mod_keystore:stop(<<"second.com">>),
    clean_after_testcase(C);
end_per_testcase(multiple_domains_one_stopped, C) ->
    mod_keystore:stop(<<"second.com">>),
    clean_after_testcase(C);
end_per_testcase(_CaseName, C) ->
    mod_keystore:stop(<<"localhost">>),
    clean_after_testcase(C).

clean_after_testcase(C) ->
    meck:unload(mongoose_metrics),
    async_helper:stop_all(C),
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
    ?ae([{{key_from_file, <<"localhost">>}, RawKey}],
        get_key(<<"localhost">>, key_from_file)).

module_startup_create_ram_key(Config) ->
    module_startup_create_ram_key(Config, ram_key()),
    %% then we can access the key
    [{{ram_key, <<"localhost">>}, Key}] = get_key(<<"localhost">>, ram_key),
    true = is_binary(Key).

module_startup_create_ram_key_of_given_size(Config) ->
    KeySize = 4,
    module_startup_create_ram_key(Config, sized_ram_key(KeySize)),
    %% then
    [{{ram_key, <<"localhost">>}, Key}] = get_key(<<"localhost">>, ram_key),
    true = is_binary(Key),
    KeySize = byte_size(Key).

module_startup_create_ram_key(_, ModKeystoreOpts) ->
    %% given no key
    [] = get_key(<<"localhost">>, ram_key),
    %% when keystore starts with config to generate a memory-only key
    ok = mod_keystore:start(<<"localhost">>, ModKeystoreOpts).

module_startup_for_multiple_domains(_Config) ->
    %% given
    [] = get_key(<<"first.com">>, key_from_file),
    [] = get_key(<<"second.com">>, key_from_file),
    FirstKey = <<"random-first.com-key-content">>,
    SecondKey = <<"random-second.com-key-content">>,
    {ok, FirstKeyFile} = key_at("/tmp/first.com", FirstKey),
    {ok, SecondKeyFile} = key_at("/tmp/second.com", SecondKey),
    %% when
    ok = mod_keystore:start(<<"first.com">>, key_from_file(FirstKeyFile)),
    ok = mod_keystore:start(<<"second.com">>, key_from_file(SecondKeyFile)),
    %% then
    ?ae([{{key_from_file, <<"first.com">>}, FirstKey}],
        get_key(<<"first.com">>, key_from_file)),
    ?ae([{{key_from_file, <<"second.com">>}, SecondKey}],
        get_key(<<"second.com">>, key_from_file)).

module_startup_non_unique_key_ids(_) ->
    %% given
    NonUniqueKeyIDsOpts = [{keys, [{some_key, ram},
                                   {some_key, {file, "some_key.dat"}}]}],
    %% when
    try
        mod_keystore:start(<<"localhost">>, NonUniqueKeyIDsOpts)
    %% then
    catch
        error:non_unique_key_ids -> ok
    end.

multiple_domains_one_stopped(_Config) ->
    % given
    [] = get_key(<<"first.com">>, key_from_file),
    [] = get_key(<<"second.com">>, key_from_file),
    FirstKey = <<"random-first.com-key-content">>,
    SecondKey = <<"random-second.com-key-content">>,
    {ok, FirstKeyFile} = key_at("/tmp/first.com", FirstKey),
    {ok, SecondKeyFile} = key_at("/tmp/second.com", SecondKey),
    % when
    ok = mod_keystore:start(<<"first.com">>, key_from_file(FirstKeyFile)),
    ok = mod_keystore:start(<<"second.com">>, key_from_file(SecondKeyFile)),
    ok = mod_keystore:stop(<<"first.com">>),
    % then
    ?ae([{{key_from_file, <<"second.com">>}, SecondKey}],
        get_key(<<"second.com">>, key_from_file)).

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
    meck:expect(mongoose_metrics, create_generic_hook_metric, fun (_, _) -> ok end),
    meck:expect(mongoose_metrics, increment_generic_hook_metric, fun (_, _) -> ok end),
    ok.

%% Use a function like this in your module which is a client of mod_keystore.
-spec get_key(Domain, KeyName) -> Result when
      Domain :: ejabberd:server(),
      KeyName :: mod_keystore:key_name(),
      Result :: mod_keystore:key_list().
get_key(Domain, KeyName) ->
    ejabberd_hooks:run_fold(get_key, Domain, [], [{KeyName, Domain}]).

%%{mod_keystore, [{keys, [{asdqwe_access_secret, ram},
%%                        {asdqwe_access_psk,    {file, "priv/asdqwe_access_psk"}},
%%                        {asdqwe_provision_psk, {file, "priv/asdqwe_access_psk"}}]}]},
