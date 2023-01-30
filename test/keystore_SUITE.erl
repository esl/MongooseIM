-module(keystore_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-import(config_parser_helper, [default_mod_config/1, mod_config/2]).

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [
     module_startup_no_opts,
     module_startup_read_key_from_file,
     module_startup_create_ram_key,
     module_startup_create_ram_key_of_given_size,
     module_startup_for_multiple_domains,
     multiple_domains_one_stopped
    ].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    C.

end_per_suite(C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    C.

init_per_testcase(_, Config) ->
    mock_mongoose_metrics(),
    async_helper:start(Config, gen_hook, start_link, []).

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
    {ok, _} = start(<<"localhost">>, default_mod_config(mod_keystore)).

module_startup_read_key_from_file(_) ->
    %% given
    RawKey = <<"qwe123">>,
    {ok, KeyFile} = key_at("/tmp/key-from-file", RawKey),
    %% when
    {ok, _} = start(<<"localhost">>, key_from_file(KeyFile)),
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
    {ok, _} = start(<<"localhost">>, ModKeystoreOpts).

module_startup_for_multiple_domains(_Config) ->
    %% given
    [] = get_key(<<"first.com">>, key_from_file),
    [] = get_key(<<"second.com">>, key_from_file),
    FirstKey = <<"random-first.com-key-content">>,
    SecondKey = <<"random-second.com-key-content">>,
    {ok, FirstKeyFile} = key_at("/tmp/first.com", FirstKey),
    {ok, SecondKeyFile} = key_at("/tmp/second.com", SecondKey),
    %% when
    {ok, _} = start(<<"first.com">>, key_from_file(FirstKeyFile)),
    {ok, _} = start(<<"second.com">>, key_from_file(SecondKeyFile)),
    %% then
    ?ae([{{key_from_file, <<"first.com">>}, FirstKey}],
        get_key(<<"first.com">>, key_from_file)),
    ?ae([{{key_from_file, <<"second.com">>}, SecondKey}],
        get_key(<<"second.com">>, key_from_file)).

multiple_domains_one_stopped(_Config) ->
    % given
    [] = get_key(<<"first.com">>, key_from_file),
    [] = get_key(<<"second.com">>, key_from_file),
    FirstKey = <<"random-first.com-key-content">>,
    SecondKey = <<"random-second.com-key-content">>,
    {ok, FirstKeyFile} = key_at("/tmp/first.com", FirstKey),
    {ok, SecondKeyFile} = key_at("/tmp/second.com", SecondKey),
    % when
    {ok, _} = start(<<"first.com">>, key_from_file(FirstKeyFile)),
    {ok, _} = start(<<"second.com">>, key_from_file(SecondKeyFile)),
    ok = mod_keystore:stop(<<"first.com">>),
    % then
    ?ae([{{key_from_file, <<"second.com">>}, SecondKey}],
        get_key(<<"second.com">>, key_from_file)).

%%
%% Helpers
%%

key_at(Path, Data) ->
    ok = file:write_file(Path, Data),
    {ok, Path}.

key_from_file(KeyFile) ->
    mod_config(mod_keystore, #{keys => #{key_from_file => {file, KeyFile}}}).

ram_key() ->
    mod_config(mod_keystore, #{keys => #{ram_key => ram}}).

sized_ram_key(Size) ->
    mod_config(mod_keystore, #{keys => #{ram_key => ram},
                               ram_key_size => Size}).

mock_mongoose_metrics() ->
    meck:new(mongoose_metrics, []),
    meck:expect(mongoose_metrics, create_generic_hook_metric, fun (_, _) -> ok end),
    meck:expect(mongoose_metrics, increment_generic_hook_metric, fun (_, _) -> ok end),
    ok.

%% Use a function like this in your module which is a client of mod_keystore.
-spec get_key(HostType, KeyName) -> Result when
      HostType :: mongooseim:host_type(),
      KeyName :: mod_keystore:key_name(),
      Result :: mod_keystore:key_list().
get_key(HostType, KeyName) ->
    mongoose_hooks:get_key(HostType, KeyName).

start(Hostname, Opts) ->
    mongoose_config:set_opt({modules, Hostname}, #{mod_keystore => Opts}),
    mongoose_config:set_opt(hosts, [Hostname]),
    gen_mod:start_module(Hostname, mod_keystore, Opts).
