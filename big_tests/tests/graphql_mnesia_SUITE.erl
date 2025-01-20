-module(graphql_mnesia_SUITE).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1, mim/0, mim2/0, rpc/4]).
-import(domain_helper, [host_type/1]).
-import(mongooseimctl_helper, [rpc_call/3]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, user_to_bin/1,
                         get_ok_value/2, get_err_code/1, get_err_value/2, get_unauthorized/1,
                         get_coercion_err_msg/1, get_not_loaded/1]).

-record(mnesia_table_test, {key :: integer(), name :: binary()}).
-record(vcard, {us, vcard}).

all() ->
    [{group, admin_mnesia_cli},
     {group, admin_mnesia_http},
     {group, domain_admin_mnesia},
     {group, mnesia_not_configured}].

groups() ->
    [{admin_mnesia_http, [sequence], admin_mnesia_tests()},
     {admin_mnesia_cli, [sequence], admin_mnesia_tests()},
     {domain_admin_mnesia, [], domain_admin_tests()},
     {mnesia_not_configured, [parallel], mnesia_not_configured_tests()}].

admin_mnesia_tests() ->
    [dump_mnesia_table_test,
     dump_mnesia_table_file_error_test,
     dump_mnesia_table_no_table_error_test,
     dump_mnesia_test,
     dump_mnesia_file_error_test,
     backup_and_restore_test,
     backup_wrong_filename_test,
     backup_wrong_path_test,
     restore_no_file_test,
     restore_wrong_file_format_test,
     restore_bad_file_test,
     restore_bad_path_test,
     load_mnesia_test,
     load_mnesia_no_file_test,
     load_mnesia_bad_file_test,
     load_mnesia_bad_file2_test,
     change_nodename_test,
     change_nodename_bad_name,
     change_nodename_empty_name,
     change_nodename_no_file_error_test,
     change_nodename_bad_file_error_test,
     get_info_test,
     get_all_info_test,
     install_fallback_error_test,
     set_master_test,
     set_master_self_test,
     set_master_bad_name_test,
     set_master_empty_name_test].

domain_admin_tests() ->
    [domain_admin_dump_mnesia_table_test,
     domain_admin_dump_mnesia_test,
     domain_admin_backup_test,
     domain_admin_restore_test,
     domain_admin_load_mnesia_test,
     domain_admin_change_nodename_test,
     domain_admin_install_fallback_test,
     domain_admin_set_master_test,
     domain_admin_get_info_test].

mnesia_not_configured_tests() ->
    [backup_not_configured_test,
     change_nodename_not_configured_test,
     dump_not_configured_test,
     dump_table_not_configured_test,
     install_fallback_not_configured_test,
     load_not_configured_test,
     restore_not_configured_test,
     set_master_not_configured_test,
     system_info_not_configured_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    Config1 = escalus:init_per_suite(Config),
    ejabberd_node_utils:init(mim(), Config1).

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

init_per_group(admin_mnesia_http, Config) ->
    Config1 = graphql_helper:init_admin_handler(Config),
    skip_if_mnesia_not_configured(Config1);
init_per_group(admin_mnesia_cli, Config) ->
    Config1 = graphql_helper:init_admin_cli(Config),
    skip_if_mnesia_not_configured(Config1);
init_per_group(domain_admin_mnesia, Config) ->
    Config1 = graphql_helper:init_domain_admin_handler(Config),
    skip_if_mnesia_not_configured(Config1);
init_per_group(mnesia_not_configured, Config) ->
    case rpc_call(mongoose_config, lookup_opt, [[internal_databases, mnesia]]) of
        {error, not_found} ->
            graphql_helper:init_admin_handler(Config);
        {ok, _} ->
            {skip, "Mnesia is configured"}
    end.
end_per_group(_, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

skip_if_mnesia_not_configured(Config) ->
    case rpc_call(mongoose_config, lookup_opt, [[internal_databases, mnesia]]) of
        {error, not_found} ->
            {skip, "Mnesia is not configured"};
        {ok, _} ->
            Config
    end.

% Admin tests

dump_mnesia_table_test(Config) ->
    Filename = <<"dump_mnesia_table_test">>,
    create_mnesia_table_and_write([{attributes, record_info(fields, mnesia_table_test)}]),
    Res = dump_mnesia_table(Filename, <<"mnesia_table_test">>, Config),
    ParsedRes = get_ok_value([data, mnesia, dumpTable], Res),
    ?assertEqual(<<"Mnesia table successfully dumped">>, ParsedRes),
    delete_mnesia_table(),
    check_created_file(create_full_filename(Filename), <<"{mnesia_table_test,1,<<\"TEST\">>}">>).

dump_mnesia_table_file_error_test(Config) ->
    Res = dump_mnesia_table(<<>>, <<"vcard">>, Config),
    ?assertEqual(<<"file_error">>, get_err_code(Res)).

dump_mnesia_table_no_table_error_test(Config) ->
    Res = dump_mnesia_table(<<"AA">>, <<"NON_EXISTING">>, Config),
    ?assertEqual(<<"table_does_not_exist">>, get_err_code(Res)).

dump_mnesia_test(Config) ->
    Filename = <<"dump_mnesia_test">>,
    create_mnesia_table_and_write([{disc_copies, [maps:get(node, mim())]},
                                   {attributes, record_info(fields, mnesia_table_test)}]),
    Res = dump_mnesia(Filename, Config),
    ParsedRes = get_ok_value([data, mnesia, dump], Res),
    ?assertEqual(<<"Mnesia successfully dumped">>, ParsedRes),
    delete_mnesia_table(),
    check_created_file(create_full_filename(Filename), <<"{mnesia_table_test,1,<<\"TEST\">>}">>).

dump_mnesia_file_error_test(Config) ->
    Res = dump_mnesia(<<>>, Config),
    ?assertEqual(<<"file_error">>, get_err_code(Res)).

backup_and_restore_test(Config) ->
    Filename = <<"backup_restore_mnesia_test">>,
    create_vcard_table(),
    write_to_vcard(),
    ?assert(is_record_in_vcard_table()),
    Res = backup_mnesia(Filename, Config),
    ParsedRes = get_ok_value([data, mnesia, backup], Res),
    ?assertEqual(<<"Mnesia backup was successfully created">>, ParsedRes),
    delete_record_from_table(),
    ?assertEqual(false, is_record_in_vcard_table()),
    Res2 = restore_mnesia(Filename, Config),
    ParsedRes2 = get_ok_value([data, mnesia, restore], Res2),
    ?assertEqual(<<"Mnesia was successfully restored">>, ParsedRes2),
    ?assert(is_record_in_vcard_table()),
    delete_record_from_table(),
    delete_file(create_full_filename(Filename)).

backup_wrong_filename_test(Config) ->
    Res = backup_mnesia(<<>>, Config),
    ?assertEqual(<<"wrong_filename">>, get_err_code(Res)).

backup_wrong_path_test(Config) ->
    Res = backup_mnesia(<<"/etc/">>, Config),
    ?assertEqual(<<"cannot_backup">>, get_err_code(Res)).

restore_no_file_test(Config) ->
    Res = restore_mnesia(<<>>, Config),
    ?assertEqual(<<"file_not_found">>, get_err_code(Res)).

restore_bad_file_test(Config) ->
    Res = restore_mnesia(<<"NON_EXISTING">>, Config),
    ?assertEqual(<<"file_not_found">>, get_err_code(Res)).

restore_bad_path_test(Config) ->
    Res = restore_mnesia(<<"/etc/">>, Config),
    ?assertEqual(<<"cannot_restore">>, get_err_code(Res)).

restore_wrong_file_format_test(Config) ->
    Filename = <<"restore_error">>,
    FileFullPath = create_full_filename(Filename),
    create_file(FileFullPath),
    Res = restore_mnesia(Filename, Config),
    delete_file(Filename),
    ?assertEqual(<<"not_a_log_file_error">>, get_err_code(Res)).

load_mnesia_test(Config) ->
    Filename = <<"load_mnesia_test">>,
    create_mnesia_table_and_write([{disc_copies, [maps:get(node, mim())]},
                                   {attributes, record_info(fields, mnesia_table_test)}]),
    Res = dump_mnesia(Filename, Config),
    ParsedRes = get_ok_value([data, mnesia, dump], Res),
    ?assertEqual(<<"Mnesia successfully dumped">>, ParsedRes),
    delete_mnesia_table(),
    check_if_response_contains(load_mnesia(Filename, Config), <<"Mnesia was successfully loaded">>),
    ?assert(is_record_in_table()),
    delete_mnesia_table().

load_mnesia_bad_file_test(Config) ->
    Filename = <<"EXISTING_BUT_EMPTY">>,
    create_file(create_full_filename(Filename)),
    check_if_response_contains(load_mnesia(Filename, Config), <<"bad_file_format">>).

load_mnesia_bad_file2_test(Config) ->
    Filename = <<"EXISTING_FILE">>,
    create_and_write_file(create_full_filename(Filename)),
    check_if_response_contains(load_mnesia(Filename, Config), <<"bad_file_format">>).

load_mnesia_no_file_test(Config) ->
    Filename = <<"NON_EXISTING">>,
    check_if_response_contains(load_mnesia(Filename, Config), <<"file_not_found">>).

change_nodename_test(Config) ->
    Filename1 = <<"change_nodename_mnesia_test">>,
    Filename2 = <<"change_nodename2_mnesia_test">>,
    create_vcard_table(),
    write_to_vcard(),
    ?assert(is_record_in_vcard_table()),
    Res = backup_mnesia(Filename1, Config),
    ParsedRes = get_ok_value([data, mnesia, backup], Res),
    ?assertEqual(<<"Mnesia backup was successfully created">>, ParsedRes),
    ChangeFrom = <<"mongooseim@localhost">>,
    ChangeTo = <<"change_nodename_test@localhost">>,
    Value = change_nodename(ChangeFrom, ChangeTo, Filename1, Filename2, Config),
    check_if_response_contains(Value,
        <<"Name of the node in the backup was successfully changed">>).

change_nodename_bad_name(Config) ->
    Filename1 = <<"change_incorrect_nodename_mnesia_test">>,
    Filename2 = <<"change_incorrect_nodename2_mnesia_test">>,
    ChangeFrom = <<"mongooseim@localhost">>,
    ChangeTo = <<"incorrect_format">>,
    Value = change_nodename(ChangeFrom, ChangeTo, Filename1, Filename2, Config),
    get_coercion_err_msg(Value).

change_nodename_empty_name(Config) ->
    Filename1 = <<"change_incorrect_nodename_mnesia_test">>,
    Filename2 = <<"change_incorrect_nodename2_mnesia_test">>,
    ChangeFrom = <<"mongooseim@localhost">>,
    ChangeTo = <<>>,
    Value = change_nodename(ChangeFrom, ChangeTo, Filename1, Filename2, Config),
    get_coercion_err_msg(Value).

change_nodename_no_file_error_test(Config) ->
    Filename1 = <<"non_existing">>,
    Filename2 = <<"change_nodename2_mnesia_test">>,
    ChangeFrom = <<"mongooseim@localhost">>,
    ChangeTo = <<"change_nodename_test@localhost">>,
    Value = change_nodename(ChangeFrom, ChangeTo, Filename1, Filename2, Config),
    ?assertEqual(<<"file_not_found">>, get_err_code(Value)).

change_nodename_bad_file_error_test(Config) ->
    Filename1 = <<"existing_but_having_wrong_structure">>,
    Filename2 = <<"change_nodename2_mnesia_test">>,
    create_and_write_file(create_full_filename(Filename1)),
    ChangeFrom = <<"mongooseim@localhost">>,
    ChangeTo = <<"change_nodename_test@localhost">>,
    Value = change_nodename(ChangeFrom, ChangeTo, Filename1, Filename2, Config),
    ?assertEqual(<<"bad_file_format">>, get_err_code(Value)).

get_info_test(Config) ->
    Res = get_info(maps:keys(mnesia_info_check()) ++ [<<"AAA">>], Config),
    ?assertEqual(<<"bad_key_error">>, get_err_code(Res)),
    ParsedRes = get_err_value([data, mnesia, systemInfo], Res),
    Map = mnesia_info_check(),
    lists:foreach(fun
        (#{<<"result">> := Element, <<"key">> := Key}) ->
            Fun = maps:get(Key, Map),
            ?assertEqual({true, Element, Key, Fun}, {?MODULE:Fun(Element), Element, Key, Fun});
        (null) ->
            ok
    end, ParsedRes).

get_all_info_test(Config) ->
    Res = get_info(null, Config),
    ParsedRes = get_ok_value([data, mnesia, systemInfo], Res),
    Map = mnesia_info_check(),
    lists:foreach(fun (#{<<"result">> := Element, <<"key">> := Key}) ->
        Fun = maps:get(Key, Map),
        ?assertEqual({true, Element, Key, Fun}, {?MODULE:Fun(Element), Element, Key, Fun})
    end, ParsedRes).

install_fallback_error_test(Config) ->
    Res = install_fallback(<<"AAAA">>, Config),
    ?assertEqual(<<"cannot_fallback">>, get_err_code(Res)).

set_master_test(Config) ->
    ParsedRes = get_ok_value([data, mnesia, setMaster], set_master(mim(), Config)),
    ?assertEqual(<<"Master node set">>, ParsedRes).

set_master_self_test(Config) ->
    ParsedRes = get_ok_value([data, mnesia, setMaster], set_master(#{node => self}, Config)),
    ?assertEqual(<<"Master node set">>, ParsedRes).

set_master_bad_name_test(Config) ->
    Res = set_master(#{node => incorrect_name}, Config),
    get_coercion_err_msg(Res).

set_master_empty_name_test(Config) ->
    Res = set_master(#{node => ''}, Config),
    get_coercion_err_msg(Res).

% Domain admin tests

domain_admin_dump_mnesia_table_test(Config) ->
    get_unauthorized(dump_mnesia_table(<<"File">>, <<"mnesia_table_test">>, Config)).

domain_admin_dump_mnesia_test(Config) ->
    get_unauthorized(dump_mnesia(<<"File">>, Config)).

domain_admin_backup_test(Config) ->
    get_unauthorized(backup_mnesia(<<"Path">>, Config)).

domain_admin_restore_test(Config) ->
    get_unauthorized(restore_mnesia(<<"Path">>, Config)).

domain_admin_load_mnesia_test(Config) ->
    get_unauthorized(load_mnesia(<<"Path">>, Config)).

domain_admin_change_nodename_test(Config) ->
    Filename1 = <<"change_nodename_mnesia_test">>,
    Filename2 = <<"change_nodename2_mnesia_test">>,
    ChangeFrom = <<"mongooseim@localhost">>,
    ChangeTo = <<"change_nodename_test@localhost">>,
    get_unauthorized(change_nodename(ChangeFrom, ChangeTo, Filename1, Filename2, Config)).

domain_admin_install_fallback_test(Config) ->
    get_unauthorized(install_fallback(<<"Path">>, Config)).

domain_admin_set_master_test(Config) ->
    get_unauthorized(set_master(mim(), Config)).

domain_admin_get_info_test(Config) ->
    get_unauthorized(get_info([<<"running_db_nodes">>], Config)).

backup_not_configured_test(Config) ->
    Filename = <<"backup_not_configured_test">>,
    get_not_loaded(backup_mnesia(Filename, Config)).

change_nodename_not_configured_test(Config) ->
    Filename1 = <<"change_nodename_not_configured_test">>,
    Filename2 = <<"change_nodename2_not_configured_test">>,
    ChangeFrom = <<"mongooseim@localhost">>,
    ChangeTo = <<"change_nodename_not_configured_test@localhost">>,
    get_not_loaded(change_nodename(ChangeFrom, ChangeTo, Filename1, Filename2, Config)).

dump_not_configured_test(Config) ->
    get_not_loaded(dump_mnesia(<<"File">>, Config)).

dump_table_not_configured_test(Config) ->
    get_not_loaded(dump_mnesia_table(<<"File">>, <<"mnesia_table_test">>, Config)).

install_fallback_not_configured_test(Config) ->
    get_not_loaded(install_fallback(<<"Path">>, Config)).

load_not_configured_test(Config) ->
    get_not_loaded(load_mnesia(<<"Path">>, Config)).

restore_not_configured_test(Config) ->
    get_not_loaded(restore_mnesia(<<"Path">>, Config)).

set_master_not_configured_test(Config) ->
    get_not_loaded(set_master(mim(), Config)).

system_info_not_configured_test(Config) ->
    get_not_loaded(get_info([<<"running_db_nodes">>], Config)).

%--------------------------------------------------------------------------------------------------
%                                         Helpers
%--------------------------------------------------------------------------------------------------

create_mnesia_table_and_write(Attrs) ->
    rpc_call(mnesia, delete_table, [mnesia_table_test]),
    {atomic, ok} = rpc_call(mnesia, create_table, [mnesia_table_test, Attrs]),
    rpc_call(mnesia, dirty_write,
        [mnesia_table_test, #mnesia_table_test{key = 1, name = <<"TEST">>}]).

create_vcard_table() ->
    rpc_call(mnesia, delete_table, [vcard]),
    rpc_call(mnesia, create_table, [vcard, [{disc_copies, [maps:get(node, mim())]},
                                            {attributes, record_info(fields, vcard)}]]).

write_to_vcard() ->
    rpc_call(mnesia, dirty_write, [vcard, #vcard{us = 1, vcard = <<"TEST">>}]).

is_record_in_table() ->
    Expected = [#mnesia_table_test{key = 1, name = <<"TEST">>}],
    Record = rpc_call(mnesia, dirty_read, [mnesia_table_test, 1]),
    Expected == Record.

is_record_in_vcard_table() ->
    Expected = [#vcard{us = 1, vcard = <<"TEST">>}],
    Record = rpc_call(mnesia, dirty_read, [vcard, 1]),
    Expected == Record.

delete_record_from_table() ->
    ok = rpc_call(mnesia, dirty_delete, [vcard, 1]).

create_full_filename(Filename) ->
    Path  = list_to_binary(get_mim_cwd() ++ "/"),
    <<Path/binary, Filename/binary>>.

delete_mnesia_table() ->
    {atomic, ok} = rpc_call(mnesia, delete_table, [mnesia_table_test]).

check_created_file(FullPath, ExpectedContent) ->
    {ok, FileInsides} = file:read_file(FullPath),
    ?assertMatch({_,_}, binary:match(FileInsides, ExpectedContent)),
    delete_file(FullPath).

create_file(FullPath) ->
    file:open(FullPath, [write]),
    file:close(FullPath).

create_and_write_file(FullPath) ->
    {ok, File} = file:open(FullPath, [write]),
    io:format(File, "~s~n", ["TEST"]),
    file:close(FullPath).

check_if_response_contains(Response, String) ->
    ParsedLoadRes = io_lib:format("~p", [Response]),
    ?assertMatch({_,_}, binary:match(list_to_binary(ParsedLoadRes), String)).

delete_file(FullPath) ->
    file:delete(FullPath).

get_info(null, Config) ->
    execute_command(<<"mnesia">>, <<"systemInfo">>, #{}, Config);
get_info(Keys, Config) ->
    execute_command(<<"mnesia">>, <<"systemInfo">>, #{keys => Keys}, Config).

install_fallback(Path, Config) ->
    execute_command(<<"mnesia">>, <<"installFallback">>, #{path => Path}, Config).

dump_mnesia(Path, Config) ->
    execute_command(<<"mnesia">>, <<"dump">>, #{path => Path}, Config).

backup_mnesia(Path, Config) ->
    execute_command(<<"mnesia">>, <<"backup">>, #{path => Path}, Config).

restore_mnesia(Path, Config) ->
    execute_command(<<"mnesia">>, <<"restore">>, #{path => Path}, Config).

dump_mnesia_table(Path, Table, Config) ->
    execute_command(<<"mnesia">>, <<"dumpTable">>, #{path => Path, table => Table}, Config).

load_mnesia(Path, Config) ->
    execute_command(<<"mnesia">>, <<"load">>, #{path => Path}, Config).

change_nodename(ChangeFrom, ChangeTo, Source, Target, Config) ->
    Vars = #{<<"fromString">> => ChangeFrom, <<"toString">> => ChangeTo,
             <<"source">> => Source, <<"target">> => Target},
    execute_command(<<"mnesia">>, <<"changeNodename">>, Vars, Config).

set_master(Node, Config) ->
    execute_command(<<"mnesia">>, <<"setMaster">>, Node, Config).

mnesia_info_check() ->
    #{<<"access_module">> => check_binary,
      <<"auto_repair">> => check_binary,
      <<"backend_types">> => check_list,
      <<"backup_module">> => check_binary,
      <<"checkpoints">> => check_list,
      <<"db_nodes">> => check_list,
      <<"debug">> => check_binary,
      <<"directory">> => check_binary,
      <<"dump_log_load_regulation">> => check_binary,
      <<"dump_log_time_threshold">> => check_integer,
      <<"dump_log_update_in_place">> => check_binary,
      <<"dump_log_write_threshold">> => check_integer,
      <<"event_module">> => check_binary,
      <<"extra_db_nodes">> => check_list,
      <<"fallback_activated">> => check_binary,
      <<"held_locks">> => check_list,
      <<"ignore_fallback_at_startup">> => check_binary,
      <<"fallback_error_function">> => check_binary,
      <<"is_running">> => check_binary,
      <<"local_tables">> => check_list,
      <<"lock_queue">> => check_list,
      <<"log_version">> => check_binary,
      <<"master_node_tables">> => check_list,
      <<"max_wait_for_decision">> => check_binary,
      <<"protocol_version">> => check_binary,
      <<"running_db_nodes">> => check_list,
      <<"schema_location">> => check_binary,
      <<"schema_version">> => check_binary,
      <<"subscribers">> => check_list,
      <<"tables">> => check_list,
      <<"transaction_commits">> => check_integer,
      <<"transaction_failures">> => check_integer,
      <<"transaction_log_writes">> => check_integer,
      <<"transaction_restarts">> => check_integer,
      <<"transactions">> => check_list,
      <<"use_dir">> => check_binary,
      <<"core_dir">> => check_binary,
      <<"no_table_loaders">> => check_integer,
      <<"dc_dump_limit">> => check_integer,
      <<"send_compressed">> => check_integer,
      <<"max_transfer_size">> => check_integer,
      <<"version">> => check_binary}.

check_list(L) ->
    lists:all(fun(Item) -> is_binary(Item) end, L).

check_binary(Value) -> is_binary(Value).

check_integer(Value) -> is_integer(Value).

get_mim_cwd() ->
    {ok, Cwd} = rpc(mim(), file, get_cwd, []),
    Cwd.
