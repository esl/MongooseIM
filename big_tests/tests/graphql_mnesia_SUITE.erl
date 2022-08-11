-module(graphql_mnesia_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1, mim/0, mim2/0, rpc/4]).
-import(domain_helper, [host_type/1]).
-import(mongooseimctl_helper, [rpc_call/3]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, user_to_bin/1,
                         get_ok_value/2, get_err_code/1, get_err_value/2]).

-record(mnesia_table_test, {key :: integer(), name :: binary()}).
-record(vcard, {us, vcard}).

all() ->
    [{group, admin_mnesia_cli},
     {group, admin_mnesia_http}].

groups() ->
    [{admin_mnesia_http, [sequence], admin_mnesia_tests()},
     {admin_mnesia_cli, [sequence], admin_mnesia_tests()}].

admin_mnesia_tests() ->
    [get_info_test,
     dump_mnesia_table_test,
     dump_mnesia_table_file_error_test,
     dump_mnesia_table_no_table_error_test,
     dump_mnesia_test,
     dump_mnesia_file_error_test,
     backup_and_restore_test,
     backup_wrong_filename_test,
     restore_no_file_test,
     restore_wrong_file_format_test,
     install_fallback_error_test,
     install_fallback_test,
     set_master_test].

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
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_mnesia_cli, Config) ->
    graphql_helper:init_admin_cli(Config).

end_per_group(_, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

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
    Res = dump_mnesia_table(<<"">>, <<"vcard">>, Config),
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
    Res = dump_mnesia(<<"">>, Config),
    ?assertEqual(<<"file_error">>, get_err_code(Res)).

backup_and_restore_test(Config) ->
    Filename = <<"backup_restore_mnesia_test">>,
    create_vcard_table(),
    write_to_vcard(),
    ?assert(is_record_in_a_vcard_table()),
    Res = backup_mnesia(Filename, Config),
    ParsedRes = get_ok_value([data, mnesia, backup], Res),
    ?assertEqual(<<"Mnesia was successfully backuped">>, ParsedRes),
    delete_record_from_table(),
    ?assertEqual(false, is_record_in_a_vcard_table()),
    Res2 = restore_mnesia(Filename, Config),
    ParsedRes2 = get_ok_value([data, mnesia, restore], Res2),
    ?assertEqual(<<"Mnesia was successfully restored">>, ParsedRes2),
    ?assert(is_record_in_a_vcard_table()),
    delete_record_from_table(),
    delete_file(create_full_filename(Filename)).

backup_wrong_filename_test(Config) ->
    Res = backup_mnesia(<<"">>, Config),
    ?assertEqual(<<"wrong_filename">>, get_err_code(Res)).

restore_no_file_test(Config) ->
    Res = restore_mnesia(<<"">>, Config),
    ?assertEqual(<<"file_not_found">>, get_err_code(Res)).

restore_wrong_file_format_test(Config) ->
    Filename = <<"restore_error">>,
    FileFullPath = create_full_filename(Filename),
    create_file(FileFullPath),
    Res = restore_mnesia(Filename, Config),
    delete_file(Filename),
    ?assertEqual(<<"not_a_log_file_error">>, get_err_code(Res)).

get_info_test(Config) ->
    Res = get_info(mnesia_info_keys(), Config),
    ?assertEqual(<<"bad_key_error">>, get_err_code(Res)),
    ParsedRes = get_err_value([data, mnesia, info], Res),
    Map = mnesia_info_check(),
    lists:foreach(fun
        (#{<<"result">> := Element, <<"key">> := Key}) ->
            Fun = maps:get(Key, Map),
            ?assertEqual({true, Element, Key, Fun}, {?MODULE:Fun(Element), Element, Key, Fun});
        (null) ->
            ok
    end, ParsedRes).

install_fallback_error_test(Config) ->
    Res = install_fallback(#{<<"path">> => <<"AAAA">>}, Config),
    ?assertEqual(<<"cannot_fallback">>, get_err_code(Res)).

install_fallback_test(Config) ->
    ok.
%    {ok, Path} = file:get_cwd(),
%    io:format("SDFSDFDSFDSFSDFDSFSDF"),
%    io:format(Path),
%    Res = install_fallback(#{<<"path">> => list_to_binary(Path)}, Config),
%    ParsedRes = get_ok_value([data, mnesia, installFallback], Res),
%    ?assertEqual(<<"FallbackInstalled">>, ParsedRes).

set_master_test(Config) ->
    ok.
%    catch distributed_helper:rpc(mim(), ejabberd_auth_internal, start, [host_type(mim1)]),
%    catch distributed_helper:rpc(mim2(), ejabberd_auth_internal, start, [host_type(mim2)]),
%
%    TableName = passwd,
%    NodeList =  rpc_call(mnesia, system_info, [running_db_nodes]),
%    set_master(#{<<"node">> => <<"self">>}, Config),
%    [MasterNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]),
%    true = lists:member(MasterNode, NodeList),
%    RestNodesList = lists:delete(MasterNode, NodeList),
%    OtherNode = hd(RestNodesList),
%    set_master(#{<<"node">> => atom_to_binary(OtherNode)}, Config),
%    [OtherNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]),
%    set_master(#{<<"node">> => <<"self">>}, Config),
%    [MasterNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]).

get_info(Keys, Config) ->
    execute_command(<<"mnesia">>, <<"info">>, #{keys => Keys}, Config).

install_fallback(Node, Config) ->
    execute_command(<<"mnesia">>, <<"installFallback">>, Node, Config).

dump_mnesia(Path, Config) ->
    execute_command(<<"mnesia">>, <<"dump">>, #{path => Path}, Config).

backup_mnesia(Path, Config) ->
    execute_command(<<"mnesia">>, <<"backup">>, #{path => Path}, Config).

restore_mnesia(Path, Config) ->
    execute_command(<<"mnesia">>, <<"restore">>, #{path => Path}, Config).

dump_mnesia_table(Path, Table, Config) ->
    execute_command(<<"mnesia">>, <<"dumpTable">>, #{path => Path, table => Table}, Config).

set_master(Node, Config) ->
    execute_command(<<"mnesia">>, <<"setMaster">>, Node, Config).

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

is_record_in_a_table() ->
    Expected = [#mnesia_table_test{key = 1, name = <<"TEST">>}],
    Record = rpc_call(mnesia, dirty_read, [mnesia_table_test, 1]),
    Expected == Record.

is_record_in_a_vcard_table() ->
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

check_created_file(FullPath, ExpectedInsides) ->
    {ok, FileInsides} = file:read_file(FullPath),
    ?assertMatch({_,_}, binary:match(FileInsides, ExpectedInsides)),
    delete_file(FullPath).

create_file(FullPath) ->
    file:open(FullPath, [write]),
    file:close(FullPath).

delete_file(FullPath) ->
    file:delete(FullPath).

mnesia_info_keys() ->
    [<<"all">>,
     <<"AAAA">>,
     <<"access_module">>,
     <<"auto_repair">>,
     <<"backend_types">>,
     <<"backup_module">>,
     <<"checkpoints">>,
     <<"db_nodes">>,
     <<"debug">>,
     <<"directory">>,
     <<"dump_log_load_regulation">>,
     <<"dump_log_time_threshold">>,
     <<"dump_log_update_in_place">>,
     <<"dump_log_write_threshold">>,
     <<"event_module">>,
     <<"extra_db_nodes">>,
     <<"fallback_activated">>,
     <<"held_locks">>,
     <<"ignore_fallback_at_startup">>,
     <<"fallback_error_function">>,
     <<"is_running">>,
     <<"local_tables">>,
     <<"lock_queue">>,
     <<"log_version">>,
     <<"master_node_tables">>,
     <<"max_wait_for_decision">>,
     <<"protocol_version">>,
     <<"running_db_nodes">>,
     <<"schema_location">>,
     <<"schema_version">>,
     <<"subscribers">>,
     <<"tables">>,
     <<"transaction_commits">>,
     <<"transaction_failures">>,
     <<"transaction_log_writes">>,
     <<"transaction_restarts">>,
     <<"transactions">>,
     <<"use_dir">>,
     <<"core_dir">>,
     <<"no_table_loaders">>,
     <<"dc_dump_limit">>,
     <<"send_compressed">>,
     <<"max_transfer_size">>,
     <<"version">>,
     <<"db_nodes">>,
     <<"running_db_nodes">>].

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
      <<"version">> => check_binary,
      <<"db_nodes">> => check_list,
      <<"running_db_nodes">> => check_list}.

check_list([]) -> true;
check_list([Head | Tail]) when is_binary(Head) -> check_list(Tail);
check_list(_) -> false.

check_binary(Value) when is_binary(Value) -> true;
check_binary(_) -> false.

check_integer(Value) when is_integer(Value) -> true;
check_integer(_) -> false.

get_mim_cwd() ->
    {ok, Cwd} = rpc(mim(), file, get_cwd, []),
    Cwd.
