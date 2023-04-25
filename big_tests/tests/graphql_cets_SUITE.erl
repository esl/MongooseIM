-module(graphql_cets_SUITE).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0]).
-import(domain_helper, [host_type/1]).
-import(mongooseimctl_helper, [rpc_call/3]).
-import(graphql_helper, [execute_command/4, get_unauthorized/1, get_ok_value/2]).

all() ->
    [{group, admin_cets_cli},
     {group, admin_cets_http},
     {group, domain_admin_cets}].

groups() ->
    [{admin_cets_http, [sequence], admin_cets_tests()},
     {admin_cets_cli, [sequence], admin_cets_tests()},
     {domain_admin_cets, [], domain_admin_tests()}].

admin_cets_tests() ->
    [has_sm_table_in_info].

domain_admin_tests() ->
    [domain_admin_get_info_test].

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    ejabberd_node_utils:init(mim(), Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(admin_cets_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cets_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_cets, Config) ->
    graphql_helper:init_domain_admin_handler(Config).

end_per_group(_, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

init_per_testcase(has_sm_table_in_info, Config) ->
    case rpc_call(ejabberd_sm, sm_backend, []) of
        ejabberd_sm_cets ->
            Config;
        _ ->
            {skip, "SM backend is not CETS"}
    end;
init_per_testcase(_, Config) ->
    Config.

% Admin tests

has_sm_table_in_info(Config) ->
    Res = get_info(Config),
    Tables = get_ok_value([data, cets, systemInfo], Res),
    [T] = [T || T = #{<<"tableName">> := <<"cets_session">>} <- Tables],
    #{<<"memory">> := Mem, <<"nodes">> := Nodes, <<"size">> := Size} = T,
    true = is_integer(Mem),
    true = is_integer(Size),
    #{node := Node1} = mim(),
    lists:member(Node1, Nodes).

% Domain admin tests

domain_admin_get_info_test(Config) ->
    get_unauthorized(get_info(Config)).

%--------------------------------------------------------------------------------------------------
%                                         Helpers
%--------------------------------------------------------------------------------------------------

get_info(Config) ->
    execute_command(<<"cets">>, <<"systemInfo">>, #{}, Config).
