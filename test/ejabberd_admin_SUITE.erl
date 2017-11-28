-module(ejabberd_admin_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_helper, [data/2]).


all() ->
    [{group, import_users}].

groups() ->
    [{import_users, [], [import_users_from_valid_csv,
                         import_users_from_valid_csv_with_quoted_fields,
                         import_from_invalid_csv]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    meck:new(ejabberd_auth, [no_link]),
    meck:expect(ejabberd_auth, try_register,
                fun(<<"existing_user">>, _, _) -> {error, exists};
                   (<<"null_password_user">>, _, _) -> {error, null_password};
                   (_, <<"not_allowed_domain">>, _) -> {error, not_allowed};
                   (<<"invalid_jid_user">>, _, _) -> {error, invalid_jid};
                   (_, _, _) -> ok
                end),
    Config;

init_per_group(_, Config) -> Config.

end_per_group(_, _Config) ->
    meck:unload(ejabberd_auth),
    ok;

end_per_group(_, _) -> ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%
%% Tests
%%

import_users_from_valid_csv(Config) ->
    % given
    ValidCsvPath = data(Config, "valid.csv"),
    % when
    Result = ejabberd_admin:import_users(ValidCsvPath),
    % then
    ?assertEqual([{ok, <<"user">>},
                  {exists, <<"existing_user">>},
                  {null_password, <<"null_password_user">>},
                  {not_allowed, <<"bad_domain_user">>},
                  {invalid_jid, <<"invalid_jid_user">>},
                  {bad_csv, <<"wrong,number,of,fields,line">>}],
                 Result).

import_users_from_valid_csv_with_quoted_fields(Config) ->
    % given
    ValidCsvPath = data(Config, "valid_quoted.csv"),
    % when
    Result = ejabberd_admin:import_users(ValidCsvPath),
    % then
    ?assertEqual([{ok, <<"username,with,commas">>}],
                 Result).

import_from_invalid_csv(Config) ->
    % given
    NonExistingPath = "",
    % then
    ?assertError({badmatch, {error, enoent}},
                 ejabberd_admin:import_users(NonExistingPath)).
