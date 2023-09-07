-module(ejabberd_admin_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").

-import(ejabberd_helper, [data/2]).


all() ->
    [{group, import_users}].

groups() ->
    [{import_users, [], [import_users_from_valid_csv,
                         import_users_from_valid_csv_with_quoted_fields,
                         import_from_invalid_csv]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    meck:new(ejabberd_auth, [no_link]),
    meck:expect(ejabberd_auth, try_register,
                fun(#jid{luser = <<"existing_user">>}, _) -> {error, exists};
                   (#jid{luser = <<"null_password_user">>}, _) -> {error, null_password};
                   (#jid{lserver = <<"not_allowed_domain">>}, _) -> {error, not_allowed};
                   (#jid{luser = <<"invalid_jid_user">>}, _) -> {error, invalid_jid};
                   (_, _) -> ok
                end),
    Config.

end_per_group(_, _Config) ->
    meck:unload(ejabberd_auth),
    ok.

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
    SortedResult = lists:sort(Result),
    % then
    ?assertEqual([{bad_csv, [<<"wrong,number,of,fields,line">>]},
                  {exists, [#jid{luser = <<"existing_user">>,
                                 lserver = <<"localhost">>,
                                 lresource = <<>>}]},
                  {invalid_jid, [<<"invalid_jid_user,localhost,password">>]},
                  {not_allowed, [#jid{luser = <<"bad_domain_user">>,
                                      lserver = <<"not_allowed_domain">>,
                                      lresource = <<>>}]},

                  {null_password, [#jid{luser = <<"null_password_user">>,
                                        lserver = <<"localhost">>,
                                        lresource = <<>>}]},
                  {ok, [#jid{luser = <<"user">>, lserver = <<"localhost">>, lresource = <<>>}]}],
                  SortedResult).

import_users_from_valid_csv_with_quoted_fields(Config) ->
    % given
    ValidCsvPath = data(Config, "valid_quoted.csv"),
    % when
    Result = ejabberd_admin:import_users(ValidCsvPath),
    % then
    ?assertEqual([{ok, [#jid{luser = <<"username,with,commas">>,
                             lserver = <<"localhost">>,
                             lresource = <<>>}]}],
                 Result).

import_from_invalid_csv(_Config) ->
    % given
    NonExistingPath = "",
    % then
    ?assertError({badmatch, {error, file_not_found}},
                 ejabberd_admin:import_users(NonExistingPath)).
