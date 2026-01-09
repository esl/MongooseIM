-module(ejabberd_auth_pagination_SUITE).

-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     %% ejabberd_auth_internal tests
     extract_pagination_opts_no_options,
     extract_pagination_opts_with_limit,
     extract_pagination_opts_with_offset,
     extract_pagination_opts_with_prefix,
     apply_prefix_filter_with_prefix,
     apply_prefix_filter_without_prefix,
     apply_pagination_no_pagination,
     apply_pagination_with_prefix_only,
     apply_pagination_with_limit,
     %% ejabberd_auth_rdbms tests
     extract_list_users_opts_no_options,
     extract_list_users_opts_with_limit,
    extract_list_users_opts_with_prefix
    ].

%%%------- ejabberd_auth_internal tests --------

extract_pagination_opts_no_options(_Config) ->
    Opts = #{},
    {Limit, Offset, Prefix} = ejabberd_auth_internal:extract_pagination_opts(Opts),
    ?assertEqual(undefined, Limit),
    ?assertEqual(0, Offset),
    ?assertEqual(undefined, Prefix).

extract_pagination_opts_with_limit(_Config) ->
    Opts = #{limit => 10},
    {Limit, Offset, Prefix} = ejabberd_auth_internal:extract_pagination_opts(Opts),
    ?assertEqual(10, Limit),
    ?assertEqual(0, Offset),
    ?assertEqual(undefined, Prefix).

extract_pagination_opts_with_offset(_Config) ->
    Opts = #{offset => 5},
    {Limit, Offset, Prefix} = ejabberd_auth_internal:extract_pagination_opts(Opts),
    ?assertEqual(undefined, Limit),
    ?assertEqual(5, Offset),
    ?assertEqual(undefined, Prefix).

extract_pagination_opts_with_prefix(_Config) ->
    Opts = #{prefix => <<"test">>},
    {Limit, Offset, Prefix} = ejabberd_auth_internal:extract_pagination_opts(Opts),
    ?assertEqual(undefined, Limit),
    ?assertEqual(0, Offset),
    ?assertEqual(<<"test">>, Prefix).

apply_prefix_filter_with_prefix(_Config) ->
    Users = [{<<"alice">>, <<"example.com">>}, {<<"bob">>, <<"example.com">>}, {<<"abc">>, <<"example.com">>}],
    Filtered = ejabberd_auth_internal:apply_prefix_filter(<<"a">>, Users),
    ?assertEqual(2, length(Filtered)),
    ?assertMatch([{<<"abc">>, _}, {<<"alice">>, _}], lists:sort(Filtered)).

apply_prefix_filter_without_prefix(_Config) ->
    Users = [{<<"alice">>, <<"example.com">>}, {<<"bob">>, <<"example.com">>}],
    Filtered = ejabberd_auth_internal:apply_prefix_filter(undefined, Users),
    ?assertEqual(Users, Filtered).

apply_pagination_no_pagination(_Config) ->
    Users = [{<<"u1">>, <<"server">>}, {<<"u2">>, <<"server">>}],
    Result = ejabberd_auth_internal:apply_pagination(Users, undefined, 0, undefined),
    ?assertEqual(Users, Result).

apply_pagination_with_prefix_only(_Config) ->
    Users = [{<<"b">>, <<"server">>}, {<<"a">>, <<"server">>}],
    Result = ejabberd_auth_internal:apply_pagination(Users, undefined, 0, <<"a">>),
    ?assertEqual([{<<"a">>, <<"server">>}, {<<"b">>, <<"server">>}], lists:sort(Result)).

apply_pagination_with_limit(_Config) ->
    Users = [{<<"u1">>, <<"server">>}, {<<"u2">>, <<"server">>}, {<<"u3">>, <<"server">>}],
    Result = ejabberd_auth_internal:apply_pagination(Users, 2, 1, undefined),
    ?assertEqual([{<<"u2">>, <<"server">>}, {<<"u3">>, <<"server">>}], Result).

%%%------- ejabberd_auth_rdbms tests --------

extract_list_users_opts_no_options(_Config) ->
    Opts = #{},
    {Limit, Offset, Prefix} = ejabberd_auth_rdbms:extract_list_users_opts(Opts),
    ?assertEqual(undefined, Limit),
    ?assertEqual(0, Offset),
    ?assertEqual(undefined, Prefix).

extract_list_users_opts_with_limit(_Config) ->
    Opts = #{limit => 10},
    {Limit, Offset, Prefix} = ejabberd_auth_rdbms:extract_list_users_opts(Opts),
    ?assertEqual(10, Limit),
    ?assertEqual(0, Offset),
    ?assertEqual(undefined, Prefix).

extract_list_users_opts_with_prefix(_Config) ->
    Opts = #{prefix => <<"test">>},
    {Limit, Offset, Prefix} = ejabberd_auth_rdbms:extract_list_users_opts(Opts),
    ?assertEqual(undefined, Limit),
    ?assertEqual(0, Offset),
    ?assertEqual(<<"test">>, Prefix).

