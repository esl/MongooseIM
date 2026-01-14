-module(mod_broadcast_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [normalize_recipients_all_users_test,
     normalize_recipients_usernames_test,
     normalize_recipients_usernames_empty_test,
     normalize_recipients_usernames_too_many_test,
     normalize_recipients_invalid_test,
     make_job_id_uniqueness_test,
     status_conversion_test,
     validate_message_content_valid_test,
     validate_message_content_body_too_long_test,
     validate_message_content_empty_body_test,
     validate_message_content_subject_too_long_test,
     validate_message_content_invalid_chars_test,
     normalize_limit_test,
     normalize_index_test].

%% Test normalize_recipients with all_users type
normalize_recipients_all_users_test(_Config) ->
    meck:new(ejabberd_auth, [passthrough]),
    meck:expect(ejabberd_auth, get_vh_registered_users, fun(<<"example.com">>) ->
        [{<<"user1">>, <<"example.com">>}, {<<"user2">>, <<"example.com">>}]
    end),

    Result = mod_broadcast_api:normalize_recipients(<<"example.com">>, #{type => all_users}),
    ?assertMatch({ok, [<<"user1">>, <<"user2">>]}, Result),

    meck:unload(ejabberd_auth).

%% Test normalize_recipients with valid usernames
normalize_recipients_usernames_test(_Config) ->
    Input = #{type => usernames, usernames => [<<"alice">>, <<"bob">>, <<"alice">>]},
    Result = mod_broadcast_api:normalize_recipients(<<"example.com">>, Input),
    ?assertMatch({ok, [<<"alice">>, <<"bob">>]}, Result).

%% Test normalize_recipients with empty usernames
normalize_recipients_usernames_empty_test(_Config) ->
    Input = #{type => usernames, usernames => []},
    Result = mod_broadcast_api:normalize_recipients(<<"example.com">>, Input),
    ?assertMatch({error, <<"No valid recipients provided">>}, Result).

%% Test normalize_recipients exceeding limit
normalize_recipients_usernames_too_many_test(_Config) ->
    Usernames = [integer_to_binary(N) || N <- lists:seq(1, 10001)],
    Input = #{type => usernames, usernames => Usernames},
    Result = mod_broadcast_api:normalize_recipients(<<"example.com">>, Input),
    ?assertMatch({error, _}, Result).

%% Test normalize_recipients with invalid input
normalize_recipients_invalid_test(_Config) ->
    Result = mod_broadcast_api:normalize_recipients(<<"example.com">>, #{type => invalid}),
    ?assertMatch({error, <<"Invalid recipients input">>}, Result).

%% Test make_job_id generates unique IDs
make_job_id_uniqueness_test(_Config) ->
    Ids = [mod_broadcast_api:make_job_id() || _ <- lists:seq(1, 1000)],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(1000, length(UniqueIds)).

%% Test status conversion functions
status_conversion_test(_Config) ->
    ?assertEqual(<<"running">>, mod_broadcast_rdbms:status_to_bin(running)),
    ?assertEqual(<<"success">>, mod_broadcast_rdbms:status_to_bin(success)),
    ?assertEqual(<<"aborted_admin">>, mod_broadcast_rdbms:status_to_bin(aborted_admin)),
    ?assertEqual(<<"aborted_errors">>, mod_broadcast_rdbms:status_to_bin(aborted_errors)),

    ?assertEqual(running, mod_broadcast_rdbms:bin_to_status(<<"running">>)),
    ?assertEqual(success, mod_broadcast_rdbms:bin_to_status(<<"success">>)),
    ?assertEqual(aborted_admin, mod_broadcast_rdbms:bin_to_status(<<"aborted_admin">>)),
    ?assertEqual(aborted_errors, mod_broadcast_rdbms:bin_to_status(<<"aborted_errors">>)).

%% Test valid message content
validate_message_content_valid_test(_Config) ->
    Result1 = mod_broadcast_api:validate_message_content(<<"Test">>, <<"Hello, world!">>),
    ?assertEqual(ok, Result1),

    Result2 = mod_broadcast_api:validate_message_content(undefined, <<"Body only">>),
    ?assertEqual(ok, Result2).

%% Test body too long
validate_message_content_body_too_long_test(_Config) ->
    LongBody = binary:copy(<<"x">>, 10001),
    Result = mod_broadcast_api:validate_message_content(undefined, LongBody),
    ?assertMatch({error, _}, Result).

%% Test empty body
validate_message_content_empty_body_test(_Config) ->
    Result = mod_broadcast_api:validate_message_content(undefined, <<>>),
    ?assertMatch({error, <<"Body cannot be empty">>}, Result).

%% Test subject too long
validate_message_content_subject_too_long_test(_Config) ->
    LongSubject = binary:copy(<<"x">>, 1001),
    Result = mod_broadcast_api:validate_message_content(LongSubject, <<"Body">>),
    ?assertMatch({error, _}, Result).

%% Test invalid control characters
validate_message_content_invalid_chars_test(_Config) ->
    %% Test null byte in body
    Result1 = mod_broadcast_api:validate_message_content(undefined, <<"Test", 0, "msg">>),
    ?assertMatch({error, <<"Message contains invalid control characters">>}, Result1),

    %% Test bell character in subject
    Result2 = mod_broadcast_api:validate_message_content(<<"Alert", 7>>, <<"Body">>),
    ?assertMatch({error, <<"Message contains invalid control characters">>}, Result2),

    %% Test valid newline (should pass)
    Result3 = mod_broadcast_api:validate_message_content(<<"Line1\nLine2">>, <<"Body">>),
    ?assertEqual(ok, Result3).

%% Test normalize_limit
normalize_limit_test(_Config) ->
    ?assertEqual(50, mod_broadcast_api:normalize_limit(undefined)),
    ?assertEqual(100, mod_broadcast_api:normalize_limit(100)),
    ?assertEqual(500, mod_broadcast_api:normalize_limit(1000)),
    ?assertEqual(50, mod_broadcast_api:normalize_limit(-1)).

%% Test normalize_index
normalize_index_test(_Config) ->
    ?assertEqual(0, mod_broadcast_api:normalize_index(undefined)),
    ?assertEqual(10, mod_broadcast_api:normalize_index(10)),
    ?assertEqual(0, mod_broadcast_api:normalize_index(-5)).
