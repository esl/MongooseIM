-module(mnesia_api_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [mnesia_info_known_key_still_works,
     mnesia_info_unknown_key_returns_bad_key_error_not_crash,
     mnesia_info_repeated_unknown_key_does_not_leak_atoms].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

mnesia_info_known_key_still_works(_Config) ->
    {ok, [{ok, #{<<"key">> := <<"is_running">>}}]} =
        mnesia_api:mnesia_info([<<"is_running">>]).

mnesia_info_unknown_key_returns_bad_key_error_not_crash(_Config) ->
    {ok, [{{bad_key_error, _}, #{key := <<"not_a_real_mnesia_key">>}}]} =
        mnesia_api:mnesia_info([<<"not_a_real_mnesia_key">>]).

mnesia_info_repeated_unknown_key_does_not_leak_atoms(_Config) ->
    Before = erlang:system_info(atom_count),
    [mnesia_api:mnesia_info([<<"bogus_key_", (integer_to_binary(N))/binary>>])
     || N <- lists:seq(1, 50)],
    After = erlang:system_info(atom_count),
    %% Distinct binaries never create atoms; small tolerance for the VM's own atom churn.
    true = (After - Before) < 5.
