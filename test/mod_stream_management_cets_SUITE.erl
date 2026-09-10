-module(mod_stream_management_cets_SUITE).

-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(TABLE, cets_stream_management_session).
-define(HOST_TYPE, <<"localhost">>).

all() ->
    [old_session_cleanup_after_registration,
     old_session_cleanup_before_registration].

init_per_testcase(_, Config) ->
    {ok, _} = cets:start(?TABLE, #{}),
    Config.

end_per_testcase(_, _Config) ->
    cets:stop(?TABLE).

old_session_cleanup_after_registration(_Config) ->
    SMID = <<"resumed-stream">>,
    OldSID = {1, self()},
    NewSID = {2, self()},
    ok = mod_stream_management_cets:register_smid(?HOST_TYPE, SMID, OldSID),
    ok = mod_stream_management_cets:register_smid(?HOST_TYPE, SMID, NewSID),
    %% Force the interleaving where the new C2S registers before the old C2S exits.
    ?assertEqual({ok, SMID}, mod_stream_management_cets:unregister_smid(?HOST_TYPE, OldSID)),
    assert_resumed_session(SMID, OldSID, NewSID),
    assert_session_cleanup(SMID, NewSID).

old_session_cleanup_before_registration(_Config) ->
    SMID = <<"resumed-stream">>,
    OldSID = {1, self()},
    NewSID = {2, self()},
    ok = mod_stream_management_cets:register_smid(?HOST_TYPE, SMID, OldSID),
    ?assertEqual({ok, SMID}, mod_stream_management_cets:unregister_smid(?HOST_TYPE, OldSID)),
    ok = mod_stream_management_cets:register_smid(?HOST_TYPE, SMID, NewSID),
    assert_resumed_session(SMID, OldSID, NewSID),
    assert_session_cleanup(SMID, NewSID).

assert_resumed_session(SMID, OldSID, NewSID) ->
    ?assertEqual({sid, NewSID}, mod_stream_management_cets:get_sid(?HOST_TYPE, SMID)),
    ?assertEqual([], ets:lookup(?TABLE, {sid, OldSID})),
    ?assertEqual([{{sid, NewSID}, SMID}], ets:lookup(?TABLE, {sid, NewSID})),
    ?assertEqual({error, smid_not_found},
                 mod_stream_management_cets:unregister_smid(?HOST_TYPE, OldSID)),
    ?assertEqual({sid, NewSID}, mod_stream_management_cets:get_sid(?HOST_TYPE, SMID)).

assert_session_cleanup(SMID, SID) ->
    ?assertEqual({ok, SMID}, mod_stream_management_cets:unregister_smid(?HOST_TYPE, SID)),
    ?assertEqual({error, smid_not_found}, mod_stream_management_cets:get_sid(?HOST_TYPE, SMID)),
    ?assertEqual([], ets:tab2list(?TABLE)).
