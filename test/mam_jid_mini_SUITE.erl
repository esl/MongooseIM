-module(mam_jid_mini_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

all() -> [test_encode_decode_functionality].

test_encode_decode_functionality(_Config) ->
    PossibleDomainNames = [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>],
    PossibleUserNames = [<<"">> | PossibleDomainNames],
    PossibleResourceNames = [<<"just:some@random/text here">> | PossibleUserNames],
    PossibleJIDs = [{U, D, R, catch jid:make(U, D, R)} || U <- PossibleUserNames,
                                                          D <- PossibleDomainNames,
                                                          R <- PossibleResourceNames],
    FailedJIDS = [ct:log("~nJID decoding failed:~n"
                         "\tbase JID      - ~p~n"
                         "\tJID to encode - ~p~n"
                         "\tencoded JID   - ~p~n"
                         "\tdecoded JID   - ~p~n", [{U1, D1, R1}, {U2, D2, R2},
                                                    EncodedJID, DecodedJID])
                  || {U1, D1, R1, JID1} <- PossibleJIDs,
                     {U2, D2, R2, JID2} <- PossibleJIDs,
                     EncodedJID <- [catch mam_jid_mini:encode(JID1, JID2)],
                     DecodedJID <- [catch mam_jid_mini:decode(JID1, EncodedJID)],
                     DecodedJID =/= JID2],
    case FailedJIDS of
        [] -> ok;
        _ -> ct:fail("Failed to encode/decode some of the JIDs,"
                     " see test suite logs for more details", [])
    end.

