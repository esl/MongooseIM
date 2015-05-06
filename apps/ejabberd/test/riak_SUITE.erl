-module(riak_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/src/ejabberd_c2s.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-compile([export_all]).


-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).

-define(B(C), (proplists:get_value(backend, C))).
-define(MAX_USER_SESSIONS, 2).


all() -> [{group, mam}].

groups() ->
    [{mam, [], [put_in_different_buckets, read_all_from_different_buckets]}].

init_per_suite(C) ->
    application:start(p1_stringprep),
    Parent = self(),
    F = fun() ->
        mim_ct_sup:start_link(ejabberd_sup),
        R = mongoose_riak_sup:start(3, [["localhost", 8087]]),
        case R of
            {ok, _} ->
                Parent ! {self(), started},
                receive stop -> ok end;
            _ ->
                ok
        end
    end,
    Pid = spawn(F),
    receive
        {Pid, started} ->
            [{pid, Pid} | C]
    after 2000 ->
        {skip, "Riak is not running"}
    end.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

put_in_different_buckets(_Config) ->
    Users = generate_users(3),
    Entries = generate_msgs_in_different_buckets(5, {2, 4}, Users),
    put_messages(Entries),
    MsgsInWeeks = msgs_in_weeks(Entries),
    try
        assert_all_msgs_in_their_buckets(MsgsInWeeks)
    catch
        Class:Reason  ->
            ct:fail("assert msgs in buckets failed ~p:~p", [Class, Reason])
    after
        clear_buckets(MsgsInWeeks)
    end.

read_all_from_different_buckets(_Config) ->
    Weeks = 3,
    Users = generate_users(3),
    Entries = generate_msgs_in_different_buckets(Weeks, {2,4}, Users),
    put_messages(Entries),
    MsgsInWeeks = msgs_in_weeks(Entries),
    UsersArchive = users_archive(Entries),
    UserPairs = [{From, To} || From <- Users, To <- Users, To /= From],
    F = fun mod_mam_riak_timed_arch:get_message/3,
    try
        UserPair = hd(UserPairs),
        {OwnerJID, _RemoteJID} = UserPair,
        {L, RiakMsgs} = R = mod_mam_riak_timed_arch:read_archive(OwnerJID, undefined,
                                                 undefined, undefined,
                                                 undefined, Weeks,  F),
        UserArchive = dict:fetch(OwnerJID, UsersArchive),
        F2 = fun({_, {MsgID, _, _, _, _} = Msg}) ->
            case lists:keymember(MsgID, 1, RiakMsgs) of
                false ->
                    throw({missing_msg, Msg});
                _ ->
                    ok
            end
        end,
        lists:foreach(F2, UserArchive),
        L = length(UserArchive)
    catch
        Class:Reason  ->
            ct:fail("assert msgs read failed ~p:~p~n~p", [Class, Reason, erlang:get_stacktrace()])
    after
        clear_buckets(MsgsInWeeks)
    end.

generate_msgs_in_different_buckets(BucketsNo, MsgsFromGivenUserRange, Users) ->
    random:seed(now()),
    Dates = generate_dates(BucketsNo),
    Msgs = [generate_msgs_in_week(Date, Users, MsgsFromGivenUserRange) || Date <- Dates],
    lists:flatten(Msgs).

generate_dates(BucketsNo) ->
    {CurrentDate, _} = calendar:local_time(),
    CurrentGregorianDay = calendar:date_to_gregorian_days(CurrentDate),
    StartGregorianDay = CurrentGregorianDay - 7 * (BucketsNo - 1),
    ListOfDays = lists:seq(StartGregorianDay, CurrentGregorianDay, 7),
    [calendar:gregorian_days_to_date(Days) || Days <- ListOfDays].

generate_msgs_in_week(Date, Users, MsgsFromGivenUserRange) ->
    [generate_msgs_in_week(Date, From, To, MsgsFromGivenUserRange)
     || From <- Users, To <- Users, To /= From].

generate_msgs_in_week(Date, From, To, {MinMsgs, MaxMsgs}) ->
    Diff = MaxMsgs - MinMsgs,
    Msgs = random:uniform(MinMsgs) + Diff,
    [generate_msg_in_week(Date, From, To) || _ <- lists:seq(1, Msgs)].

generate_msg_in_week(Date, From, To) ->
    DateTimeInWeek = date_time_in_week(Date),
    Microseconds = mod_mam_utils:datetime_to_microseconds(DateTimeInWeek),
    MsgId = mod_mam_utils:encode_compact_uuid(Microseconds, random:uniform(20)),
    Packet = {xmlel, <<"message">>, [{<<"to">>, To}], [{xmlcdata, base16:encode(crypto:rand_bytes(4))}]},
    {Date, {MsgId, From, To, From, Packet}}.

date_time_in_week(Date) ->
    WeekDay = calendar:day_of_the_week(Date),
    GregorianDays = calendar:date_to_gregorian_days(Date),
    WeekStartDay = GregorianDays - (WeekDay - 1),
    RandDay = random:uniform(7) - 1,
    DayInWeek = WeekStartDay + RandDay,
    RandDateInWeek = calendar:gregorian_days_to_date(DayInWeek),
    MaxSecondsInDay = 86399,
    RandSeconds = random:uniform(MaxSecondsInDay),
    {RandDateInWeek, calendar:seconds_to_time(RandSeconds)}.

generate_users(DifferentUsersNo) ->
    [generate_user() || _ <- lists:seq(1, DifferentUsersNo)].

generate_user() ->
    Username = base16:encode(crypto:rand_bytes(1)),
    MicroBin = int_to_binary(mod_mam_utils:now_to_microseconds(now())),
    <<Username/binary, MicroBin/binary, "@localhost">>.

put_messages(Entries) ->
    [put_message(Entry) || Entry <- Entries].

put_message({_Date, {MsgId, From, To, SourceId, Packet}}) ->
    KeySender = mod_mam_riak_timed_arch:key(From, To, int_to_binary(MsgId)),
    KeyReceiver = mod_mam_riak_timed_arch:key(To, From, int_to_binary(MsgId)),
    RiakObjSender = mod_mam_riak_timed_arch:create_obj(MsgId, KeySender, SourceId, Packet),
    RiakObjReceiver = mod_mam_riak_timed_arch:create_obj(MsgId, KeyReceiver, SourceId, Packet),
    ok = mongoose_riak:put(RiakObjSender),
    ok = mongoose_riak:put(RiakObjReceiver).

assert_all_msgs_in_their_buckets(MsgsInWeeks) ->
    true = dict:fold(fun verify_bucket/3, false, MsgsInWeeks).


msgs_in_weeks(Entries) ->
    Fun = fun({Date, Msg}, DictAcc) ->
        YearWeek = calendar:iso_week_number(Date),
        dict:append(YearWeek, Msg, DictAcc)
    end,
    lists:foldl(Fun, dict:new(), Entries).

verify_bucket({Year, Week}, Msgs, _) ->
    {ok, KeysInRiak} = mongoose_riak:list_keys(bucket(Year, Week)),
    true = lists:all(fun({MsgId, From, To, _, _}) ->
        KeySender = mod_mam_riak_timed_arch:key(From, To, int_to_binary(MsgId)),
        KeyReceiver = mod_mam_riak_timed_arch:key(To, From, int_to_binary(MsgId)),
        true = lists:member(KeySender, KeysInRiak),
        true = lists:member(KeyReceiver, KeysInRiak)
    end, Msgs).

bucket(Year, WeekNum) ->
    YearBin = integer_to_binary(Year),
    WeekNumBin = integer_to_binary(WeekNum),
    <<"mam_",YearBin/binary, "_", WeekNumBin/binary>>.

clear_buckets(MsgsInWeeks) ->
    YearWeeks = dict:fetch_keys(MsgsInWeeks),
    [clear_bucket(bucket(Year, Week)) || {Year, Week} <- YearWeeks].

clear_bucket(Bucket) ->
    {ok, Keys} = mongoose_riak:list_keys(Bucket),
    [mongoose_riak:delete(Bucket, Key, [{dw, 2}]) || Key <- Keys].

int_to_binary(MsgId) ->
    list_to_binary(integer_to_list(MsgId)).

users_archive(Entries) ->
    F = fun({_Date, {_, From, To, _, _}} = Msg, AccDict) ->
        Dict1 = dict:append(From, Msg, AccDict),
        dict:append(To, Msg, Dict1)
    end,
    lists:foldl(F, dict:new(), Entries).
