-module(riak_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/src/ejabberd_c2s.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).


all() -> [{group, mam_put},
          {group, mam_read}].

groups() ->
    [{mam_put, [], [put_in_different_buckets]},
     {mam_read, [{repeat_until_any_fail,3}],
                [read_all_from_all_buckets,
                 read_with_jid_from_all_buckets,
                 read_all_start_from_different_buckets,
                 read_with_jid_start_from_different_buckets,
                 read_all_end_from_different_buckets,
                 read_with_jid_end_from_different_buckets,
                 read_all_start_end_from_different_buckets,
                 read_with_jid_start_end_from_different_buckets
     ]}].

init_per_suite(C) ->
    application:start(p1_stringprep),
    Parent = self(),
    F = fun() ->
        mim_ct_sup:start_link(ejabberd_sup),
        R = mongoose_riak_sup:start(3, [["localhost", 8087]]),
        case R of
            {ok, _} ->
                set_global_mock(),
                Parent ! {self(), started},
                receive stop -> ok end,
                unset_global_mock();
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

end_per_suite(C) ->
    Pid = ?config(pid, C),
    Pid ! stop,
    C.

init_per_group(G, Config) ->
    [{group, G} | Config].

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    case ?config(group, Config) of
        mam_put ->
            Config;
        _ ->
            generate_msgs_for_testcase(Config)
    end.

end_per_testcase(_, Config) ->
    case ?config(group, Config) of
        mam_put ->
            Config;
        _ ->
            clear_msgs_after_testcase(Config)
    end.

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
        clear_buckets(dict:fetch_keys(MsgsInWeeks))
    end.

read_all_from_all_buckets(Config) ->
    do_read_testcase(Config, false, false, false).

read_with_jid_from_all_buckets(Config) ->
    do_read_testcase(Config, true, false, false).

read_all_start_from_different_buckets(Config) ->
    do_read_testcase(Config, false, true, false).


read_with_jid_start_from_different_buckets(Config) ->
    do_read_testcase(Config, true, true, false).


read_all_end_from_different_buckets(Config) ->
    do_read_testcase(Config, false, false, true).

read_with_jid_end_from_different_buckets(Config) ->
    do_read_testcase(Config, true, false, true).

read_all_start_end_from_different_buckets(Config) ->
    do_read_testcase(Config, false, true, true).

read_with_jid_start_end_from_different_buckets(Config) ->
    do_read_testcase(Config, true, true, true).


do_read_testcase(Config, TakeBoth, UseStart, UseEnd) ->
    Users = ?config(users, Config),

    UserPairs = generate_pairs(Users, TakeBoth),

    [read_archive(Config, UserPair, UseStart, UseEnd)
        || UserPair <- UserPairs].

generate_pairs(Users, true) ->
    [{From, To} || From <- Users, To <- Users, To /= From];
generate_pairs(Users, _) ->
    [{From, undefined} || From <- Users].

read_archive(Config, UserPair, UseStart, UseEnd) ->
    Weeks = ?config(weeks, Config),

    SortFun = fun({MsgID1, _, _, _, _}, {MsgID2, _, _, _, _}) ->
        {Micro1, _} = mod_mam_utils:decode_compact_uuid(MsgID1),
        {Micro2, _} = mod_mam_utils:decode_compact_uuid(MsgID2),
        Micro1 =< Micro2
    end,

    UsersArchive = ?config(archive, Config),
    UserArchiveWhole = lists:sort(SortFun, dict:fetch(UserPair, UsersArchive)),

    UserArchive = select_archive_part(UserArchiveWhole, UseStart, UseEnd),
    {Start, End} = select_timestamps(UserArchive, UseStart, UseEnd),

    {OwnerJID, RemoteJID} = UserPair,

    F = fun mod_mam_riak_timed_arch:get_message/3,
    {L, RiakMsgs} = mod_mam_riak_timed_arch:read_archive(OwnerJID, RemoteJID,
                                                         Start, End,
                                                         undefined, Weeks, F),

    F2 = fun({MsgID, _, _, _, _} = Msg) ->
        case lists:keymember(MsgID, 1, RiakMsgs) of
            false ->
                throw({missing_msg, UserPair, Msg});
            _ ->
                ok
        end
    end,

    lists:foreach(F2, UserArchive),
    L = length(UserArchive).

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

clear_buckets(YearWeeks) ->
    [clear_bucket(bucket(Year, Week)) || {Year, Week} <- YearWeeks].

clear_bucket(Bucket) ->
    {ok, Keys} = mongoose_riak:list_keys(Bucket),
    [mongoose_riak:delete(Bucket, Key, [{dw, 2}]) || Key <- Keys].

int_to_binary(MsgId) ->
    list_to_binary(integer_to_list(MsgId)).

users_archive(Entries) ->
    F = fun({_Date, {_, From, To, _, _}  = Msg}, AccDict) ->
        Dict1 = dict:append({From, undefined}, Msg, AccDict),
        Dict2 = dict:append({To, undefined}, Msg, Dict1),
        Dict3 = dict:append({From, To}, Msg, Dict2),
                dict:append({To, From}, Msg, Dict3)
    end,
    lists:foldl(F, dict:new(), Entries).

generate_msgs_for_testcase(Config) ->
    Weeks = 4,
    Users = generate_users(4),
    Entries = generate_msgs_in_different_buckets(Weeks, {2,4}, Users),
    put_messages(Entries),
    UsersArchive = users_archive(Entries),
    [{users, Users}, {msgs, Entries}, {weeks, Weeks}, {archive, UsersArchive} | Config].

clear_msgs_after_testcase(Config) ->
    Weeks = ?config(weeks, Config),
    YearWeeks = [calendar:iso_week_number(Date) || Date <- generate_dates(Weeks)],
    clear_buckets(YearWeeks).

select_archive_part(Archive, false, false) ->
    Archive;
select_archive_part(Archive, true, false) ->
    Length = length(Archive),
    HowManyToDiscard = random:uniform(Length div 2) + 2,
    lists:nthtail(HowManyToDiscard, Archive);
select_archive_part(Archive, false, true) ->
    Length = length(Archive),
    HowManyToTake = random:uniform(Length div 2) + 2,
    lists:sublist(Archive, HowManyToTake);
select_archive_part(Archive, true, true) ->
    A1 = select_archive_part(Archive, true, false),
    select_archive_part(A1, false, true).

select_timestamps(Archive, Start, End) ->
    {select_start_time(Archive, Start),
     select_end_time(Archive, End)}.

select_start_time(_, false) ->
    undefined;
select_start_time(Archive, _) ->
    get_time_from_msg(hd(Archive)).

select_end_time(_, false) ->
    undefined;
select_end_time(Archive, _) ->
    get_time_from_msg(lists:last(Archive)).

get_time_from_msg({MsgId, _, _, _, _}) ->
    {Micro, _} = mod_mam_utils:decode_compact_uuid(MsgId),
    Micro.

set_global_mock() ->
    meck:new(exometer, []),
    meck:expect(exometer, update, fun(_, _) -> ok end).

unset_global_mock() ->
    meck:unload(exometer).