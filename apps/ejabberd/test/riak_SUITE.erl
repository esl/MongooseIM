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

groups() ->
    [{mam, [], [put_in_different_buckets]}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

put_in_different_buckets(Config) ->
    Entries = generate_msgs_in_different_buckets(2, {2, 4}, 3),
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

generate_msgs_in_different_buckets(BucketsNo, MsgsFromGivenUserRange, DifferentUsersNo) ->
    random:seed(now()),
    Dates = generate_dates(BucketsNo),
    Users = [generate_user() || _ <- lists:seq(1, DifferentUsersNo)],
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
     || From <- Users, To <- Users].

generate_msgs_in_week(_, From, From, _) ->
    [];
generate_msgs_in_week(Date, From, To, {MinMsgs, MaxMsgs}) ->
    Diff = MaxMsgs - MinMsgs,
    Msgs = random:uniform(MinMsgs) + Diff,
    [generate_msg_in_week(Date, From, To) || _ <- lists:seq(1, Msgs)].

generate_msg_in_week(Date, From, To) ->
    DateTimeInWeek = date_time_in_week(Date),
    Microseconds = mod_mam_utils:datetime_to_microseconds(DateTimeInWeek),
    MsgId = mod_mam_utils:encode_compact_uuid(Microseconds, random:uniform(20)),
    Packet = {xmlel, <<"message">>, [{<<"to">>, To}], [{xmlcdata, <<"ala ma kota">>}]},
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

generate_user() ->
    Username = base16:encode(crypto:rand_bytes(5)),
    <<Username/binary, "@localhost">>.

put_messages(Entries) ->
    [put_message(Entry) || Entry <- Entries].

put_message({_Date, {MsgId, From, To, SourceId, Packet}}) ->
    Key = mod_mam_riak_timed_arch:key(From, To, msgid_to_binary(MsgId)),
    RiakObj = mod_mam_riak_timed_arch:create_obj(MsgId, Key, SourceId, Packet),
    mongoose_riak:put(RiakObj),
    ok.

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
        Key = mod_mam_riak_timed_arch:key(From, To, msgid_to_binary(MsgId)),
        true = lists:member(Key, KeysInRiak)
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
    [mongoose_riak:delete(Bucket, Key) || Key <- Keys].

msgid_to_binary(MsgId) ->
    list_to_binary(integer_to_list(MsgId)).