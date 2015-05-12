%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mod_mam_riak_timed_arch).

-behaviour(ejabberd_gen_mam_archive).
-behaviour(gen_mod).

-include("ejabberd.hrl").

%% API
-export([start/2,
         stop/1,
         archive_size/4,
         archive_message/9,
         lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

-export([safe_archive_message/9,
         safe_lookup_messages/14]).

-export([key/3]).

%% For tests only
-export([create_obj/4, read_archive/7, bucket/1, get_message/3]).


start(Host, Opts) ->
    start_chat_archive(Host, Opts).

start_chat_archive(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, safe_archive_message, 50)
    end,
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, safe_lookup_messages, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50).

stop(Host) ->
    stop_chat_archive(Host).

stop_chat_archive(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, safe_archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, safe_lookup_messages, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

safe_archive_message(Result, Host, MessID, UserID,
                     LocJID, RemJID, SrcJID, Dir, Packet) ->
    try
        StartT = os:timestamp(),
        R = archive_message(Result, Host, MessID, UserID,
                            LocJID, RemJID, SrcJID, Dir, Packet),
        EndT = os:timestamp(),
        case R of
            ok ->
                Diff = timer:now_diff(EndT, StartT),
                exometer:update([Host, mam_archive_time], Diff);
            Other ->
                ?WARNING_MSG("Could not write message to archive, reason: ~p", [Other]),
                ejabberd_hooks:run(mam_drop_message, Host, [Host])
        end,
        R
    catch _Type:Reason ->
        ?WARNING_MSG("Could not write message to archive, reason: ~p", [Reason]),
        ejabberd_hooks:run(mam_drop_message, Host, [Host]),
        {error, Reason}
    end.

safe_lookup_messages({error, Reason}=Result, _Host,
                     _UserID, _UserJID, _RSM, _Borders,
                     _Start, _End, _Now, _WithJID,
                     _PageSize, _LimitPassed, _MaxResultLimit,
                     _IsSimple) ->
                     Result;
safe_lookup_messages(Result, Host,
                     UserID, UserJID, RSM, Borders,
                     Start, End, Now, WithJID,
                     PageSize, LimitPassed, MaxResultLimit,
                     IsSimple) ->
    try
        StartT = os:timestamp(),
        R = lookup_messages(Result, Host,
            UserID, UserJID, RSM, Borders,
            Start, End, Now, WithJID,
            PageSize, LimitPassed, MaxResultLimit,
            IsSimple),
        EndT = os:timestamp(),
        Diff = timer:now_diff(EndT, StartT),
        exometer:update([Host, mam_lookup_time], Diff),
        R
    catch _Type:Reason ->
        {error, Reason}
    end.

archive_size(Size, _Host, _ArchiveID, _ArchiveJID) ->
    Size.

%% use correct bucket for given date
-spec bucket(calendar:date()) -> binary().
bucket({_, _, _} = Date) ->
    bucket(calendar:iso_week_number(Date));

bucket({Year, Week}) ->
    YearBin = integer_to_binary(Year),
    WeekNumBin = integer_to_binary(Week),
    <<"mam_",YearBin/binary, "_", WeekNumBin/binary>>.

archive_message(_, _, MessID, _ArchiveID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    LocalJID = bare_jid(LocJID),
    RemoteJID = bare_jid(RemJID),
    SourceJID = bare_jid(SrcJID),
    MsgId = integer_to_binary(MessID),
    Key = key(LocalJID, RemoteJID, MsgId),
    Obj = create_obj(MessID, Key, SourceJID, Packet),
    mongoose_riak:put(Obj).

create_obj(MsgId, Key, SourceJID, Packet) ->
    {MicroSec, _} = mod_mam_utils:decode_compact_uuid(MsgId),
    MsgNow = mod_mam_utils:microseconds_to_now(MicroSec),
    {MsgDate, _} = calendar:now_to_datetime(MsgNow),
    riakc_obj:new(bucket(MsgDate), Key, encode_riak_obj(SourceJID, Packet)).

lookup_messages(_Result, Host, _ArchiveID, ArchiveJID, _RSM, _Borders, Start, End,
                _Now, WithJID, PageSize, LimitPassed, MaxResultLimit, _IsSimple) ->
    OwnerJID = bare_jid(ArchiveJID),
    RemoteJID = bare_jid(WithJID),
    F = fun get_message/3,
    MaxBuckets = get_max_buckets(Host),
    {TotalCount, Result} = read_archive(OwnerJID, RemoteJID, Start, End, PageSize, MaxBuckets, F),

    SortedResult = lists:sublist(sort_messages(Result), PageSize),
    Offset = 0,
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};
        _ ->
            {ok, {TotalCount, Offset, SortedResult}}
     end.

get_max_buckets(Host) ->
    MaxBuckets = gen_mod:get_module_opt(Host, ?MODULE, archive_size, 53),
    MaxBuckets.

get_message(Bucket, Key, {Cnt, Msgs} = Acc) ->
    case mongoose_riak:get(Bucket, Key) of
        {ok, Obj} ->
            {SourceJID, Packet} = decode_riak_obj(riakc_obj:get_value(Obj)),
            [_, _, MsgId] = decode_key(Key),
            %% increment count and add message to the list
            {Cnt + 1, [{binary_to_integer(MsgId), jlib:binary_to_jid(SourceJID), Packet} | Msgs]};
        _ ->
            Acc
    end.

remove_archive(Host, _ArchiveID, ArchiveJID) ->
    fold_buckets(fun delete_key_fun/3, get_max_buckets(Host),
                 [bare_jid(ArchiveJID), undefined, undefined, undefined], undefined).

purge_single_message(_Result, Host, MessID, _ArchiveID, ArchiveJID, _Now) ->
    ArchiveJIDBin = bare_jid(ArchiveJID),
    KeyFilters = [ArchiveJIDBin, MessID, undefined, undefined],
    fold_buckets(fun delete_key_fun/3, get_max_buckets(Host),
                 KeyFilters, undefined).

purge_multiple_messages(_Result, Host, _ArchiveID, ArchiveJID, _Borders, Start, End, _Now, WithJID) ->
    ArchiveJIDBin = bare_jid(ArchiveJID),
    KeyFilters = [ArchiveJIDBin, WithJID, Start, End],
    fold_buckets(fun delete_key_fun/3, get_max_buckets(Host),
                 KeyFilters, undefined).

delete_key_fun(Bucket, Key, _) ->
    mongoose_riak:delete(Bucket, Key, [{dw, 2}]).


key(LocalJID, RemoteJID, MsgId) ->
    <<LocalJID/binary, $/, RemoteJID/binary, $/, MsgId/binary>>.

decode_key(KeyBinary) ->
    binary:split(KeyBinary, <<"/">>, [global]).

-spec read_archive(binary(),
                   binary() | undefined,
                   term(),
                   term(),
                   integer() | undefined,
                   integer(),
                   fun()) ->
    {integer(), list()} | {error, term()}.
read_archive(OwnerJID, WithJID, Start, End, MaxResults, MaxBuckets, Fun) ->
    OldestYearWeek = get_oldest_year_week(Start, MaxBuckets),
    NewestYearWeek = get_newest_year_week(End, MaxBuckets),
    do_read_archive(OldestYearWeek, NewestYearWeek, {0, []},
                    {OwnerJID, WithJID, Start, End, MaxResults, Fun}).

do_read_archive(OldestYearWeek, CurrentYearWeek, Acc, _)
    when CurrentYearWeek < OldestYearWeek ->
    Acc;
do_read_archive(OldestYearWeek, CurrentYearWeek, Acc,
                {OwnerJID, WithJID, Start, End, MaxResults, Fun} = Spec) ->
    KeyFilters = bucket_key_filters(CurrentYearWeek, OwnerJID, WithJID, Start, End),
    {Cnt, _} = NewAcc = fold_archive(Fun, KeyFilters, Acc),
    case Cnt < MaxResults of
        true ->
            PrevWeek = prev_week(CurrentYearWeek),
            do_read_archive(OldestYearWeek, PrevWeek, NewAcc, Spec);
        _ ->
            NewAcc
    end.

sort_messages(Msgs) ->
    SortFun = fun({MsgId1, _, _}, {MsgId2, _, _}) ->
        MsgId1 =< MsgId2
    end,
    lists:sort(SortFun, Msgs).

fold_archive(Fun, KeyFilters, InitialAcc) ->
    Client = mongoose_riak:get_worker(),
    Result = riakc_pb_socket:mapred(Client, KeyFilters, []),
    case Result of
        {ok, []} ->
            InitialAcc;
        {ok, [{0, BucketKeys} | _]} ->
            do_fold_archive(Fun, BucketKeys, InitialAcc);
        {error, R} = Err ->
            ?WARNING_MSG("Error reading archive key_filters=~p, reason=~p", [KeyFilters, R]),
            Err
    end.

do_fold_archive(Fun, BucketKeys, InitialAcc) ->
    lists:foldl(fun({Bucket, Key}, Acc) ->
        Fun(Bucket, Key, Acc)
    end, InitialAcc, BucketKeys).


fold_buckets(Fun, MaxBuckets, KeyFiltersOpts, InitialAcc) ->
    {OldestYearWeek, NewestYearWeek} = get_week_boundaries(MaxBuckets, KeyFiltersOpts),
    do_fold_buckets(OldestYearWeek, NewestYearWeek, Fun, KeyFiltersOpts, InitialAcc).

do_fold_buckets(OldestYearWeek, CurrentYearWeek, _, _, Acc)
    when CurrentYearWeek < OldestYearWeek ->
    Acc;
do_fold_buckets(OldestYearWeek, CurrentYearWeek, Fun, KeyFilterOpts, Acc) ->
    AllKeyFilterOpts = [CurrentYearWeek | KeyFilterOpts],
    KeyFilters = erlang:apply(fun bucket_key_filters/5, AllKeyFilterOpts),
    NewAcc = fold_archive(Fun, KeyFilters, Acc),
    do_fold_buckets(OldestYearWeek, prev_week(CurrentYearWeek), Fun, KeyFilterOpts, NewAcc).

get_week_boundaries(MaxBuckets, [_, _, Start, End]) ->
    OldestYearWeek = get_oldest_year_week(Start, MaxBuckets),
    NewestYearWeek = get_newest_year_week(End, MaxBuckets),
    {OldestYearWeek, NewestYearWeek}.

bucket_key_filters(YearWeek, LocalJid, MsgId) when is_integer(MsgId) ->
    StartsWith = key_filters(LocalJid),
    EndsWith = [[<<"ends_with">>, integer_to_binary(MsgId)]],
    {bucket(YearWeek), [[<<"and">>, StartsWith, EndsWith]]};
bucket_key_filters(YearWeek, LocalJid, RemoteJid) ->
    {bucket(YearWeek), key_filters(LocalJid, RemoteJid)}.

bucket_key_filters(YearWeek, LocalJid, RemoteJid, undefined, undefined) ->
    bucket_key_filters(YearWeek, LocalJid, RemoteJid);
bucket_key_filters(YearWeek, LocalJid, RemoteJid, Start, End) ->
    {bucket(YearWeek), key_filters(LocalJid, RemoteJid, Start, End)}.

key_filters(Jid) ->
    [[<<"starts_with">>,Jid]].

key_filters(LocalJid, undefined) ->
    key_filters(LocalJid);
key_filters(LocalJid, RemoteJid) ->
    [[<<"starts_with">>, <<LocalJid/binary, $/, RemoteJid/binary>>]].

key_filters(LocalJid, RemoteJid, Start, End) ->
    JidFilter = key_filters(LocalJid, RemoteJid),
    IdFilter = id_filters(Start, End),
    [[<<"and">>, JidFilter, IdFilter]].

id_filters(Start, undefined) ->
    StartInt = mod_mam_utils:encode_compact_uuid(Start, 0),
    transform() ++ [[<<"greater_than">>, StartInt]];
id_filters(undefined, End) ->
    EndInt = mod_mam_utils:encode_compact_uuid(End, 1000),
    transform() ++ [[<<"less_than">>, EndInt]];
id_filters(Start, End) ->
    StartInt = mod_mam_utils:encode_compact_uuid(Start, 0),
    EndInt = mod_mam_utils:encode_compact_uuid(End, 1000),
    transform() ++ [[<<"between">>, StartInt, EndInt, true]].

transform() ->
    [[<<"tokenize">>, <<"/">>, 3], [<<"string_to_int">>]].


encode_riak_obj(SourceJID, Packet) ->
    term_to_binary({SourceJID, Packet}).

decode_riak_obj(Binary) ->
    binary_to_term(Binary).

bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jlib:jid_to_binary(jlib:jid_remove_resource(jlib:jid_to_lower(JID))).

get_oldest_year_week(undefined, MaxBuckets) ->
    {Date, _} = calendar:local_time(),
    CurrentDays = calendar:date_to_gregorian_days(Date),
    OldestDay = CurrentDays - 7 * (MaxBuckets - 1),
    OldestDate = calendar:gregorian_days_to_date(OldestDay),
    calendar:iso_week_number(OldestDate);
get_oldest_year_week(Start, MaxBuckets) ->
    {Date, _} = mod_mam_utils:microseconds_to_datetime(Start),
    OldestByStart = calendar:iso_week_number(Date),
    OldestByConfig = get_oldest_year_week(undefined, MaxBuckets),
    erlang:max(OldestByStart, OldestByConfig).

get_newest_year_week(undefined, _) ->
    {Date, _} = calendar:local_time(),
    calendar:iso_week_number(Date);
get_newest_year_week(Microsec, MaxBuckets) when is_integer(Microsec) ->
    {Date, _} = mod_mam_utils:microseconds_to_datetime(Microsec),
    OldestByConfig = get_oldest_year_week(undefined, MaxBuckets),
    YearWeekByEnd = calendar:iso_week_number(Date),
    erlang:max(OldestByConfig, YearWeekByEnd).

prev_week({Year, 1}) ->
    FirstYearWeekDays = calendar:date_to_gregorian_days({Year, 1, 1}),
    LastYearWeekPrevYearDays = FirstYearWeekDays - 7,
    calendar:iso_week_number(calendar:gregorian_days_to_date(LastYearWeekPrevYearDays));
prev_week({Year, Week}) ->
    {Year, Week - 1}.