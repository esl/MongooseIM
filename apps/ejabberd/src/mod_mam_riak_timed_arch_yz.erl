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
-module(mod_mam_riak_timed_arch_yz).

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
-export([create_obj/3, read_archive/7, bucket/1,
         list_mam_buckets/0, remove_bucket/1]).

-define(YZ_SEARCH_INDEX, <<"mam_test_index3">>).
-define(MAM_BUCKET_TYPE, <<"mam_yz_test_map3">>).

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
        R = archive_message(Result, Host, MessID, UserID,
                            LocJID, RemJID, SrcJID, Dir, Packet),
        case R of
            ok ->
                ok;
            Other ->
                throw(Other)
        end,
        R
    catch _Type:Reason ->
        ?WARNING_MSG("Could not write message to archive, reason: ~p", [Reason]),
        ejabberd_hooks:run(mam_drop_message, Host, [Host]),
        {error, Reason}
    end.

safe_lookup_messages({error, _Reason} = Result, _Host,
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
        lookup_messages(Result, Host,
            UserID, UserJID, RSM, Borders,
            Start, End, Now, WithJID,
            PageSize, LimitPassed, MaxResultLimit,
            IsSimple)
    catch _Type:Reason ->
        {error, Reason}
    end.

archive_size(Size, _Host, _ArchiveID, _ArchiveJID) ->
    Size.

%% use correct bucket for given date

-spec bucket(calendar:date() | integer()) -> binary().
bucket(MsgId) when is_integer(MsgId) ->
    {MicroSec, _} = mod_mam_utils:decode_compact_uuid(MsgId),
    MsgNow = mod_mam_utils:microseconds_to_now(MicroSec),
    {MsgDate, _} = calendar:now_to_datetime(MsgNow),
    bucket(MsgDate);
bucket({_, _, _} = Date) ->
    bucket(calendar:iso_week_number(Date));
bucket({Year, Week}) ->
    YearBin = integer_to_binary(Year),
    WeekNumBin = integer_to_binary(Week),
    {?MAM_BUCKET_TYPE, <<"mam_",YearBin/binary, "_", WeekNumBin/binary>>}.

list_mam_buckets() ->
    {ok, Buckets} = riakc_pb_socket:list_buckets(mongoose_riak:get_worker(), ?MAM_BUCKET_TYPE),
    [{?MAM_BUCKET_TYPE, Bucket} || Bucket <- Buckets].


remove_bucket(Bucket) ->
    {ok, Keys} = mongoose_riak:list_keys(Bucket),
    [mongoose_riak:delete(Bucket, Key) || Key <- Keys].

archive_message(_, _, MessID, _ArchiveID, LocJID, RemJID, SrcJID, _Dir, Packet) ->
    LocalJID = bare_jid(LocJID),
    RemoteJID = bare_jid(RemJID),
    SourceJID = bare_jid(SrcJID),
    MsgId = integer_to_binary(MessID),
    Key = key(LocalJID, RemoteJID, MsgId),

    Bucket = bucket(MessID),

    RiakMap = create_obj(MsgId, SourceJID, Packet),
    mongoose_riak:update_type(Bucket, Key, riakc_map:to_op(RiakMap)).

create_obj(MsgId, SourceJID, Packet) ->

    Ops = [{{<<"msg_id">>, register},
            fun(R) -> riakc_register:set(MsgId, R) end},
           {{<<"source_jid">>, register},
            fun(R) -> riakc_register:set(SourceJID, R) end},
           {{<<"packet">>, register},
            fun(R) -> riakc_register:set(exml:to_binary(Packet), R) end}],

    mongoose_riak:create_new_map(Ops).

lookup_messages(_Result, Host, _ArchiveID, ArchiveJID, _RSM, _Borders, Start, End,
                _Now, WithJID, PageSize, LimitPassed, MaxResultLimit, _IsSimple) ->
    OwnerJID = bare_jid(ArchiveJID),
    RemoteJID = bare_jid(WithJID),
    F = fun get_msg_id_key/3,
    MaxBuckets = get_max_buckets(Host),
    {TotalCount, Result} = read_archive(OwnerJID, RemoteJID, Start, End, PageSize, MaxBuckets, F),

    SortedKeys = lists:sublist(sort_messages(Result), PageSize),
    Offset = 0,
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};
        _ ->
            {ok, {TotalCount, Offset, get_messages(SortedKeys)}}
     end.

get_max_buckets(Host) ->
    MaxBuckets = gen_mod:get_module_opt(Host, ?MODULE, archive_size, 53),
    MaxBuckets.

get_msg_id_key(Bucket, Key, Msgs) ->
    [_, _, MsgId] = decode_key(Key),
    Item = {binary_to_integer(MsgId), Bucket, Key},
    [Item | Msgs].

get_messages(BucketKeys) ->
    lists:flatten([get_message2(MsgId, Bucket, Key) || {MsgId, Bucket, Key} <- BucketKeys]).

get_message2(MsgId, Bucket, Key) ->
    case mongoose_riak:fetch_type(Bucket, Key) of
        {ok, RiakMap} ->
            SourceJID = riakc_map:fetch({<<"source_jid">>, register}, RiakMap),
            PacketBin = riakc_map:fetch({<<"packet">>, register}, RiakMap),
            {ok, Packet} = exml:parse(PacketBin),
            {MsgId, jlib:binary_to_jid(SourceJID), Packet};
        _ ->
            []
    end.

remove_archive(Host, _ArchiveID, ArchiveJID) ->
    {ok, TotalCount, _, _} = R = remove_chunk(Host, ArchiveJID, 0),
    Result = do_remove_archive(100, R, Host, ArchiveJID),
    case Result of
        {stopped, N} ->
            lager:warning("archive removal stopped for jid after processing ~p items out of ~p total",
                          [ArchiveJID, N, TotalCount]);
        {ok, _} ->
            ok
    end,
    Result.

remove_chunk(Host, ArchiveJID, Acc) ->
    fold_buckets(fun delete_key_fun/3, get_max_buckets(Host),
        [bare_jid(ArchiveJID), undefined, undefined, undefined],
        [{rows, 50}, {sort, <<"msg_id_register asc">>}], Acc).

do_remove_archive(0, {ok, _, _, Acc}, _, _) ->
    {stopped, Acc};
do_remove_archive(_, {ok, 0, _, Acc}, _, _) ->
    {ok, Acc};
do_remove_archive(N, {ok, _TotalResults, _RowsIterated, Acc}, Host, ArchiveJID) ->
    timer:sleep(1000), %% give Riak some time to clear after just removed keys
    R = remove_chunk(Host, ArchiveJID, Acc),
    do_remove_archive(N-1, R, Host, ArchiveJID).

purge_single_message(_Result, Host, MessID, _ArchiveID, ArchiveJID, _Now) ->
    ArchiveJIDBin = bare_jid(ArchiveJID),
    KeyFilters = [ArchiveJIDBin, MessID, undefined, undefined],
    {ok, 1, 1, 1} = fold_buckets(fun delete_key_fun/3, get_max_buckets(Host),
                 KeyFilters, [], 0),
    ok.

purge_multiple_messages(_Result, Host, _ArchiveID, ArchiveJID, _Borders, Start, End, _Now, WithJID) ->
    ArchiveJIDBin = bare_jid(ArchiveJID),
    KeyFilters = [ArchiveJIDBin, WithJID, Start, End],
    {ok, Total, _Iterated, Deleted} = fold_buckets(fun delete_key_fun/3, get_max_buckets(Host),
                 KeyFilters, [{rows, 50}, {sort, <<"msg_id_register asc">>}], 0),
    case Total == Deleted of
        true ->
            ok;
        _ ->
            lager:warning("not all messages have been purged for user ~p", [ArchiveJID]),
            ok
    end.

delete_key_fun(Bucket, Key, N) ->
    ok = mongoose_riak:delete(Bucket, Key, [{dw, 2}]),
    N + 1.


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
    do_read_archive(OldestYearWeek, NewestYearWeek, [{rows, MaxResults}], [],
                    {OwnerJID, WithJID, Start, End, Fun}).

do_read_archive(OldestYearWeek, CurrentYearWeek, _SearchOpts, Acc, _)
    when CurrentYearWeek < OldestYearWeek ->
    Acc;
do_read_archive(_OldestYearWeek, CurrentYearWeek, SearchOpts, Acc,
                {OwnerJID, WithJID, Start, End, Fun}) ->
    KeyFilters = bucket_key_filters(CurrentYearWeek, OwnerJID, WithJID, Start, End),
    {ok, Cnt, _, NewAcc} = fold_archive(Fun, KeyFilters, SearchOpts, Acc),
    {Cnt, NewAcc}.


sort_messages(Msgs) ->
    SortFun = fun({MsgId1, _, _}, {MsgId2, _, _}) ->
        MsgId1 =< MsgId2
    end,
    lists:sort(SortFun, Msgs).

fold_archive(Fun, {_Bucket, Query}, SearchOpts, InitialAcc) ->
    Result = mongoose_riak:search(?YZ_SEARCH_INDEX, Query, SearchOpts),
    case Result of
        {ok, {search_results, [], _, Count}} ->
            {ok, Count, 0, InitialAcc};
        {ok, {search_results, Results, _Score, Count}} ->
            {ok, Count, length(Results), do_fold_archive(Fun, Results, InitialAcc)};
        {error, R} = Err ->
            ?WARNING_MSG("Error reading archive key_filters=~p, reason=~p", [Query, R]),
            Err
    end.

do_fold_archive(Fun, BucketKeys, InitialAcc) ->
    lists:foldl(fun({_Index, Props}, Acc) ->
        {_, Bucket} = lists:keyfind(<<"_yz_rb">>, 1, Props),
        {_, Type} = lists:keyfind(<<"_yz_rt">>, 1, Props),
        {_ , Key} = lists:keyfind(<<"_yz_rk">>, 1, Props),
        Fun({Type, Bucket}, Key, Acc)
    end, InitialAcc, BucketKeys).


fold_buckets(Fun, MaxBuckets, KeyFiltersOpts, SearchOpts, InitialAcc) ->
    {OldestYearWeek, NewestYearWeek} = get_week_boundaries(MaxBuckets, KeyFiltersOpts),
    do_fold_buckets(OldestYearWeek, NewestYearWeek, Fun, KeyFiltersOpts, SearchOpts, InitialAcc).

do_fold_buckets(_OldestYearWeek, CurrentYearWeek, Fun, KeyFilterOpts, SearchOpts, Acc) ->
    AllKeyFilterOpts = [CurrentYearWeek | KeyFilterOpts],
    KeyFilters = erlang:apply(fun bucket_key_filters/5, AllKeyFilterOpts),
    fold_archive(Fun, KeyFilters, SearchOpts, Acc).
%%     do_fold_buckets(OldestYearWeek, prev_week(CurrentYearWeek), Fun, KeyFilterOpts, NewAcc).

get_week_boundaries(MaxBuckets, [_, _, Start, End]) ->
    OldestYearWeek = get_oldest_year_week(Start, MaxBuckets),
    NewestYearWeek = get_newest_year_week(End, MaxBuckets),
    {OldestYearWeek, NewestYearWeek}.

bucket_key_filters(YearWeek, LocalJid, MsgId) when is_integer(MsgId) ->
    StartsWith = key_filters(LocalJid),
    MsgIdBin = integer_to_binary(MsgId),
    Q = <<StartsWith/binary, " AND msg_id_register:", MsgIdBin/binary>>,
    {bucket(YearWeek), Q};
bucket_key_filters(YearWeek, LocalJid, RemoteJid) ->
    {bucket(YearWeek), key_filters(LocalJid, RemoteJid)}.

bucket_key_filters(YearWeek, LocalJid, RemoteJid, undefined, undefined) ->
    bucket_key_filters(YearWeek, LocalJid, RemoteJid);
bucket_key_filters(YearWeek, LocalJid, RemoteJid, Start, End) ->
    {bucket(YearWeek), key_filters(LocalJid, RemoteJid, Start, End)}.

key_filters(Jid) ->
    <<"_yz_rk:",Jid/binary,"*">>.

key_filters(LocalJid, undefined) ->
    key_filters(LocalJid);
key_filters(LocalJid, RemoteJid) ->
    <<"_yz_rk:",LocalJid/binary,"/", RemoteJid/binary,"*">>.

key_filters(LocalJid, RemoteJid, Start, End) ->
    JidFilter = key_filters(LocalJid, RemoteJid),
    IdFilter = id_filters(Start, End),
    <<JidFilter/binary, " AND ", IdFilter/binary>>.

id_filters(Start, undefined) ->
    StartInt = mod_mam_utils:encode_compact_uuid(Start, 0),
    solr_id_filters(integer_to_binary(StartInt), <<"*">>);
id_filters(undefined, End) ->
    EndInt = mod_mam_utils:encode_compact_uuid(End, 1000),
    solr_id_filters(<<"*">>, integer_to_binary(EndInt));
id_filters(Start, End) ->
    StartInt = mod_mam_utils:encode_compact_uuid(Start, 0),
    EndInt = mod_mam_utils:encode_compact_uuid(End, 1000),
    solr_id_filters(integer_to_binary(StartInt), integer_to_binary(EndInt)).

solr_id_filters(Start, End) ->
    <<"msg_id_register:[",Start/binary," TO ", End/binary," ]">>.

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