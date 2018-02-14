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

-callback encode(term()) -> binary().
-callback decode(binary()) -> term().


-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").

%% API
-export([start/2,
         stop/1,
         archive_size/4,
         lookup_messages/2,
         remove_archive/4,
         purge_single_message/6,
         purge_multiple_messages/9]).

-export([archive_message/9,
         archive_message_muc/9,
         lookup_messages/3,
         lookup_messages_muc/3]).

-export([key/3]).

%% For tests only
-export([create_obj/5, read_archive/7, bucket/1,
         list_mam_buckets/0, remove_bucket/1]).

-type yearweeknum() :: {non_neg_integer(), 1..53}.

-define(YZ_SEARCH_INDEX, <<"mam">>).
-define(MAM_BUCKET_TYPE, <<"mam_yz">>).

%% @doc Start module
%%
%% Options:
%% - `pm' option starts one-to-one chat archives
%% - `muc' option starts multichat archives
%%
%% Use both options `pm, muc' to archive both MUC and private messages
start(Host, Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            start_chat_archive(Host, Opts);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            start_muc_archive(Host, Opts);
        false ->
            ok
    end.

start_chat_archive(Host, _Opts) ->
    ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50).

start_muc_archive(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message_muc, 50),
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages_muc, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50).

stop(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            stop_chat_archive(Host);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            stop_muc_archive(Host);
        false ->
            ok
    end.

stop_chat_archive(Host) ->
    ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, archive_message_muc, 50),
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, lookup_messages_muc, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

stop_muc_archive(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE,
                          purge_multiple_messages, 50),
    ok.

archive_message(_Result, Host, MessId, _UserID, LocJID, RemJID, SrcJID, _Dir, Packet) ->
    try
        archive_message(Host, MessId, LocJID, RemJID, SrcJID, Packet, pm)
    catch _Type:Reason ->
            ?WARNING_MSG("Could not write message to archive, reason: ~p",
                         [{Reason, erlang:get_stacktrace()}]),
            ejabberd_hooks:run(mam_drop_message, Host, [Host]),
            {error, Reason}
    end.

archive_message_muc(_Result, Host, MessId, _UserID, LocJID, RemJID, SrcJID, _Dir, Packet) ->
    RemJIDMuc = maybe_muc_jid(RemJID),
    try
        archive_message(Host, MessId, LocJID, RemJIDMuc, SrcJID, Packet, muc)
    catch _Type:Reason ->
        ?WARNING_MSG("Could not write MUC message to archive, reason: ~p",
                     [{Reason, erlang:get_stacktrace()}]),
        ejabberd_hooks:run(mam_muc_drop_message, Host, [Host]),
        {error, Reason}
    end.

maybe_muc_jid(#jid{lresource = RemRes}) ->
    {<<>>, RemRes, <<>>};
maybe_muc_jid(Other) ->
    Other.


lookup_messages({error, _Reason} = Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, Host, Params) ->
    try
        lookup_messages(Host, Params)
    catch _Type:Reason ->
              S = erlang:get_stacktrace(),
              {error, {Reason, {stacktrace, S}}}
    end.


lookup_messages_muc(Result, Host, #{with_jid := WithJID} = Params) ->
    WithJIDMuc = maybe_muc_jid(WithJID),
    lookup_messages(Result, Host, Params#{with_jid => WithJIDMuc}).


archive_size(_Size, _Host, _ArchiveID, ArchiveJID) ->
    OwnerJID = mod_mam_utils:bare_jid(ArchiveJID),
    RemoteJID = undefined,
    {MsgIdStartNoRSM, MsgIdEndNoRSM} =
    mod_mam_utils:calculate_msg_id_borders(undefined, undefined, undefined, undefined),
    F = fun get_msg_id_key/3,
    {TotalCount, _} = read_archive(OwnerJID, RemoteJID,
                                   MsgIdStartNoRSM, MsgIdEndNoRSM, undefined,
                                   [{rows, 1}], F),
    TotalCount.

%% use correct bucket for given date

-spec bucket(calendar:date() | yearweeknum() | integer()) ->
                    {binary(), binary()} | undefined.
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
    {?MAM_BUCKET_TYPE, <<"mam_", YearBin/binary, "_", WeekNumBin/binary>>};
bucket(_) ->
    undefined.

list_mam_buckets() ->
    {ok, Buckets} = riakc_pb_socket:list_buckets(mongoose_riak:get_worker(), ?MAM_BUCKET_TYPE),
    [{?MAM_BUCKET_TYPE, Bucket} || Bucket <- Buckets].


remove_bucket(Bucket) ->
    {ok, Keys} = mongoose_riak:list_keys(Bucket),
    [mongoose_riak:delete(Bucket, Key) || Key <- Keys].

archive_message(Host, MessID, LocJID, RemJID, SrcJID, Packet, Type) ->
    LocalJID = mod_mam_utils:bare_jid(LocJID),
    RemoteJID = mod_mam_utils:bare_jid(RemJID),
    SourceJID = mod_mam_utils:full_jid(SrcJID),
    MsgId = integer_to_binary(MessID),
    Key = key(LocalJID, RemoteJID, MsgId),

    Bucket = bucket(MessID),

    RiakMap = create_obj(Host, MsgId, SourceJID, Packet, Type),
    case mongoose_riak:update_type(Bucket, Key, riakc_map:to_op(RiakMap)) of
        ok -> ok;
        Other -> throw(Other)
    end.

create_obj(Host, MsgId, SourceJID, Packet, Type) ->
    ModMAM =
        case Type of
            pm -> mod_mam;
            muc -> mod_mam_muc
        end,
    BodyChars = mod_mam_utils:packet_to_search_body(ModMAM, Host, Packet),
    BodyValue = unicode:characters_to_binary(BodyChars),
    Ops = [
           {{<<"msg_id">>, register},
            fun(R) -> riakc_register:set(MsgId, R) end},
           {{<<"source_jid">>, register},
            fun(R) -> riakc_register:set(SourceJID, R) end},
           {{<<"packet">>, register},
            fun(R) -> riakc_register:set(packet_to_stored_binary(Host, Packet), R) end},
           {{<<"search_text">>, register},
            fun(R) -> riakc_register:set(BodyValue, R) end}
          ],

    mongoose_riak:create_new_map(Ops).

lookup_messages(Host, Params) ->
    OwnerJID = mod_mam_utils:bare_jid(maps:get(owner_jid, Params)),
    RemoteJID = mod_mam_utils:bare_jid(maps:get(with_jid, Params)),

    RSM = maps:get(rsm, Params),

    SearchOpts2 = add_sorting(RSM, [{rows, maps:get(page_size, Params)}]),
    SearchOpts = add_offset(RSM, SearchOpts2),

    F = fun get_msg_id_key/3,

    Borders = maps:get(borders, Params),
    Start = maps:get(start_ts, Params),
    End = maps:get(end_ts, Params),
    SearchText = maps:get(search_text, Params),
    {MsgIdStart, MsgIdEnd} = mod_mam_utils:calculate_msg_id_borders(RSM, Borders, Start, End),
    {TotalCountFullQuery, Result} = read_archive(OwnerJID, RemoteJID,
                                                 MsgIdStart, MsgIdEnd, SearchText,
                                                 SearchOpts, F),

    SortedKeys = sort_messages(Result),
    case maps:get(is_simple, Params) of
        true ->
            {ok, {undefined, undefined, get_messages(Host, SortedKeys)}};
        _ ->
            {MsgIdStartNoRSM, MsgIdEndNoRSM} =
            mod_mam_utils:calculate_msg_id_borders(undefined, Borders, Start, End),
            {TotalCount, _} = read_archive(OwnerJID, RemoteJID,
                                           MsgIdStartNoRSM, MsgIdEndNoRSM, SearchText,
                                           [{rows, 1}], F),
            Offset = calculate_offset(RSM, TotalCountFullQuery, length(SortedKeys),
                                      {OwnerJID, RemoteJID, MsgIdStartNoRSM, SearchText}),
            LimitPassed = maps:get(limit_passed, Params),
            MaxResultLimit = maps:get(max_result_limit, Params),
            case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
                true ->
                    {error, 'policy-violation'};
                _ ->
                    {ok, {TotalCount, Offset, get_messages(Host, SortedKeys)}}
            end
    end.


add_sorting(#rsm_in{direction = before}, Opts) ->
    [{sort, <<"msg_id_register desc">>} | Opts];
add_sorting(_, Opts) ->
    [{sort, <<"msg_id_register asc">>} | Opts].

add_offset(#rsm_in{index = Offset}, Opts) when is_integer(Offset) ->
    [{start, Offset} | Opts];
add_offset(_, Opts) ->
    Opts.

calculate_offset(#rsm_in{direction = before}, TotalCount, PageSize, _) ->
    TotalCount - PageSize;
calculate_offset(#rsm_in{direction = aft, id = Id}, _, _, {Owner, Remote, MsgIdStart, SearchText})
  when Id /= undefined ->
    {Count, _} = read_archive(Owner, Remote, MsgIdStart, Id, SearchText,
                              [{rows, 1}], fun get_msg_id_key/3),
    Count;
calculate_offset(#rsm_in{direction = undefined, index = Index}, _, _, _) when is_integer(Index) ->
    Index;
calculate_offset(_, _TotalCount, _PageSize, _) ->
    0.

get_msg_id_key(Bucket, Key, Msgs) ->
    [_, _, MsgId] = decode_key(Key),
    Item = {binary_to_integer(MsgId), Bucket, Key},
    [Item | Msgs].

get_messages(Host, BucketKeys) ->
    lists:flatten([get_message2(Host, MsgId, Bucket, Key) || {MsgId, Bucket, Key} <- BucketKeys]).

get_message2(Host, MsgId, Bucket, Key) ->
    case mongoose_riak:fetch_type(Bucket, Key) of
        {ok, RiakMap} ->
            SourceJID = riakc_map:fetch({<<"source_jid">>, register}, RiakMap),
            PacketBin = riakc_map:fetch({<<"packet">>, register}, RiakMap),
            Packet = stored_binary_to_packet(Host, PacketBin),
            {MsgId, jid:from_binary(SourceJID), Packet};
        _ ->
            []
    end.

remove_archive(Acc, Host, ArchiveID, ArchiveJID) ->
    remove_archive(Host, ArchiveID, ArchiveJID),
    Acc.

remove_archive(Host, _ArchiveID, ArchiveJID) ->
    {ok, TotalCount, _, _} = R = remove_chunk(Host, ArchiveJID, 0),
    Result = do_remove_archive(100, R, Host, ArchiveJID),
    case Result of
        {stopped, N} ->
            lager:warning("archive removal stopped for jid after processing ~p "
                          "items out of ~p total", [ArchiveJID, N, TotalCount]),
            ok;
        {ok, _} ->
            ok
    end.

remove_chunk(_Host, ArchiveJID, Acc) ->
    KeyFiletrs = key_filters(mod_mam_utils:bare_jid(ArchiveJID)),
    fold_archive(fun delete_key_fun/3,
                 KeyFiletrs,
                 [{rows, 50}, {sort, <<"msg_id_register asc">>}], Acc).

do_remove_archive(0, {ok, _, _, Acc}, _, _) ->
    {stopped, Acc};
do_remove_archive(_, {ok, 0, _, Acc}, _, _) ->
    {ok, Acc};
do_remove_archive(N, {ok, _TotalResults, _RowsIterated, Acc}, Host, ArchiveJID) ->
    timer:sleep(1000), % give Riak some time to clear after just removed keys
    R = remove_chunk(Host, ArchiveJID, Acc),
    do_remove_archive(N-1, R, Host, ArchiveJID).

purge_single_message(_Result, _Host, MessID, _ArchiveID, ArchiveJID, _Now) ->
    ArchiveJIDBin = mod_mam_utils:bare_jid(ArchiveJID),
    KeyFilters = key_filters(ArchiveJIDBin, MessID),
    {ok, 1, 1, 1} = fold_archive(fun delete_key_fun/3, KeyFilters, [], 0),
    ok.

purge_multiple_messages(_Result, _Host, _ArchiveID,
                        ArchiveJID, _Borders, Start, End, _Now, WithJID) ->
    ArchiveJIDBin = mod_mam_utils:bare_jid(ArchiveJID),
    KeyFilters = key_filters(ArchiveJIDBin, WithJID, Start, End),
    {ok, Total, _Iterated, Deleted} =
        fold_archive(fun delete_key_fun/3,
                     KeyFilters,
                     [{rows, 50}, {sort, <<"msg_id_register asc">>}], 0),
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

-spec read_archive(binary() | undefined,
                   binary() | undefined,
                   term(),
                   term(),
                   binary() | undefined,
                   [term()],
                   fun()) ->
                          {integer(), list()} | {error, term()}.
read_archive(OwnerJID, WithJID, Start, End, SearchText, SearchOpts, Fun) ->
    KeyFilters = key_filters(OwnerJID, WithJID, Start, End, SearchText),
    {ok, Cnt, _, NewAcc} = fold_archive(Fun, KeyFilters, SearchOpts, []),
    {Cnt, NewAcc}.


sort_messages(Msgs) ->
    SortFun = fun({MsgId1, _, _}, {MsgId2, _, _}) ->
                      MsgId1 =< MsgId2
              end,
    lists:sort(SortFun, Msgs).

fold_archive(Fun, Query, SearchOpts, InitialAcc) ->
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
        {_, Key} = lists:keyfind(<<"_yz_rk">>, 1, Props),
        Fun({Type, Bucket}, Key, Acc)
    end, InitialAcc, BucketKeys).

%% Filter API
key_filters(LocalJid) ->
    key_filters(LocalJid, undefined, undefined, undefined, undefined).

key_filters(LocalJid, MsgId) ->
    key_filters(LocalJid, undefined, MsgId, MsgId, undefined).

key_filters(LocalJid, RemoteJid, Start, End) ->
    key_filters(LocalJid, RemoteJid, Start, End, undefined).


key_filters(LocalJid, RemoteJid, Start, End, SearchText) ->
    JidFilter = jid_filters(LocalJid, RemoteJid),
    IdFilter = id_filters(Start, End),
    TextFilter = search_text_filter(SearchText),

    Separator = <<" AND ">>,
    Filters0 = [JidFilter, IdFilter, TextFilter],
    Filters1 = [[Filter, Separator] || Filter <- Filters0, is_binary(Filter)],
    FiltersBin = list_to_binary(Filters1),
    binary:part(FiltersBin, 0, byte_size(FiltersBin) - byte_size(Separator)).

%% Filter helpers
-spec search_text_filter(binary() | undefined) -> binary().
search_text_filter(undefined) ->
    undefined;
search_text_filter(SearchText) ->
    Separator = "~1 AND search_text_register:",
    NormText = mod_mam_utils:normalize_search_text(SearchText, Separator) ++ "~1",
    %% Fuzzy search on tokens from search phrase
    <<"search_text_register:", (list_to_binary(NormText))/binary>>.

jid_filters(LocalJid, undefined) ->
    <<"_yz_rk:", LocalJid/binary, "*">>;
jid_filters(LocalJid, RemoteJid) ->
    <<"_yz_rk:", LocalJid/binary, "/", RemoteJid/binary, "*">>.

id_filters(undefined, undefined) ->
    undefined;
id_filters(MsgId, MsgId) ->
    MsgIdBin = integer_to_binary(MsgId),
    <<"msg_id_register:", MsgIdBin/binary>>;
id_filters(StartInt, undefined) ->
    solr_id_filters(integer_to_binary(StartInt), <<"*">>);
id_filters(undefined, EndInt) ->
    solr_id_filters(<<"*">>, integer_to_binary(EndInt));
id_filters(StartInt, EndInt) ->
    solr_id_filters(integer_to_binary(StartInt), integer_to_binary(EndInt)).

solr_id_filters(Start, End) ->
    <<"msg_id_register:[", Start/binary, " TO ", End/binary, " ]">>.

%% ----------------------------------------------------------------------
%% Optimizations

packet_to_stored_binary(Host, Packet) ->
    Module = db_message_codec(Host),
    mam_message:encode(Module, Packet).

stored_binary_to_packet(Host, Bin) ->
    Module = db_message_codec(Host),
    mam_message:decode(Module, Bin).

-spec db_message_codec(Host :: jid:server()) -> module().
db_message_codec(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, db_message_format, mam_message_xml).

