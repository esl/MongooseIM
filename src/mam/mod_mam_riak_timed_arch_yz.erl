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
-behaviour(mongoose_module_metrics).

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
         remove_archive/4]).

-export([archive_message/9,
         archive_message_muc/9,
         lookup_messages/3,
         lookup_messages_muc/3]).

-export([key/3]).

%% For tests only
-export([create_obj/6, read_archive/8, bucket/2,
         list_mam_buckets/1, remove_bucket/1]).

-export([get_mam_muc_gdpr_data/2, get_mam_pm_gdpr_data/2]).

-type yearweeknum() :: {non_neg_integer(), 1..53}.

-define(DUMMY_LOOKUP_PARAMETERS, #{with_jid => undefined,
                                   owner_jid => undefined,
                                   rsm => undefined,
                                   page_size => undefined,
                                   borders => undefined,
                                   start_ts => undefined,
                                   end_ts => undefined,
                                   search_text => undefined,
                                   is_simple => true}).

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
    ejabberd_hooks:add(get_mam_pm_gdpr_data, Host, ?MODULE, get_mam_pm_gdpr_data, 50).

start_muc_archive(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message_muc, 50),
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages_muc, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(get_mam_muc_gdpr_data, Host, ?MODULE, get_mam_muc_gdpr_data, 50).

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
    ejabberd_hooks:delete(get_mam_pm_gdpr_data, Host, ?MODULE, get_mam_pm_gdpr_data, 50),
    ok.

stop_muc_archive(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(get_mam_muc_gdpr_data, Host, ?MODULE, get_mam_muc_gdpr_data, 50),
    ok.

yz_search_index(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, search_index, <<"mam">>).

mam_bucket_type(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, bucket_type, <<"mam_yz">>).

%% LocJID - archive owner's JID
%% RemJID - interlocutor's JID
%% SrcJID - "Real" sender JID
archive_message(_Result, Host, MessId, _UserID, LocJID, RemJID, SrcJID, _Dir, Packet) ->
    try
        archive_message(Host, MessId, LocJID, RemJID, SrcJID, LocJID, Packet, pm)
    catch _Type:Reason:StackTrace ->
            ?WARNING_MSG("Could not write message to archive, reason: ~p",
                         [{Reason, StackTrace}]),
            ejabberd_hooks:run(mam_drop_message, Host, [Host]),
            {error, Reason}
    end.

%% LocJID - MUC/MUC Light room's JID
%% FromJID - "Real" sender JID
%% SrcJID - Full JID of user within room (room@domain/user)
archive_message_muc(_Result, Host, MessId, _UserID, LocJID, FromJID, SrcJID, _Dir, Packet) ->
    RemJIDMuc = maybe_muc_jid(SrcJID),
    try
        archive_message(Host, MessId, LocJID, RemJIDMuc, SrcJID, FromJID, Packet, muc)
    catch _Type:Reason:StackTrace ->
        ?WARNING_MSG("Could not write MUC message to archive, reason: ~p",
                     [{Reason, StackTrace}]),
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
    catch _Type:Reason:S ->
              {error, {Reason, {stacktrace, S}}}
    end.


lookup_messages_muc(Result, Host, #{with_jid := WithJID} = Params) ->
    WithJIDMuc = maybe_muc_jid(WithJID),
    lookup_messages(Result, Host, Params#{with_jid => WithJIDMuc}).


archive_size(_Size, Host, _ArchiveID, ArchiveJID) ->
    OwnerJID = mod_mam_utils:bare_jid(ArchiveJID),
    RemoteJID = undefined,
    {MsgIdStartNoRSM, MsgIdEndNoRSM} =
        mod_mam_utils:calculate_msg_id_borders(undefined, undefined, undefined),
    F = fun get_msg_id_key/3,
    {TotalCount, _} = read_archive(Host, OwnerJID, RemoteJID,
                                   MsgIdStartNoRSM, MsgIdEndNoRSM, undefined,
                                   [{rows, 1}], F),
    TotalCount.

%% use correct bucket for given date

-spec bucket(jid:server(), calendar:date() | yearweeknum() | integer()) ->
                    {binary(), binary()} | undefined.
bucket(Host, MsgId) when is_integer(MsgId) ->
    {MicroSec, _} = mod_mam_utils:decode_compact_uuid(MsgId),
    MsgNow = mod_mam_utils:microseconds_to_now(MicroSec),
    {MsgDate, _} = calendar:now_to_datetime(MsgNow),
    bucket(Host, MsgDate);
bucket(Host, {_, _, _} = Date) ->
    bucket(Host, calendar:iso_week_number(Date));
bucket(Host, {Year, Week}) ->
    YearBin = integer_to_binary(Year),
    WeekNumBin = integer_to_binary(Week),
    {mam_bucket_type(Host), <<"mam_", YearBin/binary, "_", WeekNumBin/binary>>};
bucket(_Host, _) ->
    undefined.

list_mam_buckets(Host) ->
    Type = mam_bucket_type(Host),
    {ok, Buckets} = riakc_pb_socket:list_buckets(mongoose_riak:get_worker(), Type),
    [{Type, Bucket} || Bucket <- Buckets].


remove_bucket(Bucket) ->
    {ok, Keys} = mongoose_riak:list_keys(Bucket),
    [mongoose_riak:delete(Bucket, Key) || Key <- Keys].


%% PM:
%%  * LocJID - archive owner's JID
%%  * RemJID - interlocutor's JID
%%  * SrcJID - "Real" sender JID
%%  * OwnerJID - Same as LocJID
%% MUC / MUC Light:
%%  * LocJID - MUC/MUC Light room's JID
%%  * RemJID - Nickname of JID of destination
%%  * SrcJID - Full JID of user within room (room@domain/user)
%%  * OwnerJID - "Real" sender JID (not room specific)
archive_message(Host, MessID, LocJID, RemJID, SrcJID, OwnerJID, Packet, Type) ->
    LocalJID = mod_mam_utils:bare_jid(LocJID),
    RemoteJID = mod_mam_utils:bare_jid(RemJID),
    SourceJID = mod_mam_utils:full_jid(SrcJID),
    BareOwnerJID = mod_mam_utils:bare_jid(OwnerJID),
    MsgId = integer_to_binary(MessID),
    Key = key(LocalJID, RemoteJID, MsgId),

    Bucket = bucket(Host, MessID),

    RiakMap = create_obj(Host, MsgId, SourceJID, BareOwnerJID, Packet, Type),
    case mongoose_riak:update_type(Bucket, Key, riakc_map:to_op(RiakMap)) of
        ok -> ok;
        Other -> throw(Other)
    end.

create_obj(Host, MsgId, SourceJID, BareOwnerJID, Packet, Type) ->
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
           {{<<"msg_owner_jid">>, register},
            fun(R) -> riakc_register:set(BareOwnerJID, R) end},
           {{<<"mam_type">>, register},
            fun(R) -> riakc_register:set(atom_to_binary(Type, latin1), R) end},
           {{<<"packet">>, register},
            fun(R) -> riakc_register:set(packet_to_stored_binary(Host, Packet), R) end},
           {{<<"search_text">>, register},
            fun(R) -> riakc_register:set(BodyValue, R) end}
          ],

    mongoose_riak:create_new_map(Ops).

lookup_messages(Host, #{rsm := #rsm_in{direction = before, id = ID} = RSM} = Params)
  when ID =/= undefined ->
    lookup_message_page(Host, RSM, Params);
lookup_messages(Host, #{rsm := #rsm_in{direction = aft, id = ID} = RSM} = Params)
  when ID =/= undefined ->
    lookup_message_page(Host, RSM, Params);
lookup_messages(Host, Params) ->
    do_lookup_messages(Host, Params).

lookup_message_page(Host, RSM, Params) ->
    PageSize = maps:get(page_size, Params),
    {ok, Result} = do_lookup_messages(Host, Params#{page_size := 1 + PageSize}),
    mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result).

do_lookup_messages(Host, Params) ->
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
    {TotalCountFullQuery, Result} = read_archive(Host, OwnerJID, RemoteJID,
                                                 MsgIdStart, MsgIdEnd, SearchText,
                                                 SearchOpts, F),

    SortedKeys = sort_messages(Result),
    case maps:get(is_simple, Params) of
        true ->
            {ok, {undefined, undefined, get_messages(Host, SortedKeys)}};
        _ ->
            {MsgIdStartNoRSM, MsgIdEndNoRSM} =
            mod_mam_utils:calculate_msg_id_borders(Borders, Start, End),
            {TotalCount, _} = read_archive(Host, OwnerJID, RemoteJID,
                                           MsgIdStartNoRSM, MsgIdEndNoRSM, SearchText,
                                           [{rows, 1}], F),
            SLen = length(SortedKeys),
            Args = {Host, OwnerJID, RemoteJID, MsgIdStartNoRSM, SearchText},
            Offset = calculate_offset(RSM, TotalCountFullQuery, SLen, Args),
            {ok, {TotalCount, Offset, get_messages(Host, SortedKeys)}}
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
calculate_offset(#rsm_in{direction = aft, id = Id}, _, _,
                 {Host, Owner, Remote, MsgIdStart, SearchText})
  when Id /= undefined ->
    {Count, _} = read_archive(Host, Owner, Remote,
                              MsgIdStart, Id, SearchText,
                              [{rows, 1}], fun get_msg_id_key/3),
    Count;
calculate_offset(#rsm_in{direction = undefined, index = Index}, _, _, _)
  when is_integer(Index) ->
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
-spec get_mam_pm_gdpr_data(ejabberd_gen_mam_archive:mam_pm_gdpr_data(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(Acc, OwnerJid) ->
    Messages = get_mam_gdpr_data(OwnerJid, <<"pm">>),
    [{Id, jid:to_binary(Jid), exml:to_binary(Packet)} || {Id, Jid, Packet} <- Messages] ++ Acc.

-spec get_mam_muc_gdpr_data(ejabberd_gen_mam_archive:mam_muc_gdpr_data(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(Acc, JID) ->
    Messages = get_mam_gdpr_data(JID, <<"muc">>),
    [{MsgId, exml:to_binary(Packet)} || {MsgId, _, Packet} <- Messages] ++ Acc.

get_mam_gdpr_data(#jid{ lserver = LServer } = BareJid, Type) ->
    BareLJidBin = jid:to_binary(jid:to_lower(BareJid)),
    Query = <<"msg_owner_jid_register:", BareLJidBin/binary, " AND mam_type_register:", Type/binary>>,
    SearchOpts = [],
    {ok, _Cnt, _, MsgIds} = fold_archive(LServer, fun get_msg_id_key/3, Query, SearchOpts, []),
    get_messages(LServer, MsgIds).

remove_archive(Acc, Host, _ArchiveID, ArchiveJID) ->
    remove_archive(Host, ArchiveJID),
    Acc.

remove_archive(Host, ArchiveJID) ->
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

remove_chunk(Host, ArchiveJID, Acc) ->
    KeyFiletrs = key_filters(mod_mam_utils:bare_jid(ArchiveJID)),
    fold_archive(Host,
                 fun delete_key_fun/3,
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

delete_key_fun(Bucket, Key, N) ->
    ok = mongoose_riak:delete(Bucket, Key, [{dw, 2}]),
    N + 1.


key(LocalJID, RemoteJID, MsgId) ->
    <<LocalJID/binary, $/, RemoteJID/binary, $/, MsgId/binary>>.

decode_key(KeyBinary) ->
    binary:split(KeyBinary, <<"/">>, [global]).

-spec read_archive(jid:server(),
                   binary() | undefined,
                   binary() | undefined,
                   term(),
                   term(),
                   binary() | undefined,
                   [term()],
                   fun()) ->
                          {integer(), list()} | {error, term()}.
read_archive(Host, OwnerJID, WithJID, Start, End, SearchText, SearchOpts, Fun) ->
    KeyFilters = key_filters(OwnerJID, WithJID, Start, End, SearchText),
    {ok, Cnt, _, NewAcc} = fold_archive(Host, Fun, KeyFilters, SearchOpts, []),
    {Cnt, NewAcc}.


sort_messages(Msgs) ->
    SortFun = fun({MsgId1, _, _}, {MsgId2, _, _}) ->
                      MsgId1 =< MsgId2
              end,
    lists:sort(SortFun, Msgs).

fold_archive(Host, Fun, Query, SearchOpts, InitialAcc) ->
    Result = mongoose_riak:search(yz_search_index(Host), Query, SearchOpts),
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
    Separator = <<"~1 AND search_text_register:">>,
    NormText = mod_mam_utils:normalize_search_text(SearchText, Separator),
    %% Fuzzy search on tokens from search phrase
    <<"search_text_register:", NormText/binary, "~1">>.

jid_filters(LocalJid, undefined) ->
    <<"_yz_rk:", LocalJid/binary, "/*/*">>;
jid_filters(LocalJid, RemoteJid) ->
    <<"_yz_rk:", LocalJid/binary, "/", RemoteJid/binary, "/*">>.

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
