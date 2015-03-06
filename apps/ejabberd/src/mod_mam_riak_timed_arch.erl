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

-define(BARE_JID(JID), jlib:jid_to_binary(jlib:jid_remove_resource(jlib:jid_to_lower(JID)))).

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
        Diff = timer:now_diff(EndT, StartT),
        exometer:update([Host, mam_archive_time], Diff),
        R
    catch _Type:Reason ->
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

bucket() -> <<"mam_test">>.

archive_message(_, _, MessID, _ArchiveID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    LocalJID = ?BARE_JID(LocJID),
    RemoteJID = ?BARE_JID(RemJID),
    SourceJID = ?BARE_JID(SrcJID),
    MsgId = integer_to_binary(MessID),
    Key = key(LocalJID, RemoteJID, MsgId),
    Obj = riakc_obj:new(bucket(), Key, encode_riak_obj(SourceJID, Packet)),
    mongoose_riak:put(Obj).

lookup_messages(_Result, _Host, _ArchiveID, ArchiveJID, _RSM, _Borders, Start, End,
                _Now, WithJID, _PageSize, LimitPassed, MaxResultLimit, _IsSimple) ->
    OwnerJID = ?BARE_JID(ArchiveJID),

    F = fun(Bucket, Key, {Cnt, Msgs} = Acc) ->
        case mongoose_riak:get(Bucket, Key) of
            {ok, Obj} ->
                {SourceJID, Packet} = decode_riak_obj(riakc_obj:get_value(Obj)),
                [_, _, MsgId] = decode_key(Key),
                %% increment count and add message to the list
                {Cnt + 1, [{binary_to_integer(MsgId), jlib:binary_to_jid(SourceJID), Packet} | Msgs]};
            _ ->
                Acc
        end
    end,
    KeyFilters = bucket_key_filters(OwnerJID, WithJID, Start, End),
    {TotalCount, Result} = fold_archive(F, KeyFilters, {0, []}),
    SortFun = fun({MsgId1, _, _}, {MsgId2, _, _}) ->
        MsgId1 =< MsgId2
    end,
    SortedResult = lists:sort(SortFun, Result),
    Offset = 0,
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};
        _ ->
            {ok, {TotalCount, Offset, SortedResult}}
     end.


remove_archive(_Host, _ArchiveID, ArchiveJID) ->
    fold_archive(fun delete_key_fun/3, bucket_key_filters(?BARE_JID(ArchiveJID)), undefined).

purge_single_message(_Result, _Host, MessID, _ArchiveID, ArchiveJID, _Now) ->
    ArchiveJIDBin = ?BARE_JID(ArchiveJID),
    KeyFilters = bucket_key_filters(ArchiveJIDBin, MessID),
    fold_archive(fun delete_key_fun/3, KeyFilters, undefined).

purge_multiple_messages(_Result, _Host, _ArchiveID, ArchiveJID, _Borders, Start, End, _Now, WithJID) ->
    ArchiveJIDBin = ?BARE_JID(ArchiveJID),
    KeyFilters = bucket_key_filters(ArchiveJIDBin, WithJID, Start, End),
    fold_archive(fun delete_key_fun/3, KeyFilters, undefined).

delete_key_fun(Bucket, Key, _) ->
    mongoose_riak:delete(Bucket, Key, [{dw, 2}]).


key(LocalJID, RemoteJID, MsgId) ->
    <<LocalJID/binary, $/, RemoteJID/binary, $/, MsgId/binary>>.

decode_key(KeyBinary) ->
    binary:split(KeyBinary, <<"/">>, [global]).

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

bucket_key_filters(Jid) ->
    {bucket(), key_filters(Jid)}.

bucket_key_filters(LocalJid, MsgId) when is_integer(MsgId) ->
    StartsWith = key_filters(LocalJid),
    EndsWith = [[<<"ends_with">>, integer_to_binary(MsgId)]],
    {bucket(), [[<<"and">>, StartsWith, EndsWith]]};
bucket_key_filters(LocalJid, RemoteJid) ->
    {bucket(), key_filters(LocalJid, RemoteJid)}.

bucket_key_filters(LocalJid, RemoteJid, undefined, undefined) ->
    bucket_key_filters(LocalJid, RemoteJid);
bucket_key_filters(LocalJid, RemoteJid, Start, End) ->
    {bucket(), key_filters(LocalJid, RemoteJid, Start, End)}.

key_filters(Jid) ->
    [[<<"starts_with">>,Jid]].

key_filters(LocalJid, undefined) ->
    key_filters(LocalJid);
key_filters(LocalJid, RemoteJid) ->
    RemoteJidBin = ?BARE_JID(RemoteJid),
    [[<<"starts_with">>, <<LocalJid/binary, $/, RemoteJidBin/binary>>]].

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
