%%%----------------------------------------------------------------------
%%% File    : mod_offline_riak.erl
%%% Author  : Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_offline backend in Riak
%%%
%%%
%%% MongooseIM, Copyright (C) 2015      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------

%%% @doc Riak backend for last activity XEP
%%%
%%% The backend uses the existing riak connection pool, which is "globally" defined in
%%% the mongooseim.toml file. Therefore, we don't need to start anything in the init
%%% function.
%%%
%%% The module follows the approach taken by the other riak backends - it creates
%%% the following bucket `{<<"offline">>, <<"example.com">>}' for each xmpp domain.
%%%
%%% ```
%%% Data Layout:
%%% KV: {Username_and_timestamp:binary, Status:binary}
%%% 2i: [Username:binary, Timestamp:integer]
%%% '''
%%% @end
-module(mod_offline_riak).

-behaviour(mod_offline_backend).

-export([init/2]).
-export([pop_messages/2]).
-export([fetch_messages/2]).
-export([write_messages/4]).
-export([count_offline_messages/4]).
-export([remove_expired_messages/2]).
-export([remove_old_messages/3]).
-export([remove_user/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").
-include_lib("riakc/include/riakc.hrl").

-define(TIMESTAMP_IDX, {integer_index, "timestamp"}).
-define(USER_IDX,      {binary_index, "user"}).
-define(EXPIRE_IDX,    {integer_index, "expire"}).

-define(INFINITY, 99999999999). %% Wed, 16 Nov 5138 09:46:39 GMT

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec pop_messages(mongooseim:host_type(), jid:jid()) -> {ok, [mod_offline:msg()]}.
pop_messages(HostType, To = #jid{luser = LUser, lserver = LServer}) ->
    Keys = read_user_idx(HostType, LUser, LServer),
    Msgs = [pop_msg(HostType, Key, LUser, LServer, To) || Key <- Keys],
    {ok, lists:flatten(Msgs)}.

-spec fetch_messages(mongooseim:host_type(), jid:jid()) -> {ok, [mod_offline:msg()]}.
fetch_messages(HostType, To) ->
    {LUser, LServer} = jid:to_lus(To),
    Keys = read_user_idx(HostType, LUser, LServer),
    {ok, [fetch_msg(HostType, Key, LUser, LServer, To) || Key <- Keys]}.

-spec write_messages(mongooseim:host_type(), jid:luser(), jid:lserver(), [mod_offline:msg()]) -> ok.
write_messages(HostType, LUser, LServer, Msgs) ->
    [write_msg(HostType, LUser, LServer, Msg) || Msg <- Msgs],
    ok.

-spec count_offline_messages(mongooseim:host_type(), jid:luser(), jid:lserver(),
                             mod_offline:msg_count()) ->
          mod_offline:msg_count().
count_offline_messages(HostType, LUser, LServer, MaxArchivedMsgs) ->
    {ok, IdxResult} = mongoose_riak:get_index(bucket_type(HostType, LServer), ?USER_IDX,
                                              LUser, [{max_results, MaxArchivedMsgs}]),
    length(IdxResult?INDEX_RESULTS.keys).

-spec remove_expired_messages(mongooseim:host_type(), jid:lserver()) -> {ok, mod_offline:msg_count()}.
remove_expired_messages(HostType, LServer) ->
    TimestampInt = os:system_time(microsecond),
    {ok, Result} = mongoose_riak:get_index_range(bucket_type(HostType, LServer), ?EXPIRE_IDX,
                                                 0, TimestampInt, []),
    Keys = Result?INDEX_RESULTS.keys,
    [mongoose_riak:delete(bucket_type(HostType, LServer), Key) || Key <- Keys],
    {ok, length(Keys)}.

-spec remove_old_messages(mongooseim:host_type(), jid:lserver(), mod_offline:timestamp()) ->
          {ok, mod_offline:msg_count()}.
remove_old_messages(HostType, LServer, TimestampInt) ->
    {ok, Result} = mongoose_riak:get_index_range(bucket_type(HostType, LServer), ?TIMESTAMP_IDX,
                                                 0, TimestampInt, []),
    Keys = Result?INDEX_RESULTS.keys,
    [mongoose_riak:delete(bucket_type(HostType, LServer), Key) || Key <- Keys],
    {ok, length(Keys)}.

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(HostType, LUser, LServer) ->
    Keys = read_user_idx(HostType, LUser, LServer),
    [mongoose_riak:delete(bucket_type(HostType, LServer), Key) || Key <- Keys],
    ok.

read_user_idx(HostType, LUser, LServer) ->
    {ok, IdxResult} = mongoose_riak:get_index(bucket_type(HostType, LServer), ?USER_IDX,
                                              LUser, []),
    IdxResult?INDEX_RESULTS.keys.

write_msg(HostType, LUser, LServer, #offline_msg{from = FromJID, packet = Packet,
                                                 timestamp = TimestampIn, expire = Expire,
                                                 permanent_fields = PermanentFields}) ->
    Timestamp = TimestampIn,
    Obj = riakc_obj:new(bucket_type(HostType, LServer), key(LUser, Timestamp), exml:to_binary(Packet)),
    MD = riakc_obj:get_update_metadata(Obj),
    SecondaryIndexes = [{?TIMESTAMP_IDX, [Timestamp]},
                        {?USER_IDX, [LUser]},
                        {?EXPIRE_IDX, [maybe_encode_timestamp(Expire)]}],
    MDWithIndexes = riakc_obj:set_secondary_index(MD, SecondaryIndexes),
    From = jid:to_binary(FromJID),
    UserMetaData = [{<<"from">>, From},
                    {<<"permanent_fields">>, term_to_binary(PermanentFields)}],
    MDWithUserData = set_obj_user_metadata(MDWithIndexes, UserMetaData),
    FinalObj = riakc_obj:update_metadata(Obj, MDWithUserData),
    mongoose_riak:put(FinalObj).

set_obj_user_metadata(MD, []) ->
    MD;
set_obj_user_metadata(MD, [UserMD | Rest]) ->
    UpdatedMD = riakc_obj:set_user_metadata_entry(MD, UserMD),
    set_obj_user_metadata(UpdatedMD, Rest).

pop_msg(HostType, Key, LUser, LServer, To) ->
    try
        {ok, Obj} = mongoose_riak:get(bucket_type(HostType, LServer), Key),

        PacketRaw = riakc_obj:get_value(Obj),
        {ok, Packet} = exml:parse(PacketRaw),
        MD = riakc_obj:get_update_metadata(Obj),
        [Timestamp] = riakc_obj:get_secondary_index(MD, ?TIMESTAMP_IDX),
        [Expire] = riakc_obj:get_secondary_index(MD, ?EXPIRE_IDX),
        From = riakc_obj:get_user_metadata_entry(MD, <<"from">>),
        PermanentFields = extract_permanent_fields(MD),

        mongoose_riak:delete(bucket_type(HostType, LServer), Key),

        #offline_msg{us = {LUser, LServer},
                     timestamp = Timestamp,
                     expire = maybe_decode_timestamp(Expire),
                     from = jid:from_binary(From),
                     to = To,
                     packet = Packet,
                     permanent_fields = PermanentFields}

    catch
        Class:Reason:StackTrace ->
            ?LOG_WARNING(#{what => offline_riak_reading_key_failed,
                           text => <<"mod_offline_riak failed to read key">>,
                           server => LServer, user => LUser, riak_key => Key,
                           class => Class, reason => Reason, stacktrace => StackTrace}),
            []
    end.

extract_permanent_fields(MD) ->
    case riakc_obj:get_user_metadata_entry(MD, <<"permanent_fields">>) of
        notfound -> [];
        Fields -> binary_to_term(Fields)
    end.

-spec bucket_type(mongooseim:host_type(), jid:lserver()) -> {binary(), jid:lserver()}.
bucket_type(HostType, LServer) ->
    {gen_mod:get_module_opt(LServer, mod_offline, [riak, bucket_type]), HostType}.

-spec key(binary(), integer()) -> binary().
key(LUser, TimestampInt) ->
    Timestamp = integer_to_binary(TimestampInt),
    Random = integer_to_binary(rand:uniform(1024)),
    <<LUser/binary, "@", Timestamp/binary, "@", Random/binary>>.

maybe_encode_timestamp(never) ->
    ?INFINITY;
maybe_encode_timestamp(TS) ->
    TS.

maybe_decode_timestamp(?INFINITY) ->
    never;
maybe_decode_timestamp(TS) ->
    TS.

fetch_msg(HostType, Key, LUser, LServer, To) ->
    try
        {ok, Obj} = mongoose_riak:get(bucket_type(HostType, LServer), Key),

        PacketRaw = riakc_obj:get_value(Obj),
        {ok, Packet} = exml:parse(PacketRaw),
        MD = riakc_obj:get_update_metadata(Obj),
        [Timestamp] = riakc_obj:get_secondary_index(MD, ?TIMESTAMP_IDX),
        From = riakc_obj:get_user_metadata_entry(MD, <<"from">>),
        [Expire] = riakc_obj:get_secondary_index(MD, ?EXPIRE_IDX),

        #offline_msg{us = {LUser, LServer},
            timestamp = Timestamp,
            expire = maybe_decode_timestamp(Expire),
            from = jid:from_binary(From),
            to = To,
            packet = Packet}

    catch
        Class:Reason:StackTrace ->
            ?LOG_WARNING(#{what => offline_riak_reading_key_failed,
                           text => <<"mod_offline_riak failed to read key">>,
                           server => LServer, user => LUser, riak_key => Key,
                           class => Class, reason => Reason, stacktrace => StackTrace}),
            []
    end.

