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
%%% the ejebberd.cfg file. Therefore, we don't need to start anything in the init
%%% function.
%%%
%%% The module follows the approach taken by the other riak backends - it creates
%%% the following bucket {<<"offline">>, <<"example.com">>} for each xmpp domain.
%%%
%%% Data Layout:
%%% KV: {Username_and_timestamp:binary, Status:binary}
%%% 2i: [Username:binary, Timestamp:integer]
%%%
-module(mod_offline_riak).

-behaviour(mod_offline).

-export([init/2]).
-export([pop_messages/2]).
-export([fetch_messages/2]).
-export([write_messages/3]).
-export([remove_expired_messages/1]).
-export([remove_old_messages/2]).
-export([remove_user/2]).
-export([count_offline_messages/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").
-include_lib("riakc/include/riakc.hrl").

-define(TIMESTAMP_IDX, {integer_index, "timestamp"}).
-define(USER_IDX,      {binary_index, "user"}).
-define(EXPIRE_IDX,    {integer_index, "expire"}).

-define(INFINITY, 99999999999). %% Wed, 16 Nov 5138 09:46:39 GMT

-spec init(Host :: jid:lserver(), Opts :: []) -> ok.
init(_Host, _Opts) ->
    ok.

-spec pop_messages(LUser, LServer) -> {ok, Result} | {error, Reason} when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Reason :: term(),
    Result :: list(mod_offline:msg()).
pop_messages(LUser, LServer) ->
    Keys = read_user_idx(LUser, LServer),
    To = jid:make(LUser, LServer, <<>>),
    Msgs = [pop_msg(Key, LUser, LServer, To) || Key <- Keys],
    {ok, lists:flatten(Msgs)}.

-spec write_messages(LUser, LServer, Msgs) ->
    ok  when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Msgs :: list().
write_messages(LUser, LServer, Msgs) ->
    [write_msg(LUser, LServer, Msg) || Msg <- Msgs],
    ok.

-spec count_offline_messages(LUser, LServer, MaxArchivedMsgs) ->
    integer() when
      LUser :: jid:luser(),
      LServer :: jid:lserver(),
      MaxArchivedMsgs :: integer().
count_offline_messages(LUser, LServer, MaxArchivedMsgs) ->
    {ok, IdxResult} = mongoose_riak:get_index(bucket_type(LServer), ?USER_IDX,
                                              LUser, [{max_results, MaxArchivedMsgs}]),
    length(IdxResult?INDEX_RESULTS.keys).

-spec remove_expired_messages(Host) -> {error, Reason} | {ok, Count} when
    Host :: jid:lserver(),
    Reason :: term(),
    Count :: integer().
remove_expired_messages(Host) ->
    TimestampInt = usec:from_now(os:timestamp()),
    {ok, Result} = mongoose_riak:get_index_range(bucket_type(Host), ?EXPIRE_IDX,
                                                 0, TimestampInt, []),
    Keys = Result?INDEX_RESULTS.keys,
    [mongoose_riak:delete(bucket_type(Host), Key) || Key <- Keys],
    {ok, length(Keys)}.

-spec remove_old_messages(Host, Days) -> {error, Reason} | {ok, Count} when
    Host :: jid:lserver(),
    Days :: erlang:timestamp(),
    Reason :: term(),
    Count :: integer().
remove_old_messages(Host, Timestamp) ->
    TimestampInt = usec:from_now(Timestamp),
    {ok, Result} = mongoose_riak:get_index_range(bucket_type(Host), ?TIMESTAMP_IDX,
                                                 0, TimestampInt, []),
    Keys = Result?INDEX_RESULTS.keys,
    [mongoose_riak:delete(bucket_type(Host), Key) || Key <- Keys],
    {ok, length(Keys)}.

-spec remove_user(LUser, LServer) -> any() when
    LUser :: binary(),
    LServer :: binary().

remove_user(LUser, LServer) ->
    Keys = read_user_idx(LUser, LServer),
    [mongoose_riak:delete(bucket_type(LServer), Key) || Key <- Keys].

read_user_idx(LUser, LServer) ->
    {ok, IdxResult} = mongoose_riak:get_index(bucket_type(LServer), ?USER_IDX,
                                              LUser, []),
    IdxResult?INDEX_RESULTS.keys.

write_msg(LUser, LServer, #offline_msg{from = FromJID, packet = Packet,
                                       timestamp = TimestampIn, expire = Expire}) ->
    Timestamp = usec:from_now(TimestampIn),
    Obj = riakc_obj:new(bucket_type(LServer), key(LUser, Timestamp), exml:to_binary(Packet)),
    MD = riakc_obj:get_update_metadata(Obj),
    SecondaryIndexes = [{?TIMESTAMP_IDX, [Timestamp]},
                        {?USER_IDX, [LUser]},
                        {?EXPIRE_IDX, [maybe_encode_timestamp(Expire)]}],
    MDWithIndexes = riakc_obj:set_secondary_index(MD, SecondaryIndexes),
    From = jid:to_binary(FromJID),
    UserMetaData = [{<<"from">>, From}],
    MDWithUserData = set_obj_user_metadata(MDWithIndexes, UserMetaData),
    FinalObj = riakc_obj:update_metadata(Obj, MDWithUserData),
    mongoose_riak:put(FinalObj).

set_obj_user_metadata(MD, []) ->
    MD;
set_obj_user_metadata(MD, [UserMD | Rest]) ->
    UpdatedMD = riakc_obj:set_user_metadata_entry(MD, UserMD),
    set_obj_user_metadata(UpdatedMD, Rest).

pop_msg(Key, LUser, LServer, To) ->
    try
        {ok, Obj} = mongoose_riak:get(bucket_type(LServer), Key),

        PacketRaw = riakc_obj:get_value(Obj),
        {ok, Packet} = exml:parse(PacketRaw),
        MD = riakc_obj:get_update_metadata(Obj),
        [Timestamp] = riakc_obj:get_secondary_index(MD, ?TIMESTAMP_IDX),
        [Expire] = riakc_obj:get_secondary_index(MD, ?EXPIRE_IDX),
        From = riakc_obj:get_user_metadata_entry(MD, <<"from">>),

        mongoose_riak:delete(bucket_type(LServer), Key),

        #offline_msg{us = {LUser, LServer},
                     timestamp = usec:to_now(Timestamp),
                     expire = maybe_decode_timestamp(Expire),
                     from = jid:from_binary(From),
                     to = To,
                     packet = Packet}

    catch
        Error:Reason ->
            ?WARNING_MSG("issue=~p, action=reading_key, host=~s, reason=~p, stack_trace=~p",
                         [Error, LServer, Reason, erlang:get_stacktrace()]),
            []
    end.


-spec bucket_type(jid:lserver()) -> {binary(), jid:lserver()}.
bucket_type(LServer) ->
    {<<"offline">>, LServer}.

-spec key(binary(), integer()) -> binary().
key(LUser, TimestampInt) ->
    Timestamp = integer_to_binary(TimestampInt),
    Random = integer_to_binary(rand:uniform(1024)),
    <<LUser/binary, "@", Timestamp/binary, "@", Random/binary>>.

maybe_encode_timestamp(never) ->
    ?INFINITY;
maybe_encode_timestamp(TS) ->
    usec:from_now(TS).

maybe_decode_timestamp(?INFINITY) ->
    never;
maybe_decode_timestamp(TS) ->
    usec:to_now(TS).


fetch_messages(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nodeprep(Server),
    Keys = read_user_idx(LUser, LServer),
    To = jid:make({User, LServer, <<>>}),
    {ok, [fetch_msg(Key, LUser, LServer, To) || Key <- Keys]}.

fetch_msg(Key, LUser, LServer, To) ->
    try
        {ok, Obj} = mongoose_riak:get(bucket_type(LServer), Key),

        PacketRaw = riakc_obj:get_value(Obj),
        {ok, Packet} = exml:parse(PacketRaw),
        MD = riakc_obj:get_update_metadata(Obj),
        [Timestamp] = riakc_obj:get_secondary_index(MD, ?TIMESTAMP_IDX),
        From = riakc_obj:get_user_metadata_entry(MD, <<"from">>),
        [Expire] = riakc_obj:get_secondary_index(MD, ?EXPIRE_IDX),

        #offline_msg{us = {LUser, LServer},
            timestamp = usec:to_now(Timestamp),
            expire = maybe_decode_timestamp(Expire),
            from = jid:from_binary(From),
            to = To,
            packet = Packet}

    catch
        Error:Reason ->
            ?WARNING_MSG("issue=~p, action=reading_key, host=~s, reason=~p, stack_trace=~p",
                [Error, LServer, Reason, erlang:get_stacktrace()]),
            []
    end.

