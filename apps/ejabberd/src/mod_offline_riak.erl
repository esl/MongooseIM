%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Pawel Pikula <pawel.pikula@erlang-solutions.com>
%%% Purpose : mod_last riak backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2014      Erlang Solutions Ltd.
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
-export([write_messages/3]).
-export([remove_expired_messages/1]).
-export([remove_old_messages/2]).
-export([remove_user/2]).
-export([count_offline_messages/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").
-include_lib("riakc/include/riakc.hrl").

-define(TIMESTAMP_IDX, {integer_index, "timestamp"}).
-define(USER_IDX,      {binary_index, "user"}).

-spec init(Host :: ejabberd:lserver(), Opts :: []) -> ok.
init(_Host, _Opts) ->
    ok.

-spec pop_messages(LUser, LServer) -> {ok, Result} | {error, Reason} when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Reason :: term(),
    Result :: list(#offline_msg{}).
pop_messages(LUser, LServer) ->
    Keys = read_user_idx(LUser, LServer),
    To = jid:make(LUser, LServer, <<>>),
    Msgs = [pop_msg(Key, LUser, LServer, To) || Key <- Keys],
    {ok, lists:flatten(Msgs)}.

-spec write_messages(LUser, LServer, Msgs) ->
    ok  when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Msgs :: list().
write_messages(LUser, LServer, Msgs) ->
    [write_msg(LUser, LServer, Msg) || Msg <- Msgs],
    ok.

-spec count_offline_messages(LUser, LServer, MaxArchivedMsgs) ->
    integer() when
      LUser :: ejabberd:luser(),
      LServer :: ejabberd:lserver(),
      MaxArchivedMsgs :: integer().
count_offline_messages(LUser, LServer, MaxArchivedMsgs) ->
    {ok, IdxResult} = mongoose_riak:get_index(bucket_type(LServer), ?USER_IDX,
                                              LUser, [{max_results, MaxArchivedMsgs}]),
    length(IdxResult?INDEX_RESULTS.keys).

-spec remove_expired_messages(Host) -> {error, Reason} | {ok, Count} when
    Host :: ejabberd:lserver(),
    Reason :: term(),
    Count :: integer().
remove_expired_messages(Host) ->
    {ok, 0}.

-spec remove_old_messages(Host, Days) -> {error, Reason} | {ok, Count} when
    Host :: ejabberd:lserver(),
    Days :: erlang:timestamp(),
    Reason :: term(),
    Count :: integer().
remove_old_messages(Host, Days) ->
    {ok, 0}.

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
                        {?USER_IDX, [LUser]}],
    MDWithIndexes = riakc_obj:set_secondary_index(MD, SecondaryIndexes),
    From = jid:to_binary(FromJID),
    UserMetaData = [{<<"from">>, From}, {<<"expire">>, maybe_encode_timestamp(Expire)}],
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
        User = riakc_obj:get_secondary_index(MD, ?USER_IDX),
        From = riakc_obj:get_user_metadata_entry(MD, <<"from">>),

        mongoose_riak:delete(bucket_type(LServer), Key),

        #offline_msg{us = {LUser, LServer},
                     timestamp = usec:to_now(Timestamp),
                     expire = never,
                     from = jid:from_binary(From),
                     to = To,
                     packet = Packet}

    catch
        Error:Reason ->
            ?WARNING_MSG("issue=~p, action=reading_key, host=~s, reason=~p, stack_trace=~p",
                         [Error, LServer, Reason, erlang:get_stacktrace()]),
            []
    end.


-spec bucket_type(ejabberd:lserver()) -> {binary(), ejabberd:lserver()}.
bucket_type(LServer) ->
    {<<"offline">>, LServer}.

-spec key(binary(), integer()) -> binary().
key(LUser, TimestampInt) ->
    Timestamp = integer_to_binary(TimestampInt),
    Random = integer_to_binary(random:uniform(1024)),
    <<LUser/binary, "@", Timestamp/binary, "@", Random/binary>>.

maybe_encode_timestamp(never) ->
    <<"never">>;
maybe_encode_timestamp(TS) ->
    integer_to_binary(usec:from_now(TS)).

