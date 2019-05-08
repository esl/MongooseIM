%%%----------------------------------------------------------------------
%%% File    : mod_offline_rdbms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages in relational database.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline_rdbms).
-behaviour(mod_offline).
-export([init/2,
         pop_messages/2,
         fetch_messages/2,
         write_messages/3,
         count_offline_messages/3,
         remove_expired_messages/1,
         remove_old_messages/2,
         remove_user/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").

-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

init(_Host, _Opts) ->
    ok.

pop_messages(LUser, LServer) ->
    US = {LUser, LServer},
    To = jid:make(LUser, LServer, <<>>),
    SUser = mongoose_rdbms:escape_string(LUser),
    SServer = mongoose_rdbms:escape_string(LServer),
    TimeStamp = p1_time_compat:timestamp(),
    STimeStamp = encode_timestamp(TimeStamp),
    case rdbms_queries:pop_offline_messages(LServer, SUser, SServer, STimeStamp) of
        {atomic, {selected, Rows}} ->
            {ok, rows_to_records(US, To, Rows)};
        {aborted, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

fetch_messages(LUser, LServer) ->
    US = {LUser, LServer},
    To = jid:make(LUser, LServer, <<>>),
    TimeStamp = p1_time_compat:timestamp(),
    SUser = mongoose_rdbms:escape_string(LUser),
    SServer = mongoose_rdbms:escape_string(LServer),
    STimeStamp = encode_timestamp(TimeStamp),
    case rdbms_queries:fetch_offline_messages(LServer, SUser, SServer, STimeStamp) of
        {selected, Rows} ->
            {ok, rows_to_records(US, To, Rows)};
        {error, Reason} ->
            {error, Reason}
    end.

rows_to_records(US, To, Rows) ->
    [row_to_record(US, To, Row) || Row <- Rows].

row_to_record(US, To, {STimeStamp, SFrom, SPacket}) ->
    {ok, Packet} = exml:parse(SPacket),
    TimeStamp = usec:to_now(mongoose_rdbms:result_to_integer(STimeStamp)),
    From = jid:from_binary(SFrom),
    #offline_msg{us = US,
             timestamp = TimeStamp,
             expire = never,
             from = From,
             to = To,
             packet = Packet}.


write_messages(LUser, LServer, Msgs) ->
    SUser = mongoose_rdbms:escape_string(LUser),
    SServer = mongoose_rdbms:escape_string(LServer),
    write_all_messages_t(LServer, SUser, SServer, Msgs).

count_offline_messages(LUser, LServer, MaxArchivedMsgs) ->
    SUser = mongoose_rdbms:escape_string(LUser),
    SServer = mongoose_rdbms:escape_string(LServer),
    count_offline_messages(LServer, SUser, SServer, MaxArchivedMsgs + 1).

write_all_messages_t(LServer, SUser, SServer, Msgs) ->
    Rows = [record_to_row(SUser, SServer, Msg) || Msg <- Msgs],
    case rdbms_queries:push_offline_messages(LServer, Rows) of
        {updated, _} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

record_to_row(SUser, SServer, #offline_msg{
        from = From, packet = Packet, timestamp = TimeStamp, expire = Expire}) ->
    SFrom = mongoose_rdbms:escape_string(jid:to_binary(From)),
    SPacket = mongoose_rdbms:escape_string(exml:to_binary(Packet)),
    STimeStamp = encode_timestamp(TimeStamp),
    SExpire = maybe_encode_timestamp(Expire),
    rdbms_queries:prepare_offline_message(SUser, SServer, STimeStamp, SExpire, SFrom, SPacket).

remove_user(LUser, LServer) ->
    SUser   = mongoose_rdbms:escape_string(LUser),
    SServer = mongoose_rdbms:escape_string(LServer),
    rdbms_queries:remove_offline_messages(LServer, SUser, SServer).

-spec remove_expired_messages(jid:lserver()) -> {error, term()} | {ok, HowManyRemoved} when
    HowManyRemoved :: integer().
remove_expired_messages(LServer) ->
    TimeStamp = p1_time_compat:timestamp(),
    STimeStamp = encode_timestamp(TimeStamp),
    Result = rdbms_queries:remove_expired_offline_messages(LServer, STimeStamp),
    case Result of
        {error, Reason} ->
            {error, Reason};
        {updated, Count} ->
            {ok, Count}
    end.
-spec remove_old_messages(LServer, Timestamp) ->
    {error, term()} | {ok, HowManyRemoved} when
    LServer :: jid:lserver(),
    Timestamp :: erlang:timestamp(),
    HowManyRemoved :: integer().
remove_old_messages(LServer, TimeStamp) ->
    STimeStamp = encode_timestamp(TimeStamp),
    Result = rdbms_queries:remove_old_offline_messages(LServer, STimeStamp),
    case Result of
        {error, Reason} ->
            {error, Reason};
        {updated, Count} ->
            {ok, Count}
    end.

count_offline_messages(LServer, SUser, SServer, Limit) ->
    case rdbms_queries:count_offline_messages(LServer, SUser, SServer, Limit) of
        {selected, [{Count}]} ->
            mongoose_rdbms:result_to_integer(Count);
        Error ->
            ?ERROR_MSG("count_offline_messages failed ~p", [Error]),
            0
    end.

encode_timestamp(TimeStamp) ->
    mongoose_rdbms:escape_integer(usec:from_now(TimeStamp)).

maybe_encode_timestamp(never) ->
    mongoose_rdbms:escape_null();
maybe_encode_timestamp(TimeStamp) ->
    encode_timestamp(TimeStamp).
