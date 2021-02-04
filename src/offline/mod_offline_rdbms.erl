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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline_rdbms).
-behaviour(mod_offline).
-export([init/2,
         pop_messages/1,
         fetch_messages/1,
         write_messages/3,
         count_offline_messages/3,
         remove_expired_messages/1,
         remove_old_messages/2,
         remove_user/2]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").

-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

init(_Host, _Opts) ->
    prepare_queries(),
    ok.

prepare_queries() ->
    prepare(offline_insert, offline_message,
            [username, server, from_jid, timestamp, expire,
             packet, permanent_fields],
            <<"INSERT INTO offline_message "
              "(username, server, from_jid, timestamp, expire,"
              " packet, permanent_fields) "
              "VALUES (?, ?, ?, ?, ?, ?, ?)">>),
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(),
    prepare(offline_count_limit, offline_message,
            rdbms_queries:add_limit_arg(limit, [server, username]),
            <<"SELECT ", LimitMSSQL/binary,
              " count(*) FROM offline_message "
              "WHERE server=? AND username=? ", LimitSQL/binary>>),
    prepare(offline_select, offline_message,
            [server, username, expire],
            <<"SELECT timestamp, from_jid, packet, permanent_fields "
              "FROM offline_message "
              "WHERE server = ? AND username = ? AND "
                     "(expire IS null OR expire > ?) "
              "ORDER BY timestamp">>),
    prepare(offline_delete, offline_message,
            [server, username],
            <<"DELETE FROM offline_message "
              "WHERE server = ? AND username = ?">>),
    prepare(offline_delete_old, offline_message,
            [timestamp],
            <<"DELETE FROM offline_message WHERE timestamp < ?">>),
    prepare(offline_delete_expired, offline_message,
            [expire],
            <<"DELETE FROM offline_message "
              "WHERE expire IS NOT null AND expire < ?">>),
    ok.

execute_count_offline_messages(LUser, LServer, Limit) ->
    Args = rdbms_queries:add_limit_arg(Limit, [LServer, LUser]),
    execute_successfully(LServer, offline_count_limit, Args).

execute_fetch_offline_messages(LServer, LUser, ExtTimeStamp) ->
    execute_successfully(LServer, offline_select, [LServer, LUser, ExtTimeStamp]).

execute_remove_expired_offline_messages(LServer, ExtTimeStamp) ->
    execute_successfully(LServer, offline_delete_expired, [ExtTimeStamp]).

execute_remove_old_offline_messages(LServer, ExtTimeStamp) ->
    execute_successfully(LServer, offline_delete_old, [ExtTimeStamp]).

execute_remove_user(LServer, LUser) ->
    execute_successfully(LServer, offline_delete, [LServer, LUser]).

%% Transactions

pop_offline_messages(LServer, LUser, ExtTimeStamp) ->
    F = fun() ->
            Res = execute_fetch_offline_messages(LServer, LUser, ExtTimeStamp),
            execute_remove_user(LServer, LUser),
            Res
        end,
    mongoose_rdbms:sql_transaction(LServer, F).

push_offline_messages(LServer, Rows) ->
    F = fun() ->
            [execute_successfully(LServer, offline_insert, Row)
             || Row <- Rows], ok
        end,
    mongoose_rdbms:sql_transaction(LServer, F).

%% API functions

pop_messages(#jid{} = To) ->
    US = {LUser, LServer} = jid:to_lus(To),
    ExtTimeStamp = os:system_time(microsecond),
    case pop_offline_messages(LServer, LUser, ExtTimeStamp) of
        {atomic, {selected, Rows}} ->
            {ok, rows_to_records(US, To, Rows)};
        {aborted, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%% Fetch messages for GDPR
%% User and Server are not normalized
fetch_messages(#jid{} = To) ->
    US = {LUser, LServer} = jid:to_lus(To),
    ExtTimeStamp = os:system_time(microsecond),
    {selected, Rows} = execute_fetch_offline_messages(LServer, LUser, ExtTimeStamp),
    {ok, rows_to_records(US, To, Rows)}.

write_messages(LUser, LServer, Msgs) ->
    Rows = [record_to_row(LUser, LServer, Msg) || Msg <- Msgs],
    case push_offline_messages(LServer, Rows) of
        {atomic, ok} ->
            ok;
        Other ->
            {error, Other}
    end.

count_offline_messages(LUser, LServer, MaxArchivedMsgs) ->
    Result = execute_count_offline_messages(LUser, LServer, MaxArchivedMsgs + 1),
    mongoose_rdbms:selected_to_integer(Result).

remove_user(LUser, LServer) ->
    execute_remove_user(LServer, LUser).

-spec remove_expired_messages(jid:lserver()) ->
    {error, term()} | {ok, HowManyRemoved :: non_neg_integer()}.
remove_expired_messages(LServer) ->
    ExtTimeStamp = os:system_time(microsecond),
    Result = execute_remove_expired_offline_messages(LServer, ExtTimeStamp),
    updated_ok(Result).

-spec remove_old_messages(LServer, Timestamp) ->
    {error, term()} | {ok, HowManyRemoved} when
    LServer :: jid:lserver(),
    Timestamp :: erlang:timestamp(),
    HowManyRemoved :: integer().
remove_old_messages(LServer, TimeStamp) ->
    ExtTimeStamp = encode_timestamp(TimeStamp),
    Result = execute_remove_old_offline_messages(LServer, ExtTimeStamp),
    updated_ok(Result).

%% Pure helper functions
record_to_row(LUser, LServer,
              #offline_msg{from = From, packet = Packet, timestamp = TimeStamp,
                           expire = Expire, permanent_fields = PermanentFields}) ->
    ExtFrom = jid:to_binary(From),
    ExtTimeStamp = encode_timestamp(TimeStamp),
    ExtExpire = maybe_encode_timestamp(Expire),
    ExtPacket = exml:to_binary(Packet),
    ExtFields = encode_permanent_fields(PermanentFields),
    prepare_offline_message(LUser, LServer, ExtFrom, ExtTimeStamp,ExtExpire,
                            ExtPacket, ExtFields).

prepare_offline_message(LUser, LServer, ExtFrom, ExtTimeStamp, ExtExpire, ExtPacket, ExtFields) ->
    [LUser, LServer, ExtFrom, ExtTimeStamp, ExtExpire, ExtPacket, ExtFields].

encode_permanent_fields(Fields) ->
    term_to_binary(Fields).

encode_timestamp(TimeStamp) -> usec:from_now(TimeStamp). %% to microseconds

maybe_encode_timestamp(never) -> null;
maybe_encode_timestamp(TimeStamp) -> encode_timestamp(TimeStamp).

rows_to_records(US, To, Rows) ->
    [row_to_record(US, To, Row) || Row <- Rows].

row_to_record(US, To, {ExtTimeStamp, ExtFrom, ExtPacket, ExtPermanentFields}) ->
    {ok, Packet} = exml:parse(ExtPacket),
    TimeStamp = usec:to_now(mongoose_rdbms:result_to_integer(ExtTimeStamp)),
    From = jid:from_binary(ExtFrom),
    PermanentFields = extract_permanent_fields(ExtPermanentFields),
    #offline_msg{us = US, timestamp = TimeStamp, expire = never,
                 from = From, to = To, packet = Packet,
                 permanent_fields = PermanentFields}.

extract_permanent_fields(Escaped) ->
    Bin = mongoose_rdbms:unescape_binary(global, Escaped),
    binary_to_term(Bin).

updated_ok({updated, Count}) -> {ok, Count}.
