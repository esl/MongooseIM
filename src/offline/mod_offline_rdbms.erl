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
-behaviour(mod_offline_backend).
-export([init/2,
         pop_messages/2,
         fetch_messages/2,
         write_messages/4,
         count_offline_messages/4,
         remove_expired_messages/2,
         remove_old_messages/3,
         remove_user/3,
         remove_domain/2]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

-include("jlib.hrl").
-include("mod_offline.hrl").

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, _Opts) ->
    prepare_queries(),
    ok.

prepare_queries() ->
    prepare(offline_insert, offline_message,
            [username, server, timestamp, expire,
             from_jid, packet, permanent_fields],
            <<"INSERT INTO offline_message "
              "(username, server, timestamp, expire,"
              " from_jid, packet, permanent_fields) "
              "VALUES (?, ?, ?, ?, ?, ?, ?)">>),
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(),
    prepare(offline_count_limit, offline_message,
            rdbms_queries:add_limit_arg(limit, [server, username]),
            <<"SELECT ", LimitMSSQL/binary,
              " count(*) FROM offline_message "
              "WHERE server = ? AND username = ? ", LimitSQL/binary>>),
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
    prepare(offline_remove_domain, offline_message,
            [server],
            <<"DELETE FROM offline_message WHERE server = ?">>),
    prepare(offline_delete_old, offline_message,
            [server, timestamp],
            <<"DELETE FROM offline_message WHERE server = ? AND timestamp < ?">>),
    prepare(offline_delete_expired, offline_message,
            [server, expire],
            <<"DELETE FROM offline_message "
              "WHERE server = ? AND expire IS NOT null AND expire < ?">>).

-spec execute_count_offline_messages(mongooseim:host_type(), jid:luser(), jid:lserver(),
                                     pos_integer()) ->
          mongoose_rdbms:query_result().
execute_count_offline_messages(HostType, LUser, LServer, Limit) ->
    Args = rdbms_queries:add_limit_arg(Limit, [LServer, LUser]),
    execute_successfully(HostType, offline_count_limit, Args).

-spec execute_fetch_offline_messages(mongooseim:host_type(), jid:luser(), jid:lserver(),
                                     integer()) ->
          mongoose_rdbms:query_result().
execute_fetch_offline_messages(HostType, LUser, LServer, ExtTimeStamp) ->
    execute_successfully(HostType, offline_select, [LServer, LUser, ExtTimeStamp]).

-spec execute_remove_expired_offline_messages(mongooseim:host_type(), jid:lserver(), integer()) ->
          mongoose_rdbms:query_result().
execute_remove_expired_offline_messages(HostType, LServer, ExtTimeStamp) ->
    execute_successfully(HostType, offline_delete_expired, [LServer, ExtTimeStamp]).

-spec execute_remove_old_offline_messages(mongooseim:host_type(), jid:lserver(), integer()) ->
          mongoose_rdbms:query_result().
execute_remove_old_offline_messages(HostType, LServer, ExtTimeStamp) ->
    execute_successfully(HostType, offline_delete_old, [LServer, ExtTimeStamp]).

-spec execute_offline_delete(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          mongoose_rdbms:query_result().
execute_offline_delete(HostType, LUser, LServer) ->
    execute_successfully(HostType, offline_delete, [LServer, LUser]).

%% Transactions

-spec pop_offline_messages(mongooseim:host_type(), jid:luser(), jid:server(), integer()) ->
          mongoose_rdbms:transaction_result().
pop_offline_messages(HostType, LUser, LServer, ExtTimeStamp) ->
    F = fun() ->
            Res = execute_fetch_offline_messages(HostType, LUser, LServer, ExtTimeStamp),
            execute_offline_delete(HostType, LUser, LServer),
            Res
        end,
    mongoose_rdbms:sql_transaction(HostType, F).

-spec push_offline_messages(mongooseim:host_type(), [list()]) ->
          mongoose_rdbms:transaction_result().
push_offline_messages(HostType, Rows) ->
    F = fun() ->
            [execute_successfully(HostType, offline_insert, Row)
             || Row <- Rows], ok
        end,
    mongoose_rdbms:sql_transaction(HostType, F).

%% API functions

-spec pop_messages(mongooseim:host_type(), jid:jid()) -> {ok, [mod_offline:msg()]} | {error, any()}.
pop_messages(HostType, #jid{} = To) ->
    US = {LUser, LServer} = jid:to_lus(To),
    ExtTimeStamp = os:system_time(microsecond),
    case pop_offline_messages(HostType, LUser, LServer, ExtTimeStamp) of
        {atomic, {selected, Rows}} ->
            {ok, rows_to_records(US, To, Rows)};
        {aborted, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%% Fetch messages for GDPR
-spec fetch_messages(mongooseim:host_type(), jid:jid()) -> {ok, [mod_offline:msg()]}.
fetch_messages(HostType, #jid{} = To) ->
    US = {LUser, LServer} = jid:to_lus(To),
    ExtTimeStamp = os:system_time(microsecond),
    {selected, Rows} = execute_fetch_offline_messages(HostType, LUser, LServer, ExtTimeStamp),
    {ok, rows_to_records(US, To, Rows)}.

-spec write_messages(mongooseim:host_type(), jid:luser(), jid:lserver(), [mod_offline:msg()]) ->
          ok | {error, any()}.
write_messages(HostType, LUser, LServer, Msgs) ->
    Rows = [record_to_row(LUser, LServer, Msg) || Msg <- Msgs],
    case push_offline_messages(HostType, Rows) of
        {atomic, ok} ->
            ok;
        Other ->
            {error, Other}
    end.

-spec count_offline_messages(mongooseim:host_type(), jid:luser(), jid:lserver(),
                             mod_offline:msg_count()) ->
          mod_offline:msg_count().
count_offline_messages(HostType, LUser, LServer, Limit) ->
    Result = execute_count_offline_messages(HostType, LUser, LServer, Limit),
    mongoose_rdbms:selected_to_integer(Result).

-spec remove_expired_messages(mongooseim:host_type(), jid:lserver()) -> {ok, mod_offline:msg_count()}.
remove_expired_messages(HostType, LServer) ->
    TimeStamp = os:system_time(microsecond),
    Result = execute_remove_expired_offline_messages(HostType, LServer, TimeStamp),
    updated_ok(Result).

-spec remove_old_messages(mongooseim:host_type(), jid:lserver(), mod_offline:timestamp()) ->
          {ok, mod_offline:msg_count()}.
remove_old_messages(HostType, LServer, TimeStamp) ->
    Result = execute_remove_old_offline_messages(HostType, LServer, TimeStamp),
    updated_ok(Result).

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(HostType, LUser, LServer) ->
    execute_offline_delete(HostType, LUser, LServer),
    ok.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, Domain) ->
    mongoose_rdbms:execute_successfully(HostType, offline_remove_domain, [Domain]),
    ok.

%% Pure helper functions
record_to_row(LUser, LServer,
              #offline_msg{timestamp = TimeStamp, expire = Expire, from = From,
                           packet = Packet, permanent_fields = PermanentFields}) ->
    ExtExpire = maybe_encode_timestamp(Expire),
    ExtFrom = jid:to_binary(From),
    ExtPacket = exml:to_binary(Packet),
    ExtFields = encode_permanent_fields(PermanentFields),
    prepare_offline_message(LUser, LServer, TimeStamp, ExtExpire,
                            ExtFrom, ExtPacket, ExtFields).

prepare_offline_message(LUser, LServer, ExtTimeStamp, ExtExpire, ExtFrom, ExtPacket, ExtFields) ->
    [LUser, LServer, ExtTimeStamp, ExtExpire, ExtFrom, ExtPacket, ExtFields].

encode_permanent_fields(Fields) ->
    term_to_binary(Fields).

maybe_encode_timestamp(never) -> null;
maybe_encode_timestamp(TimeStamp) -> TimeStamp.

rows_to_records(US, To, Rows) ->
    [row_to_record(US, To, Row) || Row <- Rows].

row_to_record(US, To, {ExtTimeStamp, ExtFrom, ExtPacket, ExtPermanentFields}) ->
    {ok, Packet} = exml:parse(ExtPacket),
    TimeStamp = mongoose_rdbms:result_to_integer(ExtTimeStamp),
    From = jid:from_binary(ExtFrom),
    PermanentFields = extract_permanent_fields(ExtPermanentFields),
    #offline_msg{us = US, timestamp = TimeStamp, expire = never,
                 from = From, to = To, packet = Packet,
                 permanent_fields = PermanentFields}.

extract_permanent_fields(null) ->
    []; %% This is needed in transition period when upgrading to MongooseIM above 3.5.0
extract_permanent_fields(Escaped) ->
    Bin = mongoose_rdbms:unescape_binary(global, Escaped),
    binary_to_term(Bin).

updated_ok({updated, Count}) -> {ok, Count}.
