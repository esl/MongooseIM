%%%----------------------------------------------------------------------
%%% File    : mod_offline_odbc.erl
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

-module(mod_offline_odbc_legacy).
-behaviour(mod_offline).

-export([init/2,
         pop_messages/2,
         write_messages/4,
         remove_expired_messages/1,
         remove_old_messages/2,
         remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").

-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

init(_Host, _Opts) ->
    ok.

pop_messages(LUser, LServer) ->
    US = {LUser, LServer},
    SUser = ejabberd_odbc:escape(LUser),
    case odbc_queries:get_and_del_spool_msg_t(LServer, SUser) of
        {atomic, {selected, [<<"username">>, <<"xml">>], Rows}} ->
            {ok, rows_to_records(US, Rows)};
        {aborted, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

rows_to_records(US, Rows) ->
    [row_to_record(US, Row) || Row <- Rows].

row_to_record(US, {_UserName, SPacket}) ->
    Packet = xml_stream:parse_element(SPacket),
    To   = jlib:binary_to_jid(xml:get_tag_attr_s(<<"to">>, Packet)),
    From = jlib:binary_to_jid(xml:get_tag_attr_s(<<"from">>, Packet)),
    #offline_msg{us = US,
             timestamp = undefined,
             expire = undefined,
             from = From,
             to = To,
             packet = Packet}.


write_messages(LUser, LServer, Msgs, MaxOfflineMsgs) ->
    SUser = ejabberd_odbc:escape(LUser),
    write_messages_t(LServer, SUser, Msgs, MaxOfflineMsgs).

write_messages_t(LServer, SUser, Msgs, MaxOfflineMsgs) ->
    case is_message_count_threshold_reached(
                 LServer, SUser, Msgs, MaxOfflineMsgs) of
        false ->
            write_all_messages_t(LServer, SUser, Msgs);
        true ->
            discard_all_messages_t(Msgs)
    end.

is_message_count_threshold_reached(LServer, SUser, Msgs, MaxOfflineMsgs) ->
    Len = length(Msgs),
    case MaxOfflineMsgs of
        infinity ->
            false;
        MaxOfflineMsgs when Len > MaxOfflineMsgs ->
            true;
        MaxOfflineMsgs ->
            %% Only count messages if needed.
            MaxOfflineMsgs < count_offline_messages(LServer, SUser)
    end.

write_all_messages_t(LServer, SUser, Msgs) ->
    Rows = [record_to_row(SUser, Msg) || Msg <- Msgs],
    case catch odbc_queries:add_spool(LServer, Rows) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

record_to_row(SUser, #offline_msg{from = From, to = To,
                                  timestamp = TimeStamp, packet = Packet}) ->
    Time  = calendar:now_to_universal_time(TimeStamp),
    TimeStampElem = jlib:timestamp_to_xml(Time),
    Packet2 = jlib:replace_from_to(From, To, Packet),
    Packet3 = xml:append_subtags(Packet2, [TimeStampElem]),
    SPacket = ejabberd_odbc:escape(xml:element_to_binary(Packet3)),
    odbc_queries:add_spool_sql(SUser, SPacket).

discard_all_messages_t(Msgs) ->
    {discarded, Msgs}.

remove_user(LUser, LServer) ->
    SUser = ejabberd_odbc:escape(LUser),
    odbc_queries:del_spool_msg(LServer, SUser).

remove_expired_messages(_LServer) ->
    %% TODO
    ok.

remove_old_messages(_LServer, _Days) ->
    %% TODO
    ok.

count_offline_messages(LServer, SUser) ->
    case odbc_queries:count_spool_msg(LServer, SUser) of
        {selected, [_], [{Count}]} ->
            list_to_integer(binary_to_list(Count));
        _ ->
            0
    end.
