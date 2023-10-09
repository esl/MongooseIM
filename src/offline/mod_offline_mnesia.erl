%%%----------------------------------------------------------------------
%%% Copyright notice from the originam mod_offline.erl
%%%
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages in Mnesia database.
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

-module(mod_offline_mnesia).
-behaviour(mod_offline_backend).
-export([init/2,
         pop_messages/2,
         fetch_messages/2,
         write_messages/4,
         count_offline_messages/4,
         remove_expired_messages/2,
         remove_old_messages/3,
         remove_user/3]).

-include("mod_offline.hrl").

-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, _Opts) ->
    mongoose_mnesia:create_table(offline_msg,
        [{disc_only_copies, [node()]}, {type, bag},
         {attributes, record_info(fields, offline_msg)}]),
    upgrade_table(),
    ok.

-spec pop_messages(mongooseim:host_type(), jid:jid()) ->
          {ok, [mod_offline:msg()]} | {error, any()}.
pop_messages(_HostType, To) ->
    US = jid:to_lus(To),
    F = fun() ->
                Rs = mnesia:wread({offline_msg, US}),
                mnesia:delete({offline_msg, US}),
                Rs
        end,
    case mnesia:transaction(F) of
        {atomic, Rs} ->
            {ok, Rs};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec fetch_messages(mongooseim:host_type(), jid:jid()) ->
          {ok, [mod_offline:msg()]} | {error, any()}.
fetch_messages(_HostType, To) ->
    US = jid:to_lus(To),
    F = fun() -> mnesia:wread({offline_msg, US}) end,
    case mnesia:transaction(F) of
        {atomic, Rs} ->
            {ok, Rs};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec write_messages(mongooseim:host_type(), jid:luser(), jid:lserver(), [mod_offline:msg()]) ->
          ok | {error, any()}.
write_messages(_HostType, _LUser, _LServer, Msgs) ->
    F = fun() -> write_messages_t(Msgs) end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

write_messages_t(Msgs) ->
    Len = length(Msgs),
    write_all_messages_t(Len, Msgs).

write_all_messages_t(Len, Msgs) ->
    case Len >= ?OFFLINE_TABLE_LOCK_THRESHOLD of
        true -> mnesia:write_lock_table(offline_msg);
        false -> ok
    end,
    [mnesia:write(M) || M <- Msgs],
    ok.

-spec count_offline_messages(mongooseim:host_type(), jid:luser(), jid:lserver(),
                             mod_offline:msg_count()) ->
          mod_offline:msg_count().
count_offline_messages(_HostType, LUser, LServer, _MaxNeeded) ->
    US = {LUser, LServer},
    F = fun () ->
        Result = mnesia:read(offline_msg, US, read),
        length(Result)
    end,
    case catch mnesia:async_dirty(F) of
        I when is_integer(I) -> I;
        _ -> 0
    end.

-spec remove_expired_messages(mongooseim:host_type(), jid:lserver()) ->
          {ok, mod_offline:msg_count()} | {error, any()}.
remove_expired_messages(_HostType, LServer) ->
    TimeStamp = erlang:system_time(microsecond),
    F = fun() ->
                mnesia:write_lock_table(offline_msg),
                mnesia:foldl(
                  fun(Rec, Acc) ->
                          Acc + remove_expired_message(LServer, TimeStamp, Rec)
                  end, 0, offline_msg)
        end,
    case mnesia:transaction(F) of
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Result} ->
            {ok, Result}
    end.

-spec remove_old_messages(mongooseim:host_type(), jid:lserver(), mod_offline:timestamp()) ->
          {ok, mod_offline:msg_count()} | {error, any()}.
remove_old_messages(_HostType, LServer, TimeStamp) ->
    F = fun() ->
                mnesia:write_lock_table(offline_msg),
                mnesia:foldl(
                  fun(Rec, Acc) ->
                          Acc + remove_old_message(LServer, TimeStamp, Rec)
                  end, 0, offline_msg)
        end,
    case mnesia:transaction(F) of
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Result} ->
            {ok, Result}
    end.

remove_expired_message(LServer, TimeStamp, Rec = #offline_msg{us = {_, LServer}}) ->
    case mod_offline:is_expired_message(TimeStamp, Rec) of
        true ->
            mnesia:delete_object(Rec),
            1;
        false ->
            0
    end;
remove_expired_message(_, _, _) -> 0.

remove_old_message(LServer, TimeStamp, Rec = #offline_msg{us = {_, LServer}}) ->
    case is_old_message(TimeStamp, Rec) of
        true ->
            mnesia:delete_object(Rec),
            1;
        false ->
            0
    end;
remove_old_message(_, _, _) -> 0.

is_old_message(MaxAllowedTimeStamp, #offline_msg{timestamp=TimeStamp}) ->
    TimeStamp < MaxAllowedTimeStamp.

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() ->
                mnesia:delete({offline_msg, US})
        end,
    mnesia:transaction(F),
    ok.

upgrade_table() ->
    Fields = record_info(fields, offline_msg),
    case mnesia:table_info(offline_msg, attributes) of
        Fields ->
            ok;
        [us, timestamp, expire, from, to, packet] -> %% MongooseIM 3.5.0 and older
            mnesia:transform_table(offline_msg, fun transform_from_3_5_0_to_next/1, Fields)
    end.

transform_from_3_5_0_to_next(Record) ->
    erlang:append_element(Record, []).
