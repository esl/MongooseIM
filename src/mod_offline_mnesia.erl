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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline_mnesia).
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
-define(BATCHSIZE, 100).

init(_Host, _Opts) ->
    mnesia:create_table(offline_msg,
                        [{disc_only_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, offline_msg)}]),
    mnesia:add_table_copy(offline_msg, node(), disc_only_copies),
    ok.

pop_messages(LUser, LServer) ->
    US = {LUser, LServer},
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

fetch_messages(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() -> mnesia:wread({offline_msg, US}) end,
    case mnesia:transaction(F) of
        {atomic, Rs} ->
            {ok, Rs};
        {aborted, Reason} ->
            {error, Reason}
    end.

write_messages(_LUser, _LServer, Msgs) ->
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

count_offline_messages(LUser, LServer, _MaxNeeded) ->
    US = {LUser, LServer},
    F = fun () ->
        Result = mnesia:read(offline_msg, US, read),
        length(Result)
    end,
    case catch mnesia:async_dirty(F) of
        I when is_integer(I) -> I;
        _ -> 0
    end.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
                mnesia:delete({offline_msg, US})
        end,
    mnesia:transaction(F).

-spec remove_expired_messages(jid:lserver()) -> {error, term()} | {ok, HowManyRemoved} when
    HowManyRemoved :: integer().
remove_expired_messages(_Host) ->
    TimeStamp = p1_time_compat:timestamp(),
    F = fun() ->
                mnesia:write_lock_table(offline_msg),
                mnesia:foldl(
                  fun(Rec, Acc) ->
              Acc + remove_expired_message(TimeStamp, Rec)
                  end, 0, offline_msg)
        end,
    case mnesia:transaction(F) of
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Result} ->
            {ok, Result}
    end.

-spec remove_old_messages(jid:lserver(), erlang:timestamp()) ->
    {error, term()} | {ok, HowManyRemoved} when
    HowManyRemoved :: integer().
remove_old_messages(_Host, TimeStamp) ->
    F = fun() ->
                mnesia:write_lock_table(offline_msg),
                mnesia:foldl(
                  fun(Rec, Acc) ->
                          Acc + remove_old_message(TimeStamp, Rec)
                  end, 0, offline_msg)
        end,
    case mnesia:transaction(F) of
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Result} ->
            {ok, Result}
    end.

remove_expired_message(TimeStamp, Rec) ->
    case mod_offline:is_expired_message(TimeStamp, Rec) of
        true ->
            mnesia:delete_object(Rec),
            1;
        false ->
            0
    end.

remove_old_message(TimeStamp, Rec) ->
    case is_old_message(TimeStamp, Rec) of
        true ->
            mnesia:delete_object(Rec),
            1;
        false ->
            0
    end.

is_old_message(MaxAllowedTimeStamp, #offline_msg{timestamp=TimeStamp}) ->
    TimeStamp < MaxAllowedTimeStamp.
