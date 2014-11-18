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
         write_messages/4,
	     remove_expired_messages/1,
	     remove_old_messages/2,
	     remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").

-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

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
            TimeStamp = now(),
            {ok, skip_expired_messages(TimeStamp, lists:keysort(#offline_msg.timestamp, Rs))};
        {aborted, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

write_messages(LUser, LServer, Msgs, MaxOfflineMsgs) ->
    F = fun() -> write_messages_t(LUser, LServer, Msgs, MaxOfflineMsgs) end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

write_messages_t(LUser, LServer, Msgs, MaxOfflineMsgs) ->
    Len = length(Msgs),
    case is_message_count_threshold_reached(MaxOfflineMsgs, Len, LUser, LServer) of
        false ->
            write_all_messages_t(Len, Msgs);
        true ->
            discard_all_messages_t(Msgs)
    end.

is_message_count_threshold_reached(MaxOfflineMsgs, Len, LUser, LServer) ->
    case MaxOfflineMsgs of
        infinity ->
            false;
        MaxOfflineMsgs when Len > MaxOfflineMsgs ->
            true;
        MaxOfflineMsgs ->
			%% Only count messages if needed.
            MaxArchivedMsg = MaxOfflineMsgs - Len,
            MaxArchivedMsg < count_offline_messages(LUser, LServer)

            %% Do not need to count all messages in archive.
       %    MaxOfflineMsgs < count_offline_messages(LUser, LServer, MaxArchivedMsg + 1)
    end.

write_all_messages_t(Len, Msgs) ->
    if
        Len >= ?OFFLINE_TABLE_LOCK_THRESHOLD ->
            mnesia:write_lock_table(offline_msg);
        true ->
            ok
    end,
    [mnesia:write(M) || M <- Msgs],
    ok.

discard_all_messages_t(Msgs) ->
    {discarded, Msgs}.

count_offline_messages(LUser, LServer) ->
    US = {LUser, LServer},
    p1_mnesia:count_records(offline_msg, #offline_msg{us=US, _='_'}).

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete({offline_msg, US})
	end,
    mnesia:transaction(F).

remove_expired_messages(_Host) ->
    TimeStamp = now(),
    F = fun() ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(
		  fun(Rec, _Acc) ->
              remove_expired_message(TimeStamp, Rec)
		  end, ok, offline_msg)
	end,
    mnesia:transaction(F).

remove_old_messages(_Host, Days) ->
    TimeStamp = fallback_timestamp(Days, now()),
    F = fun() ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(
		  fun(Rec, _Acc) ->
              remove_old_message(TimeStamp, Rec)
		  end, ok, offline_msg)
	end,
    mnesia:transaction(F).

fallback_timestamp(Days, {MegaSecs, Secs, _MicroSecs}) ->
    S = MegaSecs * 1000000 + Secs - 60 * 60 * 24 * Days,
    MegaSecs1 = S div 1000000,
    Secs1 = S rem 1000000,
    {MegaSecs1, Secs1, 0}.

remove_expired_message(TimeStamp, Rec) ->
    case is_expired_message(TimeStamp, Rec) of
        true ->
            mnesia:delete_object(Rec);
        false ->
            ok
    end.

remove_old_message(TimeStamp, Rec) ->
    case is_old_message(TimeStamp, Rec) of
        true ->
            mnesia:delete_object(Rec);
        false ->
            ok
    end.

skip_expired_messages(TimeStamp, Rs) ->
    [R || R <- Rs, not is_expired_message(TimeStamp, R)].

is_expired_message(_TimeStamp, #offline_msg{expire=never}) ->
    false;
is_expired_message(TimeStamp, #offline_msg{expire=ExpireTimeStamp}) ->
   ExpireTimeStamp < TimeStamp.

is_old_message(MaxAllowedTimeStamp, #offline_msg{timestamp=TimeStamp}) ->
    TimeStamp < MaxAllowedTimeStamp.
