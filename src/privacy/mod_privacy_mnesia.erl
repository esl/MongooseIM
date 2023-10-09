%%%----------------------------------------------------------------------
%%% Copyright notice from original mod_privacy
%%%
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_privacy_mnesia).
-author('alexey@process-one.net').
-author('arcusfelis@gmail.com').
-behaviour(mod_privacy_backend).

-export([init/2,
         get_default_list/3,
         get_list_names/3,
         get_privacy_list/4,
         set_default_list/4,
         forget_default_list/3,
         remove_privacy_list/4,
         replace_privacy_list/5,
         remove_user/3,
         remove_domain/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

init(_HostType, _Opts) ->
    mongoose_mnesia:create_table(privacy, 
        [{disc_copies, [node()]},
         {attributes, record_info(fields, privacy)}]),
    ok.

get_default_list(_HostType, LUser, LServer) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
        [] ->
            {error, not_found};
        [#privacy{default = Default, lists = Lists}] ->
            case lists:keysearch(Default, 1, Lists) of
                {value, {_, List}} ->
                    {ok, {Default, List}};
                _ ->
                    {error, not_found}
            end;
        {'EXIT', Reason} ->
            {error, Reason}
    end.

get_list_names(_HostType, LUser, LServer) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
        {'EXIT', Reason} ->
            {error, Reason};
        [] ->
            {error, not_found};
        [#privacy{default = Default, lists = Lists}] ->
            Names = [Name || {Name, _} <- Lists],
            {ok, {Default, Names}}
    end.

get_privacy_list(_HostType, LUser, LServer, Name) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
        {'EXIT', Reason} ->
            {error, Reason};
        [] ->
            {error, not_found};
        [#privacy{lists = Lists}] ->
            case lists:keysearch(Name, 1, Lists) of
                {value, {_, List}} ->
                    {ok, List};
                _ ->
                    {error, not_found}
            end
    end.

%% @doc Set no default list for user.
forget_default_list(_HostType, LUser, LServer) ->
    F = fun() ->
            case mnesia:read({privacy, {LUser, LServer}}) of
                [] ->
                    ok;
                [R] ->
                    mnesia:write(R#privacy{default = none}),
                    ok
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

set_default_list(_HostType, LUser, LServer, Name) ->
    case mnesia:transaction(fun() -> set_default_list_t(LUser, LServer, Name) end) of
        {atomic, ok} ->
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

-spec set_default_list_t(jid:luser(), jid:lserver(), Name :: binary()) ->
    ok | {error, not_found}.
set_default_list_t(LUser, LServer, Name) ->
    case mnesia:read({privacy, {LUser, LServer}}) of
        [] ->
            {error, not_found};
        [#privacy{lists = Lists} = P] ->
            case lists:keymember(Name, 1, Lists) of
                true ->
                    mnesia:write(P#privacy{default = Name}),
                    ok;
                false ->
                    {error, not_found}
            end
    end.

remove_privacy_list(_HostType, LUser, LServer, Name) ->
    F = fun() ->
                case mnesia:read({privacy, {LUser, LServer}}) of
                    [] ->
                        ok;
                    [#privacy{default = Default}] when Name == Default ->
                        {error, conflict};
                    [#privacy{lists = Lists} = P] ->
                        NewLists = lists:keydelete(Name, 1, Lists),
                        mnesia:write(P#privacy{lists = NewLists}),
                        ok
                end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {atomic, {error, _} = Error} ->
            Error;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

replace_privacy_list(_HostType, LUser, LServer, Name, List) ->
    US = {LUser, LServer},
    F = fun() ->
            case mnesia:wread({privacy, US}) of
                [] ->
                    NewLists = [{Name, List}],
                    mnesia:write(#privacy{us = US, lists = NewLists}),
                    ok;
                [#privacy{lists = Lists} = P] ->
                    NewLists1 = lists:keydelete(Name, 1, Lists),
                    NewLists = [{Name, List} | NewLists1],
                    mnesia:write(P#privacy{lists = NewLists}),
                    ok
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

remove_user(_HostType, LUser, LServer) ->
    F = fun() -> mnesia:delete({privacy, {LUser, LServer}}) end,
    mnesia:transaction(F).

remove_domain(_HostType, LServer) ->
    F = fun(#privacy{us = {_, LS}} = Rec, _) when LS =:= LServer ->
                mnesia:delete_object(Rec),
                ok;
           (_, _) ->
                ok
        end,
    mnesia:transaction(fun mnesia:foldl/3, [F, ok, privacy]).
