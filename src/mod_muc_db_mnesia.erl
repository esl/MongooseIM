%%%----------------------------------------------------------------------
%%% Based on file mod_muc.
%%%
%%% Original header and copyright notice:
%%%
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
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

-module(mod_muc_db_mnesia).
-behaviour(mod_muc_db).
-export([init/2,
         store_room/4,
         restore_room/3,
         forget_room/3,
         get_rooms/2,
         can_use_nick/4,
         get_nick/3,
         set_nick/4,
         unset_nick/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc.hrl").

init(_Host, _Opts) ->
    mnesia:create_table(muc_room,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, muc_room)}]),
    mnesia:create_table(muc_registered,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, muc_registered)}]),
    mnesia:add_table_copy(muc_room, node(), disc_copies),
    mnesia:add_table_copy(muc_registered, node(), disc_copies),
    mnesia:add_table_index(muc_registered, nick),
    ok.

-spec store_room(ejabberd:server(), ejabberd:server(), mod_muc:room(), list())
            -> ok | {error, term()}.
store_room(_ServerHost, MucHost, RoomName, Opts) ->
    F = fun() ->
                mnesia:write(#muc_room{name_host = {RoomName, MucHost},
                                       opts = Opts})
        end,
    Result = mnesia:transaction(F),
    case Result of
        {atomic, _} ->
            ok;
        _ ->
            ?ERROR_MSG("event=store_room_failed room=~ts reason=~p",
                       [RoomName, Result]),
            {error, Result}
    end.

restore_room(_ServerHost, MucHost, RoomName) ->
    try mnesia:dirty_read(muc_room, {RoomName, MucHost}) of
        [#muc_room{opts = Opts}] ->
            {ok, Opts};
        [] ->
            {error, room_not_found};
        Other ->
            {error, Other}
    catch Class:Reason ->
        ?ERROR_MSG("event=restore_room_failed room=~ts reason=~p:~p",
                   [RoomName, Class, Reason]),
        {error, {Class, Reason}}
    end.

-spec forget_room(ejabberd:server(), ejabberd:server(), mod_muc:room()) ->
    ok | {error, term()}.
forget_room(_ServerHost, MucHost, RoomName) ->
    F = fun() ->
                mnesia:delete({muc_room, {RoomName, MucHost}})
        end,
    Result = mnesia:transaction(F),
    case Result of
        {atomic, _} ->
            ok;
        _ ->
            ?ERROR_MSG("event=forget_room_failed room=~ts reason=~p",
                       [RoomName, Result]),
            {error, Result}
    end.

get_rooms(_Lserver, MucHost) ->
    Query = [{#muc_room{name_host = {'_', MucHost}, _ = '_'},
             [],
             ['$_']}],
    try
        {ok, mnesia:dirty_select(muc_room, Query)}
    catch Class:Reason ->
        ?ERROR_MSG("event=get_rooms_failed reason=~p:~p",
                   [Class, Reason]),
        {error, {Class, Reason}}
    end.

-spec can_use_nick(ejabberd:server(), ejabberd:server(),
                   ejabberd:jid(), mod_muc:nick()) -> boolean().
can_use_nick(_ServerHost, MucHost, JID, Nick) ->
    LUS = jid:to_lus(JID),
    can_use_nick_internal(MucHost, Nick, LUS).

can_use_nick_internal(MucHost, Nick, LUS) ->
    Query = [{#muc_registered{us_host = '$1', nick = Nick, _ = '_'},
             [{'==', {element, 2, '$1'}, MucHost}],
             ['$_']}],
    try mnesia:dirty_select(muc_registered, Query) of
        [] ->
            true;
        [#muc_registered{us_host = {U, _Host}}] ->
            U == LUS
    catch Class:Reason ->
        ?ERROR_MSG("event=can_use_nick_failed jid=~ts nick=~ts reason=~p:~p",
                   [jid:to_binary(LUS), Nick, Class, Reason]),
        false
    end.

get_nick(_ServerHost, MucHost, From) ->
    LUS = jid:to_lus(From),
    try mnesia:dirty_read(muc_registered, {LUS, MucHost}) of
        [] ->
            {error, not_registered};
        [#muc_registered{nick = Nick}] ->
            {ok, Nick}
    catch Class:Reason ->
        ?ERROR_MSG("event=get_nick_failed jid=~ts reason=~p:~p",
                   [jid:to_binary(From), Class, Reason]),
        {error, {Class, Reason}}
    end.

set_nick(_ServerHost, MucHost, From, Nick)
      when is_binary(Nick), Nick =/= <<>> ->
    LUS = jid:to_lus(From),
    F = fun () ->
            case can_use_nick_internal(MucHost, Nick, LUS) of
                true ->
                    Object = #muc_registered{us_host = {LUS, MucHost}, nick = Nick},
                    mnesia:write(Object),
                    ok;
                false ->
                    {error, conflict}
            end
        end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        ErrorResult ->
            ?ERROR_MSG("event=set_nick_failed jid=~ts nick=~ts reason=~1000p",
                       [jid:to_binary(From), Nick, ErrorResult]),
            {error, ErrorResult}
    end.

unset_nick(_ServerHost, MucHost, From) ->
    LUS = jid:to_lus(From),
    F = fun () ->
            mnesia:delete({muc_registered, {LUS, MucHost}})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            ok;
        ErrorResult ->
            ?ERROR_MSG("event=unset_nick_failed jid=~ts reason=~1000p",
                       [jid:to_binary(From), ErrorResult]),
            {error, ErrorResult}
    end.
