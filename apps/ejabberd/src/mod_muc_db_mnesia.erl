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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
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
         set_nick/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("ejabberd/include/mod_muc.hrl").

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
            -> {'aborted', _} | {'atomic', _}.
store_room(_LServer, Host, Name, Opts) ->
    F = fun() ->
                mnesia:write(#muc_room{name_host = {Name, Host},
                                       opts = Opts})
        end,
    mnesia:transaction(F).


-spec restore_room(ejabberd:server(), ejabberd:server(), mod_muc:room())
                                    -> 'error' | 'undefined' | [any()].
restore_room(_LServer, Host, Name) ->
    try mnesia:dirty_read(muc_room, {Name, Host}) of
        [#muc_room{opts = Opts}] ->
            Opts;
        _ ->
            error
        catch Class:Reason ->
            ?ERROR_MSG("issue=restore_room_failed, reason=~p:~p", [Class, Reason]),
            error
    end.

-spec forget_room(ejabberd:server(), ejabberd:server(), mod_muc:room()) -> 'ok'.
forget_room(_LServer, Host, Name) ->
    F = fun() ->
                mnesia:delete({muc_room, {Name, Host}})
        end,
    mnesia:transaction(F),
    ejabberd_hooks:run(forget_room, Host, [Host, Name]),
    ok.

get_rooms(_Lserver, Host) ->
    Query = [{#muc_room{name_host = {'_', Host}, _ = '_'},
             [],
             ['$_']}],
    mnesia:dirty_select(muc_room, Query).

-spec can_use_nick(ejabberd:server(), ejabberd:server(),
                   ejabberd:jid(), mod_muc:nick()) -> boolean().
can_use_nick(_LServer, Host, JID, Nick) ->
    LUS = jid_to_lower_us(JID),
    can_use_nick_internal(Host, Nick, LUS).

can_use_nick_internal(Host, Nick, LUS) ->
    Query = [{#muc_registered{us_host = '$1', nick = Nick, _ = '_'},
             [{'==', {element, 2, '$1'}, Host}],
             ['$_']}],
    try mnesia:dirty_select(muc_registered, Query) of
        [] ->
            true;
        [#muc_registered{us_host = {U, _Host}}] ->
            U == LUS
        catch Class:Reason ->
            ?ERROR_MSG("issue=can_use_nick_failed, reason=~p:~p", [Class, Reason]),
            true
    end.

get_nick(_LServer, Host, From) ->
    LUS = jid_to_lower_us(From),
    case mnesia:dirty_read(muc_registered, {LUS, Host}) of
        [] ->
            error;
        [#muc_registered{nick = Nick}] ->
            Nick
    end.

set_nick(_LServer, Host, From, Nick) ->
    LUS = jid_to_lower_us(From),
    F = fun () ->
                set_nick_transaction_body(Host, Nick, LUS)
        end,
    mnesia:transaction(F).


set_nick_transaction_body(Host, <<>>, LUS) ->
    mnesia:delete({muc_registered, {LUS, Host}});
set_nick_transaction_body(Host, Nick, LUS) ->
    case can_use_nick_internal(Host, Nick, LUS) of
        true ->
            mnesia:write(#muc_registered{us_host = {LUS, Host}, nick = Nick}),
            ok;
        false ->
            false
    end.

jid_to_lower_us(JID) ->
    {LUser, LServer, _} = jid:to_lower(JID),
    {LUser, LServer}.
