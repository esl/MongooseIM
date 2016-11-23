%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

init(Host, Opts) ->
    mnesia:create_table(muc_room,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, muc_room)}]),
    mnesia:create_table(muc_registered,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, muc_registered)}]),
    mnesia:add_table_copy(muc_room, node(), disc_copies),
    mnesia:add_table_copy(muc_registered, node(), disc_copies),
    MyHost = gen_mod:get_opt_subhost(Host, Opts, <<"conference.@HOST@">>),
    update_tables(MyHost),
    mnesia:add_table_index(muc_registered, nick),
    ok.

-spec store_room(ejabberd:server(), ejabberd:server(), mod_muc:room(), list())
            -> {'aborted',_} | {'atomic',_}.
store_room(_LServer, Host, Name, Opts) ->
    F = fun() ->
                mnesia:write(#muc_room{name_host = {Name, Host},
                                       opts = Opts})
        end,
    mnesia:transaction(F).


-spec restore_room(ejabberd:server(), ejabberd:server(), mod_muc:room())
                                    -> 'error' | 'undefined' | [any()].
restore_room(_LServer, Host, Name) ->
    case catch mnesia:dirty_read(muc_room, {Name, Host}) of
        [#muc_room{opts = Opts}] ->
            Opts;
        _ ->
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
    mnesia:dirty_select(
                 muc_room, [{#muc_room{name_host = {'_', Host}, _ = '_'},
                             [],
                             ['$_']}]).

-spec can_use_nick(ejabberd:server(), ejabberd:server(),
                   ejabberd:jid(), mod_muc:nick()) -> boolean().
can_use_nick(_LServer, Host, JID, Nick) ->
    {LUser, LServer, _} = jid:to_lower(JID),
    LUS = {LUser, LServer},
    case catch mnesia:dirty_select(
                 muc_registered,
                 [{#muc_registered{us_host = '$1',
                                   nick = Nick,
                                   _ = '_'},
                   [{'==', {element, 2, '$1'}, Host}],
                   ['$_']}]) of
        {'EXIT', _Reason} ->
            true;
        [] ->
            true;
        [#muc_registered{us_host = {U, _Host}}] ->
            U == LUS
    end.

get_nick(_LServer, Host, From) ->
    {LUser, LServer, _} = jid:to_lower(From),
    LUS = {LUser, LServer},
    case mnesia:dirty_read(muc_registered, {LUS, Host}) of
    [] -> error;
    [#muc_registered{nick = Nick}] -> Nick
    end.

set_nick(_LServer, Host, From, Nick) ->
    {LUser, LServer, _} = jid:to_lower(From),
    LUS = {LUser, LServer},
    F = fun () ->
        case Nick of
            <<"">> ->
            mnesia:delete({muc_registered, {LUS, Host}}),
            ok;
            _ ->
            Allow = case mnesia:select(
                       muc_registered,
                       [{#muc_registered{us_host =
                                 '$1',
                             nick = Nick,
                             _ = '_'},
                     [{'==', {element, 2, '$1'},
                       Host}],
                     ['$_']}]) of
                    [] -> true;
                    [#muc_registered{us_host = {U, _Host}}] ->
                    U == LUS
                end,
            if Allow ->
                mnesia:write(#muc_registered{
                        us_host = {LUS, Host},
                        nick = Nick}),
                ok;
               true ->
                false
            end
        end
    end,
    mnesia:transaction(F).

-spec update_tables(ejabberd:server()) -> any().
update_tables(Host) ->
    update_muc_room_table(Host),
    update_muc_registered_table(Host).

-spec update_muc_room_table(ejabberd:server()) -> any().
update_muc_room_table(Host) ->
    Fields = record_info(fields, muc_room),
    case mnesia:table_info(muc_room, attributes) of
        Fields ->
            ok;
        [name, opts] ->
            ?INFO_MSG("Converting muc_room table from {name, opts} format", []),
            {atomic, ok} = mnesia:create_table(
                             mod_muc_tmp_table,
                             [{disc_only_copies, [node()]},
                              {type, bag},
                              {local_content, true},
                              {record_name, muc_room},
                              {attributes, record_info(fields, muc_room)}]),
            mnesia:transform_table(muc_room, ignore, Fields),
            F1 = fun() ->
                         mnesia:write_lock_table(mod_muc_tmp_table),
                         mnesia:foldl(
                           fun(#muc_room{name_host = Name} = R, _) ->
                                   mnesia:dirty_write(
                                     mod_muc_tmp_table,
                                     R#muc_room{name_host = {Name, Host}})
                           end, ok, muc_room)
                 end,
            mnesia:transaction(F1),
            mnesia:clear_table(muc_room),
            F2 = fun() ->
                         mnesia:write_lock_table(muc_room),
                         mnesia:foldl(
                           fun(R, _) ->
                                   mnesia:dirty_write(R)
                           end, ok, mod_muc_tmp_table)
                 end,
            mnesia:transaction(F2),
            mnesia:delete_table(mod_muc_tmp_table);
        _ ->
            ?INFO_MSG("Recreating muc_room table", []),
            mnesia:transform_table(muc_room, ignore, Fields)
    end.

-spec update_muc_registered_table(ejabberd:server()) -> any().
update_muc_registered_table(Host) ->
    Fields = record_info(fields, muc_registered),
    case mnesia:table_info(muc_registered, attributes) of
        Fields ->
            ok;
        [user, nick] ->
            ?INFO_MSG("Converting muc_registered table from {user, nick} format", []),
            {atomic, ok} = mnesia:create_table(
                             mod_muc_tmp_table,
                             [{disc_only_copies, [node()]},
                              {type, bag},
                              {local_content, true},
                              {record_name, muc_registered},
                              {attributes, record_info(fields, muc_registered)}]),
            mnesia:del_table_index(muc_registered, nick),
            mnesia:transform_table(muc_registered, ignore, Fields),
            F1 = fun() ->
                         mnesia:write_lock_table(mod_muc_tmp_table),
                         mnesia:foldl(
                           fun(#muc_registered{us_host = US} = R, _) ->
                                   mnesia:dirty_write(
                                     mod_muc_tmp_table,
                                     R#muc_registered{us_host = {US, Host}})
                           end, ok, muc_registered)
                 end,
            mnesia:transaction(F1),
            mnesia:clear_table(muc_registered),
            F2 = fun() ->
                         mnesia:write_lock_table(muc_registered),
                         mnesia:foldl(
                           fun(R, _) ->
                                   mnesia:dirty_write(R)
                           end, ok, mod_muc_tmp_table)
                 end,
            mnesia:transaction(F2),
            mnesia:delete_table(mod_muc_tmp_table);
        _ ->
            ?INFO_MSG("Recreating muc_registered table", []),
            mnesia:transform_table(muc_registered, ignore, Fields)
    end.
