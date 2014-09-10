%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_db_mnesia.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Mnesia backend for mod_muc_light
%%% Created : 8 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
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

-module(mod_muc_light_db_mnesia).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([
         start/2,
         stop/2,

         create_room/3,
         destroy_room/2,
         room_exists/1,

         get_configuration/1,
         get_configuration/2,
         set_configuration/2,
         set_configuration/3,

         get_room_process/1,
         register_room_process/2,
         unregister_room_process/1,

         get_occupants/1,
         modify_occupants/3
        ]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(TAB, muc_light_room).

-record(?TAB, {
          room_us :: {User :: binary(), Server :: binary()},
          config :: [{atom(), term()}],
          occupants :: [binary()]
         }).

%%====================================================================
%% API
%%====================================================================

start(_Host, _MUCHost) ->
    init_tables().

stop(_Host, _MUCHost) ->
    ok.


create_room(RoomJID, OwnerJID, Configuration) ->
    T = fun() ->
                RoomUS = to_us(RoomJID),
                case mnesia:wread({?TAB, RoomUS}) of
                    [_] ->
                        {error, exists};
                    [] ->
                        RoomRecord = #?TAB{
                                         room_us = RoomUS,
                                         config = Configuration,
                                         occupants = [OwnerJID]
                                        },
                        mnesia:write(RoomRecord),
                        ok
                end
        end,
    {atomic, Res} = mnesia:transaction(T),
    Res.

destroy_room(RoomJID, UserJID) ->
    T = fun() ->
                RoomUS = to_us(RoomJID),
                case mnesia:wread({?TAB, RoomUS}) of
                    [] -> {error, not_exists};
                    [#?TAB{ occupants = [UserJID] }] -> mnesia:delete(?TAB, RoomUS);
                    [#?TAB{ occupants = [] }] -> mnesia:delete(?TAB, RoomUS);
                    _ -> {error, not_allowed}
                end
        end,
    {atomic, Res} = mnesia:transaction(T),
    Res.

room_exists(RoomJID) ->
    mnesia:dirty_read(?TAB, to_us(RoomJID)) =/= [].

get_configuration(RoomJID) ->
    case mnesia:dirty_read(?TAB, to_us(RoomJID)) of
        [] ->
            {error, not_exists};
        [#?TAB{ config = Config }] ->
            {ok, Config}
    end.

get_configuration(RoomJID, Option) ->
    case get_configuration(RoomJID) of
        {ok, Config} ->
            {_, Value} = lists:keyfind(Option, 1, Config),
            {ok, Value};
        Error ->
            Error
    end.

set_configuration(RoomJID, ConfigurationChanges) ->
    T = fun() ->
                RoomUS = to_us(RoomJID),
                case mnesia:wread(?TAB, RoomUS) of
                    [] ->
                        {error, not_exists};
                    [#?TAB{ config = Config } = Rec] ->
                        NewConfig = lists:foldl(
                                      fun({Key, Val}, ConfigAcc) ->
                                              lists:keystore(Key, 1, ConfigAcc,
                                                             {Key, Val})
                                      end, ConfigurationChanges, Config),
                        mnesia:write(Rec#?TAB{ config = NewConfig })
                end
        end,
    {atomic, Res} = mnesia:transaction(T),
    Res.

set_configuration(RoomJID, Option, Value) ->
    set_configuration(RoomJID, [{Option, Value}]).

get_room_process(_RoomJID) ->
    throw(not_implemented).

register_room_process(_RoomJID, _Pid) ->
    throw(not_implemented).

unregister_room_process(_RoomJID) ->
    throw(not_implemented).


get_occupants(RoomJID) ->
    case mnesia:dirty_read(?TAB, to_us(RoomJID)) of
        [] ->
            {error, not_exists};
        [#?TAB{ occupants = Occupants }] ->
            {ok, expand_occupants(Occupants)}
    end.

modify_occupants(RoomJID, UserJID, ItemsToChange) ->
    {atomic, Res} = mnesia:transaction(
                      fun modify_occupants_main/3,
                      [RoomJID, UserJID, ItemsToChange]),
    Res.


%%====================================================================
%% Internal functions
%%====================================================================

init_tables() ->
    mnesia:create_table(?TAB,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, ?TAB)}]),
    mnesia:add_table_copy(?TAB, node(), disc_copies),
    set = mnesia:table_info(schema, type). % checks if table exists

to_us(#jid{ luser = LUser, lserver = LServer }) -> {LUser, LServer}.

expand_occupants([Owner | Members]) ->
    {ok, [{Owner, owner} | [{Member, member} || Member <- Members]]}.

modify_occupants_main(RoomJID, UserJID, ItemsToChange) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread(?TAB, RoomUS) of
        [] ->
            {error, not_exists};
        [#?TAB{ occupants = [ UserJID | _ ] = Occupants } = Rec] ->
            case apply_occupants_change(Occupants, ItemsToChange) of
                {ok, [NewOwner | Members] = NewOccupants} ->
                    ExtraItemsChanged
                        = get_owner_change_item(UserJID, NewOwner, Members),
                    mnesia:write(Rec#?TAB{ occupants = NewOccupants }),
                    {ok, ExtraItemsChanged ++ ItemsToChange, NewOccupants};
                Error ->
                    Error
            end;
        _ ->
            {error, not_allowed}
    end.

get_owner_change_item(OldOwner, OldOwner, _) ->
    [];
get_owner_change_item(OldOwner, NewOwner, Members) ->
    [{NewOwner, owner} |
    case lists:member(OldOwner, Members) of
        true -> [{OldOwner, member}];
        false -> []
    end].

apply_occupants_change(Occupants, []) ->
    {ok, Occupants};
apply_occupants_change(Occupants, [{JID, none} | RToChange]) ->
    apply_occupants_change(lists:delete(JID, Occupants), RToChange);
apply_occupants_change([JID], [{JID, member} | _]) ->
    {error, only_owner_in_room};
apply_occupants_change(Occupants, [{JID, member} | RToChange]) ->
    [NewOwner | Members] = lists:delete(JID, Occupants),
    apply_occupants_change([NewOwner, JID | Members], RToChange);
apply_occupants_change(Occupants, [{JID, owner} | RToChange]) ->
    apply_occupants_change([JID | lists:delete(JID, Occupants)], RToChange).
