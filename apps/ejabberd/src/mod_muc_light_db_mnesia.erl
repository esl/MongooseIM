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
         destroy_room/1,
         room_exists/1,
         get_user_rooms/1,
         remove_user/1,

         get_configuration/1,
         get_configuration/2,
         set_configuration/2,
         set_configuration/3,

         get_affiliated_users/1,
         modify_affiliated_users/2
        ]).

%% Extra API for testing
-export([
         force_destroy_room/1
        ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-define(ROOM_TAB, muc_light_room).
-define(USER_ROOM_TAB, muc_light_user_room).

-record(?ROOM_TAB, {
          room_us :: {ejabberd:username(), ejabberd:server()},
          config :: [{atom(), term()}],
          affiliated_users :: affiliated_users()
         }).

-record(?USER_ROOM_TAB, {
           user_us :: {ejabberd:username(), ejabberd:server()},
           room_us :: {ejabberd:username(), ejabberd:server()}
          }).

%%====================================================================
%% API
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start(ejabberd:server(), ejabberd:server()) -> ok.
start(_Host, _MUCHost) ->
    init_tables().

-spec stop(ejabberd:server(), ejabberd:server()) -> ok.
stop(_Host, _MUCHost) ->
    ok.

%% ------------------------ General room management ------------------------

-spec create_room(jid(), ljid(), configuration()) -> ok | {error, exists}.
create_room(RoomJID, Owner, Configuration) ->
    {atomic, Res} = mnesia:transaction(
                     fun create_room_transaction/3,
                     [RoomJID, Owner, Configuration]),
    Res.

-spec destroy_room(jid()) -> ok | {error, not_exists | not_empty}.
destroy_room(RoomJID) ->
    {atomic, Res} = mnesia:transaction(
                     fun destroy_room_transaction/1,
                     [RoomJID]),
    Res.

-spec room_exists(jid()) -> boolean().
room_exists(RoomJID) ->
    mnesia:dirty_read(?ROOM_TAB, to_us(RoomJID)) =/= [].

-spec get_user_rooms(ljid()) -> {ok, [RoomBareJID :: binary()]} | {error, term()}.
get_user_rooms(UserLJID) ->
    UsersRooms = mnesia:dirty_read(?USER_ROOM_TAB, to_us(UserLJID)),
    {ok, [ <<RoomU/binary, $@, RoomS/binary>>
           || #?USER_ROOM_TAB{ room_us = {RoomU, RoomS} } <- UsersRooms ]}.

-spec remove_user(ljid()) -> ok | {error, term()}.
remove_user(UserLJID) ->
    {atomic, Res} = mnesia:transaction(
                     fun remove_user_transaction/1,
                     [UserLJID]),
    Res.

%% ------------------------ Configuration manipulation ------------------------

-spec get_configuration(jid()) -> {ok, configuration()} | {error, not_exists}.
get_configuration(RoomJID) ->
    case mnesia:dirty_read(?ROOM_TAB, to_us(RoomJID)) of
        [] ->
            {error, not_exists};
        [#?ROOM_TAB{ config = Config }] ->
            {ok, Config}
    end.

-spec get_configuration(jid(), atom()) ->
    {ok, term()} | {error, not_exists | invalid_opt}.
get_configuration(RoomJID, Option) ->
    case get_configuration(RoomJID) of
        {ok, Config} ->
            case lists:keyfind(Option, 1, Config) of
                {_, Value} -> {ok, Value};
                false -> {error, invalid_opt}
            end;
        Error ->
            Error
    end.

-spec set_configuration(jid(), configuration()) -> ok | {error, not_exists}.
set_configuration(RoomJID, ConfigurationChanges) ->
    {atomic, Res} = mnesia:transaction(
                     fun set_configuration_transaction/2,
                     [RoomJID, ConfigurationChanges]),
    Res.

-spec set_configuration(jid(), atom(), term()) -> ok | {error, not_exists}.
set_configuration(RoomJID, Option, Value) ->
    set_configuration(RoomJID, [{Option, Value}]).

%% ------------------------ Affiliations manipulation ------------------------

-spec get_affiliated_users(jid()) -> {ok, affiliated_users()} | {error, not_exists}.
get_affiliated_users(RoomJID) ->
    case mnesia:dirty_read(?ROOM_TAB, to_us(RoomJID)) of
        [] ->
            {error, not_exists};
        [#?ROOM_TAB{ affiliated_users = Affiliations }] ->
            {ok, Affiliations}
    end.

-spec modify_affiliated_users(jid(), affiliated_users()) ->
    {ok, CurrentAffiliations :: affiliated_users(), ChangedAffiliations :: affiliated_users()}
    | {error, not_exists | only_owner_in_room}.
modify_affiliated_users(RoomJID, AffiliationsToChange) ->
    {atomic, Res} = mnesia:transaction(
                      fun modify_affiliated_users_transaction/2,
                      [RoomJID, AffiliationsToChange]),
    Res.

%%====================================================================
%% API for tests
%%====================================================================

-spec force_destroy_room(ljid()) -> ok.
force_destroy_room(RoomJID) ->
    mnesia:dirty_delete(?ROOM_TAB, to_us(RoomJID)).

%%====================================================================
%% Internal functions
%%====================================================================

%% ------------------------ Utils ------------------------

-spec to_us(jid() | ljid()) -> {LUSer :: binary(), LServer :: binary()}.
to_us(#jid{ luser = LUser, lserver = LServer }) -> {LUser, LServer};
to_us({LUser, LServer, _}) -> {LUser, LServer}.

%% ------------------------ Schema creation ------------------------

-spec init_tables() -> ok.
init_tables() ->
    create_table(?ROOM_TAB,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, ?ROOM_TAB)}]),
    create_table(?USER_ROOM_TAB,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, ?ROOM_TAB)},
                  {type, bag}]),
    ok.

-spec create_table(atom(), list()) -> ok.
create_table(Name, TabDef) ->
    case mnesia:create_table(Name, TabDef) of
        {atomic, ok} -> ok;
        {aborted, exists} -> ok
    end,
    case mnesia:add_table_copy(Name, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, exists} -> ok
    end.

%% ------------------------ General room management ------------------------

-spec create_room_transaction(jid(), ljid(), configuration()) ->
    ok | {error, exists}.
create_room_transaction(RoomJID, Owner, Configuration) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [_] ->
            {error, exists};
        [] ->
            RoomRecord = #?ROOM_TAB{
                             room_us = RoomUS,
                             config = Configuration,
                             affiliated_users = [{Owner, owner}]
                            },
            ok = mnesia:write(RoomRecord),
            UserRoomRecord = #?USER_ROOM_TAB{
                                 user_us = to_us(Owner),
                                 room_us = RoomUS
                                },
            ok = mnesia:write(UserRoomRecord)
    end.

-spec destroy_room_transaction(jid()) -> ok | {error, not_exists | not_empty}.
destroy_room_transaction(RoomJID) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [] -> {error, not_exists};
        [#?ROOM_TAB{ affiliated_users = [] }] -> mnesia:delete({?ROOM_TAB, RoomUS});
        _ -> {error, not_empty}
    end.

-spec remove_user_transaction(ljid()) -> ok.
remove_user_transaction(UserLJID) ->
    lists:foreach(
      fun(#?USER_ROOM_TAB{ room_us = RoomUS }) ->
        modify_affiliated_users_transaction(RoomUS, [{UserLJID, none}])
      end, mnesia:read(?USER_ROOM_TAB, to_us(UserLJID))),
    ok.

%% ------------------------ Configuration manipulation ------------------------

-spec set_configuration_transaction(jid(), configuration()) ->
    ok | {error, not_exists}.
set_configuration_transaction(RoomJID, ConfigurationChanges) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [] ->
            {error, not_exists};
        [#?ROOM_TAB{ config = Config } = Rec] ->
            NewConfig = lists:foldl(
                          fun({Key, Val}, ConfigAcc) ->
                                  lists:keystore(Key, 1, ConfigAcc, {Key, Val})
                          end, Config, ConfigurationChanges),
            mnesia:write(Rec#?ROOM_TAB{ config = NewConfig })
    end.

%% ------------------------ Affiliations manipulation ------------------------

-spec modify_affiliated_users_transaction(
        jid() | {ejabberd:user(), ejabberd:server()}, affiliated_users()) ->
    {ok, NewAffiliations :: affiliated_users(),
     AffiliationsChanged :: affiliated_users()} | {error, only_owner_in_room}.
modify_affiliated_users_transaction(Room, AffiliationsToChange) ->
    RoomUS = to_us(Room),
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [] ->
            {error, not_exists};
        [#?ROOM_TAB{ affiliated_users = Affiliations } = Rec] ->
            case mod_muc_light_utils:change_affiliated_users(Affiliations, AffiliationsToChange) of
                {ok, NewAffiliations, AffiliationsChanged, JoiningUsers, LeavingUsers} ->
                    ok = mnesia:write(Rec#?ROOM_TAB{ affiliated_users = NewAffiliations }),
                    update_users_rooms(RoomUS, JoiningUsers, LeavingUsers),
                    {ok, NewAffiliations, AffiliationsChanged};
                Error ->
                    Error
            end
    end.

-spec update_users_rooms({ejabberd:user(), ejabberd:server()}, [ljid()], [ljid()]) -> ok.
update_users_rooms(RoomUS, [User | RJoiningUsers], LeavingUsers) ->
    ok = mnesia:write(#?USER_ROOM_TAB{ user_us = to_us(User), room_us = RoomUS }),
    update_users_rooms(RoomUS, RJoiningUsers, LeavingUsers);
update_users_rooms(RoomUS, [], [User | RLeavingUsers]) ->
    ok = mnesia:delete_object(#?USER_ROOM_TAB{ user_us = to_us(User), room_us = RoomUS }),
    update_users_rooms(RoomUS, [], RLeavingUsers);
update_users_rooms(_RoomUS, [], []) ->
    ok.

