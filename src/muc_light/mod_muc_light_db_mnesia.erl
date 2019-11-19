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

         create_room/4,
         destroy_room/1,
         room_exists/1,
         get_user_rooms/2,
         get_user_rooms_count/2,
         remove_user/2,

         get_config/1,
         get_config/2,
         set_config/3,
         set_config/4,

         get_blocking/2,
         get_blocking/3,
         set_blocking/3,

         get_aff_users/1,
         modify_aff_users/4,

         get_info/1
        ]).

%% Extra API for testing
-export([
         force_clear/0
        ]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-record(muc_light_room, {
          room :: jid:simple_bare_jid(),
          config :: [{atom(), term()}],
          aff_users :: aff_users(),
          version :: binary()
         }).

-record(muc_light_user_room, {
           user :: jid:simple_bare_jid(),
           room :: jid:simple_bare_jid()
          }).

-record(muc_light_blocking, {
           user :: jid:simple_bare_jid(),
           item :: {user | room, jid:simple_bare_jid()}
          }).

-type muc_light_room() :: #muc_light_room{}.
-type muc_light_blocking() :: #muc_light_blocking{}.

%%====================================================================
%% API
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start(Host :: jid:server(), MUCHost :: jid:server()) -> ok.
start(_Host, _MUCHost) ->
    init_tables().

-spec stop(Host :: jid:server(), MUCHost :: jid:server()) -> ok.
stop(_Host, _MUCHost) ->
    ok.

%% ------------------------ General room management ------------------------

-spec create_room(RoomUS :: jid:simple_bare_jid(), Config :: mod_muc_light_room_config:kv(),
                  AffUsers :: aff_users(), Version :: binary()) ->
    {ok, FinalRoomUS :: jid:simple_bare_jid()} | {error, exists}.
create_room(RoomUS, Config, AffUsers, Version) ->
    {atomic, Res} = mnesia:transaction(fun create_room_transaction/4,
                                       [RoomUS, Config, AffUsers, Version]),
    Res.

-spec destroy_room(RoomUS :: jid:simple_bare_jid()) -> ok | {error, not_exists | not_empty}.
destroy_room(RoomUS) ->
    {atomic, Res} = mnesia:transaction(fun destroy_room_transaction/1, [RoomUS]),
    Res.

-spec room_exists(RoomUS :: jid:simple_bare_jid()) -> boolean().
room_exists(RoomUS) ->
    mnesia:dirty_read(muc_light_room, RoomUS) =/= [].

-spec get_user_rooms(UserUS :: jid:simple_bare_jid(),
                     MUCServer :: jid:lserver() | undefined) ->
    [RoomUS :: jid:simple_bare_jid()].
get_user_rooms(UserUS, _MUCHost) ->
    UsersRooms = mnesia:dirty_read(muc_light_user_room, UserUS),
    [ UserRoom#muc_light_user_room.room || UserRoom <- UsersRooms ].

-spec get_user_rooms_count(UserUS :: jid:simple_bare_jid(),
                           MUCServer :: jid:lserver()) ->
    non_neg_integer().
get_user_rooms_count(UserUS, _MUCServer) ->
    length(mnesia:dirty_read(muc_light_user_room, UserUS)).

-spec remove_user(UserUS :: jid:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return() | {error, term()}.
remove_user(UserUS, Version) ->
    mnesia:dirty_delete(muc_light_blocking, UserUS),
    {atomic, Res} = mnesia:transaction(fun remove_user_transaction/2, [UserUS, Version]),
    Res.

%% ------------------------ Configuration manipulation ------------------------

-spec get_config(RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), Version :: binary()} | {error, not_exists}.
get_config(RoomUS) ->
    case mnesia:dirty_read(muc_light_room, RoomUS) of
        [] -> {error, not_exists};
        [#muc_light_room{ config = Config, version = Version }] -> {ok, Config, Version}
    end.

-spec get_config(RoomUS :: jid:simple_bare_jid(), Key :: atom()) ->
    {ok, term(), Version :: binary()} | {error, not_exists | invalid_opt}.
get_config(RoomUS, Option) ->
    case get_config(RoomUS) of
        {ok, Config, Version} ->
            case lists:keyfind(Option, 1, Config) of
                {_, Value} -> {ok, Value, Version};
                false -> {error, invalid_opt}
            end;
        Error ->
            Error
    end.

-spec set_config(RoomUS :: jid:simple_bare_jid(),
                 Config :: mod_muc_light_room_config:kv(),
                 Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomUS, ConfigChanges, Version) ->
    {atomic, Res} = mnesia:transaction(fun set_config_transaction/3,
                                       [RoomUS, ConfigChanges, Version]),
    Res.

-spec set_config(RoomUS :: jid:simple_bare_jid(),
                 Key :: atom(), Val :: term(), Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomJID, Key, Val, Version) ->
    set_config(RoomJID, [{Key, Val}], Version).

%% ------------------------ Blocking manipulation ------------------------

-spec get_blocking(UserUS :: jid:simple_bare_jid(), MUCServer :: jid:lserver()) ->
    [blocking_item()].
get_blocking(UserUS, _MUCServer) ->
    [ {What, deny, Who}
      || #muc_light_blocking{ item = {What, Who} } <- dirty_get_blocking_raw(UserUS) ].

-spec get_blocking(UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   WhatWhos :: [{blocking_what(), jid:simple_bare_jid()}]) ->
    blocking_action().
get_blocking(UserUS, _MUCServer, WhatWhos) ->
    Blocklist = dirty_get_blocking_raw(UserUS),
    case lists:any(
           fun(WhatWho) ->
                   lists:keyfind(WhatWho, #muc_light_blocking.item, Blocklist) =/= false
           end, WhatWhos) of
        true -> deny;
        false -> allow
    end.

-spec set_blocking(UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   BlockingItems :: [blocking_item()]) -> ok.
set_blocking(_UserUS, _MUCServer, []) ->
    ok;
set_blocking(UserUS, MUCServer, [{What, deny, Who} | RBlockingItems]) ->
    mnesia:dirty_write(#muc_light_blocking{ user = UserUS, item = {What, Who} }),
    set_blocking(UserUS, MUCServer, RBlockingItems);
set_blocking(UserUS, MUCServer, [{What, allow, Who} | RBlockingItems]) ->
    mnesia:dirty_delete_object(#muc_light_blocking{ user = UserUS, item = {What, Who} }),
    set_blocking(UserUS, MUCServer, RBlockingItems).

%% ------------------------ Affiliations manipulation ------------------------

-spec get_aff_users(RoomUS :: jid:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.
get_aff_users(RoomUS) ->
    case mnesia:dirty_read(muc_light_room, RoomUS) of
        [] -> {error, not_exists};
        [#muc_light_room{ aff_users = AffUsers, version = Version }] -> {ok, AffUsers, Version}
    end.

-spec modify_aff_users(RoomUS :: jid:simple_bare_jid(),
                       AffUsersChanges :: aff_users(),
                       ExternalCheck :: external_check_fun(),
                       Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users(RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    {atomic, Res} = mnesia:transaction(fun modify_aff_users_transaction/4,
                                       [RoomUS, AffUsersChanges, ExternalCheck, Version]),
    Res.

%% ------------------------ Misc ------------------------

-spec get_info(RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), aff_users(), Version :: binary()}
    | {error, not_exists}.
get_info(RoomUS) ->
    case mnesia:dirty_read(muc_light_room, RoomUS) of
        [] ->
            {error, not_exists};
        [#muc_light_room{ config = Config, aff_users = AffUsers, version = Version }] ->
            {ok, Config, AffUsers, Version}
    end.

%%====================================================================
%% API for tests
%%====================================================================

-spec force_clear() -> ok.
force_clear() ->
    lists:foreach(fun({RoomU, RoomS}) -> ejabberd_hooks:run(forget_room, RoomS, [RoomS, RoomU]) end,
                  mnesia:dirty_all_keys(muc_light_room)),
    lists:foreach(fun mnesia:clear_table/1,
                  [muc_light_room, muc_light_user_room, muc_light_blocking]).

%%====================================================================
%% Internal functions
%%====================================================================

%% ------------------------ Schema creation ------------------------

-spec init_tables() -> ok.
init_tables() ->
    create_table(muc_light_room,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, muc_light_room)}]),
    create_table(muc_light_user_room,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, muc_light_user_room)},
                  {type, bag}]),
    create_table(muc_light_blocking,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, muc_light_blocking)},
                  {type, bag}]),
    ok.

-spec create_table(Name :: atom(), TabDef :: list()) -> ok.
create_table(Name, TabDef) ->
    case mnesia:create_table(Name, TabDef) of
        {atomic, ok} -> ok;
        {aborted, exists} -> ok;
        {aborted, {already_exists, _}} -> ok
    end,
    case mnesia:add_table_copy(Name, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, exists} -> ok;
        {aborted, {already_exists, _, _}} -> ok
    end.

%% ------------------------ General room management ------------------------

%% Expects config to have unique fields!
-spec create_room_transaction(RoomUS :: jid:simple_bare_jid(),
                              Config :: mod_muc_light_room_config:kv(),
                              AffUsers :: aff_users(),
                              Version :: binary()) ->
    {ok, FinalRoomUS :: jid:simple_bare_jid()} | {error, exists}.
create_room_transaction({<<>>, Domain}, Config, AffUsers, Version) ->
    NodeCandidate = mongoose_bin:gen_from_timestamp(),
    NewNode = case mnesia:wread({muc_light_room, {NodeCandidate, Domain}}) of
                  [_] -> <<>>;
                  [] -> NodeCandidate
              end,
    create_room_transaction({NewNode, Domain}, Config, AffUsers, Version);
create_room_transaction(RoomUS, Config, AffUsers, Version) ->
    case mnesia:wread({muc_light_room, RoomUS}) of
        [_] ->
            {error, exists};
        [] ->
            RoomRecord = #muc_light_room{
                             room = RoomUS,
                             config = lists:sort(Config),
                             aff_users = AffUsers,
                             version = Version
                            },
            ok = mnesia:write(RoomRecord),
            lists:foreach(
              fun({User, _}) ->
                      UserRoomRecord = #muc_light_user_room{
                                           user = User,
                                           room = RoomUS
                                          },
                      ok = mnesia:write(UserRoomRecord)
              end, AffUsers),
            {ok, RoomUS}
    end.

-spec destroy_room_transaction(RoomUS :: jid:simple_bare_jid()) -> ok | {error, not_exists}.
destroy_room_transaction(RoomUS) ->
    case mnesia:wread({muc_light_room, RoomUS}) of
        [] ->
            {error, not_exists};
        [Rec] ->
            AffUsers = Rec#muc_light_room.aff_users,
            lists:foreach(
                fun({User, _}) ->
                    ok = mnesia:delete_object(#muc_light_user_room{user = User, room = RoomUS})
                end, AffUsers),
            mnesia:delete({muc_light_room, RoomUS})
    end.

-spec remove_user_transaction(UserUS :: jid:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return().
remove_user_transaction(UserUS, Version) ->
    lists:map(
      fun(#muc_light_user_room{ room = RoomUS }) ->
              {RoomUS, modify_aff_users_transaction(
                         RoomUS, [{UserUS, none}], fun(_, _) -> ok end, Version)}
      end, mnesia:read(muc_light_user_room, UserUS)).

%% ------------------------ Configuration manipulation ------------------------

%% Expects config changes to have unique fields!
-spec set_config_transaction(RoomUS :: jid:simple_bare_jid(),
                             ConfigChanges :: mod_muc_light_room_config:kv(),
                             Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config_transaction(RoomUS, ConfigChanges, Version) ->
    case mnesia:wread({muc_light_room, RoomUS}) of
        [] ->
            {error, not_exists};
        [#muc_light_room{ config = Config } = Rec] ->
            NewConfig = lists:ukeymerge(1, lists:sort(ConfigChanges), Config),
            mnesia:write(Rec#muc_light_room{ config = NewConfig, version = Version }),
            {ok, Rec#muc_light_room.version}
    end.

%% ------------------------ Blocking manipulation ------------------------

-spec dirty_get_blocking_raw(UserUS :: jid:simple_bare_jid()) -> [muc_light_blocking()].
dirty_get_blocking_raw(UserUS) ->
    mnesia:dirty_read(muc_light_blocking, UserUS).

%% ------------------------ Affiliations manipulation ------------------------

-spec modify_aff_users_transaction(RoomUS :: jid:simple_bare_jid(),
                                   AffUsersChanges :: aff_users(),
                                   ExternalCheck :: external_check_fun(),
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users_transaction(RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    case mnesia:wread({muc_light_room, RoomUS}) of
        [] ->
            {error, not_exists};
        [#muc_light_room{ aff_users = AffUsers } = RoomRec] ->
            case mod_muc_light_utils:change_aff_users(AffUsers, AffUsersChanges) of
                {ok, NewAffUsers, _, _, _} = ChangeResult ->
                    verify_externally_and_submit(
                      RoomUS, RoomRec, ChangeResult, ExternalCheck(RoomUS, NewAffUsers), Version);
                Error ->
                    Error
            end
    end.

-spec verify_externally_and_submit(RoomUS :: jid:simple_bare_jid(),
                                   RoomRec :: muc_light_room(),
                                   ChangeResult :: mod_muc_light_utils:change_aff_success(),
                                   CheckResult :: ok | {error, any()},
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
verify_externally_and_submit(
  RoomUS, #muc_light_room{ aff_users = OldAffUsers, version = PrevVersion } = RoomRec,
  {ok, NewAffUsers, AffUsersChanged, JoiningUsers, LeavingUsers}, ok, Version) ->
    ok = mnesia:write(RoomRec#muc_light_room{ aff_users = NewAffUsers, version = Version }),
    update_users_rooms(RoomUS, JoiningUsers, LeavingUsers),
    {ok, OldAffUsers, NewAffUsers, AffUsersChanged, PrevVersion};
verify_externally_and_submit(_, _, _, Error, _) ->
    Error.

-spec update_users_rooms(RoomUS :: jid:simple_bare_jid(),
                         JoiningUsers :: [jid:simple_bare_jid()],
                         LeavingUsers :: [jid:simple_bare_jid()]) -> ok.
update_users_rooms(RoomUS, [User | RJoiningUsers], LeavingUsers) ->
    ok = mnesia:write(#muc_light_user_room{ user = User, room = RoomUS }),
    update_users_rooms(RoomUS, RJoiningUsers, LeavingUsers);
update_users_rooms(RoomUS, [], [User | RLeavingUsers]) ->
    ok = mnesia:delete_object(#muc_light_user_room{ user = User, room = RoomUS }),
    update_users_rooms(RoomUS, [], RLeavingUsers);
update_users_rooms(_RoomUS, [], []) ->
    ok.

