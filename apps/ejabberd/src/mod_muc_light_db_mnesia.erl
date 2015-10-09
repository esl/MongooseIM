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
         get_user_rooms/1,
         remove_user/2,

         get_config/1,
         get_config/2,
         set_config/3,
         set_config/4,

         get_blocking/1,
         get_blocking/2,
         set_blocking/2,

         get_aff_users/1,
         modify_aff_users/4,

         get_info/1
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
-define(BLOCKING_TAB, muc_light_blocking).

-record(?ROOM_TAB, {
          room :: {ejabberd:username(), ejabberd:server()},
          config :: [{atom(), term()}],
          aff_users :: aff_users(),
          version :: binary()
         }).

-record(?USER_ROOM_TAB, {
           user :: ejabberd:simple_bare_jid(),
           room :: ejabberd:simple_bare_jid()
          }).

-record(?BLOCKING_TAB, {
           user :: ejabberd:simple_bare_jid(),
           item :: {user | room, ejabberd:simple_bare_jid()}
          }).

%%====================================================================
%% API
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start(Host :: ejabberd:server(), MUCHost :: ejabberd:server()) -> ok.
start(_Host, _MUCHost) ->
    init_tables().

-spec stop(Host :: ejabberd:server(), MUCHost :: ejabberd:server()) -> ok.
stop(_Host, _MUCHost) ->
    ok.

%% ------------------------ General room management ------------------------

-spec create_room(RoomUS :: ejabberd:simple_bare_jid(), Config :: config(),
                  AffUsers :: aff_users(), Version :: binary()) ->
    {ok, FinalRoomUS :: ejabberd:simple_bare_jid()} | {error, exists}.
create_room(RoomUS, Config, AffUsers, Version) ->
    {atomic, Res} = mnesia:transaction(fun create_room_transaction/4,
                                       [RoomUS, Config, AffUsers, Version]),
    Res.

-spec destroy_room(RoomUS :: ejabberd:simple_bare_jid()) -> ok | {error, not_exists | not_empty}.
destroy_room(RoomUS) ->
    {atomic, Res} = mnesia:transaction(fun destroy_room_transaction/1, [RoomUS]),
    Res.

-spec room_exists(RoomUS :: ejabberd:simple_bare_jid()) -> boolean().
room_exists(RoomUS) ->
    mnesia:dirty_read(?ROOM_TAB, RoomUS) =/= [].

-spec get_user_rooms(UserUS :: ejabberd:simple_bare_jid()) ->
    {ok, [RoomUS :: ejabberd:simple_bare_jid()]} | {error, term()}.
get_user_rooms(UserUS) ->
    UsersRooms = mnesia:dirty_read(?USER_ROOM_TAB, UserUS),
    {ok, [ UserRoom#?USER_ROOM_TAB.room || UserRoom <- UsersRooms ]}.

-spec remove_user(UserUS :: ejabberd:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return() | {error, term()}.
remove_user(UserUS, Version) ->
    {atomic, Res} = mnesia:transaction(fun remove_user_transaction/2, [UserUS, Version]),
    Res.

%% ------------------------ Configuration manipulation ------------------------

-spec get_config(RoomUS :: ejabberd:simple_bare_jid()) ->
    {ok, config(), Version :: binary()} | {error, not_exists}.
get_config(RoomUS) ->
    case mnesia:dirty_read(?ROOM_TAB, RoomUS) of
        [] -> {error, not_exists};
        [#?ROOM_TAB{ config = Config, version = Version }] -> {ok, Config, Version}
    end.

-spec get_config(RoomUS :: ejabberd:simple_bare_jid(), Key :: atom()) ->
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

-spec set_config(RoomUS :: ejabberd:simple_bare_jid(), Config :: config(), Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomUS, ConfigChanges, Version) ->
    {atomic, Res} = mnesia:transaction(fun set_config_transaction/3,
                                       [RoomUS, ConfigChanges, Version]),
    Res.

-spec set_config(RoomUS :: ejabberd:simple_bare_jid(),
                 Key :: atom(), Val :: term(), Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomJID, Key, Val, Version) ->
    set_config(RoomJID, [{Key, Val}], Version).

%% ------------------------ Blocking manipulation ------------------------

-spec get_blocking(UserUS :: ejabberd:simple_bare_jid()) -> {ok, [blocking_item()]}.
get_blocking(UserUS) ->
    [ {What, deny, Who}
      || #?BLOCKING_TAB{ item = {What, Who} } <- dirty_get_blocking_raw(UserUS) ].

-spec get_blocking(UserUS :: ejabberd:simple_bare_jid(),
                   WhatWhos :: [{blocking_who(), ejabberd:simple_bare_jid()}]) ->
    blocking_action().
get_blocking(UserUS, WhatWhos) ->
    Blocklist = dirty_get_blocking_raw(UserUS),
    case lists:any(
           fun(WhatWho) ->
                   lists:keyfind(WhatWho, #?BLOCKING_TAB.item, Blocklist) =/= false
           end, WhatWhos) of
        true -> deny;
        false -> allow
    end.

-spec set_blocking(UserUS :: ejabberd:simple_bare_jid(), BlockingItems :: [blocking_item()]) -> ok.
set_blocking(_UserUS, []) ->
    ok;
set_blocking(UserUS, [{What, allow, Who} | RBlockingItems]) ->
    mnesia:dirty_write(#?BLOCKING_TAB{ user = UserUS, item = {What, Who} }),
    set_blocking(UserUS, RBlockingItems);
set_blocking(UserUS, [{What, deny, Who} | RBlockingItems]) ->
    mnesia:dirty_delete_object(#?BLOCKING_TAB{ user = UserUS, item = {What, Who} }),
    set_blocking(UserUS, RBlockingItems).

%% ------------------------ Affiliations manipulation ------------------------

-spec get_aff_users(RoomUS :: ejabberd:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.
get_aff_users(RoomUS) ->
    case mnesia:dirty_read(?ROOM_TAB, RoomUS) of
        [] -> {error, not_exists};
        [#?ROOM_TAB{ aff_users = AffUsers, version = Version }] -> {ok, AffUsers, Version}
    end.

-spec modify_aff_users(RoomUS :: ejabberd:simple_bare_jid(),
                       AffUsersChanges :: aff_users(),
                       ExternalCheck :: external_check_fun(),
                       Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users(RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    {atomic, Res} = mnesia:transaction(fun modify_aff_users_transaction/4,
                                       [RoomUS, AffUsersChanges, ExternalCheck, Version]),
    Res.

%% ------------------------ Misc ------------------------

-spec get_info(RoomUS :: ejabberd:simple_bare_jid()) ->
    {ok, config(), aff_users(), Version :: binary()} | {error, not_exists}.
get_info(RoomUS) ->
    case mnesia:dirty_read(?ROOM_TAB, RoomUS) of
        [] ->
            {error, not_exists};
        [#?ROOM_TAB{ config = Config, aff_users = AffUsers, version = Version }] ->
            {ok, Config, AffUsers, Version}
    end.

%%====================================================================
%% API for tests
%%====================================================================

-spec force_destroy_room(RoomUS :: ejabberd:simple_bare_jid()) -> ok.
force_destroy_room(RoomUS) ->
    mnesia:dirty_delete(?ROOM_TAB, RoomUS).

%%====================================================================
%% Internal functions
%%====================================================================

%% ------------------------ Schema creation ------------------------

-spec init_tables() -> ok.
init_tables() ->
    create_table(?ROOM_TAB,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, ?ROOM_TAB)}]),
    create_table(?USER_ROOM_TAB,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, ?USER_ROOM_TAB)},
                  {type, bag}]),
    create_table(?BLOCKING_TAB,
                 [{disc_copies, [node()]},
                  {attributes, record_info(fields, ?BLOCKING_TAB)},
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

-spec create_room_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                              Config :: config(), AffUsers :: aff_users(),
                              Version :: binary()) ->
    {ok, FinalRoomUS :: ejabberd:simple_bare_jid()} | {error, exists}.
create_room_transaction({<<>>, Domain}, Config, AffUsers, Version) ->
    NodeCandidate = mod_muc_light_utils:bin_ts(),
    NewNode = case mnesia:wread({?ROOM_TAB, {NodeCandidate, Domain}}) of
                  [_] -> <<>>;
                  [] -> NodeCandidate
              end,
    create_room_transaction({NewNode, Domain}, Config, AffUsers, Version);
create_room_transaction(RoomUS, Config, AffUsers, Version) ->
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [_] ->
            {error, exists};
        [] ->
            RoomRecord = #?ROOM_TAB{
                             room = RoomUS,
                             config = Config,
                             aff_users = AffUsers,
                             version = Version
                            },
            ok = mnesia:write(RoomRecord),
            lists:foreach(
              fun({User, _}) ->
                      UserRoomRecord = #?USER_ROOM_TAB{
                                           user = User,
                                           room = RoomUS
                                          },
                      ok = mnesia:write(UserRoomRecord)
              end, AffUsers)
    end.

-spec destroy_room_transaction(RoomUS :: ejabberd:simple_bare_jid()) ->
    ok | {error, not_exists | not_empty}.
destroy_room_transaction(RoomUS) ->
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [] -> {error, not_exists};
        [#?ROOM_TAB{ aff_users = [] }] -> mnesia:delete({?ROOM_TAB, RoomUS});
        [_] -> {error, not_empty}
    end.

-spec remove_user_transaction(UserUS :: ejabberd:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return().
remove_user_transaction(UserUS, Version) ->
    lists:map(
      fun(#?USER_ROOM_TAB{ room = RoomUS }) ->
              {RoomUS, modify_aff_users_transaction(
                         RoomUS, [{UserUS, none}], fun(_,_) -> ok end, Version)}
      end, mnesia:read(?USER_ROOM_TAB, UserUS)).

%% ------------------------ Configuration manipulation ------------------------

-spec set_config_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                             ConfigChanges :: config(),
                             Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config_transaction(RoomUS, ConfigChanges, Version) ->
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [] ->
            {error, not_exists};
        [#?ROOM_TAB{ config = Config } = Rec] ->
            NewConfig = lists:umerge(ConfigChanges, Config),
            mnesia:write(Rec#?ROOM_TAB{ config = NewConfig, version = Version }),
            {ok, Rec#?ROOM_TAB.version}
    end.

%% ------------------------ Blocking manipulation ------------------------

-spec dirty_get_blocking_raw(UserUS :: ejabberd:simple_bare_jid()) -> [#?BLOCKING_TAB{}].
dirty_get_blocking_raw(UserUS) ->
    mnesia:dirty_read(?BLOCKING_TAB, UserUS).

%% ------------------------ Affiliations manipulation ------------------------

-spec modify_aff_users_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                                   AffUsersChanges :: aff_users(),
                                   ExternalCheck :: external_check_fun(),
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users_transaction(RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    case mnesia:wread({?ROOM_TAB, RoomUS}) of
        [] ->
            {error, not_exists};
        [#?ROOM_TAB{ aff_users = AffUsers } = RoomRec] ->
            case mod_muc_light_utils:change_aff_users(AffUsers, AffUsersChanges) of
                {ok, NewAffUsers, _, _, _} = ChangeResult ->
                    verify_externally_and_submit(
                      RoomUS, RoomRec, ChangeResult, ExternalCheck(RoomUS, NewAffUsers), Version);
                Error ->
                    Error
            end
    end.

-spec verify_externally_and_submit(RoomUS :: ejabberd:simple_bare_jid(),
                                   RoomRec :: #?ROOM_TAB{},
                                   ChangeResult :: mod_muc_light_utils:change_aff_success(),
                                   CheckResult :: ok | {error, any()},
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
verify_externally_and_submit(
  RoomUS, #?ROOM_TAB{ aff_users = OldAffUsers, version = PrevVersion } = RoomRec,
  {ok, NewAffUsers, AffUsersChanged, JoiningUsers, LeavingUsers}, ok, Version) ->
    ok = mnesia:write(RoomRec#?ROOM_TAB{ aff_users = NewAffUsers, version = Version }),
    update_users_rooms(RoomUS, JoiningUsers, LeavingUsers),
    {ok, OldAffUsers, NewAffUsers, AffUsersChanged, PrevVersion};
verify_externally_and_submit(_, _, _, Error, _) ->
    Error.

-spec update_users_rooms(RoomUS :: ejabberd:simple_bare_jid(),
                         JoiningUsers :: [ejabberd:simple_bare_jid()],
                         LeavingUsers :: [ejabberd:simple_bare_jid()]) -> ok.
update_users_rooms(RoomUS, [User | RJoiningUsers], LeavingUsers) ->
    ok = mnesia:write(#?USER_ROOM_TAB{ user = User, room = RoomUS }),
    update_users_rooms(RoomUS, RJoiningUsers, LeavingUsers);
update_users_rooms(RoomUS, [], [User | RLeavingUsers]) ->
    ok = mnesia:delete_object(#?USER_ROOM_TAB{ user = User, room = RoomUS }),
    update_users_rooms(RoomUS, [], RLeavingUsers);
update_users_rooms(_RoomUS, [], []) ->
    ok.

