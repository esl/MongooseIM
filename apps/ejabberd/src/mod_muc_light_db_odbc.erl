%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_db_odbc.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : ODBC backend for mod_muc_light
%%% Created : 24 Nov 2016 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
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

-module(mod_muc_light_db_odbc).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([
         start/2,
         stop/2,

         create_room/4,
         destroy_room/1,
         room_exists/1,
         get_user_rooms/2,
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

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-define(esc(T), ejabberd_odbc:escape(T)).

%%====================================================================
%% API
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start(Host :: ejabberd:server(), MUCHost :: ejabberd:server()) -> ok.
start(_Host, _MUCHost) ->
    ok.

-spec stop(Host :: ejabberd:server(), MUCHost :: ejabberd:server()) -> ok.
stop(_Host, _MUCHost) ->
    ok.

%% ------------------------ General room management ------------------------

-spec create_room(RoomUS :: ejabberd:simple_bare_jid(), Config :: config(),
                  AffUsers :: aff_users(), Version :: binary()) ->
    {ok, FinalRoomUS :: ejabberd:simple_bare_jid()} | {error, exists}.
create_room(RoomUS, Config, AffUsers, Version) ->
    MainHost = main_host(RoomUS),
    {atomic, Res}
    = ejabberd_odbc:sql_transaction(
        MainHost, fun() -> create_room_transaction(RoomUS, Config, AffUsers, Version) end),
    Res.

-spec destroy_room(RoomUS :: ejabberd:simple_bare_jid()) -> ok | {error, not_exists | not_empty}.
destroy_room(RoomUS) ->
    MainHost = main_host(RoomUS),
    {atomic, Res}
    = ejabberd_odbc:sql_transaction(MainHost, fun() -> destroy_room_transaction(RoomUS) end),
    Res.

-spec room_exists(RoomUS :: ejabberd:simple_bare_jid()) -> boolean().
room_exists({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),
    {selected, _, [{Cnt}]} = ejabberd_odbc:sql_query(MainHost, room_exists_sql(RoomU, RoomS)),
    ejabberd_odbc:result_to_integer(Cnt) > 0.

-spec get_user_rooms(UserUS :: ejabberd:simple_bare_jid(),
                     MUCServer :: ejabberd:lserver() | undefined) ->
    [RoomUS :: ejabberd:simple_bare_jid()].
get_user_rooms({LUser, LServer}, undefined) ->
    SQL = get_user_rooms_sql(LUser, LServer),
    lists:usort(lists:flatmap(
                  fun(Host) ->
                          {selected, _, Rooms} = ejabberd_odbc:sql_query(Host, SQL),
                          Rooms
                  end, ?MYHOSTS));
get_user_rooms({LUser, LServer}, MUCServer) ->
    MainHost = main_host(MUCServer),
    {selected, _, Rooms} = ejabberd_odbc:sql_query(MainHost, get_user_rooms_sql(LUser, LServer)),
    Rooms.

-spec remove_user(UserUS :: ejabberd:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return() | {error, term()}.
remove_user({_, UserS} = UserUS, Version) ->
    {atomic, Res}
    = ejabberd_odbc:sql_transaction(UserS, fun() -> remove_user_transaction(UserUS, Version) end),
    Res.

%% ------------------------ Configuration manipulation ------------------------

-spec get_config(RoomUS :: ejabberd:simple_bare_jid()) ->
    {ok, config(), Version :: binary()} | {error, not_exists}.
get_config({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),

    {selected, _ColNames, Result} = ejabberd_odbc:sql_query(MainHost, get_config_sql(RoomU, RoomS)),

    case Result of
        [] ->
            {error, not_exists};
        [{Version, null, null}] ->
            {ok, [], Version};
        [{Version, _, _} | _] ->
            RawConfig = [{Key, Val} || {_, Key, Val} <- Result],
            {ok, Config} = mod_muc_light_utils:process_raw_config(
                             RawConfig, [], mod_muc_light:config_schema(RoomS)),
            {ok, Config, Version}
    end.

-spec get_config(RoomUS :: ejabberd:simple_bare_jid(), Key :: atom()) ->
    {ok, term(), Version :: binary()} | {error, not_exists | invalid_opt}.
get_config({RoomU, RoomS} = RoomUS, Key) ->
    MainHost = main_host(RoomUS),
    ConfigSchema = mod_muc_light:config_schema(RoomS),
    {KeyDB, _, _} = lists:keyfind(Key, 2, ConfigSchema),

    {selected, _ColNames, Result} = ejabberd_odbc:sql_query(
                                      MainHost, get_config_sql(RoomU, RoomS, KeyDB)),

    case Result of
        [] ->
            {error, not_exists};
        [{_Version, null, null}] ->
            {error, invalid_opt};
        [{Version, _, ValDB}] ->
            RawConfig = [{KeyDB, ValDB}],
            {ok, [{_, Val}]} = mod_muc_light_utils:process_raw_config(RawConfig, [], ConfigSchema),
            {ok, Val, Version}
    end.


-spec set_config(RoomUS :: ejabberd:simple_bare_jid(), Config :: config(), Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomUS, ConfigChanges, Version) ->
    MainHost = main_host(RoomUS),
    {atomic, Res}
    = ejabberd_odbc:sql_transaction(
        MainHost, fun() -> set_config_transaction(RoomUS, ConfigChanges, Version) end),
    Res.

-spec set_config(RoomUS :: ejabberd:simple_bare_jid(),
                 Key :: atom(), Val :: term(), Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomJID, Key, Val, Version) ->
    set_config(RoomJID, [{Key, Val}], Version).

%% ------------------------ Blocking manipulation ------------------------

-spec get_blocking(UserUS :: ejabberd:simple_bare_jid(), MUCServer :: ejabberd:lserver()) ->
    [blocking_item()].
get_blocking({LUser, LServer}, MUCServer) ->
    MainHost = main_host(MUCServer),
    {selected, _, WhatWhos} = ejabberd_odbc:sql_query(MainHost, get_blocking_sql(LUser, LServer)),
    [ {what_db2atom(What), deny, jid:to_lus(jid:from_binary(Who))} || {What, Who} <- WhatWhos ].

-spec get_blocking(UserUS :: ejabberd:simple_bare_jid(),
                   MUCServer :: ejabberd:lserver(),
                   WhatWhos :: [{blocking_who(), ejabberd:simple_bare_jid()}]) ->
    blocking_action().
get_blocking({LUser, LServer}, MUCServer, WhatWhos) ->
    MainHost = main_host(MUCServer),
    {selected, _, [{Count}]} = ejabberd_odbc:sql_query(
                                 MainHost, get_blocking_cnt_sql(LUser, LServer, WhatWhos)),
    case ejabberd_odbc:result_to_integer(Count) of
        0 -> allow;
        _ -> deny
    end.

-spec set_blocking(UserUS :: ejabberd:simple_bare_jid(),
                   MUCServer :: ejabberd:lserver(),
                   BlockingItems :: [blocking_item()]) -> ok.
set_blocking(_UserUS, _MUCServer, []) ->
    ok;
set_blocking({LUser, LServer} = UserUS, MUCServer, [{What, deny, Who} | RBlockingItems]) ->
    ejabberd_odbc:sql_query(main_host(MUCServer), set_blocking_sql(LUser, LServer, What, Who)),
    set_blocking(UserUS, MUCServer, RBlockingItems);
set_blocking({LUser, LServer} = UserUS, MUCServer, [{What, allow, Who} | RBlockingItems]) ->
    ejabberd_odbc:sql_query(main_host(MUCServer), unset_blocking_sql(LUser, LServer, What, Who)),
    set_blocking(UserUS, MUCServer, RBlockingItems).

%% ------------------------ Affiliations manipulation ------------------------

-spec get_aff_users(RoomUS :: ejabberd:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.
get_aff_users({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),
    case ejabberd_odbc:sql_query(MainHost, get_affs_sql(RoomU, RoomS)) of
        {selected, _, []} ->
            {error, not_exists};
        {selected, _, [{Version, null, null, null}]} ->
            {ok, [], Version};
        {selected, _, [{Version, _, _, _} | _] = Res} ->
            AffUsers = [{{UserU, UserS}, aff_db2atom(Aff)} || {_, UserU, UserS, Aff} <- Res],
            {ok, lists:sort(AffUsers), Version}
    end.

-spec get_affs_sql(RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver()) -> iolist().
get_affs_sql(RoomU, RoomS) ->
    ["SELECT version, o.luser, o.lserver, aff"
     " FROM muc_light_rooms AS r LEFT OUTER JOIN muc_light_occupants AS o ON r.id = o.room_id"
     " WHERE r.luser = '", ?esc(RoomU), "' AND r.lserver = '", ?esc(RoomS), "'"].

-spec modify_aff_users(RoomUS :: ejabberd:simple_bare_jid(),
                       AffUsersChanges :: aff_users(),
                       ExternalCheck :: external_check_fun(),
                       Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users(RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    MainHost = main_host(RoomUS),
    {atomic, Res}
    = ejabberd_odbc:sql_transaction(
        MainHost, fun() -> modify_aff_users_transaction(
                             RoomUS, AffUsersChanges, ExternalCheck, Version) end),
    Res.

%% ------------------------ Misc ------------------------

-spec get_info(RoomUS :: ejabberd:simple_bare_jid()) ->
    {ok, config(), aff_users(), Version :: binary()} | {error, not_exists}.
get_info({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),
    case ejabberd_odbc:sql_query(MainHost, get_room_id_and_version_sql(RoomU, RoomS)) of
        {selected, _, [{RoomID, Version}]} ->
            {selected, _, AffUsersDB} = ejabberd_odbc:sql_query(MainHost, get_affs_sql(RoomID)),
            AffUsers = [{{UserU, UserS}, aff_db2atom(Aff)} || {UserU, UserS, Aff} <- AffUsersDB],
            
            {selected, _, ConfigDB} = ejabberd_odbc:sql_query(MainHost, get_config_sql(RoomID)),
            {ok, Config} = mod_muc_light_utils:process_raw_config(
                             ConfigDB, [], mod_muc_light:config_schema(RoomS)),

            {ok, Config, lists:sort(AffUsers), Version};
        {selected, _, []} ->
            {error, not_exists}
    end.

-spec get_affs_sql(RoomID :: binary()) -> iolist().
get_affs_sql(RoomID) ->
    ["SELECT luser, lserver, aff FROM muc_light_occupants WHERE room_id = ", RoomID].

-spec get_config_sql(RoomID :: binary()) -> iolist().
get_config_sql(RoomID) ->
    ["SELECT opt, val FROM muc_light_config WHERE room_id = ", RoomID].

%%====================================================================
%% API for tests
%%====================================================================

-spec force_clear() -> ok.
force_clear() ->
    lists:foreach(
      fun(Host) ->
              ejabberd_odbc:sql_query(Host, ["DELETE FROM muc_light_config"]),
              ejabberd_odbc:sql_query(Host, ["DELETE FROM muc_light_occupants"]),
              ejabberd_odbc:sql_query(Host, ["DELETE FROM muc_light_rooms"]),
              ejabberd_odbc:sql_query(Host, ["DELETE FROM muc_light_blocking"])
      end, ?MYHOSTS).

%%====================================================================
%% Internal functions
%%====================================================================

%% ------------------------ General room management ------------------------

%% Expects config to have unique fields!
-spec create_room_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                              Config :: config(), AffUsers :: aff_users(),
                              Version :: binary()) ->
    {ok, FinalRoomUS :: ejabberd:simple_bare_jid()} | {error, exists}.
create_room_transaction({NodeCandidate, RoomS}, Config, AffUsers, Version) ->
    RoomU = case NodeCandidate of
                <<>> -> mod_muc_light_utils:bin_ts();
                _ -> NodeCandidate
            end,
    case catch ejabberd_odbc:sql_query_t(room_insert_sql(RoomU, RoomS, Version)) of
        {aborted, Reason} ->
            case ejabberd_odbc:sql_query_t(room_exists_sql(RoomU, RoomS)) of
                {selected, _, [{<<"0">>}]} ->
                    throw({aborted, Reason});
                {selected, _, [{<<"1">>}]} ->
                    case NodeCandidate of
                        <<>> -> create_room_transaction({<<>>, RoomS}, Config, AffUsers, Version);
                        _ -> {error, exists}
                    end
            end;
        {updated, _} ->
            {selected, _, [{RoomID}]} = ejabberd_odbc:sql_query_t(get_room_id_sql(RoomU, RoomS)),
            lists:foreach(
              fun({{UserU, UserS}, Aff}) ->
                      {updated, _} = ejabberd_odbc:sql_query_t(aff_insert_sql(RoomID, UserU, UserS, Aff))
              end, AffUsers),
            lists:foreach(
              fun({Key, Val}) ->
                      {updated, _} = ejabberd_odbc:sql_query_t(config_insert_sql(RoomID, Key, Val))
              end, mod_muc_light_utils:config_to_raw(Config, mod_muc_light:config_schema(RoomS))),
            {ok, {RoomU, RoomS}}
    end.

-spec room_insert_sql(
        RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver(), Version :: binary()) -> iolist().
room_insert_sql(RoomU, RoomS, Version) ->
    ["INSERT INTO muc_light_rooms (luser, lserver, version)"
     " VALUES ('", ?esc(RoomU), "', '", ?esc(RoomS), "', '", ?esc(Version), "')"].

-spec get_room_id_sql(RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver()) -> iolist().
get_room_id_sql(RoomU, RoomS) ->
    ["SELECT id FROM muc_light_rooms WHERE luser = '", ?esc(RoomU), "'"
                                     " AND lserver = '", ?esc(RoomS), "'"].

-spec aff_insert_sql(RoomID :: binary(), UserU :: ejabberd:luser(),
                     UserS :: ejabberd:lserver(), Aff :: aff()) -> iolist().
aff_insert_sql(RoomID, UserU, UserS, Aff) ->
    ["INSERT INTO muc_light_occupants (room_id, luser, lserver, aff)"
     " VALUES(", RoomID, ", '", ?esc(UserU), "', '", ?esc(UserS), "', ", aff_atom2db(Aff), ")"].

-spec config_insert_sql(RoomID :: binary(), Key :: binary(), Val :: binary()) -> iolist().
config_insert_sql(RoomID, Key, Val) ->
    ["INSERT INTO muc_light_config (room_id, opt, val)"
     " VALUES(", RoomID, ", '", ?esc(Key), "', '", ?esc(Val), "')"].

-spec destroy_room_transaction(RoomUS :: ejabberd:simple_bare_jid()) -> ok | {error, not_exists}.
destroy_room_transaction({RoomU, RoomS}) ->
    case ejabberd_odbc:sql_query_t(get_room_id_sql(RoomU, RoomS)) of
        {selected, _, [{RoomID}]} ->
            {updated, _} = ejabberd_odbc:sql_query_t(affs_delete_sql(RoomID)),
            {updated, _} = ejabberd_odbc:sql_query_t(config_delete_sql(RoomID)),
            {updated, _} = ejabberd_odbc:sql_query_t(room_delete_sql(RoomU, RoomS)),
            ok;
        {selected, _, []} ->
            {error, not_exists}
    end.

-spec affs_delete_sql(RoomID :: binary()) -> iolist().
affs_delete_sql(RoomID) ->
    ["DELETE FROM muc_light_occupants WHERE room_id = ", RoomID].

-spec config_delete_sql(RoomID :: binary()) -> iolist().
config_delete_sql(RoomID) ->
    ["DELETE FROM muc_light_config WHERE room_id = ", RoomID].

-spec room_delete_sql(RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver()) -> iolist().
room_delete_sql(RoomU, RoomS) ->
    ["DELETE FROM muc_light_rooms"
     " WHERE luser = '", ?esc(RoomU), "' AND lserver = '", ?esc(RoomS), "'"].

-spec room_exists_sql(RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver()) -> iolist().
room_exists_sql(RoomU, RoomS) ->
    ["SELECT COUNT(*) FROM muc_light_rooms WHERE luser = '", ?esc(RoomU), "'"
                                           " AND lserver = '", ?esc(RoomS), "'"].

-spec get_user_rooms_sql(LUser :: ejabberd:luser(), LServer :: ejabberd:lserver()) -> iolist().
get_user_rooms_sql(LUser, LServer) ->
    ["SELECT r.luser, r.lserver"
     " FROM muc_light_occupants AS o INNER JOIN muc_light_rooms AS r ON o.room_id = r.id"
     " WHERE o.luser = '", ?esc(LUser), "' AND o.lserver = '", ?esc(LServer), "'"].

-spec remove_user_transaction(UserUS :: ejabberd:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return().
remove_user_transaction({UserU, UserS} = UserUS, Version) ->
    Rooms = get_user_rooms(UserUS, undefined),
    {updated, _} = ejabberd_odbc:sql_query_t(delete_blocking_sql(UserU, UserS)),
    lists:map(
      fun(RoomUS) ->
              {RoomUS, modify_aff_users_transaction(
                         RoomUS, [{UserUS, none}], fun(_,_) -> ok end, Version)}
      end, Rooms).

-spec delete_blocking_sql(UserU :: ejabberd:luser(), UserS :: ejabberd:lserver()) -> iolist().
delete_blocking_sql(UserU, UserS) ->
    ["DELETE FROM muc_light_blocking"
     " WHERE luser = '", ?esc(UserU), "' AND lserver = '", ?esc(UserS), "'"].

%% ------------------------ Configuration manipulation ------------------------

-spec get_config_sql(RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver()) -> iolist().
get_config_sql(RoomU, RoomS) ->
    ["SELECT version, opt, val",
     " FROM muc_light_rooms AS r LEFT OUTER JOIN muc_light_config AS c ON r.id = c.room_id"
     " WHERE r.luser = '", ?esc(RoomU), "' AND r.lserver = '", ?esc(RoomS), "'"].

-spec get_config_sql(RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver(), Key :: binary()) ->
    iolist().
get_config_sql(RoomU, RoomS, Key) ->
    [ get_config_sql(RoomU, RoomS), " AND key = '", Key, "'" ].

-spec set_config_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                             ConfigChanges :: config(),
                             Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config_transaction({RoomU, RoomS} = RoomUS, ConfigChanges, Version) ->
    MainHost = main_host(RoomUS),
    case ejabberd_odbc:sql_query_t(get_room_id_and_version_sql(RoomU, RoomS)) of
        {selected, _, [{RoomID, PrevVersion}]} ->
            {updated, _} = ejabberd_odbc:sql_query_t(set_version_sql(RoomU, RoomS, Version)),
            lists:foreach(
              fun({Key, Val}) ->
                      {updated, _}
                      = ejabberd_odbc:sql_query(MainHost, set_config_sql(RoomID, Key, Val))
              end, mod_muc_light_utils:config_to_raw(
                     ConfigChanges, mod_muc_light:config_schema(RoomS))),
            {ok, PrevVersion};
        {selected, _, []} ->
            {error, not_exists}
    end.

-spec set_config_sql(RoomID :: binary(), Key :: binary(), Val :: binary()) -> iolist().
set_config_sql(RoomID, Key, Val) ->
    ["UPDATE muc_light_config SET val = '", ?esc(Val), "'"
     " WHERE room_id = ", RoomID, " AND opt = '", ?esc(Key), "'"].

%% ------------------------ Blocking manipulation ------------------------

-spec get_blocking_sql(LUser :: ejabberd:luser(), LServer :: ejabberd:lserver()) -> iolist().
get_blocking_sql(LUser, LServer) ->
    ["SELECT what, who FROM muc_light_blocking WHERE luser = '", ?esc(LUser), "'",
                                               " AND lserver = '", ?esc(LServer), "'"].

-spec get_blocking_cnt_sql(LUser :: ejabberd:luser(), LServer :: ejabberd:lserver(),
                           WhatWhos :: [{blocking_who(), ejabberd:simple_bare_jid()}]) -> iolist().
get_blocking_cnt_sql(LUser, LServer, WhatWhos) ->
    [ _ | WhatWhosWhere ] = lists:flatmap(
                              fun({What, Who}) ->
                                      [ " OR ", "(what = ", what_atom2db(What),
                                            " AND who = '", ?esc(jid:to_binary(Who)), "')" ] end,
                              WhatWhos),
    ["SELECT COUNT(*) FROM muc_light_blocking WHERE luser = '", ?esc(LUser), "'"
                                              " AND lserver = '", ?esc(LServer), "'",
                                              " AND (", WhatWhosWhere, ")"].

-spec set_blocking_sql(LUser :: ejabberd:luser(), LServer :: ejabberd:lserver(),
                       What :: blocking_what(), Who :: blocking_who()) -> iolist().
set_blocking_sql(LUser, LServer, What, Who) ->
    ["INSERT INTO muc_light_blocking (luser, lserver, what, who)"
     " VALUES ('", ?esc(LUser), "', '", ?esc(LServer), "', ",
               what_atom2db(What), ", '", ?esc(jid:to_binary(Who)), "')"].

-spec unset_blocking_sql(LUser :: ejabberd:luser(), LServer :: ejabberd:lserver(),
                         What :: blocking_what(), Who :: blocking_who()) -> iolist().
unset_blocking_sql(LUser, LServer, What, Who) ->
    ["DELETE FROM muc_light_blocking WHERE luser = '", ?esc(LUser), "'"
                                     " AND lserver = '", ?esc(LServer), "'"
                                     " AND what = ", what_atom2db(What),
                                     " AND who = '", ?esc(jid:to_binary(Who)), "'"].

-spec what_db2atom(binary()) -> blocking_what().
what_db2atom(<<"1">>) -> room;
what_db2atom(<<"2">>) -> user.

-spec what_atom2db(blocking_what()) -> string().
what_atom2db(room) -> "1";
what_atom2db(user) -> "2".

%% ------------------------ Affiliations manipulation ------------------------

-spec modify_aff_users_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                                   AffUsersChanges :: aff_users(),
                                   CheckFun :: external_check_fun(),
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users_transaction({RoomU, RoomS} = RoomUS, AffUsersChanges, CheckFun, Version) ->
    case ejabberd_odbc:sql_query_t(get_room_id_and_version_sql(RoomU, RoomS)) of
        {selected, _, [{RoomID, PrevVersion}]} ->
            {selected, _, AffUsersDB} = ejabberd_odbc:sql_query_t(get_affs_sql(RoomID)),
            AffUsers = lists:sort(
                         [{{UserU, UserS}, aff_db2atom(Aff)} || {UserU, UserS, Aff} <- AffUsersDB]),
            case mod_muc_light_utils:change_aff_users(AffUsers, AffUsersChanges) of
                {ok, NewAffUsers, AffUsersChanged, JoiningUsers, _LeavingUsers} ->
                    case CheckFun(RoomUS, NewAffUsers) of
                        ok ->
                            lists:foreach(
                              fun({{UserU, UserS}, none}) ->
                                      {updated, _} = ejabberd_odbc:sql_query_t(
                                                       aff_delete_sql(RoomID, UserU, UserS));
                                 ({{UserU, UserS} = UserUS, Aff}) ->
                                      case lists:member(UserUS, JoiningUsers) of
                                          true ->
                                              {updated, _} = ejabberd_odbc:sql_query_t(
                                                               aff_insert_sql(RoomID, UserU, UserS, Aff));
                                          false ->
                                              {updated, _} = ejabberd_odbc:sql_query_t(
                                                               aff_update_sql(RoomID, UserU, UserS, Aff))
                                      end
                              end, AffUsersChanged),
                            {updated, _} = ejabberd_odbc:sql_query_t(
                                             set_version_sql(RoomU, RoomS, Version)),
                            {ok, AffUsers, NewAffUsers, AffUsersChanged, PrevVersion};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        {selected, _, []} ->
            {error, not_exists}
    end.

-spec aff_delete_sql(RoomID :: binary(), UserU :: ejabberd:luser(), UserS :: ejabberd:lserver()) ->
    iolist().
aff_delete_sql(RoomID, UserU, UserS) ->
    ["DELETE FROM muc_light_occupants WHERE room_id = ", RoomID,
                                      " AND luser = '", ?esc(UserU), "'"
                                      " AND lserver = '", ?esc(UserS), "'"].

-spec aff_update_sql(RoomID :: binary(), UserU :: ejabberd:luser(), UserS :: ejabberd:lserver(),
                     Aff :: aff()) -> iolist().
aff_update_sql(RoomID, UserU, UserS, Aff) ->
    ["UPDATE muc_light_occupants SET aff = ", aff_atom2db(Aff),
     " WHERE room_id = ", RoomID, " AND luser = '", ?esc(UserU), "'"
       " AND lserver = '", ?esc(UserS), "'"].

%% ------------------------ Common ------------------------

-spec main_host(JIDOrServer :: ejabberd:simple_bare_jid() | binary()) -> ejabberd:lserver().
main_host({_, RoomS}) ->
    main_host(RoomS);
main_host(MUCServer) ->
    mod_muc_light:get_opt(MUCServer, main_host, <<>>).

-spec set_version_sql(
        RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver(), Version :: binary()) -> iolist().
set_version_sql(RoomU, RoomS, Version) ->
    ["UPDATE muc_light_rooms SET version = '", ?esc(Version), "'"
     " WHERE luser = '", ?esc(RoomU), "' AND lserver = '", ?esc(RoomS), "'"].

-spec get_room_id_and_version_sql(
        RoomU :: ejabberd:luser(), RoomS :: ejabberd:lserver()) -> iolist().
get_room_id_and_version_sql(RoomU, RoomS) ->
    ["SELECT id, version FROM muc_light_rooms WHERE luser = '", ?esc(RoomU), "'"
                                              " AND lserver = '", ?esc(RoomS), "'"].

-spec aff_atom2db(aff()) -> string().
aff_atom2db(owner) -> "1";
aff_atom2db(member) -> "2".

-spec aff_db2atom(binary()) -> aff().
aff_db2atom(<<"1">>) -> owner;
aff_db2atom(<<"2">>) -> member.

