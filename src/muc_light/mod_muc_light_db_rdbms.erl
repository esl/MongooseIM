%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_db_rdbms.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : RDBMS backend for mod_muc_light
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_db_rdbms).
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

%% Conversions
-export([
         what_db2atom/1, what_atom2db/1,
         aff_db2atom/1, aff_atom2db/1
        ]).

%% Extra API for testing
-export([
         force_clear/0
        ]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

%%====================================================================
%% API
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start(Host :: jid:server(), MUCHost :: jid:server()) -> ok.
start(Host, _MUCHost) ->
    prepare_queries(Host),
    ok.

-spec stop(Host :: jid:server(), MUCHost :: jid:server()) -> ok.
stop(_Host, _MUCHost) ->
    ok.

%% ------------------------ SQL -------------------------------------------

prepare_queries(Host) ->
    mongoose_rdbms:prepare(muc_light_config_delete_all, muc_light_config, [],
                           <<"DELETE FROM muc_light_config">>),
    mongoose_rdbms:prepare(muc_light_occupants_delete_all, muc_light_occupants, [],
                           <<"DELETE FROM muc_light_occupants">>),
    mongoose_rdbms:prepare(muc_light_rooms_delete_all, muc_light_rooms, [],
                           <<"DELETE FROM muc_light_rooms">>),
    mongoose_rdbms:prepare(muc_light_blocking_delete_all, muc_light_blocking, [],
                           <<"DELETE FROM muc_light_blocking">>),
    prepare_room_queries(Host),
    prepare_affiliation_queries(Host),
    ok.

prepare_room_queries(_Host) ->
    %% Returns maximum 1 record
    mongoose_rdbms:prepare(muc_light_select_room_id, muc_light_rooms,
                           [luser, lserver],
                           <<"SELECT id FROM muc_light_rooms "
                             "WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_select_room_id_and_version, muc_light_rooms,
                           [luser, lserver],
                           <<"SELECT id, version FROM muc_light_rooms "
                             "WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_insert_room, muc_light_rooms,
                           [luser, lserver, version],
                           <<"INSERT INTO muc_light_rooms (luser, lserver, version)"
                             " VALUES (?, ?, ?)">>),
    mongoose_rdbms:prepare(muc_light_update_room_version, muc_light_rooms,
                           [luser, lserver, version],
                           <<"UPDATE muc_light_rooms SET version = ? "
                             " WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_delete_room, muc_light_rooms,
                           [luser, lserver],
                           <<"DELETE FROM muc_light_rooms"
                             " WHERE luser = ? AND lserver = ?">>),
    %% This query uses multiple table
    mongoose_rdbms:prepare(muc_light_select_user_rooms, muc_light_occupants,
                           [luser, lserver],
                           <<"SELECT r.luser, r.lserver "
                             " FROM muc_light_occupants AS o "
                             " INNER JOIN muc_light_rooms AS r ON o.room_id = r.id"
                             " WHERE o.luser = ? AND o.lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_select_user_rooms_count, muc_light_occupants,
                           [luser, lserver],
                           <<"SELECT count(*) "
                             " FROM muc_light_occupants AS o "
                             " INNER JOIN muc_light_rooms AS r ON o.room_id = r.id"
                             " WHERE o.luser = ? AND o.lserver = ?">>),
    ok.

prepare_affiliation_queries(Host) ->
    %% This query uses multiple table
    %% Also returns a room version
    mongoose_rdbms:prepare(muc_light_select_affs_by_us, muc_light_rooms,
                           [luser, lserver],
                           <<"SELECT version, o.luser, o.lserver, aff"
                             " FROM muc_light_rooms AS r "
                             " LEFT OUTER JOIN muc_light_occupants AS o ON r.id = o.room_id"
                             " WHERE r.luser = ? AND r.lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_select_affs_by_room_id, muc_light_occupants,
                           [room_id],
                           <<"SELECT luser, lserver, aff "
                             "FROM muc_light_occupants WHERE room_id = ?">>),
    mongoose_rdbms:prepare(muc_light_insert_aff, muc_light_occupants,
                           [room_id, luser, lserver, aff],
                           <<"INSERT INTO muc_light_occupants (room_id, luser, lserver, aff)"
                              " VALUES(?, ?, ?, ?)">>),
   ok.

%% ------------------------ Room SQL functions ------------------------

select_room_id(MainHost, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_select_room_id, [RoomU, RoomS]).

select_room_id_and_version(MainHost, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_select_room_id_and_version, [RoomU, RoomS]).

select_user_rooms(MainHost, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_select_user_rooms, [LUser, LServer]).

select_user_rooms_count(MainHost, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_select_user_rooms_count, [LUser, LServer]).

insert_room(MainHost, RoomU, RoomS, Version) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_insert_room, [RoomU, RoomS, Version]).

update_room_version(MainHost, RoomU, RoomS, Version) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_update_room_version, [Version, RoomU, RoomS]).

delete_room(MainHost, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_delete_room, [RoomU, RoomS]).

%% ------------------------ Affiliation SQL functions ------------------------

%% Returns affiliations with a version
select_affs_by_us(MainHost, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_select_affs_by_us, [RoomU, RoomS]).

%% Returns affiliations without a version
select_affs_by_room_id(MainHost, RoomID) ->
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_select_affs_by_room_id, [RoomID]).

insert_aff(MainHost, RoomID, UserU, UserS, Aff) ->
    DbAff = mod_muc_light_db_rdbms:aff_atom2db(Aff),
    mongoose_rdbms:execute_successfully(
        MainHost, muc_light_insert_aff, [RoomID, UserU, UserS, DbAff]).

%% ------------------------ General room management ------------------------

-spec create_room(RoomUS :: jid:simple_bare_jid(), Config :: mod_muc_light_room_config:kv(),
                  AffUsers :: aff_users(), Version :: binary()) ->
    {ok, FinalRoomUS :: jid:simple_bare_jid()} | {error, exists}.
create_room({<<>>, RoomS} = RoomUS, Config, AffUsers, Version) ->
    MainHost = main_host(RoomUS),
    create_room_with_random_name(MainHost, RoomS, Config, AffUsers, Version, 10);
create_room(RoomUS, Config, AffUsers, Version) ->
    MainHost = main_host(RoomUS),
    create_room_with_specified_name(MainHost, RoomUS, Config, AffUsers, Version).

create_room_with_random_name(_MainHost, RoomS, _Config, _AffUsers, _Version, 0) ->
    ?LOG_ERROR(#{what => muc_create_room_with_random_name_failed,
                 sub_host => RoomS}),
    error(create_room_with_random_name_failed);
create_room_with_random_name(MainHost, RoomS, Config, AffUsers, Version, Retries)
    when Retries > 0 ->
    RoomU = mongoose_bin:gen_from_timestamp(),
    RoomUS = {RoomU, RoomS},
    F = fun() -> create_room_transaction(MainHost, RoomUS, Config, AffUsers, Version) end,
    case mongoose_rdbms:sql_transaction(MainHost, F) of
        {atomic, ok} ->
            {ok, RoomUS};
        Other ->
            ?LOG_ERROR(#{what => muc_create_room_with_random_name_retry,
                         candidate_room => RoomU, sub_host => RoomS, reason => Other}),
            create_room_with_random_name(MainHost, RoomS, Config, AffUsers, Version, Retries-1)
    end.

create_room_with_specified_name(MainHost, RoomUS, Config, AffUsers, Version) ->
    F = fun() -> create_room_transaction(MainHost, RoomUS, Config, AffUsers, Version) end,
    case mongoose_rdbms:sql_transaction(MainHost, F) of
        {atomic, ok} ->
            {ok, RoomUS};
        Other ->
            case room_exists(RoomUS) of
                true ->
                    {error, exists};
                false -> %% Some unknown error condition
                    {RoomU, RoomS} = RoomUS,
                    ?LOG_ERROR(#{what => muc_create_room_with_specified_name_failed,
                                 room => RoomU, sub_host => RoomS, reason => Other}),
                    error(create_room_with_specified_name_failed)
            end
    end.

-spec destroy_room(RoomUS :: jid:simple_bare_jid()) -> ok | {error, not_exists | not_empty}.
destroy_room(RoomUS) ->
    MainHost = main_host(RoomUS),
    F = fun() -> destroy_room_transaction(MainHost, RoomUS) end,
    {atomic, Res} = mongoose_rdbms:sql_transaction(MainHost, F),
    Res.

-spec room_exists(RoomUS :: jid:simple_bare_jid()) -> boolean().
room_exists({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),
    {selected, Res} = select_room_id(MainHost, RoomU, RoomS),
    Res /= [].

-spec get_user_rooms(UserUS :: jid:simple_bare_jid(),
                     MUCServer :: jid:lserver() | undefined) ->
    [RoomUS :: jid:simple_bare_jid()].
get_user_rooms({LUser, LServer}, undefined) ->
    lists:usort(lists:flatmap(
                  fun(Host) ->
                          {selected, Rooms} = select_user_rooms(Host, LUser, LServer),
                          Rooms
                  end, ?MYHOSTS));
get_user_rooms({LUser, LServer}, MUCServer) ->
    MainHost = main_host(MUCServer),
    {selected, Rooms} = select_user_rooms(MainHost, LUser, LServer),
    Rooms.

-spec get_user_rooms_count(UserUS :: jid:simple_bare_jid(),
                           MUCServer :: jid:lserver()) ->
    non_neg_integer().
get_user_rooms_count({LUser, LServer}, MUCServer) ->
    MainHost = main_host(MUCServer),
    {selected, [{Cnt}]} = select_user_rooms_count(MainHost, LUser, LServer),
    mongoose_rdbms:result_to_integer(Cnt).

-spec remove_user(UserUS :: jid:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return() | {error, term()}.
remove_user({_, UserS} = UserUS, Version) ->
    F = fun() -> remove_user_transaction(UserS, UserUS, Version) end,
    {atomic, Res} = mongoose_rdbms:sql_transaction(UserS, F),
    Res.

%% ------------------------ Configuration manipulation ------------------------

-spec get_config(RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), Version :: binary()} | {error, not_exists}.
get_config({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),

    SQL = mod_muc_light_db_rdbms_sql:select_config(RoomU, RoomS),
    {selected, Result} = mongoose_rdbms:sql_query(MainHost, SQL),

    case Result of
        [] ->
            {error, not_exists};
        [{Version, null, null}] ->
            {ok, [], Version};
        [{Version, _, _} | _] ->
            RawConfig = [{Key, Val} || {_, Key, Val} <- Result],
            {ok, Config} = mod_muc_light_room_config:apply_binary_kv(
                             RawConfig, [], mod_muc_light:config_schema(RoomS)),
            {ok, Config, Version}
    end.

-spec get_config(RoomUS :: jid:simple_bare_jid(), Key :: atom()) ->
    {ok, term(), Version :: binary()} | {error, not_exists | invalid_opt}.
get_config({RoomU, RoomS} = RoomUS, Key) ->
    MainHost = main_host(RoomUS),
    ConfigSchema = mod_muc_light:config_schema(RoomS),
    {ok, KeyDB} = mod_muc_light_room_config:schema_reverse_find(Key, ConfigSchema),

    SQL = mod_muc_light_db_rdbms_sql:select_config(RoomU, RoomS, KeyDB),
    {selected, Result} = mongoose_rdbms:sql_query(MainHost, SQL),

    case Result of
        [] ->
            {error, not_exists};
        [{_Version, null, null}] ->
            {error, invalid_opt};
        [{Version, _, ValDB}] ->
            RawConfig = [{KeyDB, ValDB}],
            {ok, [{_, Val}]} = mod_muc_light_room_config:apply_binary_kv(
                                 RawConfig, [], ConfigSchema),
            {ok, Val, Version}
    end.


-spec set_config(RoomUS :: jid:simple_bare_jid(), Config :: mod_muc_light_room_config:kv(),
                 Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomUS, ConfigChanges, Version) ->
    MainHost = main_host(RoomUS),
    {atomic, Res}
    = mongoose_rdbms:sql_transaction(
        MainHost, fun() -> set_config_transaction(RoomUS, ConfigChanges, Version) end),
    Res.

-spec set_config(RoomUS :: jid:simple_bare_jid(),
                 Key :: atom(), Val :: term(), Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(RoomJID, Key, Val, Version) ->
    set_config(RoomJID, [{Key, Val}], Version).

%% ------------------------ Blocking manipulation ------------------------

-spec get_blocking(UserUS :: jid:simple_bare_jid(), MUCServer :: jid:lserver()) ->
    [blocking_item()].
get_blocking({LUser, LServer}, MUCServer) ->
    MainHost = main_host(MUCServer),
    SQL = mod_muc_light_db_rdbms_sql:select_blocking(LUser, LServer),
    {selected, WhatWhos} = mongoose_rdbms:sql_query(MainHost, SQL),
    [ {what_db2atom(What), deny, jid:to_lus(jid:from_binary(Who))} || {What, Who} <- WhatWhos ].

-spec get_blocking(UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   WhatWhos :: [{blocking_who(), jid:simple_bare_jid()}]) ->
    blocking_action().
get_blocking({LUser, LServer}, MUCServer, WhatWhos) ->
    MainHost = main_host(MUCServer),
    SQL = mod_muc_light_db_rdbms_sql:select_blocking_cnt(LUser, LServer, WhatWhos),
    {selected, [{Count}]} = mongoose_rdbms:sql_query(MainHost, SQL),
    case mongoose_rdbms:result_to_integer(Count) of
        0 -> allow;
        _ -> deny
    end.

-spec set_blocking(UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   BlockingItems :: [blocking_item()]) -> ok.
set_blocking(_UserUS, _MUCServer, []) ->
    ok;
set_blocking({LUser, LServer} = UserUS, MUCServer, [{What, deny, Who} | RBlockingItems]) ->
    {updated, _} =
    mongoose_rdbms:sql_query(
      main_host(MUCServer), mod_muc_light_db_rdbms_sql:insert_blocking(LUser, LServer, What, Who)),
    set_blocking(UserUS, MUCServer, RBlockingItems);
set_blocking({LUser, LServer} = UserUS, MUCServer, [{What, allow, Who} | RBlockingItems]) ->
    {updated, _} =
    mongoose_rdbms:sql_query(
      main_host(MUCServer), mod_muc_light_db_rdbms_sql:delete_blocking(LUser, LServer, What, Who)),
    set_blocking(UserUS, MUCServer, RBlockingItems).

%% ------------------------ Affiliations manipulation ------------------------

-spec get_aff_users(RoomUS :: jid:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.
get_aff_users({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),
    case select_affs_by_us(MainHost, RoomU, RoomS) of
        {selected, []} ->
            {error, not_exists};
        {selected, [{Version, null, null, null}]} ->
            {ok, [], Version};
        {selected, [{Version, _, _, _} | _] = Res} ->
            AffUsers = decode_affs_with_versions(Res),
            {ok, AffUsers, Version}
    end.

-spec modify_aff_users(RoomUS :: jid:simple_bare_jid(),
                       AffUsersChanges :: aff_users(),
                       ExternalCheck :: external_check_fun(),
                       Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users(RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    MainHost = main_host(RoomUS),
    F = fun() -> modify_aff_users_transaction(MainHost, RoomUS, AffUsersChanges,
                                              ExternalCheck, Version) end,
    {atomic, Res} = mongoose_rdbms:sql_transaction(MainHost, F),
    Res.

%% ------------------------ Misc ------------------------

-spec get_info(RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), aff_users(), Version :: binary()}
    | {error, not_exists}.
get_info({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),
    case select_room_id_and_version(MainHost, RoomU, RoomS) of
        {selected, [{RoomID, Version}]} ->
            {selected, AffUsersDB} = select_affs_by_room_id(MainHost, RoomID),
            AffUsers = decode_affs(AffUsersDB),
            {selected, ConfigDB} = mongoose_rdbms:sql_query(
                                        MainHost, mod_muc_light_db_rdbms_sql:select_config(RoomID)),
            {ok, Config} = mod_muc_light_room_config:apply_binary_kv(
                             ConfigDB, [], mod_muc_light:config_schema(RoomS)),

            {ok, Config, AffUsers, Version};
        {selected, []} ->
            {error, not_exists}
    end.

%% ------------------------ Conversions ------------------------

decode_affs(AffUsersDB) ->
    US2Aff = [{{UserU, UserS}, aff_db2atom(Aff)}
              || {UserU, UserS, Aff} <- AffUsersDB],
    lists:sort(US2Aff).

decode_affs_with_versions(AffUsersDB) ->
    US2Aff = [{{UserU, UserS}, aff_db2atom(Aff)}
              || {_Version, UserU, UserS, Aff} <- AffUsersDB],
    lists:sort(US2Aff).

-spec what_db2atom(binary() | pos_integer()) -> blocking_what().
what_db2atom(1) -> room;
what_db2atom(2) -> user;
what_db2atom(Bin) -> what_db2atom(mongoose_rdbms:result_to_integer(Bin)).

-spec what_atom2db(blocking_what()) -> string().
what_atom2db(room) -> "1";
what_atom2db(user) -> "2".

-spec aff_atom2db(aff()) -> string().
aff_atom2db(owner) -> "1";
aff_atom2db(member) -> "2".

-spec aff_db2atom(binary() | pos_integer()) -> aff().
aff_db2atom(1) -> owner;
aff_db2atom(2) -> member;
aff_db2atom(Bin) -> aff_db2atom(mongoose_rdbms:result_to_integer(Bin)).

%%====================================================================
%% API for tests
%%====================================================================

force_clear_statements() ->
    [muc_light_config_delete_all,
     muc_light_occupants_delete_all,
     muc_light_rooms_delete_all,
     muc_light_blocking_delete_all].

-spec force_clear() -> ok.
force_clear() ->
    [mongoose_rdbms:execute_successfully(Host, Statement, [])
     || Host <- ?MYHOSTS, Statement <- force_clear_statements()],
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% ------------------------ General room management ------------------------

%% Expects config to have unique fields!
-spec create_room_transaction(MainHost :: jid:lserver(),
                              RoomUS :: jid:simple_bare_jid(),
                              Config :: mod_muc_light_room_config:kv(),
                              AffUsers :: aff_users(),
                              Version :: binary()) ->
    ok | {error, exists}.
create_room_transaction(MainHost, {RoomU, RoomS}, Config, AffUsers, Version) ->
    insert_room(MainHost, RoomU, RoomS, Version),
    {selected, [{RoomID}]} = select_room_id(MainHost, RoomU, RoomS),
    lists:foreach(
      fun({{UserU, UserS}, Aff}) ->
              {updated, _} = insert_aff(MainHost, RoomID, UserU, UserS, Aff)
      end, AffUsers),
    ConfigFields = mod_muc_light_room_config:to_binary_kv(
                     Config, mod_muc_light:config_schema(RoomS)),
    lists:foreach(
      fun({Key, Val}) ->
              Query = mod_muc_light_db_rdbms_sql:insert_config(RoomID, Key, Val),
              {updated, _} = mongoose_rdbms:sql_query_t(Query)
      end, ConfigFields),
      ok.

-spec destroy_room_transaction(MainHost :: jid:lserver(),
                               RoomUS :: jid:simple_bare_jid()) ->
    ok | {error, not_exists}.
destroy_room_transaction(MainHost, {RoomU, RoomS}) ->
    case select_room_id(MainHost, RoomU, RoomS) of
        {selected, [{RoomID}]} ->
            {updated, _} = mongoose_rdbms:sql_query_t(
                             mod_muc_light_db_rdbms_sql:delete_affs(RoomID)),
            {updated, _} = mongoose_rdbms:sql_query_t(
                             mod_muc_light_db_rdbms_sql:delete_config(RoomID)),
            {updated, _} = delete_room(MainHost, RoomU, RoomS),
            ok;
        {selected, []} ->
            {error, not_exists}
    end.

-spec remove_user_transaction(MainHost :: jid:lserver(),
                              UserUS :: jid:simple_bare_jid(),
                              Version :: binary()) ->
    mod_muc_light_db:remove_user_return().
remove_user_transaction(MainHost, {UserU, UserS} = UserUS, Version) ->
    Rooms = get_user_rooms(UserUS, undefined),
    {updated, _} = mongoose_rdbms:sql_query_t(
                     mod_muc_light_db_rdbms_sql:delete_blocking(UserU, UserS)),
    lists:map(
      fun(RoomUS) ->
              {RoomUS, modify_aff_users_transaction(MainHost,
                         RoomUS, [{UserUS, none}], fun(_, _) -> ok end, Version)}
      end, Rooms).

%% ------------------------ Configuration manipulation ------------------------

-spec set_config_transaction(RoomUS :: jid:simple_bare_jid(),
                             ConfigChanges :: mod_muc_light_room_config:kv(),
                             Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config_transaction({RoomU, RoomS} = RoomUS, ConfigChanges, Version) ->
    MainHost = main_host(RoomUS),
    case select_room_id_and_version(MainHost, RoomU, RoomS) of
        {selected, [{RoomID, PrevVersion}]} ->
            {updated, _} = update_room_version(MainHost, RoomU, RoomS, Version),
            lists:foreach(
              fun({Key, Val}) ->
                      {updated, _}
                      = mongoose_rdbms:sql_query(
                          MainHost, mod_muc_light_db_rdbms_sql:update_config(RoomID, Key, Val))
              end, mod_muc_light_room_config:to_binary_kv(
                     ConfigChanges, mod_muc_light:config_schema(RoomS))),
            {ok, PrevVersion};
        {selected, []} ->
            {error, not_exists}
    end.

%% ------------------------ Blocking manipulation ------------------------

%% ------------------------ Affiliations manipulation ------------------------

-spec modify_aff_users_transaction(MainHost :: jid:lserver(),
                                   RoomUS :: jid:simple_bare_jid(),
                                   AffUsersChanges :: aff_users(),
                                   CheckFun :: external_check_fun(),
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users_transaction(MainHost, {RoomU, RoomS} = RoomUS,
                             AffUsersChanges, CheckFun, Version) ->
    case select_room_id_and_version(MainHost, RoomU, RoomS) of
        {selected, [{RoomID, PrevVersion}]} ->
            modify_aff_users_transaction(MainHost,
              RoomUS, RoomID, AffUsersChanges, CheckFun, PrevVersion, Version);
        {selected, []} ->
            {error, not_exists}
    end.

-spec modify_aff_users_transaction(MainHost :: jid:lserver(),
                                   RoomUS :: jid:simple_bare_jid(),
                                   RoomID :: binary(),
                                   AffUsersChanges :: aff_users(),
                                   CheckFun :: external_check_fun(),
                                   PrevVersion :: binary(),
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users_transaction(MainHost, RoomUS, RoomID, AffUsersChanges,
                             CheckFun, PrevVersion, Version) ->
    {selected, AffUsersDB} = select_affs_by_room_id(MainHost, RoomID),
    AffUsers = decode_affs(AffUsersDB),
    case mod_muc_light_utils:change_aff_users(AffUsers, AffUsersChanges) of
        {ok, NewAffUsers, AffUsersChanged, JoiningUsers, _LeavingUsers} ->
            case CheckFun(RoomUS, NewAffUsers) of
                ok ->
                    apply_aff_users_transaction(MainHost, RoomID, AffUsersChanged, JoiningUsers),
                    {RoomU, RoomS} = RoomUS,
                    {updated, _} = update_room_version(MainHost, RoomU, RoomS, Version),
                    {ok, AffUsers, NewAffUsers, AffUsersChanged, PrevVersion};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec apply_aff_users_transaction(MainHost :: jid:lserver(),
                                  RoomID :: binary(),
                                  AffUsersChanges :: aff_users(),
                                  JoiningUsers :: [jid:simple_bare_jid()]) -> ok.
apply_aff_users_transaction(MainHost, RoomID, AffUsersChanged, JoiningUsers) ->
    lists:foreach(
      fun({{UserU, UserS}, none}) ->
              {updated, _} = mongoose_rdbms:sql_query_t(
                               mod_muc_light_db_rdbms_sql:delete_aff(RoomID, UserU, UserS));
         ({{UserU, UserS} = UserUS, Aff}) ->
              case lists:member(UserUS, JoiningUsers) of
                  true ->
                      {updated, _} = insert_aff(MainHost, RoomID, UserU, UserS, Aff);
                  false ->
                      {updated, _} = mongoose_rdbms:sql_query_t(
                                       mod_muc_light_db_rdbms_sql:update_aff(
                                         RoomID, UserU, UserS, Aff))
              end
      end, AffUsersChanged).

%% ------------------------ Common ------------------------

-spec main_host(JIDOrServer :: jid:simple_bare_jid() | binary()) -> jid:lserver().
main_host({_, RoomS}) ->
    main_host(RoomS);
main_host(MUCServer) ->
    {ok, MainHost} = mongoose_subhosts:get_host(MUCServer),
    MainHost.
