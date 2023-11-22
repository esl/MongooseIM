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

-behaviour(mod_muc_light_db_backend).

%% API
-export([
         start/2,
         stop/1,
         create_room/5,
         destroy_room/2,
         room_exists/2,
         get_user_rooms/3,
         get_user_rooms_count/2,
         remove_user/3,
         remove_domain/3,
         get_config/2,
         set_config/4,
         get_blocking/3,
         get_blocking/4,
         set_blocking/4,
         get_aff_users/2,
         modify_aff_users/5,
         get_info/2
        ]).

%% Extra API for testing
-export([force_clear/0,
         select_room_id/3,
         select_affs_by_room_id/2,
         select_config_by_room_id/2
        ]).

-ignore_xref([force_clear/0,
              select_room_id/3,
              select_affs_by_room_id/2,
              select_config_by_room_id/2]).

-type room_id() :: non_neg_integer().

-include("mongoose.hrl").
-include("mod_muc_light.hrl").

%%====================================================================
%% API
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start(HostType :: mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _) ->
    prepare_queries(),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

%% ------------------------ SQL -------------------------------------------

prepare_queries() ->
    prepare_cleaning_queries(),
    prepare_room_queries(),
    prepare_affiliation_queries(),
    prepare_config_queries(),
    prepare_blocking_queries(),
    prepare_domain_removal_queries(),
    ok.

prepare_cleaning_queries() ->
    mongoose_rdbms:prepare(muc_light_config_delete_all, muc_light_config, [],
                           <<"DELETE FROM muc_light_config">>),
    mongoose_rdbms:prepare(muc_light_occupants_delete_all, muc_light_occupants, [],
                           <<"DELETE FROM muc_light_occupants">>),
    mongoose_rdbms:prepare(muc_light_rooms_delete_all, muc_light_rooms, [],
                           <<"DELETE FROM muc_light_rooms">>),
    mongoose_rdbms:prepare(muc_light_blocking_delete_all, muc_light_blocking, [],
                           <<"DELETE FROM muc_light_blocking">>),
    ok.

prepare_room_queries() ->
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
    %% These queries use multiple tables
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

prepare_affiliation_queries() ->
    %% This query uses multiple tables
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
                           <<"INSERT INTO muc_light_occupants"
                              " (room_id, luser, lserver, aff)"
                              " VALUES(?, ?, ?, ?)">>),
    mongoose_rdbms:prepare(muc_light_update_aff, muc_light_occupants,
                           [aff, room_id, luser, lserver],
                           <<"UPDATE muc_light_occupants SET aff = ? "
                             "WHERE room_id = ? AND luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_delete_affs, muc_light_occupants,
                           [room_id],
                           <<"DELETE FROM muc_light_occupants WHERE room_id = ?">>),
    mongoose_rdbms:prepare(muc_light_delete_aff, muc_light_occupants,
                           [room_id, luser, lserver],
                           <<"DELETE FROM muc_light_occupants "
                             "WHERE room_id = ? AND luser = ? AND lserver = ?">>),
   ok.

prepare_config_queries() ->
    mongoose_rdbms:prepare(muc_light_select_config_by_room_id, muc_light_config,
                           [room_id],
                           <<"SELECT opt, val FROM muc_light_config WHERE room_id = ?">>),
    %% This query uses multiple tables
    mongoose_rdbms:prepare(muc_light_select_config_by_us, muc_light_rooms,
                           [luser, lserver],
                           <<"SELECT version, opt, val "
                             "FROM muc_light_rooms AS r "
                             "LEFT OUTER JOIN muc_light_config AS c ON r.id = c.room_id "
                             "WHERE r.luser = ? AND r.lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_insert_config, muc_light_config,
                           [room_id, opt, val],
                           <<"INSERT INTO muc_light_config (room_id, opt, val)"
                             " VALUES(?, ?, ?)">>),
    mongoose_rdbms:prepare(muc_light_update_config, muc_light_config,
                           [val, room_id, opt],
                           <<"UPDATE muc_light_config SET val = ? "
                             "WHERE room_id = ? AND opt = ?">>),
    mongoose_rdbms:prepare(muc_light_delete_config, muc_light_config,
                           [room_id],
                           <<"DELETE FROM muc_light_config WHERE room_id = ?">>),
   ok.

prepare_blocking_queries() ->
    mongoose_rdbms:prepare(muc_light_select_blocking, muc_light_blocking,
                           [luser, lserver],
                           <<"SELECT what, who FROM muc_light_blocking "
                             "WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(muc_light_select_blocking_cnt, muc_light_blocking,
                           [luser, lserver, what, who],
                           <<"SELECT COUNT(*) FROM muc_light_blocking "
                             "WHERE luser = ? AND lserver = ? AND "
                             "what = ? AND who = ?">>),
    mongoose_rdbms:prepare(muc_light_select_blocking_cnt2, muc_light_blocking,
                           [luser, lserver, what, who, what, who],
                           <<"SELECT COUNT(*) FROM muc_light_blocking "
                             "WHERE luser = ? AND lserver = ? AND "
                             "((what = ? AND who = ?) OR (what = ? AND who = ?))">>),
    mongoose_rdbms:prepare(muc_light_insert_blocking, muc_light_blocking,
                           [luser, lserver, what, who],
                           <<"INSERT INTO muc_light_blocking"
                             " (luser, lserver, what, who)"
                             " VALUES (?, ?, ?, ?)">>),
    mongoose_rdbms:prepare(muc_light_delete_blocking1, muc_light_blocking,
                           [luser, lserver, what, who],
                           <<"DELETE FROM muc_light_blocking "
                             "WHERE luser = ? AND lserver = ? AND what = ? AND who = ?">>),
    mongoose_rdbms:prepare(muc_light_delete_blocking, muc_light_blocking,
                           [luser, lserver],
                           <<"DELETE FROM muc_light_blocking"
                             " WHERE luser = ? AND lserver = ?">>),
    ok.

prepare_domain_removal_queries() ->
    %% RoomS argument
    mongoose_rdbms:prepare(muc_light_occupants_remove_room_domain, muc_light_occupants,
                           ['muc_light_rooms.lserver'],
                           <<"DELETE FROM muc_light_occupants WHERE room_id IN "
                             "(SELECT id FROM muc_light_rooms WHERE lserver = ?)">>),
    %% User's LServer argument
    mongoose_rdbms:prepare(muc_light_occupants_remove_user_domain, muc_light_occupants,
                           [lserver],
                           <<"DELETE FROM muc_light_occupants WHERE lserver = ?">>),
    %% RoomS argument
    mongoose_rdbms:prepare(muc_light_config_remove_room_domain, muc_light_config,
                           ['muc_light_rooms.lserver'],
                           <<"DELETE FROM muc_light_config WHERE room_id IN "
                             "(SELECT id FROM muc_light_rooms WHERE lserver = ?)">>),
    %% User's LServer argument
    mongoose_rdbms:prepare(muc_light_blocking_remove_user_domain, muc_light_blocking,
                           [lserver],
                           <<"DELETE FROM muc_light_blocking WHERE lserver = ?">>),
    %% RoomS argument
    mongoose_rdbms:prepare(muc_light_rooms_remove_room_domain, muc_light_rooms,
                           [lserver],
                           <<"DELETE FROM muc_light_rooms WHERE lserver = ?">>),
    ok.

%% ------------------------ Room SQL functions ------------------------

select_room_id(HostType, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_room_id, [RoomU, RoomS]).

select_room_id_and_version(HostType, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_room_id_and_version, [RoomU, RoomS]).

select_user_rooms(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_user_rooms, [LUser, LServer]).

select_user_rooms_count(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_user_rooms_count, [LUser, LServer]).

insert_room(HostType, RoomU, RoomS, Version) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_insert_room, [RoomU, RoomS, Version]).

update_room_version(HostType, RoomU, RoomS, Version) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_update_room_version, [Version, RoomU, RoomS]).

delete_room(HostType, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_delete_room, [RoomU, RoomS]).

%% ------------------------ Affiliation SQL functions ------------------------

%% Returns affiliations with a version
select_affs_by_us(HostType, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_affs_by_us, [RoomU, RoomS]).

%% Returns affiliations without a version
select_affs_by_room_id(HostType, RoomID) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_affs_by_room_id, [RoomID]).

insert_aff(HostType, RoomID, UserU, UserS, Aff) ->
    DbAff = aff_atom2db(Aff),
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_insert_aff, [RoomID, UserU, UserS, DbAff]).

update_aff(HostType, RoomID, UserU, UserS, Aff) ->
    DbAff = aff_atom2db(Aff),
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_update_aff, [DbAff, RoomID, UserU, UserS]).

delete_affs(HostType, RoomID) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_delete_affs, [RoomID]).

delete_aff(HostType, RoomID, UserU, UserS) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_delete_aff, [RoomID, UserU, UserS]).

%% ------------------------ Config SQL functions ---------------------------
select_config_by_room_id(HostType, RoomID) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_config_by_room_id, [RoomID]).

select_config_by_us(HostType, RoomU, RoomS) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_config_by_us, [RoomU, RoomS]).

insert_config(HostType, RoomID, Key, Val) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_insert_config, [RoomID, Key, Val]).

update_config(HostType, RoomID, Key, Val) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_update_config, [Val, RoomID, Key]).

delete_config(HostType, RoomID) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_delete_config, [RoomID]).

%% ------------------------ Blocking SQL functions -------------------------

select_blocking(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_blocking, [LUser, LServer]).

select_blocking_cnt(HostType, LUser, LServer, [{What, Who}]) ->
    DbWhat = what_atom2db(What),
    DbWho = jid:to_binary(Who),
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_blocking_cnt,
        [LUser, LServer, DbWhat, DbWho]);
select_blocking_cnt(HostType, LUser, LServer, [{What1, Who1}, {What2, Who2}]) ->
    DbWhat1 = what_atom2db(What1),
    DbWhat2 = what_atom2db(What2),
    DbWho1 = jid:to_binary(Who1),
    DbWho2 = jid:to_binary(Who2),
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_select_blocking_cnt2,
        [LUser, LServer, DbWhat1, DbWho1, DbWhat2, DbWho2]).

insert_blocking(HostType, LUser, LServer, What, Who) ->
    DbWhat = what_atom2db(What),
    DbWho = jid:to_binary(Who),
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_insert_blocking,
        [LUser, LServer, DbWhat, DbWho]).

delete_blocking1(HostType, LUser, LServer, What, Who) ->
    DbWhat = what_atom2db(What),
    DbWho = jid:to_binary(Who),
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_delete_blocking1,
        [LUser, LServer, DbWhat, DbWho]).

delete_blocking(HostType, UserU, UserS) ->
    mongoose_rdbms:execute_successfully(
        HostType, muc_light_delete_blocking, [UserU, UserS]).

%% ------------------------ General room management ------------------------

-spec create_room(HostType :: mongooseim:host_type(),
                  RoomUS :: jid:simple_bare_jid(), Config :: mod_muc_light_room_config:kv(),
                  AffUsers :: aff_users(), Version :: binary()) ->
    {ok, FinalRoomUS :: jid:simple_bare_jid()} | {error, exists}.
create_room(HostType, {<<>>, RoomS}, Config, AffUsers, Version) ->
    create_room_with_random_name(HostType, RoomS, Config, AffUsers, Version, 10);
create_room(HostType, RoomUS, Config, AffUsers, Version) ->
    create_room_with_specified_name(HostType, RoomUS, Config, AffUsers, Version).

create_room_with_random_name(_HostType, RoomS, _Config, _AffUsers, _Version, 0) ->
    ?LOG_ERROR(#{what => muc_create_room_with_random_name_failed,
                 sub_host => RoomS}),
    error(create_room_with_random_name_failed);
create_room_with_random_name(HostType, RoomS, Config, AffUsers, Version, Retries)
    when Retries > 0 ->
    RoomU = mongoose_bin:gen_from_timestamp(),
    RoomUS = {RoomU, RoomS},
    F = fun() -> create_room_transaction(HostType, RoomUS, Config, AffUsers, Version) end,
    case mongoose_rdbms:sql_transaction(HostType, F) of
        {atomic, ok} ->
            {ok, RoomUS};
        Other ->
            ?LOG_ERROR(#{what => muc_create_room_with_random_name_retry,
                         candidate_room => RoomU, sub_host => RoomS, reason => Other}),
            create_room_with_random_name(HostType, RoomS, Config, AffUsers, Version, Retries-1)
    end.

create_room_with_specified_name(HostType, RoomUS, Config, AffUsers, Version) ->
    F = fun() -> create_room_transaction(HostType, RoomUS, Config, AffUsers, Version) end,
    case mongoose_rdbms:sql_transaction(HostType, F) of
        {atomic, ok} ->
            {ok, RoomUS};
        Other ->
            case room_exists(HostType, RoomUS) of
                true ->
                    {error, exists};
                false -> %% Some unknown error condition
                    {RoomU, RoomS} = RoomUS,
                    ?LOG_ERROR(#{what => muc_create_room_with_specified_name_failed,
                                 room => RoomU, sub_host => RoomS, reason => Other}),
                    error(create_room_with_specified_name_failed)
            end
    end.

-spec destroy_room(HostType :: mongooseim:host_type(),
                   RoomUS :: jid:simple_bare_jid()) ->
    ok | {error, not_exists}.
destroy_room(HostType, RoomUS) ->
    F = fun() -> destroy_room_transaction(HostType, RoomUS) end,
    {atomic, Res} = mongoose_rdbms:sql_transaction(HostType, F),
    Res.

-spec room_exists(HostType :: mongooseim:host_type(),
                  RoomUS :: jid:simple_bare_jid()) -> boolean().
room_exists(HostType, {RoomU, RoomS}) ->
    {selected, Res} = select_room_id(HostType, RoomU, RoomS),
    Res /= [].

-spec get_user_rooms(HostType :: mongooseim:host_type(),
                     UserUS :: jid:simple_bare_jid(),
                     MUCServer :: jid:lserver() | undefined) ->
    [RoomUS :: jid:simple_bare_jid()].
get_user_rooms(HostType, {LUser, LServer}, undefined) ->
    %% Only one hosttype is handled here
    %% It is used to be map over MYHOSTS
    {selected, Rooms} = select_user_rooms(HostType, LUser, LServer),
    lists:usort(Rooms);
get_user_rooms(HostType, {LUser, LServer}, _MUCServer) ->
    {selected, Rooms} = select_user_rooms(HostType, LUser, LServer),
    Rooms.

-spec get_user_rooms_count(HostType :: mongooseim:host_type(),
                           UserUS :: jid:simple_bare_jid()) ->
    non_neg_integer().
get_user_rooms_count(HostType, {LUser, LServer}) ->
    {selected, [{Cnt}]} = select_user_rooms_count(HostType, LUser, LServer),
    mongoose_rdbms:result_to_integer(Cnt).

-spec remove_user(HostType :: mongooseim:host_type(),
                  UserUS :: jid:simple_bare_jid(),
                  Version :: binary()) ->
    mod_muc_light_db_backend:remove_user_return() | {error, term()}.
remove_user(HostType, {_, UserS} = UserUS, Version) ->
    F = fun() -> remove_user_transaction(HostType, UserUS, Version) end,
    {atomic, Res} = mongoose_rdbms:sql_transaction(UserS, F),
    Res.

-spec remove_domain(mongooseim:host_type(), jid:lserver(), jid:lserver()) -> ok.
remove_domain(HostType, RoomS, LServer) ->
    F = fun() ->
        mongoose_rdbms:execute_successfully(
            HostType, muc_light_occupants_remove_room_domain, [RoomS]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_light_occupants_remove_user_domain, [LServer]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_light_config_remove_room_domain, [RoomS]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_light_blocking_remove_user_domain, [LServer]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_light_rooms_remove_room_domain, [RoomS]),
        ok
        end,
    {atomic, ok} = mongoose_rdbms:sql_transaction(HostType, F),
    ok.

%% ------------------------ Configuration manipulation ------------------------

-spec get_config(HostType :: mongooseim:host_type(),
                 RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), Version :: binary()} | {error, not_exists}.
get_config(HostType, {RoomU, RoomS}) ->
    {selected, Result} = select_config_by_us(HostType, RoomU, RoomS),
    case Result of
        [] ->
            {error, not_exists};
        [{Version, null, null}] ->
            {ok, [], Version};
        [{Version, _, _} | _] ->
            RawConfig = [{Key, Val} || {_, Key, Val} <- Result],
            {ok, Config} = mod_muc_light_room_config:from_binary_kv(
                             RawConfig, mod_muc_light:config_schema(RoomS)),
            {ok, Config, Version}
    end.

-spec set_config(HostType :: mongooseim:host_type(),
                 RoomUS :: jid:simple_bare_jid(), Config :: mod_muc_light_room_config:kv(),
                 Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(HostType, RoomUS, ConfigChanges, Version) ->
    {atomic, Res}
    = mongoose_rdbms:sql_transaction(
        HostType, fun() -> set_config_transaction(RoomUS, ConfigChanges, Version) end),
    Res.

%% ------------------------ Blocking manipulation ------------------------

-spec get_blocking(HostType :: mongooseim:host_type(),
                   UserUS :: jid:simple_bare_jid(), MUCServer :: jid:lserver()) ->
    [blocking_item()].
get_blocking(HostType, {LUser, LServer}, _MUCServer) ->
    {selected, WhatWhos} = select_blocking(HostType, LUser, LServer),
    decode_blocking(WhatWhos).

-spec get_blocking(HostType :: mongooseim:host_type(),
                   UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   WhatWhos :: [{blocking_what(), jid:simple_bare_jid()}]) ->
    blocking_action().
get_blocking(HostType, {LUser, LServer}, _MUCServer, WhatWhos) ->
    {selected, [{Count}]} = select_blocking_cnt(HostType, LUser, LServer, WhatWhos),
    case mongoose_rdbms:result_to_integer(Count) of
        0 -> allow;
        _ -> deny
    end.

-spec set_blocking(HostType :: mongooseim:host_type(),
                   UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   BlockingItems :: [blocking_item()]) -> ok.
set_blocking(HostType, UserUS, MUCServer, BlockingItems) ->
    set_blocking_loop(HostType, UserUS, MUCServer, BlockingItems).

set_blocking_loop(_HostType, _UserUS, _MUCServer, []) ->
    ok;
set_blocking_loop(HostType, {LUser, LServer} = UserUS, MUCServer,
             [{What, deny, Who} | RBlockingItems]) ->
    {updated, _} = insert_blocking(HostType, LUser, LServer, What, Who),
    set_blocking_loop(HostType, UserUS, MUCServer, RBlockingItems);
set_blocking_loop(HostType, {LUser, LServer} = UserUS, MUCServer,
             [{What, allow, Who} | RBlockingItems]) ->
    {updated, _} = delete_blocking1(HostType, LUser, LServer, What, Who),
    set_blocking_loop(HostType, UserUS, MUCServer, RBlockingItems).

%% ------------------------ Affiliations manipulation ------------------------

-spec get_aff_users(HostType :: mongooseim:host_type(),
                    RoomUS :: jid:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.
get_aff_users(HostType, {RoomU, RoomS}) ->
    case select_affs_by_us(HostType, RoomU, RoomS) of
        {selected, []} ->
            {error, not_exists};
        {selected, [{Version, null, null, null}]} ->
            {ok, [], Version};
        {selected, [{Version, _, _, _} | _] = Res} ->
            AffUsers = decode_affs_with_versions(Res),
            {ok, AffUsers, Version}
    end.

-spec modify_aff_users(HostType :: mongooseim:host_type(),
                       RoomUS :: jid:simple_bare_jid(),
                       AffUsersChanges :: aff_users(),
                       ExternalCheck :: external_check_fun(),
                       Version :: binary()) ->
    mod_muc_light_db_backend:modify_aff_users_return().
modify_aff_users(HostType, RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    F = fun() -> modify_aff_users_transaction(HostType, RoomUS, AffUsersChanges,
                                              ExternalCheck, Version) end,
    {atomic, Res} = mongoose_rdbms:sql_transaction(HostType, F),
    Res.

%% ------------------------ Misc ------------------------

-spec get_info(HostType :: mongooseim:host_type(),
               RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), aff_users(), Version :: binary()}
    | {error, not_exists}.
get_info(HostType, {RoomU, RoomS}) ->
    case select_room_id_and_version(HostType, RoomU, RoomS) of
        {selected, [{DbRoomID, Version}]} ->
            RoomID = mongoose_rdbms:result_to_integer(DbRoomID),
            {selected, AffUsersDB} = select_affs_by_room_id(HostType, RoomID),
            AffUsers = decode_affs(AffUsersDB),
            {selected, ConfigDB} = select_config_by_room_id(HostType, RoomID),
            {ok, Config} = mod_muc_light_room_config:from_binary_kv(
                             ConfigDB, mod_muc_light:config_schema(RoomS)),

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

decode_blocking(WhatWhos) ->
    [ {what_db2atom(What), deny, jid:to_lus(jid:from_binary(Who))}
      || {What, Who} <- WhatWhos ].

-spec what_db2atom(binary() | pos_integer()) -> blocking_what().
what_db2atom(1) -> room;
what_db2atom(2) -> user;
what_db2atom(Bin) -> what_db2atom(mongoose_rdbms:result_to_integer(Bin)).

-spec what_atom2db(blocking_what()) -> non_neg_integer().
what_atom2db(room) -> 1;
what_atom2db(user) -> 2.

-spec aff_atom2db(aff()) -> non_neg_integer().
aff_atom2db(owner) -> 1;
aff_atom2db(member) -> 2.

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
-spec create_room_transaction(HostType :: mongooseim:host_type(),
                              RoomUS :: jid:simple_bare_jid(),
                              Config :: mod_muc_light_room_config:kv(),
                              AffUsers :: aff_users(),
                              Version :: binary()) -> ok.
create_room_transaction(HostType, {RoomU, RoomS}, Config, AffUsers, Version) ->
    insert_room(HostType, RoomU, RoomS, Version),
    RoomID = mongoose_rdbms:selected_to_integer(select_room_id(HostType, RoomU, RoomS)),
    Schema = mod_muc_light:config_schema(RoomS),
    {ok, ConfigFields} = mod_muc_light_room_config:to_binary_kv(Config, Schema),
    [insert_aff_tuple(HostType, RoomID, AffUser) || AffUser <- AffUsers],
    [insert_config_kv(HostType, RoomID, KV) || KV <- ConfigFields],
    ok.

insert_aff_tuple(HostType, RoomID, {{UserU, UserS}, Aff}) ->
    insert_aff(HostType, RoomID, UserU, UserS, Aff).

insert_config_kv(HostType, RoomID, {Key, Val}) ->
    insert_config(HostType, RoomID, Key, Val).

-spec destroy_room_transaction(HostType :: mongooseim:host_type(),
                               RoomUS :: jid:simple_bare_jid()) ->
    ok | {error, not_exists}.
destroy_room_transaction(HostType, {RoomU, RoomS}) ->
    case select_room_id(HostType, RoomU, RoomS) of
        {selected, [{DbRoomID}]} ->
            RoomID = mongoose_rdbms:result_to_integer(DbRoomID),
            {updated, _} = delete_affs(HostType, RoomID),
            {updated, _} = delete_config(HostType, RoomID),
            {updated, _} = delete_room(HostType, RoomU, RoomS),
            ok;
        {selected, []} ->
            {error, not_exists}
    end.

-spec remove_user_transaction(HostType :: mongooseim:host_type(),
                              UserUS :: jid:simple_bare_jid(),
                              Version :: binary()) ->
    mod_muc_light_db_backend:remove_user_return().
remove_user_transaction(HostType, {UserU, UserS} = UserUS, Version) ->
    Rooms = get_user_rooms(HostType, UserUS, undefined),
    {updated, _} = delete_blocking(HostType, UserU, UserS),
    lists:map(
      fun(RoomUS) ->
              {RoomUS, modify_aff_users_transaction(HostType,
                         RoomUS, [{UserUS, none}], fun(_, _) -> ok end, Version)}
      end, Rooms).

%% ------------------------ Configuration manipulation ------------------------

-spec set_config_transaction(RoomUS :: jid:simple_bare_jid(),
                             ConfigChanges :: mod_muc_light_room_config:kv(),
                             Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config_transaction({RoomU, RoomS} = RoomUS, ConfigChanges, Version) ->
    HostType = room_us_to_host_type(RoomUS),
    case select_room_id_and_version(HostType, RoomU, RoomS) of
        {selected, [{DbRoomID, PrevVersion}]} ->
            RoomID = mongoose_rdbms:result_to_integer(DbRoomID),
            {updated, _} = update_room_version(HostType, RoomU, RoomS, Version),
            {ok, Config} = mod_muc_light_room_config:to_binary_kv_diff(
                             ConfigChanges, mod_muc_light:config_schema(RoomS)),
            lists:foreach(
              fun({Key, Val}) ->
                      {updated, _} = update_config(HostType, RoomID, Key, Val)
              end, Config),
            {ok, PrevVersion};
        {selected, []} ->
            {error, not_exists}
    end.

%% ------------------------ Blocking manipulation ------------------------

%% ------------------------ Affiliations manipulation ------------------------

-spec modify_aff_users_transaction(HostType :: mongooseim:host_type(),
                                   RoomUS :: jid:simple_bare_jid(),
                                   AffUsersChanges :: aff_users(),
                                   CheckFun :: external_check_fun(),
                                   Version :: binary()) ->
    mod_muc_light_db_backend:modify_aff_users_return().
modify_aff_users_transaction(HostType, {RoomU, RoomS} = RoomUS,
                             AffUsersChanges, CheckFun, Version) ->
    case select_room_id_and_version(HostType, RoomU, RoomS) of
        {selected, [{DbRoomID, PrevVersion}]} ->
            RoomID = mongoose_rdbms:result_to_integer(DbRoomID),
            modify_aff_users_transaction(HostType,
              RoomUS, RoomID, AffUsersChanges, CheckFun, PrevVersion, Version);
        {selected, []} ->
            {error, not_exists}
    end.

-spec modify_aff_users_transaction(HostType :: mongooseim:host_type(),
                                   RoomUS :: jid:simple_bare_jid(),
                                   RoomID :: room_id(),
                                   AffUsersChanges :: aff_users(),
                                   CheckFun :: external_check_fun(),
                                   PrevVersion :: binary(),
                                   Version :: binary()) ->
    mod_muc_light_db_backend:modify_aff_users_return().
modify_aff_users_transaction(HostType, RoomUS, RoomID, AffUsersChanges,
                             CheckFun, PrevVersion, Version) ->
    {selected, AffUsersDB} = select_affs_by_room_id(HostType, RoomID),
    AffUsers = decode_affs(AffUsersDB),
    case mod_muc_light_utils:change_aff_users(AffUsers, AffUsersChanges) of
        {ok, NewAffUsers, AffUsersChanged, JoiningUsers, _LeavingUsers} ->
            case CheckFun(RoomUS, NewAffUsers) of
                ok ->
                    apply_aff_users_transaction(HostType, RoomID, AffUsersChanged, JoiningUsers),
                    {RoomU, RoomS} = RoomUS,
                    {updated, _} = update_room_version(HostType, RoomU, RoomS, Version),
                    {ok, AffUsers, NewAffUsers, AffUsersChanged, PrevVersion};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec apply_aff_users_transaction(HostType :: mongooseim:host_type(),
                                  RoomID :: room_id(),
                                  AffUsersChanges :: aff_users(),
                                  JoiningUsers :: [jid:simple_bare_jid()]) -> ok.
apply_aff_users_transaction(HostType, RoomID, AffUsersChanged, JoiningUsers) ->
    lists:foreach(
      fun({{UserU, UserS}, none}) ->
              {updated, _} = delete_aff(HostType, RoomID, UserU, UserS);
         ({{UserU, UserS} = UserUS, Aff}) ->
              case lists:member(UserUS, JoiningUsers) of
                  true ->
                      {updated, _} = insert_aff(HostType, RoomID, UserU, UserS, Aff);
                  false ->
                      {updated, _} = update_aff(HostType, RoomID, UserU, UserS, Aff)
              end
      end, AffUsersChanged).

%% ------------------------ Common ------------------------

-spec room_us_to_host_type(jid:simple_bare_jid()) -> mongooseim:host_type().
room_us_to_host_type({_, RoomS}) ->
    muc_server_to_host_type(RoomS).

-spec muc_server_to_host_type(jid:lserver()) -> mongooseim:host_type().
muc_server_to_host_type(MUCServer) ->
    mod_muc_light_utils:muc_host_to_host_type(MUCServer).
