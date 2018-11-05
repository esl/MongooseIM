%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_db_rdbms_sql.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : RDBMS backend queries for mod_muc_light
%%% Created : 29 Nov 2016 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
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

-module(mod_muc_light_db_rdbms_sql).
-author('piotr.nosek@erlang-solutions.com').

-include("mod_muc_light.hrl").

-export([select_room_id/2, select_room_id_and_version/2,
         select_user_rooms/2, select_user_rooms_count/2,
         insert_room/3, update_room_version/3, delete_room/2]).
-export([select_affs/2, select_affs/1, insert_aff/4, update_aff/4, delete_affs/1, delete_aff/3]).
-export([select_config/1, select_config/2, select_config/3, insert_config/3, update_config/3,
        delete_config/1]).
-export([select_blocking/2, select_blocking_cnt/3, insert_blocking/4,
         delete_blocking/4, delete_blocking/2]).

-define(ESC(T), mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(T))).

%%====================================================================
%% General room queries
%%====================================================================

-spec select_room_id(RoomU :: jid:luser(), RoomS :: jid:lserver()) -> iolist().
select_room_id(RoomU, RoomS) ->
    ["SELECT id FROM muc_light_rooms WHERE luser = ", ?ESC(RoomU),
                                     " AND lserver = ", ?ESC(RoomS)].

-spec select_room_id_and_version(
        RoomU :: jid:luser(), RoomS :: jid:lserver()) -> iolist().
select_room_id_and_version(RoomU, RoomS) ->
    ["SELECT id, version FROM muc_light_rooms WHERE luser = ", ?ESC(RoomU),
                                              " AND lserver = ", ?ESC(RoomS)].

-spec select_user_rooms(LUser :: jid:luser(), LServer :: jid:lserver()) -> iolist().
select_user_rooms(LUser, LServer) ->
    select_user_rooms(LUser, LServer, "r.luser, r.lserver").

-spec select_user_rooms_count(LUser :: jid:luser(), LServer :: jid:lserver()) -> iolist().
select_user_rooms_count(LUser, LServer) ->
    select_user_rooms(LUser, LServer, "COUNT(*)").

-spec select_user_rooms(LUser :: jid:luser(),
                        LServer :: jid:lserver(),
                        ReturnStatement :: iodata()) -> iolist().
select_user_rooms(LUser, LServer, ReturnStatement) ->
    ["SELECT ", ReturnStatement,
     " FROM muc_light_occupants AS o INNER JOIN muc_light_rooms AS r ON o.room_id = r.id"
     " WHERE o.luser = ", ?ESC(LUser), " AND o.lserver = ", ?ESC(LServer)].

-spec insert_room(
        RoomU :: jid:luser(), RoomS :: jid:lserver(), Version :: binary()) -> iolist().
insert_room(RoomU, RoomS, Version) ->
    ["INSERT INTO muc_light_rooms (luser, lserver, version)"
     " VALUES (", ?ESC(RoomU), ", ", ?ESC(RoomS), ", ", ?ESC(Version), ")"].

-spec update_room_version(
        RoomU :: jid:luser(), RoomS :: jid:lserver(), Version :: binary()) -> iolist().
update_room_version(RoomU, RoomS, Version) ->
    ["UPDATE muc_light_rooms SET version = ", ?ESC(Version),
     " WHERE luser = ", ?ESC(RoomU), " AND lserver = ", ?ESC(RoomS)].

-spec delete_room(RoomU :: jid:luser(), RoomS :: jid:lserver()) -> iolist().
delete_room(RoomU, RoomS) ->
    ["DELETE FROM muc_light_rooms"
     " WHERE luser = ", ?ESC(RoomU), " AND lserver = ", ?ESC(RoomS)].

%%====================================================================
%% Affiliations
%%====================================================================

-spec select_affs(RoomU :: jid:luser(), RoomS :: jid:lserver()) -> iolist().
select_affs(RoomU, RoomS) ->
    ["SELECT version, o.luser, o.lserver, aff"
     " FROM muc_light_rooms AS r LEFT OUTER JOIN muc_light_occupants AS o ON r.id = o.room_id"
     " WHERE r.luser = ", ?ESC(RoomU), " AND r.lserver = ", ?ESC(RoomS)].

-spec select_affs(RoomID :: integer() | binary()) -> iolist().
select_affs(RoomID) ->
    ["SELECT luser, lserver, aff FROM muc_light_occupants WHERE room_id = ", bin(RoomID)].

-spec insert_aff(RoomID :: integer() | binary(), UserU :: jid:luser(),
                 UserS :: jid:lserver(), Aff :: aff()) -> iolist().
insert_aff(RoomID, UserU, UserS, Aff) ->
    ["INSERT INTO muc_light_occupants (room_id, luser, lserver, aff)"
     " VALUES(", bin(RoomID), ", ", ?ESC(UserU), ", ", ?ESC(UserS), ", ",
              mod_muc_light_db_rdbms:aff_atom2db(Aff), ")"].

-spec update_aff(RoomID :: integer() | binary(), UserU :: jid:luser(),
                 UserS :: jid:lserver(), Aff :: aff()) -> iolist().
update_aff(RoomID, UserU, UserS, Aff) ->
    ["UPDATE muc_light_occupants SET aff = ", mod_muc_light_db_rdbms:aff_atom2db(Aff),
     " WHERE room_id = ", bin(RoomID), " AND luser = ", ?ESC(UserU),
       " AND lserver = ", ?ESC(UserS)].

-spec delete_affs(RoomID :: integer() | binary()) -> iolist().
delete_affs(RoomID) ->
    ["DELETE FROM muc_light_occupants WHERE room_id = ", bin(RoomID)].

-spec delete_aff(RoomID :: integer() | binary(), UserU :: jid:luser(),
                 UserS :: jid:lserver()) ->
    iolist().
delete_aff(RoomID, UserU, UserS) ->
    ["DELETE FROM muc_light_occupants WHERE room_id = ", bin(RoomID),
                                      " AND luser = ", ?ESC(UserU),
                                      " AND lserver = ", ?ESC(UserS)].

%%====================================================================
%% Config
%%====================================================================

-spec select_config(RoomID :: integer() | binary()) -> iolist().
select_config(RoomID) ->
    ["SELECT opt, val FROM muc_light_config WHERE room_id = ", bin(RoomID)].

-spec select_config(RoomU :: jid:luser(), RoomS :: jid:lserver()) -> iolist().
select_config(RoomU, RoomS) ->
    ["SELECT version, opt, val",
     " FROM muc_light_rooms AS r LEFT OUTER JOIN muc_light_config AS c ON r.id = c.room_id"
     " WHERE r.luser = ", ?ESC(RoomU), " AND r.lserver = ", ?ESC(RoomS)].

-spec select_config(RoomU :: jid:luser(), RoomS :: jid:lserver(), Key :: binary()) ->
    iolist().
select_config(RoomU, RoomS, Key) ->
    [ select_config(RoomU, RoomS), " AND key = '", Key, "'" ].

-spec insert_config(RoomID :: integer() | binary(), Key :: binary(), Val :: binary()) -> iolist().
insert_config(RoomID, Key, Val) ->
    ["INSERT INTO muc_light_config (room_id, opt, val)"
     " VALUES(", bin(RoomID), ", ", ?ESC(Key), ", ", ?ESC(Val), ")"].

-spec update_config(RoomID :: integer() | binary(), Key :: binary(), Val :: binary()) -> iolist().
update_config(RoomID, Key, Val) ->
    ["UPDATE muc_light_config SET val = ", ?ESC(Val),
     " WHERE room_id = ", bin(RoomID), " AND opt = ", ?ESC(Key)].

-spec delete_config(RoomID :: integer() | binary()) -> iolist().
delete_config(RoomID) ->
    ["DELETE FROM muc_light_config WHERE room_id = ", bin(RoomID)].

%%====================================================================
%% Blocking
%%====================================================================

-spec select_blocking(LUser :: jid:luser(), LServer :: jid:lserver()) -> iolist().
select_blocking(LUser, LServer) ->
    ["SELECT what, who FROM muc_light_blocking WHERE luser = ", ?ESC(LUser),
                                               " AND lserver = ", ?ESC(LServer)].

-spec select_blocking_cnt(LUser :: jid:luser(), LServer :: jid:lserver(),
                           WhatWhos :: [{blocking_who(), jid:simple_bare_jid()}]) -> iolist().
select_blocking_cnt(LUser, LServer, WhatWhos) ->
    [ _ | WhatWhosWhere ] = lists:flatmap(
                              fun({What, Who}) ->
                                      [" OR ", "(what = ", mod_muc_light_db_rdbms:what_atom2db(What),
                                           " AND who = ", ?ESC(jid:to_binary(Who)), ")"] end,
                              WhatWhos),
    ["SELECT COUNT(*) FROM muc_light_blocking WHERE luser = ", ?ESC(LUser),
                                              " AND lserver = ", ?ESC(LServer),
                                              " AND (", WhatWhosWhere, ")"].

-spec insert_blocking(LUser :: jid:luser(), LServer :: jid:lserver(),
                       What :: blocking_what(), Who :: blocking_who()) -> iolist().
insert_blocking(LUser, LServer, What, Who) ->
    ["INSERT INTO muc_light_blocking (luser, lserver, what, who)"
     " VALUES (", ?ESC(LUser), ", ", ?ESC(LServer), ", ",
               mod_muc_light_db_rdbms:what_atom2db(What), ", ", ?ESC(jid:to_binary(Who)), ")"].

-spec delete_blocking(LUser :: jid:luser(), LServer :: jid:lserver(),
                         What :: blocking_what(), Who :: blocking_who()) -> iolist().
delete_blocking(LUser, LServer, What, Who) ->
    ["DELETE FROM muc_light_blocking WHERE luser = ", ?ESC(LUser),
                                     " AND lserver = ", ?ESC(LServer),
                                     " AND what = ", mod_muc_light_db_rdbms:what_atom2db(What),
                                     " AND who = ", ?ESC(jid:to_binary(Who))].

-spec delete_blocking(UserU :: jid:luser(), UserS :: jid:lserver()) -> iolist().
delete_blocking(UserU, UserS) ->
    ["DELETE FROM muc_light_blocking"
     " WHERE luser = ", ?ESC(UserU), " AND lserver = ", ?ESC(UserS)].

%%====================================================================
%% Helpers
%%====================================================================

-spec bin(integer() | binary()) -> binary().
bin(Int) when is_integer(Int) -> integer_to_binary(Int);
bin(Bin) when is_binary(Bin) -> Bin.
