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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_db_rdbms_sql).
-author('piotr.nosek@erlang-solutions.com').

-include("mod_muc_light.hrl").

-export([delete_affs/1, delete_aff/3]).
-export([select_config/1, select_config/2, select_config/3, insert_config/3, update_config/3,
        delete_config/1]).
-export([select_blocking/2, select_blocking_cnt/3, insert_blocking/4,
         delete_blocking/4, delete_blocking/2]).

-define(ESC(T), mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(T))).

%%====================================================================
%% Affiliations
%%====================================================================

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
