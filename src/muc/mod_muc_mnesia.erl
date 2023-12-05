%%%----------------------------------------------------------------------
%%% Based on file mod_muc.
%%%
%%% Original header and copyright notice:
%%%
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_mnesia).
-behaviour(mod_muc_backend).
-export([init/2,
         store_room/4,
         restore_room/3,
         forget_room/3,
         get_rooms/2,
         can_use_nick/4,
         get_nick/3,
         set_nick/4,
         unset_nick/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc.hrl").

-record(muc_registered, {
            us_host    :: {US :: jid:simple_bare_jid(), MucHost :: jid:lserver()} | '$1',
            nick       :: mod_muc:nick()
         }).

init(_HostType, _Opts) ->
    mongoose_mnesia:create_table(muc_room,
        [{disc_copies, [node()]},
         {attributes, record_info(fields, muc_room)}]),
    mongoose_mnesia:create_table(muc_registered,
        [{disc_copies, [node()]},
         {attributes, record_info(fields, muc_registered)}]),
    mnesia:add_table_index(muc_registered, nick),
    ok.

-spec store_room(mongooseim:host_type(), jid:server(), mod_muc:room(), list())
            -> ok | {error, term()}.
store_room(HostType, MucHost, RoomName, Opts) ->
    F = fun() ->
                mnesia:write(#muc_room{name_host = {RoomName, MucHost},
                                       opts = Opts})
        end,
    Result = mnesia:transaction(F),
    case Result of
        {atomic, _} ->
            ok;
        _ ->
            ?LOG_ERROR(#{what => muc_store_room_failed,
                         sub_host => MucHost, host_type => HostType,
                         room => RoomName, reason => Result}),
            {error, Result}
    end.

restore_room(HostType, MucHost, RoomName) ->
    try mnesia:dirty_read(muc_room, {RoomName, MucHost}) of
        [#muc_room{opts = Opts}] ->
            {ok, Opts};
        [] ->
            {error, room_not_found};
        Other ->
            {error, Other}
    catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{what => muc_restore_room_failed, room => RoomName,
                     sub_host => MucHost, host_type => HostType,
                     class => Class, reason => Reason, stacktrace => Stacktrace}),
        {error, {Class, Reason}}
    end.

-spec forget_room(mongooseim:host_type(), jid:server(), mod_muc:room()) ->
    ok | {error, term()}.
forget_room(HostType, MucHost, RoomName) ->
    F = fun() ->
                mnesia:delete({muc_room, {RoomName, MucHost}})
        end,
    Result = mnesia:transaction(F),
    case Result of
        {atomic, _} ->
            ok;
        _ ->
            ?LOG_ERROR(#{what => muc_forget_room_failed,
                         sub_host => MucHost, host_type => HostType,
                         room => RoomName, reason => Result}),
            {error, Result}
    end.

get_rooms(HostType, MucHost) ->
    Query = [{#muc_room{name_host = {'_', MucHost}, _ = '_'},
             [],
             ['$_']}],
    try
        {ok, mnesia:dirty_select(muc_room, Query)}
    catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{what => muc_get_rooms_failed,
                     sub_host => MucHost, host_type => HostType,
                     class => Class, reason => Reason, stacktrace => Stacktrace}),
        {error, {Class, Reason}}
    end.

-spec can_use_nick(mongooseim:host_type(), jid:server(),
                   jid:jid(), mod_muc:nick()) -> boolean().
can_use_nick(_HostType, MucHost, JID, Nick) ->
    LUS = jid:to_lus(JID),
    can_use_nick_internal(MucHost, Nick, LUS).

can_use_nick_internal(MucHost, Nick, LUS) ->
    Query = [{#muc_registered{us_host = '$1', nick = Nick, _ = '_'},
             [{'==', {element, 2, '$1'}, MucHost}],
             ['$_']}],
    try mnesia:dirty_select(muc_registered, Query) of
        [] ->
            true;
        [#muc_registered{us_host = {U, _Host}}] ->
            U == LUS
    catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{what => muc_can_use_nick_failed, sub_host => MucHost,
                     class => Class, reason => Reason, stacktrace => Stacktrace}),
        false
    end.

get_nick(_HostType, MucHost, From) ->
    LUS = jid:to_lus(From),
    try mnesia:dirty_read(muc_registered, {LUS, MucHost}) of
        [] ->
            {error, not_registered};
        [#muc_registered{nick = Nick}] ->
            {ok, Nick}
    catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{what => muc_get_nick_failed,
                     sub_host => MucHost, from_jid => jid:to_binary(From),
                     class => Class, reason => Reason, stacktrace => Stacktrace}),
        {error, {Class, Reason}}
    end.

set_nick(HostType, MucHost, From, Nick)
      when is_binary(Nick), Nick =/= <<>> ->
    LUS = jid:to_lus(From),
    F = fun () ->
            case can_use_nick_internal(MucHost, Nick, LUS) of
                true ->
                    Object = #muc_registered{us_host = {LUS, MucHost}, nick = Nick},
                    mnesia:write(Object),
                    ok;
                false ->
                    {error, conflict}
            end
        end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        ErrorResult ->
            ?LOG_ERROR(#{what => muc_set_nick_failed, reason => ErrorResult,
                         server => HostType, sub_host => MucHost,
                         from_jid => jid:to_binary(From), nick => Nick}),
            {error, ErrorResult}
    end.

unset_nick(HostType, MucHost, From) ->
    LUS = jid:to_lus(From),
    F = fun () ->
            mnesia:delete({muc_registered, {LUS, MucHost}})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            ok;
        ErrorResult ->
            ?LOG_ERROR(#{what => muc_unset_nick_failed, reason => ErrorResult,
                         server => HostType, sub_host => MucHost,
                         from_jid => jid:to_binary(From)}),
            {error, ErrorResult}
    end.
