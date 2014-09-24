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

         get_configuration/1,
         get_configuration/2,
         set_configuration/2,
         set_configuration/3,

         get_room_process/1,
         register_room_process/2,
         unregister_room_process/1,

         get_affiliations/1,
         modify_affiliations/2
        ]).

%% Extra API for testing
-export([
         force_destroy_room/1
        ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-define(TAB, muc_light_room).

-record(?TAB, {
          room_us :: {ejabberd:username(), ejabberd:server()},
          config :: [{atom(), term()}],
          affiliations :: affiliations()
         }).

%%====================================================================
%% API
%%====================================================================

-spec start(ejabberd:server(), ejabberd:server()) -> ok.
start(_Host, _MUCHost) ->
    init_tables().

-spec stop(ejabberd:server(), ejabberd:server()) -> ok.
stop(_Host, _MUCHost) ->
    ok.


-spec create_room(#jid{}, jid() | ljid(), configuration()) -> ok | {error, exists}.
create_room(RoomJID, Owner, Configuration) ->
    {atomic, Res} = mnesia:transaction(
                     fun create_room_transaction/3,
                     [RoomJID, Owner, Configuration]),
    Res.

-spec destroy_room(#jid{}) -> ok | {error, not_exists | not_empty}.
destroy_room(RoomJID) ->
    {atomic, Res} = mnesia:transaction(
                     fun destroy_room_transaction/1,
                     [RoomJID]),
    Res.

-spec room_exists(#jid{}) -> boolean().
room_exists(RoomJID) ->
    mnesia:dirty_read(?TAB, to_us(RoomJID)) =/= [].


-spec get_configuration(#jid{}) -> {ok, configuration()} | {error, not_exists}.
get_configuration(RoomJID) ->
    case mnesia:dirty_read(?TAB, to_us(RoomJID)) of
        [] ->
            {error, not_exists};
        [#?TAB{ config = Config }] ->
            {ok, Config}
    end.

-spec get_configuration(#jid{}, atom()) ->
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

-spec set_configuration(#jid{}, configuration()) -> ok | {error, not_exists}.
set_configuration(RoomJID, ConfigurationChanges) ->
    {atomic, Res} = mnesia:transaction(
                     fun set_configuration_transaction/2,
                     [RoomJID, ConfigurationChanges]),
    Res.

-spec set_configuration(#jid{}, atom(), term()) -> ok | {error, not_exists}.
set_configuration(RoomJID, Option, Value) ->
    set_configuration(RoomJID, [{Option, Value}]).


-spec get_room_process(#jid{}) -> pid() | {error, not_exists}.
get_room_process(_RoomJID) ->
    throw(not_implemented).

-spec register_room_process(#jid{}, pid()) -> ok | {error, exists}.
register_room_process(_RoomJID, _Pid) ->
    throw(not_implemented).

-spec unregister_room_process(#jid{}) -> ok.
unregister_room_process(_RoomJID) ->
    throw(not_implemented).


-spec get_affiliations(#jid{}) -> {ok, affiliations()} | {error, not_exists}.
get_affiliations(RoomJID) ->
    case mnesia:dirty_read(?TAB, to_us(RoomJID)) of
        [] ->
            {error, not_exists};
        [#?TAB{ affiliations = Affiliations }] ->
            {ok, Affiliations}
    end.

-spec modify_affiliations(#jid{}, affiliations()) ->
    {ok, CurrentAffiliations :: affiliations(), ChangedAffiliations :: affiliations()}
    | {error, not_exists | only_owner_in_room}.
modify_affiliations(RoomJID, AffiliationsToChange) ->
    {atomic, Res} = mnesia:transaction(
                      fun modify_affiliations_transaction/2,
                      [RoomJID, AffiliationsToChange]),
    Res.

%%====================================================================
%% API for tests
%%====================================================================

-spec force_destroy_room(jid() | ljid()) -> ok.
force_destroy_room(RoomJID) ->
    mnesia:dirty_delete(?TAB, to_us(RoomJID)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec init_tables() -> ok.
init_tables() ->
    mnesia:create_table(?TAB,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, ?TAB)}]),
    mnesia:add_table_copy(?TAB, node(), disc_copies),
    set = mnesia:table_info(schema, type),
    ok. % checks if table exists

-spec to_us(#jid{}) -> {LUSer :: binary(), LServer :: binary()}.
to_us(#jid{ luser = LUser, lserver = LServer }) -> {LUser, LServer}.

-spec create_room_transaction(#jid{}, jid() | ljid(), configuration()) ->
    ok | {error, exists}.
create_room_transaction(RoomJID, Owner, Configuration) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread({?TAB, RoomUS}) of
        [_] ->
            {error, exists};
        [] ->
            RoomRecord = #?TAB{
                             room_us = RoomUS,
                             config = Configuration,
                             affiliations = [{Owner, owner}]
                            },
            mnesia:write(RoomRecord),
            ok
    end.

-spec destroy_room_transaction(#jid{}) -> ok | {error, not_exists | not_empty}.
destroy_room_transaction(RoomJID) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread({?TAB, RoomUS}) of
        [] -> {error, not_exists};
        [#?TAB{ affiliations = [] }] -> mnesia:delete({?TAB, RoomUS});
        _ -> {error, not_empty}
    end.

-spec set_configuration_transaction(#jid{}, configuration()) ->
    ok | {error, not_exists}.
set_configuration_transaction(RoomJID, ConfigurationChanges) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread({?TAB, RoomUS}) of
        [] ->
            {error, not_exists};
        [#?TAB{ config = Config } = Rec] ->
            NewConfig = lists:foldl(
                          fun({Key, Val}, ConfigAcc) ->
                                  lists:keystore(Key, 1, ConfigAcc,
                                                 {Key, Val})
                          end, Config, ConfigurationChanges),
            mnesia:write(Rec#?TAB{ config = NewConfig })
    end.

-spec modify_affiliations_transaction(jid(), affiliations()) ->
    {ok, NewAffiliations :: affiliations(),
     AffiliationsChanged :: affiliations()} | {error, only_owner_in_room}.
modify_affiliations_transaction(RoomJID, AffiliationsToChange) ->
    RoomUS = to_us(RoomJID),
    case mnesia:wread({?TAB, RoomUS}) of
        [] ->
            {error, not_exists};
        [#?TAB{ affiliations = Affiliations } = Rec] ->
            case mod_muc_light_utils:change_affiliations(
                   Affiliations, AffiliationsToChange) of
                {ok, NewAffiliations, AffiliationsChanged} ->
                    mnesia:write(Rec#?TAB{ affiliations = NewAffiliations }),
                    {ok, NewAffiliations, AffiliationsChanged};
                Error ->
                    Error
            end
    end.
