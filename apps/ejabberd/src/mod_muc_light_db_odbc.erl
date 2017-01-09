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

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

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
    {selected, _, Res} = ejabberd_odbc:sql_query(
                           MainHost, mod_muc_light_db_odbc_sql:select_room_id(RoomU, RoomS)),
    Res /= [].

-spec get_user_rooms(UserUS :: ejabberd:simple_bare_jid(),
                     MUCServer :: ejabberd:lserver() | undefined) ->
    [RoomUS :: ejabberd:simple_bare_jid()].
get_user_rooms({LUser, LServer}, undefined) ->
    SQL = mod_muc_light_db_odbc_sql:select_user_rooms(LUser, LServer),
    lists:usort(lists:flatmap(
                  fun(Host) ->
                          {selected, _, Rooms} = ejabberd_odbc:sql_query(Host, SQL),
                          Rooms
                  end, ?MYHOSTS));
get_user_rooms({LUser, LServer}, MUCServer) ->
    MainHost = main_host(MUCServer),
    {selected, _, Rooms} = ejabberd_odbc:sql_query(
                             MainHost, mod_muc_light_db_odbc_sql:select_user_rooms(LUser, LServer)),
    Rooms.

-spec get_user_rooms_count(UserUS :: ejabberd:simple_bare_jid(),
                           MUCServer :: ejabberd:lserver()) ->
    non_neg_integer().
get_user_rooms_count({LUser, LServer}, MUCServer) ->
    MainHost = main_host(MUCServer),
    {selected, _, [{Cnt}]}
    = ejabberd_odbc:sql_query(
        MainHost, mod_muc_light_db_odbc_sql:select_user_rooms_count(LUser, LServer)),
    ejabberd_odbc:result_to_integer(Cnt).

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

    SQL = mod_muc_light_db_odbc_sql:select_config(RoomU, RoomS),
    {selected, _ColNames, Result} = ejabberd_odbc:sql_query(MainHost, SQL),

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

    SQL = mod_muc_light_db_odbc_sql:select_config(RoomU, RoomS, KeyDB),
    {selected, _ColNames, Result} = ejabberd_odbc:sql_query(MainHost, SQL),

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
    SQL = mod_muc_light_db_odbc_sql:select_blocking(LUser, LServer),
    {selected, _, WhatWhos} = ejabberd_odbc:sql_query(MainHost, SQL),
    [ {what_db2atom(What), deny, jid:to_lus(jid:from_binary(Who))} || {What, Who} <- WhatWhos ].

-spec get_blocking(UserUS :: ejabberd:simple_bare_jid(),
                   MUCServer :: ejabberd:lserver(),
                   WhatWhos :: [{blocking_who(), ejabberd:simple_bare_jid()}]) ->
    blocking_action().
get_blocking({LUser, LServer}, MUCServer, WhatWhos) ->
    MainHost = main_host(MUCServer),
    SQL = mod_muc_light_db_odbc_sql:select_blocking_cnt(LUser, LServer, WhatWhos),
    {selected, _, [{Count}]} = ejabberd_odbc:sql_query(MainHost, SQL),
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
    {updated, _} =
    ejabberd_odbc:sql_query(
      main_host(MUCServer), mod_muc_light_db_odbc_sql:insert_blocking(LUser, LServer, What, Who)),
    set_blocking(UserUS, MUCServer, RBlockingItems);
set_blocking({LUser, LServer} = UserUS, MUCServer, [{What, allow, Who} | RBlockingItems]) ->
    {updated, _} =
    ejabberd_odbc:sql_query(
      main_host(MUCServer), mod_muc_light_db_odbc_sql:delete_blocking(LUser, LServer, What, Who)),
    set_blocking(UserUS, MUCServer, RBlockingItems).

%% ------------------------ Affiliations manipulation ------------------------

-spec get_aff_users(RoomUS :: ejabberd:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.
get_aff_users({RoomU, RoomS} = RoomUS) ->
    MainHost = main_host(RoomUS),
    case ejabberd_odbc:sql_query(MainHost, mod_muc_light_db_odbc_sql:select_affs(RoomU, RoomS)) of
        {selected, _, []} ->
            {error, not_exists};
        {selected, _, [{Version, null, null, null}]} ->
            {ok, [], Version};
        {selected, _, [{Version, _, _, _} | _] = Res} ->
            AffUsers = [{{UserU, UserS}, aff_db2atom(Aff)} || {_, UserU, UserS, Aff} <- Res],
            {ok, lists:sort(AffUsers), Version}
    end.

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
    case ejabberd_odbc:sql_query(
           MainHost, mod_muc_light_db_odbc_sql:select_room_id_and_version(RoomU, RoomS)) of
        {selected, _, [{RoomID, Version}]} ->
            {selected, _, AffUsersDB} = ejabberd_odbc:sql_query(
                                          MainHost, mod_muc_light_db_odbc_sql:select_affs(RoomID)),
            AffUsers = [{{UserU, UserS}, aff_db2atom(Aff)} || {UserU, UserS, Aff} <- AffUsersDB],

            {selected, _, ConfigDB} = ejabberd_odbc:sql_query(
                                        MainHost, mod_muc_light_db_odbc_sql:select_config(RoomID)),
            {ok, Config} = mod_muc_light_utils:process_raw_config(
                             ConfigDB, [], mod_muc_light:config_schema(RoomS)),

            {ok, Config, lists:sort(AffUsers), Version};
        {selected, _, []} ->
            {error, not_exists}
    end.

%% ------------------------ Conversions ------------------------

-spec what_db2atom(binary() | pos_integer()) -> blocking_what().
what_db2atom(1) -> room;
what_db2atom(2) -> user;
what_db2atom(Bin) -> what_db2atom(ejabberd_odbc:result_to_integer(Bin)).

-spec what_atom2db(blocking_what()) -> string().
what_atom2db(room) -> "1";
what_atom2db(user) -> "2".

-spec aff_atom2db(aff()) -> string().
aff_atom2db(owner) -> "1";
aff_atom2db(member) -> "2".

-spec aff_db2atom(binary() | pos_integer()) -> aff().
aff_db2atom(1) -> owner;
aff_db2atom(2) -> member;
aff_db2atom(Bin) -> aff_db2atom(ejabberd_odbc:result_to_integer(Bin)).

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
    case catch ejabberd_odbc:sql_query_t(
                 mod_muc_light_db_odbc_sql:insert_room(RoomU, RoomS, Version)) of
        {aborted, Reason} ->
            case {ejabberd_odbc:sql_query_t(
                    mod_muc_light_db_odbc_sql:select_room_id(RoomU, RoomS)), NodeCandidate} of
                {{selected, _, []}, _} ->
                    throw({aborted, Reason});
                {{selected, _, [_]}, <<>>} ->
                    create_room_transaction({<<>>, RoomS}, Config, AffUsers, Version);
                {{selected, _, [_]}, _} ->
                    {error, exists}
            end;
        {updated, _} ->
            {selected, _, [{RoomID}]} = ejabberd_odbc:sql_query_t(
                                          mod_muc_light_db_odbc_sql:select_room_id(RoomU, RoomS)),
            lists:foreach(
              fun({{UserU, UserS}, Aff}) ->
                      {updated, _} = ejabberd_odbc:sql_query_t(
                                       mod_muc_light_db_odbc_sql:insert_aff(
                                         RoomID, UserU, UserS, Aff))
              end, AffUsers),
            lists:foreach(
              fun({Key, Val}) ->
                      {updated, _} = ejabberd_odbc:sql_query_t(
                                       mod_muc_light_db_odbc_sql:insert_config(RoomID, Key, Val))
              end, mod_muc_light_utils:config_to_raw(Config, mod_muc_light:config_schema(RoomS))),
            {ok, {RoomU, RoomS}}
    end.

-spec destroy_room_transaction(RoomUS :: ejabberd:simple_bare_jid()) -> ok | {error, not_exists}.
destroy_room_transaction({RoomU, RoomS}) ->
    case ejabberd_odbc:sql_query_t(mod_muc_light_db_odbc_sql:select_room_id(RoomU, RoomS)) of
        {selected, _, [{RoomID}]} ->
            {updated, _} = ejabberd_odbc:sql_query_t(
                             mod_muc_light_db_odbc_sql:delete_affs(RoomID)),
            {updated, _} = ejabberd_odbc:sql_query_t(
                             mod_muc_light_db_odbc_sql:delete_config(RoomID)),
            {updated, _} = ejabberd_odbc:sql_query_t(
                             mod_muc_light_db_odbc_sql:delete_room(RoomU, RoomS)),
            ok;
        {selected, _, []} ->
            {error, not_exists}
    end.

-spec remove_user_transaction(UserUS :: ejabberd:simple_bare_jid(), Version :: binary()) ->
    mod_muc_light_db:remove_user_return().
remove_user_transaction({UserU, UserS} = UserUS, Version) ->
    Rooms = get_user_rooms(UserUS, undefined),
    {updated, _} = ejabberd_odbc:sql_query_t(
                     mod_muc_light_db_odbc_sql:delete_blocking(UserU, UserS)),
    lists:map(
      fun(RoomUS) ->
              {RoomUS, modify_aff_users_transaction(
                         RoomUS, [{UserUS, none}], fun(_, _) -> ok end, Version)}
      end, Rooms).

%% ------------------------ Configuration manipulation ------------------------

-spec set_config_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                             ConfigChanges :: config(),
                             Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config_transaction({RoomU, RoomS} = RoomUS, ConfigChanges, Version) ->
    MainHost = main_host(RoomUS),
    case ejabberd_odbc:sql_query_t(
           mod_muc_light_db_odbc_sql:select_room_id_and_version(RoomU, RoomS)) of
        {selected, _, [{RoomID, PrevVersion}]} ->
            {updated, _} = ejabberd_odbc:sql_query_t(
                             mod_muc_light_db_odbc_sql:update_room_version(RoomU, RoomS, Version)),
            lists:foreach(
              fun({Key, Val}) ->
                      {updated, _}
                      = ejabberd_odbc:sql_query(
                          MainHost, mod_muc_light_db_odbc_sql:update_config(RoomID, Key, Val))
              end, mod_muc_light_utils:config_to_raw(
                     ConfigChanges, mod_muc_light:config_schema(RoomS))),
            {ok, PrevVersion};
        {selected, _, []} ->
            {error, not_exists}
    end.

%% ------------------------ Blocking manipulation ------------------------

%% ------------------------ Affiliations manipulation ------------------------

-spec modify_aff_users_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                                   AffUsersChanges :: aff_users(),
                                   CheckFun :: external_check_fun(),
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users_transaction({RoomU, RoomS} = RoomUS, AffUsersChanges, CheckFun, Version) ->
    case ejabberd_odbc:sql_query_t(
           mod_muc_light_db_odbc_sql:select_room_id_and_version(RoomU, RoomS)) of
        {selected, _, [{RoomID, PrevVersion}]} ->
            modify_aff_users_transaction(
              RoomUS, RoomID, AffUsersChanges, CheckFun, PrevVersion, Version);
        {selected, _, []} ->
            {error, not_exists}
    end.

-spec modify_aff_users_transaction(RoomUS :: ejabberd:simple_bare_jid(),
                                   RoomID :: binary(),
                                   AffUsersChanges :: aff_users(),
                                   CheckFun :: external_check_fun(),
                                   PrevVersion :: binary(),
                                   Version :: binary()) ->
    mod_muc_light_db:modify_aff_users_return().
modify_aff_users_transaction(RoomUS, RoomID, AffUsersChanges, CheckFun, PrevVersion, Version) ->
    {selected, _, AffUsersDB}
    = ejabberd_odbc:sql_query_t(mod_muc_light_db_odbc_sql:select_affs(RoomID)),
    AffUsers = lists:sort(
                 [{{UserU, UserS}, aff_db2atom(Aff)} || {UserU, UserS, Aff} <- AffUsersDB]),
    case mod_muc_light_utils:change_aff_users(AffUsers, AffUsersChanges) of
        {ok, NewAffUsers, AffUsersChanged, JoiningUsers, _LeavingUsers} ->
            case CheckFun(RoomUS, NewAffUsers) of
                ok ->
                    apply_aff_users_transaction(RoomID, AffUsersChanged, JoiningUsers),
                    update_room_version_transaction(RoomUS, Version),
                    {ok, AffUsers, NewAffUsers, AffUsersChanged, PrevVersion};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec apply_aff_users_transaction(RoomID :: binary(),
                                  AffUsersChanges :: aff_users(),
                                  JoiningUsers :: [ejabberd:simple_bare_jid()]) -> ok.
apply_aff_users_transaction(RoomID, AffUsersChanged, JoiningUsers) ->
    lists:foreach(
      fun({{UserU, UserS}, none}) ->
              {updated, _} = ejabberd_odbc:sql_query_t(
                               mod_muc_light_db_odbc_sql:delete_aff(RoomID, UserU, UserS));
         ({{UserU, UserS} = UserUS, Aff}) ->
              case lists:member(UserUS, JoiningUsers) of
                  true ->
                      {updated, _} = ejabberd_odbc:sql_query_t(
                                       mod_muc_light_db_odbc_sql:insert_aff(
                                         RoomID, UserU, UserS, Aff));
                  false ->
                      {updated, _} = ejabberd_odbc:sql_query_t(
                                       mod_muc_light_db_odbc_sql:update_aff(
                                         RoomID, UserU, UserS, Aff))
              end
      end, AffUsersChanged).

-spec update_room_version_transaction(RoomUS :: ejabberd:simple_bare_jid(), Version :: binary()) ->
    {updated, integer()}.
update_room_version_transaction({RoomU, RoomS}, Version) ->
    {updated, _} = ejabberd_odbc:sql_query_t(
                     mod_muc_light_db_odbc_sql:update_room_version(RoomU, RoomS, Version)).

%% ------------------------ Common ------------------------

-spec main_host(JIDOrServer :: ejabberd:simple_bare_jid() | binary()) -> ejabberd:lserver().
main_host({_, RoomS}) ->
    main_host(RoomS);
main_host(MUCServer) ->
    {ok, MainHost} = mongoose_subhosts:get_host(MUCServer),
    MainHost.

