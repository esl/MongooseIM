%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_db.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC light DB behaviour
%%% Created : 6 Oct 2015 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_muc_light_db_backend).
-author('piotr.nosek@erlang-solutions.com').

-define(MAIN_MODULE, mod_muc_light_db).

-include("mod_muc_light.hrl").

-type modify_aff_users_return() :: {ok, OldAffUsers :: aff_users(),
                                        NewAffUsers :: aff_users(),
                                        AffUsersChanged :: aff_users(),
                                        PrevVersion :: binary()}
                                 | {error, any()}.

-type remove_user_return() :: [{RoomUS :: jid:simple_bare_jid(),
                                modify_aff_users_return()}].

-export_type([modify_aff_users_return/0,
              remove_user_return/0]).

%% API
-export([start/2]).
-export([stop/1]).
-export([create_room/5]).
-export([destroy_room/2]).
-export([room_exists/2]).
-export([get_user_rooms/3]).
-export([get_user_rooms_count/2]).
-export([remove_user/3]).
-export([remove_domain/3]).
-export([get_config/2]).
-export([set_config/4]).
-export([get_blocking/3]).
-export([get_blocking/4]).
-export([set_blocking/4]).
-export([get_aff_users/2]).
-export([modify_aff_users/5]).
-export([get_info/2]).

%% For tests
-export([force_clear/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------
-callback start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback stop(mongooseim:host_type()) -> ok.

%% ------------------------ General room management ------------------------
-callback create_room(HostType :: mongooseim:host_type(),
                      RoomUS :: jid:simple_bare_jid(),
                      Config :: mod_muc_light_room_config:kv(),
                      AffUsers :: aff_users(),
                      Version :: binary()) ->
    {ok, FinalRoomUS :: jid:simple_bare_jid()} | {error, exists}.

-callback destroy_room(HostType :: mongooseim:host_type(),
                       RoomUS :: jid:simple_bare_jid()) ->
    ok | {error, not_exists}.

-callback room_exists(HostType :: mongooseim:host_type(),
                      RoomUS :: jid:simple_bare_jid()) ->
    boolean().

-callback get_user_rooms(HostType :: mongooseim:host_type(),
                         UserUS :: jid:simple_bare_jid(),
                         MUCServer :: jid:lserver() | undefined) ->
    [RoomUS :: jid:simple_bare_jid()].

-callback get_user_rooms_count(HostType :: mongooseim:host_type(),
                               UserUS :: jid:simple_bare_jid()) ->
    non_neg_integer().

-callback remove_user(HostType :: mongooseim:host_type(),
                      UserUS :: jid:simple_bare_jid(),
                      Version :: binary()) ->
    remove_user_return() | {error, term()}.

-callback remove_domain(mongooseim:host_type(), jid:lserver(), jid:lserver()) ->
    ok.

%% ------------------------ Configuration manipulation ------------------------
-callback get_config(HostType :: mongooseim:host_type(),
                     RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), Version :: binary()} | {error, not_exists}.

-callback set_config(HostType :: mongooseim:host_type(),
                     RoomUS :: jid:simple_bare_jid(),
                     Config :: mod_muc_light_room_config:kv(),
                     Version :: binary()) ->
    {ok, PrevVersion :: binary()} | {error, not_exists}.

%% ------------------------ Blocking manipulation ------------------------
-callback get_blocking(HostType :: mongooseim:host_type(),
                       UserUS :: jid:simple_bare_jid(),
                       MUCServer :: jid:lserver()) ->
    [blocking_item()].

-callback get_blocking(HostType :: mongooseim:host_type(),
                       UserUS :: jid:simple_bare_jid(),
                       MUCServer :: jid:lserver(),
                       WhatWhos :: [{blocking_what(), blocking_who()}]) ->
    blocking_action().

-callback set_blocking(HostType :: mongooseim:host_type(),
                       UserUS :: jid:simple_bare_jid(),
                       MUCServer :: jid:lserver(),
                       BlockingItems :: [blocking_item()]) ->
    ok.

%% ------------------------ Affiliations manipulation ------------------------
-callback get_aff_users(HostType :: mongooseim:host_type(),
                        RoomUS :: jid:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.

-callback modify_aff_users(HostType :: mongooseim:host_type(),
                           RoomUS :: jid:simple_bare_jid(),
                           AffUsersChanges :: aff_users(),
                           ExternalCheck :: external_check_fun(),
                           Version :: binary()) ->
    modify_aff_users_return().

%% ------------------------ Getting room configuration ------------------------
-callback get_info(HostType :: mongooseim:host_type(),
                   RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), aff_users(), Version :: binary()}
    | {error, not_exists}.

%% ------------------------ API for tests ------------------------
-callback force_clear() -> ok.

%%====================================================================
%% API
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    TrackedFuns = [create_room, destroy_room,
                   room_exists, get_user_rooms,
                   remove_user, remove_domain,
                   get_config, set_config,
                   get_blocking, set_blocking,
                   get_aff_users, modify_aff_users],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Args = [HostType],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec create_room(HostType :: mongooseim:host_type(),
                  RoomUS :: jid:simple_bare_jid(),
                  Config :: mod_muc_light_room_config:kv(),
                  AffUsers :: aff_users(),
                  Version :: binary()) ->
    {ok, FinalRoomUS :: jid:simple_bare_jid()} | {error, exists}.
create_room(HostType, RoomUS, Config, AffUsers, Version) ->
    Args = [HostType, RoomUS, Config, AffUsers, Version],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec destroy_room(HostType :: mongooseim:host_type(),
                   RoomUS :: jid:simple_bare_jid()) ->
    ok | {error, not_exists}.
destroy_room(HostType, RoomUS) ->
    Args = [HostType, RoomUS],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec room_exists(HostType :: mongooseim:host_type(),
                  RoomUS :: jid:simple_bare_jid()) -> boolean().
room_exists(HostType, RoomUS) ->
    Args = [HostType, RoomUS],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_user_rooms(HostType :: mongooseim:host_type(),
                     UserUS :: jid:simple_bare_jid(),
                     MUCServer :: jid:lserver() | undefined) ->
    [RoomUS :: jid:simple_bare_jid()].
get_user_rooms(HostType, UserUS, MUCServer) ->
    Args = [HostType, UserUS, MUCServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_user_rooms_count(HostType :: mongooseim:host_type(),
                           UserUS :: jid:simple_bare_jid()) ->
    non_neg_integer().
get_user_rooms_count(HostType, UserUS) ->
    Args = [HostType, UserUS],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(HostType :: mongooseim:host_type(),
                  UserUS :: jid:simple_bare_jid(),
                  Version :: binary()) ->
    remove_user_return() | {error, term()}.
remove_user(HostType, UserUS, Version) ->
    Args = [HostType, UserUS, Version],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mongooseim:host_type(), jid:lserver(), jid:lserver()) ->
    ok.
remove_domain(HostType, RoomS, LServer) ->
    Args = [HostType, RoomS, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_config(HostType :: mongooseim:host_type(),
                 RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), Version :: binary()} | {error, not_exists}.
get_config(HostType, RoomUS) ->
    Args = [HostType, RoomUS],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_config(HostType :: mongooseim:host_type(),
                 RoomUS :: jid:simple_bare_jid(),
                 Config :: mod_muc_light_room_config:kv(),
                 Version :: binary()) -> {ok, PrevVersion :: binary()} | {error, not_exists}.
set_config(HostType, RoomUS, ConfigChanges, Version) ->
    Args = [HostType, RoomUS, ConfigChanges, Version],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_blocking(HostType :: mongooseim:host_type(),
                   UserUS :: jid:simple_bare_jid(), MUCServer :: jid:lserver()) ->
    [blocking_item()].
get_blocking(HostType, RoomUS, MUCServer) ->
    Args = [HostType, RoomUS, MUCServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_blocking(HostType :: mongooseim:host_type(),
                   UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   WhatWhos :: [{blocking_what(), blocking_who()}]) ->
    blocking_action().
get_blocking(HostType, RoomUS, MUCServer, WhatWhos) ->
    Args = [HostType, RoomUS, MUCServer, WhatWhos],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_blocking(HostType :: mongooseim:host_type(),
                   UserUS :: jid:simple_bare_jid(),
                   MUCServer :: jid:lserver(),
                   BlockingItems :: [blocking_item()]) -> ok.
set_blocking(HostType, UserUS, MUCServer, BlockingItems) ->
    Args = [HostType, UserUS, MUCServer, BlockingItems],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_aff_users(HostType :: mongooseim:host_type(),
                    RoomUS :: jid:simple_bare_jid()) ->
    {ok, aff_users(), Version :: binary()} | {error, not_exists}.
get_aff_users(HostType, RoomUS) ->
    Args = [HostType, RoomUS],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec modify_aff_users(HostType :: mongooseim:host_type(),
                       RoomUS :: jid:simple_bare_jid(),
                       AffUsersChanges :: aff_users(),
                       ExternalCheck :: external_check_fun(),
                       Version :: binary()) ->
    modify_aff_users_return().
modify_aff_users(HostType, RoomUS, AffUsersChanges, ExternalCheck, Version) ->
    Args = [HostType, RoomUS, AffUsersChanges, ExternalCheck, Version],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_info(HostType :: mongooseim:host_type(),
               RoomUS :: jid:simple_bare_jid()) ->
    {ok, mod_muc_light_room_config:kv(), aff_users(), Version :: binary()}
    | {error, not_exists}.
get_info(HostType, RoomUS) ->
    Args = [HostType, RoomUS],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec force_clear(HostType :: mongooseim:host_type()) -> ok.
force_clear(HostType) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, []).
