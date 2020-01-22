%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_room.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Room logic for mod_muc_light
%%% Created : 9 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
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

-module(mod_muc_light_room).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([handle_request/4, maybe_forget/2, process_request/4]).

%% Callbacks
-export([participant_limit_check/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-type packet_processing_result() :: muc_light_encode_request() | {error, Reason :: term()}.

%%====================================================================
%% API
%%====================================================================

-spec handle_request(From :: jid:jid(), RoomJID :: jid:jid(),
                     OrigPacket :: exml:element(), Request :: muc_light_packet()) -> ok.
handle_request(From, To, OrigPacket, Request) ->
    RoomUS = jid:to_lus(To),
    AffUsersRes = mod_muc_light_db_backend:get_aff_users(RoomUS),
    Response = process_request(From, RoomUS, Request, AffUsersRes),
    send_response(From, To, RoomUS, OrigPacket, Response).

-spec maybe_forget(RoomUS :: jid:simple_bare_jid(), NewAffUsers :: aff_users()) -> any().
maybe_forget({RoomU, RoomS} = RoomUS, []) ->
    ejabberd_hooks:run(forget_room, RoomS, [RoomS, RoomU]),
    mod_muc_light_db_backend:destroy_room(RoomUS);
maybe_forget(_, _) ->
    my_room_will_go_on.

%%====================================================================
%% Callbacks
%%====================================================================

-spec participant_limit_check(RoomUS :: jid:simple_bare_jid(),
                              NewAffUsers :: aff_users()) ->
    ok | {error, occupant_limit_exceeded}.
participant_limit_check({_, MUCServer} = _RoomUS, NewAffUsers) ->
    MaxOccupants = gen_mod:get_module_opt_by_subhost(
                     MUCServer, mod_muc_light, max_occupants, ?DEFAULT_MAX_OCCUPANTS),
    case length(NewAffUsers) > MaxOccupants of
        true -> {error, occupant_limit_exceeded};
        false -> ok
    end.

%%====================================================================
%% Packet handling
%%====================================================================

-spec process_request(From :: jid:jid(),
                      RoomUS :: jid:simple_bare_jid(),
                      Request :: muc_light_packet(),
                      AffUsersRes :: {ok, aff_users(), binary()} | {error, term()}) ->
    packet_processing_result().
process_request(_From, _RoomUS, _Request, {error, _} = Error) ->
    Error;
process_request(From, RoomUS, Request, {ok, AffUsers, _Ver}) ->
    UserUS = jid:to_lus(From),
    Auth = lists:keyfind(UserUS, 1, AffUsers),
    process_request(Request, From, UserUS, RoomUS, Auth, AffUsers).

-spec process_request(Request :: muc_light_packet(),
                      From ::jid:jid(),
                      UserUS :: jid:simple_bare_jid(),
                      RoomUS :: jid:simple_bare_jid(),
                      Auth :: false | aff_user(),
                      AffUsers :: aff_users()) ->
    packet_processing_result().
process_request(_Request, _From, _UserUS, _RoomUS, false, _AffUsers) ->
    {error, item_not_found};
process_request(#msg{} = Msg, _From, _UserUS, _RoomUS, _Auth, AffUsers) ->
    {Msg, AffUsers};
process_request({get, #config{} = ConfigReq}, _From, _UserUS, RoomUS, _Auth, _AffUsers) ->
    {_, RoomS} = RoomUS,
    {ok, Config, RoomVersion} = mod_muc_light_db_backend:get_config(RoomUS),
    RawConfig = mod_muc_light_room_config:to_binary_kv(Config, mod_muc_light:config_schema(RoomS)),
    {get, ConfigReq#config{ version = RoomVersion,
                            raw_config = RawConfig }};
process_request({get, #affiliations{} = AffReq}, _From, _UserUS, RoomUS, _Auth, _AffUsers) ->
    {ok, AffUsers, RoomVersion} = mod_muc_light_db_backend:get_aff_users(RoomUS),
    {get, AffReq#affiliations{ version = RoomVersion,
                               aff_users = AffUsers }};
process_request({get, #info{} = InfoReq}, _From, _UserUS, {_, RoomS} = RoomUS, _Auth, _AffUsers) ->
    {ok, Config, AffUsers, RoomVersion} = mod_muc_light_db_backend:get_info(RoomUS),
    RawConfig = mod_muc_light_room_config:to_binary_kv(Config, mod_muc_light:config_schema(RoomS)),
    {get, InfoReq#info{ version = RoomVersion, aff_users = AffUsers,
                        raw_config = RawConfig }};
process_request({set, #config{} = ConfigReq}, _From, _UserUS, {_, MUCServer} = RoomUS,
                {_, UserAff}, AffUsers) ->
    AllCanConfigure = gen_mod:get_module_opt_by_subhost(
                        MUCServer, mod_muc_light, all_can_configure, ?DEFAULT_ALL_CAN_CONFIGURE),
    process_config_set(ConfigReq, RoomUS, UserAff, AffUsers, AllCanConfigure);
process_request({set, #affiliations{} = AffReq}, _From, UserUS, {_, MUCServer} = RoomUS,
                {_, UserAff}, AffUsers) ->
    OwnerUS = case lists:keyfind(owner, 2, AffUsers) of
                  false -> undefined;
                  {OwnerUS0, _} -> OwnerUS0
              end,
    ValidateResult
    = case UserAff of
          owner ->
              {ok, mod_muc_light_utils:filter_out_prevented(
                     UserUS, RoomUS, AffReq#affiliations.aff_users)};
          member ->
              AllCanInvite = gen_mod:get_module_opt_by_subhost(
                               MUCServer, mod_muc_light, all_can_invite, ?DEFAULT_ALL_CAN_INVITE),
              validate_aff_changes_by_member(
                AffReq#affiliations.aff_users, [], UserUS, OwnerUS, RoomUS, AllCanInvite)
      end,
    process_aff_set(AffReq, RoomUS, ValidateResult);
process_request({set, #destroy{} = DestroyReq}, _From, _UserUS, RoomUS, {_, owner}, AffUsers) ->
    ok = mod_muc_light_db_backend:destroy_room(RoomUS),
    maybe_forget(RoomUS, []),
    {set, DestroyReq, AffUsers};
process_request({set, #destroy{}}, _From, _UserUS, _RoomUS, _Auth, _AffUsers) ->
    {error, not_allowed};
process_request(_UnknownReq, _From, _UserUS, _RoomUS, _Auth, _AffUsers) ->
    {error, bad_request}.

%% --------- Config set ---------

-spec process_config_set(ConfigReq :: config_req_props(),
                         RoomUS :: jid:simple_bare_jid(),
                         UserAff :: member | owner, AffUsers :: aff_users(),
                         UserAllowedToConfigure :: boolean()) ->
    {set, config_req_props(), aff_users()} | {error, not_allowed} | validation_error().
process_config_set(#config{ raw_config = [{<<"subject">>, _}] } = ConfigReq, RoomUS, UserAff,
                   AffUsers, false) ->
    % Everyone is allowed to change subject
    process_config_set(ConfigReq, RoomUS, UserAff, AffUsers, true);
process_config_set(_ConfigReq, _RoomUS, member, _AffUsers, false) ->
    {error, not_allowed};
process_config_set(ConfigReq, {_, RoomS} = RoomUS, _UserAff, AffUsers, _AllCanConfigure) ->
    case mod_muc_light_room_config:apply_binary_kv(
           ConfigReq#config.raw_config, [], mod_muc_light:config_schema(RoomS)) of
        {ok, Config} ->
            NewVersion = mongoose_bin:gen_from_timestamp(),
            {ok, PrevVersion} = mod_muc_light_db_backend:set_config(RoomUS, Config, NewVersion),
            {set, ConfigReq#config{ prev_version = PrevVersion, version = NewVersion }, AffUsers};
        Error ->
            Error
    end.

%% --------- Affiliation set ---------

%% Member can only add new members or leave
-spec validate_aff_changes_by_member(AffUsersChanges :: aff_users(),
                                   AffUsersChangesAcc :: aff_users(),
                                   UserUS :: jid:simple_bare_jid(),
                                   OwnerUS :: jid:simple_bare_jid(),
                                   RoomUS :: jid:simple_bare_jid(),
                                   AllCanInvite :: boolean()) ->
    {ok, aff_users()} | {error, not_allowed}.
validate_aff_changes_by_member([], Acc, _UserUS, _OwnerUS, _RoomUS, _AllCanInvite) ->
    {ok, Acc};
validate_aff_changes_by_member([{UserUS, none} | RAffUsersChanges], Acc, UserUS, OwnerUS,
                               RoomUS, AllCanInvite) ->
    validate_aff_changes_by_member(RAffUsersChanges, [{UserUS, none} | Acc], UserUS, OwnerUS,
                                   RoomUS, AllCanInvite);
validate_aff_changes_by_member([{OwnerUS, _} | _RAffUsersChanges], _Acc, _UserUS, OwnerUS,
                               _RoomUS, _AllCanInvite) ->
    {error, not_allowed};
validate_aff_changes_by_member([{_, member} = AffUserChange | RAffUsersChanges], Acc, UserUS,
                               OwnerUS, RoomUS, true) ->
    validate_aff_changes_by_member(
      RAffUsersChanges, [AffUserChange | Acc], UserUS, OwnerUS, RoomUS, true);
validate_aff_changes_by_member(_AffUsersChanges, _Acc, _UserUS, _OwnerUS, _RoomUS, _AllCanInvite) ->
    {error, not_allowed}.

-spec process_aff_set(AffReq :: affiliations_req_props(),
                      RoomUS :: jid:simple_bare_jid(),
                      ValidateResult :: {ok, aff_users()} | {error, not_allowed}) ->
    {set, affiliations_req_props(), OldAffUsers :: aff_users(), NewAffUsers :: aff_users()}
    | {error, not_allowed}.
process_aff_set(AffReq, _RoomUS, {ok, []}) -> % It seems that all users blocked this request
    {set, AffReq, [], []}; % Just return result to the user, don't change or broadcast anything
process_aff_set(AffReq, RoomUS, {ok, FilteredAffUsers}) ->
    NewVersion = mongoose_bin:gen_from_timestamp(),
    case mod_muc_light_db_backend:modify_aff_users(RoomUS, FilteredAffUsers,
                                   fun ?MODULE:participant_limit_check/2, NewVersion) of
        {ok, OldAffUsers, NewAffUsers, AffUsersChanged, OldVersion} ->
            maybe_forget(RoomUS, NewAffUsers),
            {set, AffReq#affiliations{
                    prev_version = OldVersion,
                    version = NewVersion,
                    aff_users = AffUsersChanged
                   }, OldAffUsers, NewAffUsers};
        Error ->
            Error
    end;
process_aff_set(_AffReq, _RoomUS, Error) ->
    Error.

%%====================================================================
%% Response processing
%%====================================================================

-spec send_response(From :: jid:jid(), RoomJID :: jid:jid(),
                    RoomUS :: jid:simple_bare_jid(), OrigPacket :: exml:element(),
                    Result :: packet_processing_result()) -> ok.
send_response(From, RoomJID, _RoomUS, OrigPacket, {error, _} = Err) ->
    mod_muc_light_codec_backend:encode_error(
      Err, From, RoomJID, OrigPacket, fun ejabberd_router:route/3);
send_response(From, _RoomJID, RoomUS, _OriginalPacket, Response) ->
    mod_muc_light_codec_backend:encode(Response, From, RoomUS, fun ejabberd_router:route/3).

%%====================================================================
%% Internal functions
%%====================================================================

