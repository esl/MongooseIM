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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_room).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([handle_request/5, maybe_forget/4, process_request/5]).

%% Callbacks
-export([participant_limit_check/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-type packet_processing_result() :: muc_light_encode_request() | {error, Reason :: term()}.

%%====================================================================
%% API
%%====================================================================

-spec handle_request(From :: jid:jid(), RoomJID :: jid:jid(), OrigPacket :: exml:element(),
                     Request :: muc_light_packet(), Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_request(From, RoomJID, OrigPacket, Request, Acc1) ->
    {Acc2, AffUsersRes} = mod_muc_light:get_acc_room_affiliations(Acc1, RoomJID),
    Response = process_request(From, RoomJID, Request, AffUsersRes, Acc2),
    send_response(From, RoomJID, OrigPacket, Response, Acc2).

-spec maybe_forget(Acc :: mongoose_acc:t(),
                   RoomUS :: jid:simple_bare_jid(),
                   NewAffUsers :: aff_users(),
                   Version :: binary() ) -> any().
maybe_forget(Acc, {RoomU, RoomS} = RoomUS, [], _Version) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    mongoose_hooks:forget_room(HostType, RoomS, RoomU),
    mod_muc_light_db_backend:destroy_room(HostType, RoomUS);
maybe_forget(Acc, {RoomU, RoomS}, NewAffs, Version) ->
    RoomJid = jid:make_noprep(RoomU, RoomS, <<>>),
    mongoose_hooks:room_new_affiliations(Acc, RoomJid, NewAffs, Version),
    my_room_will_go_on.

%%====================================================================
%% Callbacks
%%====================================================================

-spec participant_limit_check(RoomUS :: jid:simple_bare_jid(),
                              NewAffUsers :: aff_users()) ->
    ok | {error, occupant_limit_exceeded}.
participant_limit_check({_, MUCServer} = _RoomUS, NewAffUsers) ->
    HostType = mod_muc_light_utils:muc_host_to_host_type(MUCServer),
    MaxOccupants = gen_mod:get_module_opt(HostType, mod_muc_light, max_occupants),
    case length(NewAffUsers) > MaxOccupants of
        true -> {error, occupant_limit_exceeded};
        false -> ok
    end.

%%====================================================================
%% Packet handling
%%====================================================================

-spec process_request(From :: jid:jid(),
                      RoomBareJid :: jid:jid(),
                      Request :: muc_light_packet(),
                      AffUsersRes :: {ok, aff_users(), binary()} | {error, term()},
                      Acc :: mongoose_acc:t()) ->
    packet_processing_result().
process_request(_From, _RoomBareJid, _Request, {error, _} = Error, _Acc) ->
    Error;
process_request(From, RoomBareJid, Request, {ok, AffUsers, _Ver}, Acc) ->
    UserUS = jid:to_lus(From),
    RoomUS = jid:to_lus(RoomBareJid),
    Auth = lists:keyfind(UserUS, 1, AffUsers),
    process_request(Request, From, RoomUS, Auth, AffUsers, Acc).

-spec process_request(Request :: muc_light_packet(),
                      From :: jid:jid(),
                      RoomUS :: jid:simple_bare_jid(),
                      Auth :: false | aff_user(),
                      AffUsers :: aff_users(),
                      Acc :: mongoose_acc:t()) ->
    packet_processing_result().
process_request(_Request, _From, _RoomUS, false, _AffUsers, _Acc) ->
    {error, item_not_found};
process_request(#msg{} = Msg, _From, _RoomUS, _Auth, AffUsers, _Acc) ->
    {Msg, AffUsers};
process_request({get, #config{} = ConfigReq},
                _From, RoomUS, _Auth, _AffUsers, Acc) ->
    {_, RoomS} = RoomUS,
    HostType = mongoose_acc:host_type(Acc),
    {ok, Config, RoomVersion} = mod_muc_light_db_backend:get_config(HostType, RoomUS),
    {ok, RawConfig} = mod_muc_light_room_config:to_binary_kv(Config, mod_muc_light:config_schema(RoomS)),
    {get, ConfigReq#config{ version = RoomVersion,
                            raw_config = RawConfig }};
process_request({get, #affiliations{} = AffReq},
                _From, RoomUS, _Auth, _AffUsers, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    {ok, AffUsers, RoomVersion} = mod_muc_light_db_backend:get_aff_users(HostType, RoomUS),
    {get, AffReq#affiliations{ version = RoomVersion,
                               aff_users = AffUsers }};
process_request({get, #info{} = InfoReq},
                _From, {_, RoomS} = RoomUS, _Auth, _AffUsers, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    {ok, Config, AffUsers, RoomVersion} = mod_muc_light_db_backend:get_info(HostType, RoomUS),
    {ok, RawConfig} = mod_muc_light_room_config:to_binary_kv(Config, mod_muc_light:config_schema(RoomS)),
    {get, InfoReq#info{ version = RoomVersion, aff_users = AffUsers,
                        raw_config = RawConfig }};
process_request({set, #config{} = ConfigReq},
                _From, RoomUS, {_, UserAff}, AffUsers, Acc) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    AllCanConfigure = all_can_configure(HostType),
    process_config_set(HostType, ConfigReq, RoomUS, UserAff, AffUsers, AllCanConfigure);
process_request({set, #affiliations{} = AffReq},
                From, RoomUS, {_, UserAff}, AffUsers, Acc) ->
    UserUS = jid:to_lus(From),
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    OwnerUS = case lists:keyfind(owner, 2, AffUsers) of
                  false -> undefined;
                  {OwnerUS0, _} -> OwnerUS0
              end,
    ValidateResult
    = case UserAff of
          owner ->
              {ok, mod_muc_light_utils:filter_out_prevented(HostType,
                     UserUS, RoomUS, AffReq#affiliations.aff_users)};
          member ->
              AllCanInvite = all_can_invite(HostType),
              validate_aff_changes_by_member(
                AffReq#affiliations.aff_users, [], UserUS, OwnerUS, RoomUS, AllCanInvite)
      end,
    process_aff_set(AffReq, RoomUS, ValidateResult, Acc);
process_request({set, #destroy{} = DestroyReq},
                _From, RoomUS, {_, owner}, AffUsers, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    ok = mod_muc_light_db_backend:destroy_room(HostType, RoomUS),
    maybe_forget(Acc, RoomUS, [], <<>>),
    {set, DestroyReq, AffUsers};
process_request({set, #destroy{}},
                _From, _RoomUS, _Auth, _AffUsers, _Acc) ->
    {error, not_allowed};
process_request(_UnknownReq, _From, _RoomUS, _Auth, _AffUsers, _Acc) ->
    {error, bad_request}.

all_can_invite(HostType) ->
    gen_mod:get_module_opt(HostType, mod_muc_light, all_can_invite).

all_can_configure(HostType) ->
    gen_mod:get_module_opt(HostType, mod_muc_light, all_can_configure).

%% --------- Config set ---------

-spec process_config_set(HostType :: mongooseim:host_type(),
                         ConfigReq :: config_req_props(),
                         RoomUS :: jid:simple_bare_jid(),
                         UserAff :: member | owner, AffUsers :: aff_users(),
                         UserAllowedToConfigure :: boolean()) ->
    {set, config_req_props(), aff_users()} | {error, not_allowed} | validation_error().
process_config_set(HostType, #config{ raw_config = [{<<"subject">>, _}] } = ConfigReq, RoomUS, UserAff,
                   AffUsers, false) ->
    % Everyone is allowed to change subject
    process_config_set(HostType, ConfigReq, RoomUS, UserAff, AffUsers, true);
process_config_set(_HostType, _ConfigReq, _RoomUS, member, _AffUsers, false) ->
    {error, not_allowed};
process_config_set(HostType, ConfigReq, {_, RoomS} = RoomUS, _UserAff, AffUsers, _AllCanConfigure) ->
    case mod_muc_light_room_config:from_binary_kv_diff(
           ConfigReq#config.raw_config, mod_muc_light:config_schema(RoomS)) of
        {ok, Config} ->
            NewVersion = mongoose_bin:gen_from_timestamp(),
            {ok, PrevVersion} = mod_muc_light_db_backend:set_config(HostType, RoomUS, Config, NewVersion),
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
                      ValidateResult :: {ok, aff_users()} | {error, not_allowed},
                      Acc :: mongoose_acc:t()) ->
    {set, affiliations_req_props(), OldAffUsers :: aff_users(), NewAffUsers :: aff_users()}
    | {error, not_allowed}.
process_aff_set(AffReq, _RoomUS, {ok, []}, _Acc) -> % It seems that all users blocked this request
    {set, AffReq, [], []}; % Just return result to the user, don't change or broadcast anything
process_aff_set(AffReq, RoomUS, {ok, FilteredAffUsers}, Acc) ->
    NewVersion = mongoose_bin:gen_from_timestamp(),
    HostType = mongoose_acc:host_type(Acc),
    case mod_muc_light_db_backend:modify_aff_users(HostType, RoomUS, FilteredAffUsers,
                                   fun ?MODULE:participant_limit_check/2, NewVersion) of
        {ok, OldAffUsers, NewAffUsers, AffUsersChanged, OldVersion} ->
            maybe_forget(Acc, RoomUS, NewAffUsers, NewVersion),
            {set, AffReq#affiliations{
                    prev_version = OldVersion,
                    version = NewVersion,
                    aff_users = AffUsersChanged
                   }, OldAffUsers, NewAffUsers};
        Error ->
            Error
    end;
process_aff_set(_AffReq, _RoomUS, Error, _Acc) ->
    Error.

%%====================================================================
%% Response processing
%%====================================================================

-spec send_response(From :: jid:jid(), RoomJID :: jid:jid(), OrigPacket :: exml:element(),
                    Result :: packet_processing_result(), Acc :: mongoose_acc:t()) -> mongoose_acc:t().
send_response(From, RoomJID, OrigPacket, {error, _} = Err, Acc) ->
    mod_muc_light_codec_backend:encode_error(
      Err, From, RoomJID, OrigPacket, Acc);
send_response(From, RoomJID, _OriginalPacket, Response, Acc) ->
    F = make_handler_fun(Acc),
    mod_muc_light_codec_backend:encode(Response, From, RoomJID, F, Acc).

%%====================================================================
%% Internal functions
%%====================================================================
make_handler_fun(Acc) ->
    fun(From, To, Packet) ->
        NewAcc0 = mongoose_acc:new(#{location => ?LOCATION,
                                     lserver => From#jid.lserver,
                                     element => Packet,
                                     from_jid => From,
                                     to_jid => To}),
        PermanentFields = mongoose_acc:get_permanent_fields(Acc),
        NewAcc = mongoose_acc:set_permanent(PermanentFields, NewAcc0),
        ejabberd_router:route(From, To, NewAcc, Packet)
    end.
