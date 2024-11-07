%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_utils.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Stateless utilities for mod_muc_light
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_utils).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([change_aff_users/3]).
-export([b2aff/1, aff2b/1]).
-export([light_aff_to_muc_role/1]).
-export([room_limit_reached/2]).
-export([filter_out_prevented/4]).
-export([acc_to_host_type/1]).
-export([room_jid_to_host_type/1]).
-export([room_jid_to_server_host/1]).
-export([muc_host_to_host_type/1]).
-export([server_host_to_muc_host/2]).
-export([run_forget_room_hook/1]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_muc_light.hrl").

-type change_aff_success() :: {ok, NewAffUsers :: aff_users(), ChangedAffUsers :: aff_users(),
                               JoiningUsers :: [jid:simple_bare_jid()],
                               LeavingUsers :: [jid:simple_bare_jid()]}.

-type change_aff_success_without_users() :: {ok, NewAffUsers :: aff_users(),
                                             ChangedAffUsers :: aff_users()}.

-type promotion_type() :: promote_old_member | promote_joined_member | promote_demoted_owner.

-export_type([change_aff_success/0]).

-type bad_request() :: bad_request | {bad_request, binary()}.

%%====================================================================
%% API
%%====================================================================

-spec change_aff_users(HostType :: mongooseim:host_type(), CurrentAffUsers :: aff_users(), AffUsersChangesAssorted :: aff_users()) ->
    change_aff_success() | {error, bad_request()}.
change_aff_users(HostType, AffUsers, AffUsersChangesAssorted) ->
    case {lists:keyfind(owner, 2, AffUsers), lists:keyfind(owner, 2, AffUsersChangesAssorted)} of
        {false, false} -> % simple, no special cases
            apply_aff_users_change(AffUsers, AffUsersChangesAssorted);
        {false, {_, _}} -> % ownerless room!
            {error, {bad_request, <<"Ownerless room">>}};
        _ ->
            MultipleOwners = allow_multiple_owners(HostType),
            lists:foldl(fun(F, Acc) -> F(Acc) end,
                        apply_aff_users_change(AffUsers, AffUsersChangesAssorted),
                        change_aff_functions(MultipleOwners))
    end.


-spec aff2b(Aff :: aff()) -> binary().
aff2b(owner) -> <<"owner">>;
aff2b(member) -> <<"member">>;
aff2b(none) -> <<"none">>.

-spec b2aff(AffBin :: binary()) -> aff().
b2aff(<<"owner">>) -> owner;
b2aff(<<"member">>) -> member;
b2aff(<<"none">>) -> none.

-spec light_aff_to_muc_role(aff()) -> mod_muc:role().
light_aff_to_muc_role(owner) -> moderator;
light_aff_to_muc_role(member) -> participant;
light_aff_to_muc_role(none) -> none.

-spec room_limit_reached(UserJid :: jid:jid(), HostType :: mongooseim:host_type()) ->
    boolean().
room_limit_reached(UserJid, HostType) ->
    check_room_limit_reached(jid:to_lus(UserJid), HostType, rooms_per_user(HostType)).

rooms_per_user(HostType) ->
    gen_mod:get_module_opt(HostType, mod_muc_light, rooms_per_user).

-spec filter_out_prevented(HostType :: mongooseim:host_type(),
                           FromUS :: jid:simple_bare_jid(),
                           RoomUS :: jid:simple_bare_jid(),
                           AffUsers :: aff_users()) -> aff_users().
filter_out_prevented(HostType, FromUS, {RoomU, MUCServer} = RoomUS, AffUsers) ->
    RoomsPerUser = rooms_per_user(HostType),
    BlockingEnabled = gen_mod:get_module_opt(HostType, mod_muc_light, blocking),
    BlockingQuery = case {BlockingEnabled, RoomU} of
                        {true, <<>>} -> [{user, FromUS}];
                        {true, _} -> [{user, FromUS}, {room, RoomUS}];
                        {false, _} -> undefined
                    end,
    case BlockingQuery == undefined andalso RoomsPerUser == infinity of
        true -> AffUsers;
        false -> filter_out_loop(HostType, FromUS, MUCServer,
                                 BlockingQuery, RoomsPerUser, AffUsers)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% ---------------- Checks ----------------

-spec check_room_limit_reached(UserUS :: jid:simple_bare_jid(),
                               HostType :: mongooseim:host_type(),
                               RoomsPerUser :: infinity | pos_integer()) ->
    boolean().
check_room_limit_reached(_UserUS, _HostType, infinity) ->
    false;
check_room_limit_reached(UserUS, HostType, RoomsPerUser) ->
    mod_muc_light_db_backend:get_user_rooms_count(HostType, UserUS) >= RoomsPerUser.

%% ---------------- Filter for blocking ----------------

-spec filter_out_loop(HostType :: mongooseim:host_type(),
                      FromUS :: jid:simple_bare_jid(),
                      MUCServer :: jid:lserver(),
                      BlockingQuery :: [{blocking_what(), jid:simple_bare_jid()}],
                      RoomsPerUser :: rooms_per_user(),
                      AffUsers :: aff_users()) -> aff_users().
filter_out_loop(HostType, FromUS, MUCServer, BlockingQuery, RoomsPerUser,
                [{UserUS, _} = AffUser | RAffUsers]) ->
    NotBlocked = case (BlockingQuery == undefined orelse UserUS =:= FromUS) of
                     false -> mod_muc_light_db_backend:get_blocking(
                                HostType, UserUS, MUCServer, BlockingQuery) == allow;
                     true -> true
                 end,
    case NotBlocked andalso not check_room_limit_reached(FromUS, HostType, RoomsPerUser) of
        true ->
            [AffUser | filter_out_loop(HostType, FromUS, MUCServer,
                                       BlockingQuery, RoomsPerUser, RAffUsers)];
        false ->
            filter_out_loop(HostType, FromUS, MUCServer,
                            BlockingQuery, RoomsPerUser, RAffUsers)
    end;
filter_out_loop(_HostType, _FromUS, _MUCServer, _BlockingQuery, _RoomsPerUser, []) ->
    [].

%% ---------------- Affiliations manipulation ----------------

-spec change_aff_functions(AllowMultipleOwners :: boolean()) -> [fun(), ...].
change_aff_functions(_AllowMultipleOwners = false)->
    [fun maybe_demote_old_owner/1, fun maybe_select_new_owner/1];
change_aff_functions(_AllowMultipleOwners = true)->
    [fun maybe_select_new_owner/1].

-spec maybe_select_new_owner(ChangeResult :: change_aff_success() | {error, bad_request()}) ->
    change_aff_success() | {error, bad_request()}.
maybe_select_new_owner({ok, AU, AUC, JoiningUsers, LeavingUsers} = _AffRes) ->
    {AffUsers, AffUsersChanged} =
        case is_new_owner_needed(AU) andalso find_new_owner(AU, AUC, JoiningUsers) of
            {NewOwner, PromotionType} ->
                NewAU = lists:keyreplace(NewOwner, 1, AU, {NewOwner, owner}),
                NewAUC = update_auc(PromotionType, NewOwner, AUC),
                {NewAU, NewAUC};
            false ->
                {AU, AUC}
        end,
    {ok, AffUsers, AffUsersChanged, JoiningUsers, LeavingUsers};
maybe_select_new_owner(Error) ->
    Error.

update_auc(promote_old_member, NewOwner, AUC) ->
    [{NewOwner, owner} | AUC];
update_auc(promote_joined_member, NewOwner, AUC) ->
    lists:keyreplace(NewOwner, 1, AUC, {NewOwner, owner});
update_auc(promote_demoted_owner, NewOwner, AUC) ->
    lists:keydelete(NewOwner, 1, AUC).

is_new_owner_needed(AU) ->
    case lists:keyfind(owner, 2, AU) of
        false -> true;
        _ -> false
    end.


-spec find_new_owner(aff_users(), aff_users(), [jid:simple_bare_jid()]) ->
    {jid:simple_bare_jid(), promotion_type()} | false.
find_new_owner(AU, AUC, JoiningUsers) ->
    AllMembers = [U || {U, member} <- (AU)],
    NewMembers = [U || {U, member} <- (AUC)],
    OldMembers = AllMembers -- NewMembers,
    DemotedOwners = NewMembers -- JoiningUsers,
    select_promotion(OldMembers, JoiningUsers, DemotedOwners).

%% @doc try to select the new owner from:
%%   1) old unchanged room members
%%   2) new just joined room members
%%   3) demoted room owners
select_promotion([U | _], _JoiningUsers, _DemotedOwners) ->
    {U, promote_old_member};
select_promotion(_OldMembers, [U | _], _DemotedOwners) ->
    {U, promote_joined_member};
select_promotion(_OldMembers, _JoiningUsers, [U | _]) ->
    {U, promote_demoted_owner};
select_promotion(_, _, _) ->
    false.

-spec maybe_demote_old_owner(ChangeResult :: change_aff_success() | {error, bad_request()}) ->
    change_aff_success() | {error, bad_request()}.
maybe_demote_old_owner({ok, AU, AUC, JoiningUsers, LeavingUsers}) ->
    Owners = [U || {U, owner} <- AU],
    PromotedOwners = [U || {U, owner} <- AUC],
    OldOwners = Owners -- PromotedOwners,
    case {Owners, OldOwners} of
        _ when length(Owners) =< 1 ->
            {ok, AU, AUC, JoiningUsers, LeavingUsers};
        {[_, _], [OldOwner]} ->
            NewAU = lists:keyreplace(OldOwner, 1, AU, {OldOwner, member}),
            NewAUC = [{OldOwner, member} | AUC],
            {ok, NewAU, NewAUC, JoiningUsers, LeavingUsers};
        _ ->
            {error, {bad_request, <<"Failed to demote old owner">>}}
    end;
maybe_demote_old_owner(Error) ->
    Error.

-spec apply_aff_users_change(AffUsers :: aff_users(),
                             AffUsersChanges :: aff_users()) ->
                                change_aff_success() | {error, bad_request()}.
apply_aff_users_change(AU, AUC) ->
    JoiningUsers = proplists:get_keys(AUC) -- proplists:get_keys(AU),
    AffAndNewUsers = lists:sort(AU ++ [{U, none} || U <- JoiningUsers]),
    AffChanges = lists:sort(AUC),
    LeavingUsers = [U || {U, none} <- AUC],
    case apply_aff_users_change(AffAndNewUsers, [], AffChanges, []) of
        {ok, NewAffUsers, ChangesDone} ->
            {ok, NewAffUsers, ChangesDone, JoiningUsers, LeavingUsers};
        Error -> Error
    end.


-spec apply_aff_users_change(AffUsers :: aff_users(),
                             NewAffUsers :: aff_users(),
                             AffUsersChanges :: aff_users(),
                             ChangesDone :: aff_users()) ->
                                change_aff_success_without_users() | {error, bad_request()}.
apply_aff_users_change([], NAU, [], CD) ->
    %% User list must be sorted ascending but acc is currently sorted descending
    {ok, lists:reverse(NAU), CD};
apply_aff_users_change(_AU, _NAU, [{User, _}, {User, _} | _RAUC], _CD) ->
    {error, {bad_request, <<"Cannot change affiliation for the same user "
                            "twice in the same request">>}};
apply_aff_users_change([AffUser | _], _NAU, [AffUser | _], _CD) ->
    {error, {bad_request, <<"Meaningless change">>}};
apply_aff_users_change([{User, _} | RAU], NAU, [{User, none} | RAUC], CD) ->
    %% removing user from the room
    apply_aff_users_change(RAU, NAU, RAUC, [{User, none} | CD]);

apply_aff_users_change([{User, none} | RAU], NAU, [{User, _} = NewUser | RAUC], CD) ->
    %% Adding new member to a room
    apply_aff_users_change(RAU, [NewUser | NAU], RAUC, [NewUser | CD]);

apply_aff_users_change([{User, _} | RAU], NAU, [{User, NewAff} | RAUC], CD) ->
    %% Changing affiliation, owner -> member or member -> owner
    apply_aff_users_change(RAU, [{User, NewAff} | NAU], RAUC, [{User, NewAff} | CD]);

apply_aff_users_change([OldUser | RAU], NAU, AUC, CD) ->
    %% keep user affiliation unchanged
    apply_aff_users_change(RAU, [OldUser | NAU], AUC, CD).

-spec acc_to_host_type(mongoose_acc:t()) -> mongooseim:host_type().
acc_to_host_type(Acc) ->
    case mongoose_acc:host_type(Acc) of
        undefined ->
            MucHost = mongoose_acc:lserver(Acc),
            muc_host_to_host_type(MucHost);
        HostType ->
            HostType
    end.

-spec room_jid_to_host_type(jid:jid()) -> mongooseim:host_type().
room_jid_to_host_type(#jid{lserver = MucHost}) ->
    muc_host_to_host_type(MucHost).

-spec room_jid_to_server_host(jid:jid()) -> jid:lserver().
room_jid_to_server_host(#jid{lserver = MucHost}) ->
    case mongoose_domain_api:get_subdomain_info(MucHost) of
        {ok, #{parent_domain := ServerHost}} when is_binary(ServerHost) ->
            ServerHost;
        Other ->
            error({room_jid_to_server_host_failed, MucHost, Other})
    end.

muc_host_to_host_type(MucHost) ->
    case mongoose_domain_api:get_subdomain_host_type(MucHost) of
        {ok, HostType} ->
            HostType;
        Other ->
            error({muc_host_to_host_type_failed, MucHost, Other})
    end.

subdomain_pattern(HostType) ->
    gen_mod:get_module_opt(HostType, mod_muc_light, host).

server_host_to_muc_host(HostType, ServerHost) ->
    mongoose_subdomain_utils:get_fqdn(subdomain_pattern(HostType), ServerHost).

run_forget_room_hook({Room, MucHost}) ->
    case mongoose_domain_api:get_subdomain_host_type(MucHost) of
        {ok, HostType} ->
            mongoose_hooks:forget_room(HostType, MucHost, Room);
        _Other ->
            %% MUC light is not started probably
            ?LOG_ERROR(#{what => run_forget_room_hook_skipped,
                         room => Room, muc_host => MucHost})
    end.

allow_multiple_owners(HostType) ->
    gen_mod:get_module_opt(HostType, mod_muc_light, allow_multiple_owners).
