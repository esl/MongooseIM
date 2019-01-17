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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_utils).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([make_config_schema/1, make_default_config/2]).
-export([process_raw_config/3, config_to_raw/2]).
-export([change_aff_users/2]).
-export([b2aff/1, aff2b/1]).
-export([room_limit_reached/2]).
-export([filter_out_prevented/3]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_muc_light.hrl").

-type user_defined_schema_item() :: FieldName :: string()
                                    | {FieldName :: string(), FieldName :: schema_value_type()}
                                    | schema_item().
-type user_defined_schema() :: [user_defined_schema_item()].

-type user_config_defaults_item() :: {FieldName :: string(), FieldValue :: term()}.
-type user_config_defaults() :: [user_config_defaults_item()].

-type change_aff_success() :: {ok, NewAffUsers :: aff_users(), ChangedAffUsers :: aff_users(),
                               JoiningUsers :: [jid:simple_bare_jid()],
                               LeavingUsers :: [jid:simple_bare_jid()]}.

-type change_aff_success_without_users() :: {ok, NewAffUsers :: aff_users(), ChangedAffUsers :: aff_users()}.

-type promotion_type() :: promote_old_member | promote_joined_member | promote_demoted_owner.

-export_type([change_aff_success/0]).

%%====================================================================
%% API
%%====================================================================

-spec make_config_schema(UserDefinedSchema :: user_defined_schema()) -> config_schema().
make_config_schema(UserDefinedSchema) ->
    lists:map(fun expand_config_schema_field/1, UserDefinedSchema).

-spec make_default_config(UserConfigDefaults :: user_config_defaults(),
                         ConfigSchema :: config_schema()) -> config().
make_default_config(UserConfigDefaults, ConfigSchema) ->
    DefaultConfigCandidate = lists:map(fun process_config_field/1, UserConfigDefaults),
    lists:foreach(fun({Key, Value}) ->
                          try
                              {_, _, ValueType} = lists:keyfind(Key, 2, ConfigSchema),
                              value2b(Value, ValueType)
                          catch
                              _:Error -> error({invalid_default_config, Key, Value, Error})
                          end
                  end, DefaultConfigCandidate),
    DefaultConfigCandidate.

%% Guarantees that config will have unique fields
-spec process_raw_config(
        RawConfig :: raw_config(), Config :: config(), ConfigSchema :: config_schema()) ->
    {ok, config()} | validation_error().
process_raw_config([], Config, _ConfigSchema) ->
    {ok, Config};
process_raw_config([{KeyBin, ValBin} | RRawConfig], Config, ConfigSchema) ->
    case process_raw_config_opt(KeyBin, ValBin, ConfigSchema) of
        {ok, Key, Val} ->
            process_raw_config(
              RRawConfig, lists:keystore(Key, 1, Config, {Key, Val}), ConfigSchema);
        Error ->
            Error
    end.

-spec config_to_raw(Config :: config(), ConfigSchema :: config_schema()) -> raw_config().
config_to_raw([], _ConfigSchema) ->
    [];
config_to_raw([{Key, Val} | RConfig], ConfigSchema) ->
    {KeyBin, _, ValType} = lists:keyfind(Key, 2, ConfigSchema),
    [{KeyBin, value2b(Val, ValType)} | config_to_raw(RConfig, ConfigSchema)].

-spec change_aff_users(CurrentAffUsers :: aff_users(), AffUsersChangesAssorted :: aff_users()) ->
    change_aff_success() | {error, bad_request}.
change_aff_users(AffUsers, AffUsersChangesAssorted) ->
    case {lists:keyfind(owner, 2, AffUsers), lists:keyfind(owner, 2, AffUsersChangesAssorted)} of
        {false, false} -> % simple, no special cases
            apply_aff_users_change(AffUsers, AffUsersChangesAssorted);
        {false, {_, _}} -> % ownerless room!
            {error, bad_request};
        _ ->
            lists:foldl(fun(F, Acc) -> F(Acc) end,
                        apply_aff_users_change(AffUsers, AffUsersChangesAssorted),
                        [fun maybe_demote_old_owner/1,
                         fun maybe_select_new_owner/1])
    end.

-spec aff2b(Aff :: aff()) -> binary().
aff2b(owner) -> <<"owner">>;
aff2b(member) -> <<"member">>;
aff2b(none) -> <<"none">>.

-spec b2aff(AffBin :: binary()) -> aff().
b2aff(<<"owner">>) -> owner;
b2aff(<<"member">>) -> member;
b2aff(<<"none">>) -> none.

-spec room_limit_reached(UserUS :: jid:simple_bare_jid(), RoomS :: jid:lserver()) ->
    boolean().
room_limit_reached(UserUS, RoomS) ->
    room_limit_reached(
      UserUS, RoomS, gen_mod:get_module_opt_by_subhost(
                       RoomS, mod_muc_light, rooms_per_user, ?DEFAULT_ROOMS_PER_USER)).

-spec filter_out_prevented(FromUS :: jid:simple_bare_jid(),
                          RoomUS :: jid:simple_bare_jid(),
                          AffUsers :: aff_users()) -> aff_users().
filter_out_prevented(FromUS, {RoomU, MUCServer} = RoomUS, AffUsers) ->
    RoomsPerUser = gen_mod:get_module_opt_by_subhost(
                     MUCServer, mod_muc_light, rooms_per_user, ?DEFAULT_ROOMS_PER_USER),
    BlockingEnabled = gen_mod:get_module_opt_by_subhost(MUCServer, mod_muc_light,
                                                        blocking, ?DEFAULT_BLOCKING),
    BlockingQuery = case {BlockingEnabled, RoomU} of
                        {true, <<>>} -> [{user, FromUS}];
                        {true, _} -> [{user, FromUS}, {room, RoomUS}];
                        {false, _} -> undefined
                    end,
    case BlockingQuery == undefined andalso RoomsPerUser == infinity of
        true -> AffUsers;
        false -> filter_out_loop(FromUS, MUCServer, BlockingQuery, RoomsPerUser, AffUsers)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% ---------------- Checks ----------------

-spec room_limit_reached(UserUS :: jid:simple_bare_jid(),
                         RoomS :: jid:lserver(),
                         RoomsPerUser :: infinity | pos_integer()) ->
    boolean().
room_limit_reached(_UserUS, _RoomS, infinity) ->
    false;
room_limit_reached(UserUS, RoomS, RoomsPerUser) ->
    mod_muc_light_db_backend:get_user_rooms_count(UserUS, RoomS) >= RoomsPerUser.

%% ---------------- Filter for blocking ----------------

-spec filter_out_loop(FromUS :: jid:simple_bare_jid(),
                      MUCServer :: jid:lserver(),
                      BlockingQuery :: [{blocking_what(), jid:simple_bare_jid()}],
                      RoomsPerUser :: rooms_per_user(),
                      AffUsers :: aff_users()) -> aff_users().
filter_out_loop(FromUS, MUCServer, BlockingQuery, RoomsPerUser,
                [{UserUS, _} = AffUser | RAffUsers]) ->
    NotBlocked = case (BlockingQuery == undefined orelse UserUS =:= FromUS) of
                     false -> mod_muc_light_db_backend:get_blocking(
                                UserUS, MUCServer, BlockingQuery) == allow;
                     true -> true
                 end,
    case NotBlocked andalso not room_limit_reached(FromUS, MUCServer, RoomsPerUser) of
        true ->
            [AffUser | filter_out_loop(FromUS, MUCServer, BlockingQuery, RoomsPerUser, RAffUsers)];
        false ->
            filter_out_loop(FromUS, MUCServer, BlockingQuery, RoomsPerUser, RAffUsers)
    end;
filter_out_loop(_FromUS, _MUCServer, _BlockingQuery, _RoomsPerUser, []) ->
    [].

%% ---------------- Configuration processing ----------------

-spec expand_config_schema_field(UserDefinedSchemaItem :: user_defined_schema_item()) ->
    schema_item().
expand_config_schema_field({FieldName, Type}) ->
    {_, true} = {FieldName, is_valid_config_type(Type)},
    {list_to_binary(FieldName), list_to_atom(FieldName), Type};
expand_config_schema_field({FieldNameBin, FieldName, Type} = SchemaItem)
  when is_binary(FieldNameBin) andalso is_atom(FieldName) ->
    {_, true} = {FieldName, is_valid_config_type(Type)},
    SchemaItem;
expand_config_schema_field(Name) ->
    {list_to_binary(Name), list_to_atom(Name), binary}.

-spec process_config_field(UserConfigDefaultsItem :: user_config_defaults_item()) -> config_item().
process_config_field({Key, Value}) when is_list(Value) ->
    process_config_field({Key, list_to_binary(Value)});
process_config_field({Key, Value}) ->
    {list_to_atom(Key), Value}.

-spec process_raw_config_opt(
        KeyBin :: binary(), ValBin :: binary(), ConfigSchema :: config_schema()) ->
    {ok, Key :: atom(), Val :: any()} | validation_error().
process_raw_config_opt(KeyBin, ValBin, ConfigSchema) ->
    case lists:keyfind(KeyBin, 1, ConfigSchema) of
        {_, Key, Type} -> {ok, Key, b2value(ValBin, Type)};
        _ -> {error, {KeyBin, unknown}}
    end.

-spec is_valid_config_type(Type :: schema_value_type() | atom()) -> boolean().
is_valid_config_type(binary) -> true;
is_valid_config_type(integer) -> true;
is_valid_config_type(float) -> true;
is_valid_config_type(_) -> false.

-spec b2value(ValBin :: binary(), Type :: schema_value_type()) -> Converted :: any().
b2value(ValBin, binary) -> ValBin;
b2value(ValBin, integer) -> binary_to_integer(ValBin);
b2value(ValBin, float) -> binary_to_float(ValBin).

-spec value2b(Val :: any(), Type :: schema_value_type()) -> Converted :: binary().
value2b(Val, binary) -> Val;
value2b(Val, integer) -> integer_to_binary(Val);
value2b(Val, float) -> float_to_binary(Val).

%% ---------------- Affiliations manipulation ----------------

-spec maybe_select_new_owner(ChangeResult :: change_aff_success() | {error, bad_request}) ->
    change_aff_success() | {error, bad_request}.
maybe_select_new_owner({ok, AU, AUC, JoiningUsers, LeavingUsers} = AffRes) ->
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
select_promotion(_OldMembers, [U | _], DemotedOwners) ->
    {U, promote_joined_member};
select_promotion(_OldMembers, _JoiningUsers, [U | _]) ->
    {U, promote_demoted_owner};
select_promotion(_, _, _) ->
    false.

-spec maybe_demote_old_owner(ChangeResult :: change_aff_success() | {error, bad_request}) ->
    change_aff_success() | {error, bad_request}.
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
            {error, bad_request}
    end;
maybe_demote_old_owner(Error) ->
    Error.

-spec apply_aff_users_change(AffUsers :: aff_users(),
                             AffUsersChanges :: aff_users()) ->
                                change_aff_success() | {error, bad_request}.
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
                                change_aff_success_without_users() | {error, bad_request}.
apply_aff_users_change([], NAU, [], CD) ->
    %% User list must be sorted ascending but acc is currently sorted descending
    {ok, lists:reverse(NAU), CD};
apply_aff_users_change(_AU, _NAU, [{User, _}, {User, _} | _RAUC], _CD) ->
    %% Cannot change affiliation for the same user twice in the same request
    {error, bad_request};
apply_aff_users_change([AffUser | _], _NAU, [AffUser | _], _CD) ->
    %% Meaningless change
    {error, bad_request};
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




