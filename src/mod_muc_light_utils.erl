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
-export([bin_ts/0]).
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

-type new_owner_flag() :: none | pick | promoted.

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
            apply_aff_users_change(AffUsers, lists:sort(AffUsersChangesAssorted), none);
        {false, {_, _}} -> % ownerless room!
            {error, bad_request};
        {{Owner, _}, NewAffOwner} ->
            case {lists:keyfind(Owner, 1, AffUsersChangesAssorted), NewAffOwner} of
                {{_, _}, false} -> % need to pick new owner, may also result in improper state
                    fix_edge_aff_users_case(
                      apply_aff_users_change(AffUsers, lists:sort(AffUsersChangesAssorted), pick));
                {false, {_, _}} -> % just need to degrade old owner
                    apply_aff_users_change(
                      AffUsers, lists:sort([{Owner, member} | AffUsersChangesAssorted]), none);
                _ -> % simple case
                    apply_aff_users_change(AffUsers, lists:sort(AffUsersChangesAssorted), none)
            end
    end.

-spec aff2b(Aff :: aff()) -> binary().
aff2b(owner) -> <<"owner">>;
aff2b(member) -> <<"member">>;
aff2b(none) -> <<"none">>.

-spec b2aff(AffBin :: binary()) -> aff().
b2aff(<<"owner">>) -> owner;
b2aff(<<"member">>) -> member;
b2aff(<<"none">>) -> none.

-spec bin_ts() -> binary().
bin_ts() ->
    {Mega, Secs, Micro} = os:timestamp(),
    MegaB = integer_to_binary(Mega),
    SecsB = integer_to_binary(Secs),
    MicroB = integer_to_binary(Micro),
    <<MegaB/binary, $-, SecsB/binary, $-, MicroB/binary>>.

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

-spec fix_edge_aff_users_case(ChangeResult :: change_aff_success() | {error, bad_request}) ->
    change_aff_success() | {error, bad_request}.
fix_edge_aff_users_case({ok, NewAffUsers0, AffUsersChanged0, JoiningUsers, LeavingUsers}) ->
    {NewAffUsers, AffUsersChanged} =
    case NewAffUsers0 of
        [{Owner, member}] -> % edge case but can happen
            {[{Owner, owner}],
             case lists:member(Owner, JoiningUsers) of
                 false ->
                     %% Owner is an old owner
                     lists:keydelete(Owner, 1, AffUsersChanged0);
                 true ->
                     %% Owner is a newcomer, should be promoted
                     lists:keyreplace(Owner, 1, AffUsersChanged0, {Owner, owner})
             end};
        _ ->
            {NewAffUsers0, AffUsersChanged0}
    end,
    {ok, NewAffUsers, AffUsersChanged, JoiningUsers, LeavingUsers};
fix_edge_aff_users_case({error, bad_request}) ->
    {error, bad_request}.

-spec apply_aff_users_change(AffUsers :: aff_users(),
                             AffUsersChanges :: aff_users(),
                             NewOwner :: new_owner_flag()) ->
    change_aff_success() | {error, bad_request}.
apply_aff_users_change(AU, AUC, NO) ->
    apply_aff_users_change(AU, [], AUC, [], NO, [], []).

-spec apply_aff_users_change(AffUsers :: aff_users(),
                             NewAffUsers :: aff_users(),
                             AffUsersChanges :: aff_users(),
                             ChangesDone :: aff_users(),
                             NewOwner :: new_owner_flag(),
                             JoiningAcc :: [jid:simple_bare_jid()],
                             LeavingAcc :: [jid:simple_bare_jid()]) ->
    change_aff_success() | {error, bad_request}.
apply_aff_users_change([], NAU, [], CD, _NO, JA, LA) ->
    %% User list must be sorted ascending but acc is currently sorted descending
    {ok, lists:reverse(NAU), CD, JA, LA};
apply_aff_users_change(_AU, _NAU, [{User, _}, {User, _} | _RAUC], _CD, _NO, _JA, _LA) ->
    %% Cannot change affiliation for the same user twice in the same request
    {error, bad_request};
apply_aff_users_change(_AU, _NAU, [{_, owner} | _RAUC], _CD, promoted, _JA, _LA) ->
    %% Only one new owner can be explicitly stated
    {error, bad_request};
apply_aff_users_change([], _NAU, [{_, none} | _RAUC], _CD, _NO, _JA, _LA) ->
    %% Meaningless change - user not in the room
    {error, bad_request};
apply_aff_users_change([], NAU, [{User, _} = AffUser | RAUC], CD, NO, JA, LA) ->
    %% Reached end of current users' list, just appending now but still checking for owner
    apply_aff_users_change(
      [], [AffUser | NAU], RAUC, [AffUser | CD], new_owner_flag(AffUser, NO), [User | JA], LA);
apply_aff_users_change([AffUser | _], _NAU, [AffUser | _], _CD, _NO, _JA, _LA) ->
    %% Meaningless change
    {error, bad_request};
apply_aff_users_change([{User, _} | RAU], NAU, [{User, none} | RAUC], CD, NO, JA, LA) ->
    %% Leaving / removed user
    apply_aff_users_change(RAU, NAU, RAUC, [{User, none} | CD], NO, JA, [User | LA]);
apply_aff_users_change([{User, _} | RAU], NAU, [{User, NewAff} | RAUC], CD, NO, JA, LA) ->
    %% Changing affiliation, owner -> member or member -> owner
    apply_aff_users_change(RAU, [{User, NewAff} | NAU], RAUC, [{User, NewAff} | CD], NO, JA, LA);
apply_aff_users_change([{User1, _} | _] = AU, NAU, [{User2, member} | RAUC], CD, pick, JA, LA)
  when User1 > User2 ->
    %% Adding member to a room, we have to pick new owner so we promote newcomer
    apply_aff_users_change(AU, NAU, [{User2, owner} | RAUC], CD, none, JA, LA);
apply_aff_users_change([{User1, _} | _] = _AU, _NAU, [{User2, none} | _RAUC], _CD, _NO, _JA, _LA)
  when User1 > User2 ->
    % Meaningless change - user not in the room
    {error, bad_request};
apply_aff_users_change([{User1, _} | _] = AU, NAU, [{User2, _} = NewAffUser | RAUC], CD, NO, JA, LA)
  when User1 > User2 ->
    %% Adding new member to a room - owner or member
    apply_aff_users_change(AU, [NewAffUser | NAU], RAUC, [NewAffUser | CD],
                           new_owner_flag(NewAffUser, NO), [User2 | JA], LA);
apply_aff_users_change([{User, _} | _] = AU, NAU, AUC, CD, pick, JA, LA) ->
    %% Unaffected user, we have to pick new owner - we inject promotion
    apply_aff_users_change(AU, NAU, [{User, owner} | AUC], CD, none, JA, LA);
apply_aff_users_change([AffUser | RAU], NAU, AUC, CD, NO, JA, LA) ->
    %% Unaffected user, just appending
    apply_aff_users_change(RAU, [AffUser | NAU], AUC, CD, NO, JA, LA).

-spec new_owner_flag(AffUserChange :: aff_user(), NewOwner :: new_owner_flag()) ->
    new_owner_flag().
new_owner_flag({_, owner}, _NO) -> promoted;
new_owner_flag(_, NO) -> NO.

