-module(muc_light_SUITE).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("mod_muc_light.hrl").

-define(DOMAIN, <<"localhost">>).

%% ------------------------------------------------------------------
%% Common Test callbacks
%% ------------------------------------------------------------------

all() ->
    [
     {group, aff_changes}
    ].

groups() ->
    [
     {aff_changes, [parallel], [
                                aff_change_success,
                                aff_change_bad_request
                               ]}
    ].

%% ------------------------------------------------------------------
%% Test cases
%% ------------------------------------------------------------------

aff_change_success(_Config) ->
    ?assert(proper:quickcheck(prop_aff_change_success())).

aff_change_bad_request(_Config) ->
    ?assert(proper:quickcheck(prop_aff_change_bad_request())).

%% ------------------------------------------------------------------
%% Properties and validators
%% ------------------------------------------------------------------

prop_aff_change_success() ->
    ?FORALL({AffUsers, Changes, Joining, Leaving, WithOwner}, change_aff_params(),
            begin
                case mod_muc_light_utils:change_aff_users(AffUsers, Changes) of
                    {ok, NewAffUsers0, AffUsersChanged, Joining0, Leaving0} ->
                        Joining = lists:sort(Joining0),
                        Leaving = lists:sort(Leaving0),
                        % is the length correct?
                        CorrectLen = length(AffUsers) + length(Joining) - length(Leaving),
                        CorrectLen = length(NewAffUsers0),
                        % is the list unique and sorted?
                        NewAffUsers0 = uuser_sort(NewAffUsers0),
                        % are there no 'none' items?
                        false = lists:keyfind(none, 2, NewAffUsers0),
                        % are there no owners or there is exactly one?
                        true = validate_owner(NewAffUsers0, false, WithOwner),
                        % changes list applied to old list should produce the same result
                        {ok, NewAffUsers1, _, _, _} = mod_muc_light_utils:change_aff_users(AffUsers, AffUsersChanged),
                        NewAffUsers0 = NewAffUsers1,
                        true;
                    _ ->
                        false
                end
            end).

-spec validate_owner(NewAffUsers :: aff_users(), OneOwnerFound :: boolean(),
                     AreOwnersAllowed :: boolean()) -> boolean().
validate_owner([{_, owner} | _], true, _) -> false; % more than one owner
validate_owner([{_, owner} | _], _, false) -> false; % there should be no owners
validate_owner([{_, owner} | R], _, true) -> validate_owner(R, true, true); % there should be no owners
validate_owner([_ | R], Found, WithOwner) -> validate_owner(R, Found, WithOwner);
validate_owner([], _, _) -> true.

prop_aff_change_bad_request() ->
    ?FORALL({AffUsers, Changes}, bad_change_aff(),
            begin
                {error, bad_request} = mod_muc_light_utils:change_aff_users(AffUsers, Changes),
                true
            end).

%% ------------------------------------------------------------------
%% Complex generators
%% ------------------------------------------------------------------

change_aff_params() ->
    ?LET(WithOwner, with_owner(),
         ?LET(AffUsers, aff_users_list(WithOwner, 10),
              ?LET(
                 {Joining,                   Leaving,           PromoteOwner,
                  PromotedOwnerPos,                              DegradeOwner},
                 {aff_users_list(false, 15), sublist(AffUsers), boolean(),
                  integer(length(AffUsers), length(AffUsers)*2), boolean()},
                 begin
                     Changes1 = Joining ++ [{U, none} || {U, _} <- Leaving],
                     Survivors = (AffUsers -- Leaving) ++ Joining,
                     Changes2 = promote_owner(PromoteOwner andalso WithOwner, PromotedOwnerPos,
                                              Changes1, Survivors),
                     Changes3 = degrade_owner(DegradeOwner, AffUsers, Changes2),
                     {AffUsers,
                      Changes3,
                      [ U || {U, _} <- Joining ],
                      [ U || {U, _} <- Leaving ],
                      WithOwner}
                 end))).

-spec promote_owner(Promote :: boolean(), PromotedOwnerPos :: pos_integer(),
                    Changes1 :: aff_users(), Survivors :: aff_users()) ->
    ChangesWithOwnerPromotion :: aff_users().
promote_owner(true, PromotedOwnerPos, Changes1, Survivors) ->
    SurvivorsNoOwner = lists:keydelete(owner, 2, Survivors),
    {NewOwner, _} = lists:nth((PromotedOwnerPos rem length(SurvivorsNoOwner)) + 1, SurvivorsNoOwner),
    lists:keystore(NewOwner, 1, Changes1, {NewOwner, owner});
promote_owner(false, _, Changes1, _) ->
    Changes1.

-spec degrade_owner(Degrade :: boolean(), AffUsers :: aff_users(), Changes :: aff_users()) ->
    ChangesWithDegradedOwner :: aff_users().
degrade_owner(false, _, Changes1) ->
    Changes1;
degrade_owner(true, AffUsers, Changes1) ->
    case lists:keyfind(owner, 2, AffUsers) of
        {Owner, _} ->
            case lists:keyfind(Owner, 1, Changes1) of
                false -> [{Owner, member} | Changes1];
                _ -> Changes1 % Owner is already downgraded somehow
            end;
        _ ->
            Changes1
    end.

bad_change_aff() ->
    ?LET({{AffUsers, Changes, _Joining, _Leaving, WithOwner}, FailGenerator},
         {change_aff_params(), fail_gen_fun()},
         ?MODULE:FailGenerator(AffUsers, Changes, WithOwner)).

aff_users_list(WithOwner, NameLen) ->
    ?LET(AffUsers, non_empty(list(aff_user(NameLen))), with_owner(uuser_sort(AffUsers), WithOwner)).

sublist(L) ->
    ?LET(SurvivorVector,
         vector(length(L), boolean()),
         pick_survivors(L, SurvivorVector)).

owner_problem(AffUsers, Changes, false) ->
    % Change aff to owner but owners are not allowed
    ?LET({{User, _Aff}, InsertPos}, {aff_user(5), integer(1, length(Changes))},
         {AffUsers, insert({User, owner}, Changes, InsertPos)});
owner_problem(AffUsers, Changes, true) ->
    % Promote two users to owner
    ?LET({{User1, _Aff}, {User2, _Aff}, InsertPos1, InsertPos2},
         {aff_user(5), aff_user(5), integer(1, length(Changes)), integer(1, length(Changes))},
         {AffUsers, insert({User2, owner},
                           insert({User1, owner}, Changes, InsertPos1),
                           InsertPos2)}).

duplicated_user(AffUsers, Changes, _) ->
     ?LET(DuplicatePos, integer(1, length(Changes)), {AffUsers, duplicate(Changes, DuplicatePos)}).

meaningless_change(AffUsers, Changes, _) ->
    ?LET(MeaninglessAff, oneof([other, none]),
         meaningless_change_by_aff(AffUsers, Changes, MeaninglessAff)).

meaningless_change_by_aff(AffUsers, Changes, none) ->
    ?LET({{User, _Aff}, InsertPos}, {aff_user(5), integer(1, length(Changes))},
         {AffUsers, insert({User, none}, Changes, InsertPos)});
meaningless_change_by_aff(AffUsers, Changes0, other) ->
    ?LET({UserPos, InsertPos}, {integer(1, length(AffUsers)), integer(1, length(Changes0))},
         begin
             {User, _} = AffUser = lists:nth(UserPos, AffUsers),
             Changes = insert(AffUser, lists:keydelete(User, 1, Changes0), InsertPos),
             {AffUsers, Changes}
         end).

%% ------------------------------------------------------------------
%% Simple generators
%% ------------------------------------------------------------------

aff_user(NameLen) ->
    ?LET(U, bitstring(NameLen*8), {{U, ?DOMAIN}, member}).

with_owner() ->
    boolean().

with_owner(L, true) -> ?LET(OwnerPos, integer(1, length(L)), make_owner(L, OwnerPos));
with_owner(L, _) -> L.

fail_gen_fun() ->
    oneof([owner_problem, duplicated_user, meaningless_change]).

%% ------------------------------------------------------------------
%% Utils
%% ------------------------------------------------------------------

-spec insert(E :: term(), L :: list(), Pos :: pos_integer()) -> [term(), ...].
insert(E, L, 1) -> [E | L];
insert(E, [EL | L], Pos) -> [EL | insert(E, L, Pos - 1)];
insert(E, [], _) -> [E].

-spec duplicate(L :: [term(),...], Pos :: pos_integer()) -> [term(),...].
duplicate([E | L], 1) -> [E, E | L];
duplicate([E | L], Pos) -> [E | duplicate(L, Pos - 1)];
duplicate([], _) -> [].

-spec uuser_sort(AffUsers :: aff_users()) -> aff_users().
uuser_sort(AffUsers) ->
    lists:usort(fun({A, _}, {B, _}) -> A =< B end, AffUsers).

-spec pick_survivors(List :: list(), SurvivorVector :: [boolean()]) -> list().
pick_survivors([], []) -> [];
pick_survivors([_ | RL], [false | RVec]) -> pick_survivors(RL, RVec);
pick_survivors([E | RL], [_ | RVec]) -> [E | pick_survivors(RL, RVec)].

-spec make_owner(AffUsers :: aff_users(), OwnerPos :: pos_integer()) -> aff_users().
make_owner([{User, _} | RAffUsers], 1) -> [{User, owner} | RAffUsers];
make_owner([AffUser | RAffUsers], OwnerPos) -> [AffUser | make_owner(RAffUsers, OwnerPos - 1)];
make_owner([], _OwnerPos) -> [].

