%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_utils.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Utilities for mod_muc_light
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
-export([iq_to_config/2, config_to_query/1, validate_config_opt/2]).
-export([change_affiliated_users/2]).
-export([b2aff/1, aff2b/1]).

-include("jlib.hrl").
-include("mod_muc_light.hrl").

%%====================================================================
%% API
%%====================================================================

-spec iq_to_config(#xmlel{}, configuration()) ->
    {ok, configuration()} | validation_error().
iq_to_config(Packet, Defaults) ->
    Xs = exml_query:paths(Packet, [{element, <<"query">>}, {element, <<"x">>}]),
    process_form(find_form(Xs), Defaults).

-spec config_to_query(configuration()) -> #xmlel{}.
config_to_query(Config) ->
    Fields = lists:map(
               fun
                   ({Var, VarAtom, binary}) ->
                       {_, Value} = lists:keyfind(VarAtom, 1, Config),
                       make_field(Var, Value, []);
                   ({<<"FORM_TYPE">>, _, Value}) ->
                       make_field(<<"FORM_TYPE">>, Value,
                                  [{<<"type">>, <<"hidden">>}]);
                   ({Var, _, Value}) when is_binary(Value) ->
                       make_field(Var, Value, [])
               end, config_schema()),
    #xmlel{ name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_MUC_OWNER}],
            children = [
                        #xmlel{ name = <<"x">>,
                                attrs = [{<<"xmlns">>, ?NS_XDATA},
                                         {<<"type">>, <<"form">>}],
                                children = Fields }
                       ] }.

-spec validate_config_opt(binary() | atom(), term()) ->
    {ok, AtomKey :: atom()} | validation_error().
validate_config_opt(Key, Value) when is_binary(Key) ->
    validate_type(Key, Value, lists:keyfind(Key, 1, config_schema()));
validate_config_opt(Key, Value) when is_atom(Key) ->
    validate_type(Key, Value, lists:keyfind(Key, 2, config_schema())).

-spec change_affiliated_users(affiliated_users(), affiliated_users()) ->
    {ok, NewAffiliations :: affiliated_users(),
     AffiliationsChanged :: affiliated_users(),
     JoiningUsers :: [ljid()],
     LeavingUsers :: [ljid()]} | {error, bad_request}.
change_affiliated_users(Affiliations, AffiliationsToChange) ->
    {OldOwner, _} = lists:keyfind(owner, 2, Affiliations),
    SaneAffiliationsToChange = sanitize_affiliations_changes(
                                 Affiliations, AffiliationsToChange),
    apply_affiliated_users_change(Affiliations, SaneAffiliationsToChange, OldOwner).

-spec aff2b(affiliation()) -> binary().
aff2b(owner) -> <<"owner">>;
aff2b(member) -> <<"member">>;
aff2b(none) -> <<"none">>.

-spec b2aff(binary()) -> affiliation().
b2aff(<<"owner">>) -> owner;
b2aff(<<"member">>) -> member;
b2aff(<<"none">>) -> none.

%%====================================================================
%% Internal functions
%%====================================================================

%% ---------------- Configuration processing ----------------

-spec find_form([#xmlel{}]) -> FormFields :: [#xmlel{}].
find_form([]) ->
    [];
find_form([XElem | XRest]) ->
    case exml_query:attr(XElem, <<"xmlns">>) of
        ?NS_XDATA -> exml_query:paths(XElem, [{element, <<"field">>}]);
        _ -> find_form(XRest)
    end.

-spec process_form([#xmlel{}], configuration()) ->
    {ok, configuration()} | validation_error().
process_form([], Config) ->
    {ok, Config};
process_form([Field | Fields], Config) ->
    case exml_query:attr(Field, <<"var">>) of
        undefined ->
            process_form(Fields, Config);
        Key ->
            Value = exml_query:path(Field, [{element, <<"value">>}, cdata]),
            case validate_config_opt(Key, Value) of
                {ok, AtomKey} ->
                    process_form(Fields, lists:keystore(
                                           AtomKey, 1, Config, {AtomKey, Value}));
                Error ->
                    Error
            end
    end.

-spec config_schema() ->
    [{FormFieldName :: binary(), OptionName :: atom(), ValueTypeOrValue :: term()}].
config_schema() ->
    [
     {<<"FORM_TYPE">>, form_type, ?NS_MUC_CONFIG},
     {<<"roomname">>, roomname, binary}
    ].

-spec make_field(binary(), binary(), [{binary(), binary()}]) -> #xmlel{}.
make_field(Name, Value, ExtraAttrs) ->
    #xmlel{ name = <<"field">>, attrs = [{<<"var">>, Name} | ExtraAttrs],
            children = [
                        #xmlel{ name = <<"value">>,
                                children = [ #xmlcdata{ content = Value } ] }
                       ]}.

-spec validate_type(binary() | atom(), term(), term()) ->
    {ok, AtomKey :: atom()} | validation_error().
validate_type(Key, _Val, false) -> {error, {Key, unknown}};
validate_type(_Key, Val, {_, AtomKey, binary}) when is_binary(Val) -> {ok, AtomKey};
validate_type(_Key, Val, {_, AtomKey, Val}) when is_binary(Val) -> {ok, AtomKey};
validate_type(Key, _Val, _Type) -> {error, {Key, type}}.

%% ---------------- Affiliations manipulation ----------------

-spec sanitize_affiliations_changes(affiliated_users(), affiliated_users()) ->
    affiliated_users() | {error, bad_request}.
sanitize_affiliations_changes(Affiliations, AffiliationChangesUnsorted) ->
    sanitize_affiliations_changes(lists:sort(AffiliationChangesUnsorted), [], false, Affiliations).

-spec sanitize_affiliations_changes(
        affiliated_users(), affiliated_users(), boolean(), affiliated_users()) ->
    affiliated_users() | {error, bad_request}.
%% Cannot change affiliation for the same user twice in the same request
sanitize_affiliations_changes([{User, _}, {User, _} | _], _, _, _) ->
    {error, bad_request};
%% Only one new owner can be explicitly stated
sanitize_affiliations_changes([{_, owner} | _], _, true, _) ->
    {error, bad_request};
sanitize_affiliations_changes([{User, Affiliation} = Entry | Rest], Sane, OwnerFound, Affiliations) ->
    %% Only meaningful changes are allowed
    case {lists:keyfind(User, 1, Affiliations), Affiliation} of
        {false, none} -> {error, bad_request};
        {{_, member}, member} -> {error, bad_request};
        {{_, owner}, owner} -> {error, bad_request};
        _ -> sanitize_affiliations_changes(Rest, [Entry | Sane], OwnerFound, Affiliations)
    end;
sanitize_affiliations_changes([], Sane, _, _) ->
    Sane.

-spec apply_affiliated_users_change(
        affiliated_users(), affiliated_users() | {error, any()}, ljid()) ->
    {ok, affiliated_users(), affiliated_users(), [ljid()], [ljid()]} | {error, bad_request}.
apply_affiliated_users_change(_, {error, _} = Error, _) ->
    Error;
apply_affiliated_users_change(Affiliations0, AffiliationsChanges, OldOwner) ->
    %% Only transfering ownership is a tricky stuff so let's modify the affiliations so the list
    %% will look like we expected the client to provide all necessary explicit changes.
    {ok, Affiliations1, JoiningUsers, LeavingUsers}
    = naive_affiliated_users_change(Affiliations0, AffiliationsChanges),

    %% Now we fix the list if there is incorrect owner count.
    {ok, Affiliations, AffiliationsChanged}
    = ensure_one_owner(Affiliations1, AffiliationsChanges, OldOwner),

    {ok, Affiliations, AffiliationsChanged, JoiningUsers, LeavingUsers}.

-spec naive_affiliated_users_change(affiliated_users(), affiliated_users()) ->
    {ok, affiliated_users(), [ljid()], [ljid()]}.
naive_affiliated_users_change(Affiliations, AffiliationsChanges) ->
    naive_affiliated_users_change(Affiliations, AffiliationsChanges, [], []).

-spec naive_affiliated_users_change(affiliated_users(), affiliated_users(), [ljid()], [ljid()]) ->
    {ok, affiliated_users(), [ljid()], [ljid()]}.
naive_affiliated_users_change(Affiliations, [{User, none} | RAffiliationsChanges], JoiningUsers, LeavingUsers) ->
    naive_affiliated_users_change(lists:keydelete(User, 1, Affiliations), RAffiliationsChanges, JoiningUsers, [User | LeavingUsers]);
naive_affiliated_users_change(Affiliations, [{User, Affiliation} | RAffiliationsChanges], JoiningUsers0, LeavingUsers) ->
    JoiningUsers = case lists:keyfind(User, 1, Affiliations) of
                       false -> [User | JoiningUsers0];
                       _ -> JoiningUsers0
                   end,
    naive_affiliated_users_change(lists:keystore(User, 1, Affiliations, {User, Affiliation}), RAffiliationsChanges, JoiningUsers, LeavingUsers);
naive_affiliated_users_change(Affiliations, [], JoiningUsers, LeavingUsers) ->
    {ok, Affiliations, JoiningUsers, LeavingUsers}.

-spec ensure_one_owner(affiliated_users(), affiliated_users(), ljid()) ->
    {ok, affiliated_users(), affiliated_users()}.
ensure_one_owner(Affiliations, AffiliationsChanges, OldOwner) ->
    OwnersC = lists:foldl(
                fun ({_, owner}, N) -> N + 1;
                    (_, N) -> N
                end, 0, Affiliations),
    ensure_one_owner(Affiliations, AffiliationsChanges, OldOwner, OwnersC).

%% Now, since we enforce only one 'owner' item in affiliations change request,
%% there are only 3 possibilities here (regarding the state of affiliated users list):
%% 1. no owners: old owner made himself a member or even left the room
%% 2. one owner: cool, either no change in ownership or the old owner chose new owner
%% 3. two owners: someone was promoted to the owner status but old owner did not change own affiliation
%%
%% Here is how we deal with these cases:
%% 0. First of all, if the users list is empty, we do nothing :)
%% 1. We pick first user that is not the old owner; if the old owner is the only user left,
%%    then we force him back to owner status.
%% 2. One of the owners is the old one so we downgrade to 'member'
-spec ensure_one_owner(affiliated_users(), affiliated_users(), ljid(), non_neg_integer()) ->
    {ok, affiliated_users(), affiliated_users()}.
ensure_one_owner(Affiliations, AffiliationsChanges, OldOwner, 0) ->
    pick_new_owner(Affiliations, Affiliations, AffiliationsChanges, OldOwner);
ensure_one_owner(Affiliations, AffiliationsChanges, _, 1) ->
    {ok, Affiliations, AffiliationsChanges};
ensure_one_owner(Affiliations, AffiliationsChanges, OldOwner, 2) ->
    {ok, lists:keystore(OldOwner, 1, Affiliations, {OldOwner, member}), lists:keystore(OldOwner, 1, AffiliationsChanges, {OldOwner, member})}.

-spec pick_new_owner(affiliated_users(), affiliated_users(), affiliated_users(), ljid()) ->
    {ok, affiliated_users(), affiliated_users()}.
pick_new_owner([{NewOwner, _} | Rest], Affiliations, AffiliationsChanges, Excluded)
  when NewOwner =/= Excluded orelse Rest =:= []->
    {ok, lists:keystore(NewOwner, 1, Affiliations, {NewOwner, owner}), lists:keystore(NewOwner, 1, AffiliationsChanges, {NewOwner, owner})};
pick_new_owner([_ | Rest], Affiliations, AffiliationsChanges, Excluded) ->
    pick_new_owner(Rest, Affiliations, AffiliationsChanges, Excluded);
pick_new_owner([], Affiliations, AffiliationsChanges, _Excluded) ->
    {ok, Affiliations, AffiliationsChanges}.

