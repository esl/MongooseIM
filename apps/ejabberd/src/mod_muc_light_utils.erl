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
-export([change_affiliations/2]).

-include("jlib.hrl").
-include("mod_muc_light.hrl").

-type new_owner() :: {old | new | random, ljid()}.

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

-spec change_affiliations(affiliations(), affiliations()) ->
    {ok, NewAffiliations :: affiliations(),
     AffiliationsChanged :: affiliations()} | {error, only_owner_in_room}.
change_affiliations(Affiliations, AffiliationsToChange) ->
    {OldOwner, _} = lists:keyfind(owner, 2, Affiliations),
    case apply_affiliations_change(
           Affiliations, lists:sort(AffiliationsToChange), {old, OldOwner}) of
        {ok, NewAffiliations, ExtraChangedAffiliations} ->
            AffiliationsChanged
            = lists:foldl(
                fun({User, _} = Affiliation, AffiliationsChangedAcc) ->
                    lists:keystore(User, 1, AffiliationsChangedAcc, Affiliation)
                end, AffiliationsToChange, ExtraChangedAffiliations),
            {ok, NewAffiliations, AffiliationsChanged};
        Error ->
            Error
    end.

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

-spec config_schema() -> [{binary(), atom(), term()}].
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

-spec apply_affiliations_change(
        AffiliationsAcc :: affiliations(),
        AffiliationsChanges :: affiliations(),
        NewOwner :: new_owner()) ->
    {ok, affiliations(), affiliations()} | {error, bad_request}.
apply_affiliations_change(_, [{User, _}, {User, _} | _], _) ->
    {error, bad_request};
apply_affiliations_change(_, [{User, owner} | _], {new, User}) ->
    {error, bad_request};
apply_affiliations_change([{Excluded, _}], [], {random, Excluded}) ->
    {ok, [{Excluded, owner}], [{Excluded, owner}]};
apply_affiliations_change([{Excluded, ExclAff}, {NewOwner, _} | RAffiliations],
                          [], {random, Excluded}) ->
    {ok, [{Excluded, ExclAff}, {NewOwner, owner} | RAffiliations],
     [{NewOwner, owner}]};
apply_affiliations_change([{NewOwner, _} | RAffiliations],
                          [], {random, _Excluded}) ->
    {ok, [{NewOwner, owner} | RAffiliations], [{NewOwner, owner}]};
apply_affiliations_change(Affiliations, [], _) ->
    {ok, Affiliations, []};
apply_affiliations_change(Affiliations, [{User, none} | RToChange], Owner) ->
    apply_affiliations_change(
      lists:keydelete(User, 1, Affiliations),
      RToChange, maybe_new_owner(User, Owner));
apply_affiliations_change(Affiliations, [{User, member} | RToChange], Owner) ->
    apply_affiliations_change(
      lists:keystore(User, 1, Affiliations, {User, member}),
      RToChange, maybe_new_owner(User, Owner));
apply_affiliations_change(Affiliations, [{User, owner} | RToChange], _Owner) ->
    apply_affiliations_change(
      lists:keystore(User, 1, Affiliations, {User, owner}),
      RToChange, {new, User}).

-spec maybe_new_owner(ljid(), new_owner()) -> new_owner().
maybe_new_owner(OldOwner, {old, OldOwner}) -> {random, OldOwner};
maybe_new_owner(_, NewOwner) -> NewOwner.

