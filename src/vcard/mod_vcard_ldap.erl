%%%----------------------------------------------------------------------
%%% File    : mod_vcard_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for VCards from LDAP storage.
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-module(mod_vcard_ldap).
-author('alexey@process-one.net').

-behaviour(mod_vcard_backend).

%% mod_vcard_backend callbacks
-export([init/2,
         tear_down/1,
         remove_user/3,
         get_vcard/3,
         set_vcard/5,
         search/3,
         search_fields/2,
         search_reported_fields/3]).

-export([default_vcard_map/0,
         default_search_fields/0,
         default_search_reported/0]).

-include_lib("eldap/include/eldap.hrl").
-include("mod_vcard.hrl").
-include("jlib.hrl").

-record(state,
        {serverhost = <<>>          :: binary(),
         myhost = <<>>              :: binary(),
         eldap_id                   :: eldap_utils:eldap_id(),
         base = <<>>                :: binary(),
         password = <<>>            :: binary(),
         uids = []                  :: [{binary()} | {binary(), binary()}],
         vcard_map = []             :: [{binary(), binary(), [binary()]}],
         vcard_map_attrs = []       :: [binary()],
         user_filter = <<>>         :: binary(),
         search_filter              :: eldap_utils:filter(),
         search_fields = []         :: [{binary(), binary()}],
         search_reported = []       :: [{binary(), binary()}],
         search_reported_attrs = [] :: [binary()],
         search_operator            :: 'or' | 'and',
         binary_search_fields       :: [binary()],
         deref = neverDerefAliases  :: eldap_utils:deref(),
         matches = 0                :: non_neg_integer() | infinity}).

-define(VCARD_MAP,
        [{<<"NICKNAME">>, <<"%u">>, []},
         {<<"FN">>, <<"%s">>, [<<"displayName">>]},
         {<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
         {<<"GIVEN">>, <<"%s">>, [<<"givenName">>]},
         {<<"MIDDLE">>, <<"%s">>, [<<"initials">>]},
         {<<"ORGNAME">>, <<"%s">>, [<<"o">>]},
         {<<"ORGUNIT">>, <<"%s">>, [<<"ou">>]},
         {<<"CTRY">>, <<"%s">>, [<<"c">>]},
         {<<"LOCALITY">>, <<"%s">>, [<<"l">>]},
         {<<"STREET">>, <<"%s">>, [<<"street">>]},
         {<<"REGION">>, <<"%s">>, [<<"st">>]},
         {<<"PCODE">>, <<"%s">>, [<<"postalCode">>]},
         {<<"TITLE">>, <<"%s">>, [<<"title">>]},
         {<<"URL">>, <<"%s">>, [<<"labeleduri">>]},
         {<<"DESC">>, <<"%s">>, [<<"description">>]},
         {<<"TEL">>, <<"%s">>, [<<"telephoneNumber">>]},
         {<<"EMAIL">>, <<"%s">>, [<<"mail">>]},
         {<<"BDAY">>, <<"%s">>, [<<"birthDay">>]},
         {<<"ROLE">>, <<"%s">>, [<<"employeeType">>]},
         {<<"PHOTO">>, <<"%s">>, [<<"jpegPhoto">>]}]).

-define(SEARCH_FIELDS,
        [{<<"User">>, <<"%u">>},
         {<<"Full Name">>, <<"displayName">>},
         {<<"Given Name">>, <<"givenName">>},
         {<<"Middle Name">>, <<"initials">>},
         {<<"Family Name">>, <<"sn">>},
         {<<"Nickname">>, <<"%u">>},
         {<<"Birthday">>, <<"birthDay">>},
         {<<"Country">>, <<"c">>}, {<<"City">>, <<"l">>},
         {<<"Email">>, <<"mail">>},
         {<<"Organization Name">>, <<"o">>},
         {<<"Organization Unit">>, <<"ou">>}]).

-define(SEARCH_REPORTED,
        [{<<"Full Name">>, <<"FN">>},
         {<<"Given Name">>, <<"FIRST">>},
         {<<"Middle Name">>, <<"MIDDLE">>},
         {<<"Family Name">>, <<"LAST">>},
         {<<"Nickname">>, <<"NICK">>},
         {<<"Birthday">>, <<"BDAY">>},
         {<<"Country">>, <<"CTRY">>},
         {<<"City">>, <<"LOCALITY">>},
         {<<"Email">>, <<"EMAIL">>},
         {<<"Organization Name">>, <<"ORGNAME">>},
         {<<"Organization Unit">>, <<"ORGUNIT">>}]).



%%--------------------------------------------------------------------
%% mod_vcard_backend callbacks
%%--------------------------------------------------------------------
-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, _Opts) ->
    ok.

-spec tear_down(mongooseim:host_type()) -> ok.
tear_down(HostType) ->
    clear_persistent_term(HostType),
    ok.

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(_HostType, _LUser, _LServer) ->
    %% no need to handle this - in ldap
    %% removing user = delete all user info
    ok.

-spec get_vcard(mongooseim:host_type(), jid:luser(), jid:lserver()) -> {ok, [exml:element()]}.
get_vcard(HostType, LUser, LServer) ->
    State = get_state(HostType, LServer),
    JID = jid:make_bare(LUser, LServer),
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            case find_ldap_user(LUser, State) of
                #eldap_entry{attributes = Attributes} ->
                    VCardMap = State#state.vcard_map,
                    VCard = ldap_attributes_to_vcard(Attributes, VCardMap, {LUser, LServer}),
                    {ok, VCard};
                _ ->
                    {ok, []}
            end;
        _ ->
            {ok, []}
    end.

-spec set_vcard(mongooseim:host_type(), jid:luser(), jid:lserver(), exml:element(), mod_vcard:vcard_search()) ->
    {error, exml:element()}.
set_vcard(_HostType, _User, _LServer, _VCard, _VCardSearch) ->
    {error, mongoose_xmpp_errors:not_allowed()}.

-spec search(mongooseim:host_type(), jid:lserver(), [{binary(), [binary()]}]) ->
          [[mongoose_data_forms:field()]].
search(HostType, LServer, Data) ->
    State = get_state(HostType, LServer),
    search_internal(State, Data).

-spec search_fields(mongooseim:host_type(), jid:lserver()) -> [{binary(), binary()}].
search_fields(HostType, LServer) ->
    State = get_state(HostType, LServer),
    State#state.search_fields.

-spec search_reported_fields(mongooseim:host_type(), jid:lserver(), binary()) ->
          [mongoose_data_forms:field()].
search_reported_fields(HostType, LServer, Lang) ->
    State = get_state(HostType, LServer),
    SearchReported = State#state.search_reported,
    [?TLFIELD(<<"text-single">>, Name, Value) ||
        {Name, Value} <- [{<<"Jabber ID">>, <<"jid">>} | SearchReported]].

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
default_vcard_map() ->
    ?VCARD_MAP.

default_search_fields() ->
    ?SEARCH_FIELDS.

default_search_reported() ->
    ?SEARCH_REPORTED.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
find_ldap_user(User, State) ->
    Base = State#state.base,
    RFC2254Filter = State#state.user_filter,
    EldapID = State#state.eldap_id,
    VCardAttrs = State#state.vcard_map_attrs,
    case eldap_filter:parse(RFC2254Filter, [{<<"%u">>, User}]) of
        {ok, EldapFilter} ->
            Res = eldap_pool_search(EldapID, Base, EldapFilter, State#state.deref, VCardAttrs, false),
            case Res of
                [H | _] ->
                    H;
                _ ->
                    Res
            end;
        _ -> false
    end.

eldap_pool_search(EldapID, Base, EldapFilter, Deref, Attrs, NoResultRes) ->
    SearchOpts = search_opts(Base, EldapFilter, Deref, Attrs),
    case eldap_pool:search(EldapID, SearchOpts) of
        #eldap_search_result{entries = E} -> E;
        _ -> NoResultRes
    end.

search_opts(Base, EldapFilter, Deref, Attrs) ->
    [{base, Base}, {filter, EldapFilter},
     {deref, Deref}, {attributes, Attrs}].

ldap_attributes_to_vcard(Attributes, VCardMap, UD) ->
    Attrs = lists:map(fun ({VCardName, _, _}) ->
                              {jid:str_tolower(VCardName),
                               map_vcard_attr(VCardName, Attributes, VCardMap,
                                              UD)}
                      end,
                      VCardMap),
    Elts = [ldap_attribute_to_vcard(vCard, Attr)
            || Attr <- Attrs],
    NElts = [ldap_attribute_to_vcard(vCardN, Attr)
             || Attr <- Attrs],
    OElts = [ldap_attribute_to_vcard(vCardO, Attr)
             || Attr <- Attrs],
    AElts = [ldap_attribute_to_vcard(vCardA, Attr)
             || Attr <- Attrs],
    [#xmlel{name = <<"vCard">>,
            attrs = #{<<"xmlns">> => ?NS_VCARD},
            children =
                lists:append([X || X <- Elts, X /= none],
                             [#xmlel{name = <<"N">>,
                                     children = [X || X <- NElts, X /= none]},
                              #xmlel{name = <<"ORG">>,
                                     children = [X || X <- OElts, X /= none]},
                              #xmlel{name = <<"ADR">>,
                                     children =
                                         [X || X <- AElts, X /= none]}])}].

ldap_attribute_to_vcard(vCard, {<<"fn">>, Value}) ->
    #xmlel{name = <<"FN">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCard,
                        {<<"nickname">>, Value}) ->
    #xmlel{name = <<"NICKNAME">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCard, {<<"title">>, Value}) ->
    #xmlel{name = <<"TITLE">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCard, {<<"bday">>, Value}) ->
    #xmlel{name = <<"BDAY">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCard, {<<"url">>, Value}) ->
    #xmlel{name = <<"URL">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCard, {<<"desc">>, Value}) ->
    #xmlel{name = <<"DESC">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCard, {<<"role">>, Value}) ->
    #xmlel{name = <<"ROLE">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCard, {<<"tel">>, Value}) ->
    #xmlel{name = <<"TEL">>,
           children =
               [#xmlel{name = <<"VOICE">>},
                #xmlel{name = <<"WORK">>},
                #xmlel{name = <<"NUMBER">>,
                       children = [#xmlcdata{content = Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"email">>, Value}) ->
    #xmlel{name = <<"EMAIL">>,
           children =
               [#xmlel{name = <<"INTERNET">>},
                #xmlel{name = <<"PREF">>},
                #xmlel{name = <<"USERID">>,
                       children = [#xmlcdata{content = Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"photo">>, Value}) ->
    #xmlel{name = <<"PHOTO">>,
           children =
               [#xmlel{name = <<"TYPE">>,
                       children = [#xmlcdata{content = <<"image/jpeg">>}]},
                #xmlel{name = <<"BINVAL">>,
                       children = [#xmlcdata{content = base64:encode(Value)}]}]};
ldap_attribute_to_vcard(vCardN,
                        {<<"family">>, Value}) ->
    #xmlel{name = <<"FAMILY">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardN, {<<"given">>, Value}) ->
    #xmlel{name = <<"GIVEN">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardN,
                        {<<"middle">>, Value}) ->
    #xmlel{name = <<"MIDDLE">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardO,
                        {<<"orgname">>, Value}) ->
    #xmlel{name = <<"ORGNAME">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardO,
                        {<<"orgunit">>, Value}) ->
    #xmlel{name = <<"ORGUNIT">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardA,
                        {<<"locality">>, Value}) ->
    #xmlel{name = <<"LOCALITY">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardA,
                        {<<"street">>, Value}) ->
    #xmlel{name = <<"STREET">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardA, {<<"ctry">>, Value}) ->
    #xmlel{name = <<"CTRY">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardA,
                        {<<"region">>, Value}) ->
    #xmlel{name = <<"REGION">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(vCardA, {<<"pcode">>, Value}) ->
    #xmlel{name = <<"PCODE">>,
           children = [#xmlcdata{content = Value}]};
ldap_attribute_to_vcard(_, _) -> none.

search_internal(_, []) ->
    [];
search_internal(State, Data) ->
    Base = State#state.base,
    SearchFilter = State#state.search_filter,
    EldapID = State#state.eldap_id,
    UIDs = State#state.uids,
    Limit = State#state.matches,
    ReportedAttrs = State#state.search_reported_attrs,
    Op = State#state.search_operator,
    Filter = eldap:'and'([SearchFilter, eldap_utils:make_filter(Data, UIDs, Op)]),
    E = eldap_pool_search(EldapID, Base, Filter, State#state.deref, ReportedAttrs, error),
    case E of
      error ->
        error;
      E ->
        Limited = limited_results(E, Limit),
        search_items(Limited, State)
     end.

limited_results(E, Limit) when length(E) > Limit ->
  lists:sublist(E, Limit);
limited_results(E, _) when not is_list(E) ->
  [E];
limited_results(E, _) ->
   E.

search_items(Entries, State) ->
    lists:flatmap(fun(#eldap_entry{attributes = Attrs}) -> attrs_to_item(Attrs, State) end,
                  Entries).

attrs_to_item(Attrs, #state{uids = UIDs} = State) ->
    case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
        {U, UIDAttrFormat} ->
            case eldap_utils:get_user_part(U, UIDAttrFormat) of
                {ok, Username} ->
                    make_user_item_if_exists(Username, Attrs, State);
                _ -> []
            end;
        <<"">> -> []
    end.

make_user_item_if_exists(Username, Attrs,
                         #state{serverhost = LServer, search_reported = SearchReported,
                                vcard_map = VCardMap, binary_search_fields = BinFields}) ->
    JID = jid:make_bare(Username, LServer),
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            RFields = lists:map(fun ({_, VCardName}) ->
                                        {VCardName, map_vcard_attr(VCardName, Attrs, VCardMap,
                                                                   {Username, LServer})}
                                end,
                                SearchReported),
            [[?FIELD(<<"jid">>, <<Username/binary, "@", LServer/binary>>)] ++
                 [?FIELD(Name, search_item_value(Name, Value, BinFields)) ||
                     {Name, Value} <- RFields]];
        _ -> []
    end.

%%%-----------------------
%%% Auxiliary functions.
%%%-----------------------
search_item_value(Name, Value, BinaryFields) ->
    case lists:member(Name, BinaryFields) of
        true  -> base64:encode(Value);
        false -> Value
    end.

map_vcard_attr(VCardName, Attributes, Pattern, UD) ->
    Res = lists:filter(fun ({Name, _, _}) ->
                               eldap_utils:case_insensitive_match(Name,
                                                                  VCardName)
                       end,
                       Pattern),
    case Res of
      [{_, Str, Attrs}] ->
          process_pattern(Str, UD,
                          [eldap_utils:get_ldap_attr(X, Attributes)
                           || X <- Attrs]);
      _ -> <<"">>
    end.

process_pattern(Str, {User, Domain}, AttrValues) ->
    eldap_filter:do_sub(Str,
                        [{<<"%u">>, User}, {<<"%d">>, Domain}] ++
                        [{<<"%s">>, V, 1} || V <- AttrValues]).

get_state(HostType, LServer) ->
    Key = config_key(HostType, LServer),
    case persistent_term:get(Key, undefined) of
        undefined ->
            State = create_state(HostType, LServer),
            persistent_term:put(Key, State),
            State;
        State ->
            State
    end.

create_state(HostType, LServer) ->
    Opts = gen_mod:get_loaded_module_opts(HostType, mod_vcard),
    Matches = gen_mod:get_opt(matches, Opts),
    Host = gen_mod:get_opt(host, Opts),
    LDAPOpts = gen_mod:get_opt(ldap, Opts),
    #{pool_tag := PoolTag,
      base := Base,
      deref := Deref,
      uids := RawUIDs,
      filter := RawUserFilter,
      vcard_map := VCardMap,
      search_fields := SearchFields,
      search_reported := SearchReported,
      search_operator := SearchOperator,
      binary_search_fields := BinaryFields} = LDAPOpts,
    MyHost = mongoose_subdomain_utils:get_fqdn(Host, LServer),
    DerefAliases = eldap_utils:deref_aliases(Deref),
    UIDs = eldap_utils:uids_domain_subst(LServer, RawUIDs),
    UserFilter = eldap_utils:process_user_filter(UIDs, RawUserFilter),
    {ok, SearchFilter} = eldap_filter:parse(eldap_utils:get_search_filter(UserFilter)),

    UIDAttrs = lists:map(fun({UID, _}) -> UID;
                            ({UID}) -> UID end, UIDs),
    VCardMapAttrs = lists:usort(lists:append([A || {_, _, A} <- VCardMap])
                                ++ UIDAttrs),
    SearchReportedAttrs = lists:usort(lists:flatmap(
                                        fun ({_, N}) ->
                                          case lists:keysearch(N, 1, VCardMap) of
                                            {value, {_, _, L}} ->
                                              L;
                                            _ -> []
                                          end
                                        end,
                                        SearchReported) ++ UIDAttrs),
    #state{serverhost = LServer,
           myhost = MyHost,
           eldap_id = {HostType, PoolTag},
           base = Base,
           deref = DerefAliases,
           uids = UIDs, vcard_map = VCardMap,
           vcard_map_attrs = VCardMapAttrs,
           user_filter = UserFilter, search_filter = SearchFilter,
           search_fields = SearchFields,
           binary_search_fields = BinaryFields,
           search_reported = SearchReported,
           search_reported_attrs = SearchReportedAttrs,
           search_operator = SearchOperator,
           matches = Matches}.

clear_persistent_term(HostType) ->
    Terms = persistent_term:get(),
    States = lists:filter(fun({K, _V}) -> is_host_type_config_key(HostType, K) end, Terms),
    [persistent_term:erase(Key) || {Key, _V} <- States].

is_host_type_config_key(HostType, {?MODULE, HostType, _LServer}) ->
    true;
is_host_type_config_key(_HT, _K) ->
    false.

config_key(HostType, LServer) ->
    {?MODULE, HostType, LServer}.
