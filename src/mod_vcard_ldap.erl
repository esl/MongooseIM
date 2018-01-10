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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%TODO LDAP layer should be separated like odbc one.
%%     Now every ldap module creates its own ldap pool per vhost
%%TODO gen_server is created only to store the state
%%     and create/destroy pool, should it be replaced ?

-module(mod_vcard_ldap).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(mod_vcard).

%% gen_server callbacks.
-export([init/1, handle_info/2, handle_call/3,
         handle_cast/2,  code_change/3, terminate/2]).

-export([start_link/2]).

%% mod_vcards callbacks
-export([init/2,
         remove_user/2,
         get_vcard/2,
         set_vcard/4,
         search/2,
         search_fields/1,
         search_reported_fields/2]).


-include("mongoose.hrl").
-include("eldap.hrl").
-include("mod_vcard.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_vcard_ldap).

-record(state,
        {serverhost = <<"">>        :: binary(),
         myhost = <<"">>            :: binary(),
         eldap_id = <<"">>          :: binary(),
         servers = []               :: [binary()],
         backups = []               :: [binary()],
         port = ?LDAP_PORT          :: inet:port_number(),
         tls_options = []           :: list(),
         dn = <<"">>                :: binary(),
         base = <<"">>              :: binary(),
         password = <<"">>          :: binary(),
         uids = []                  :: [{binary()} | {binary(), binary()}],
         vcard_map = []             :: [{binary(), binary(), [binary()]}],
         vcard_map_attrs = []       :: [binary()],
         user_filter = <<"">>       :: binary(),
         search_filter              :: eldap:filter(),
         search_fields = []         :: [{binary(), binary()}],
         search_reported = []       :: [{binary(), binary()}],
         search_reported_attrs = [] :: [binary()],
         search_operator            :: 'or' | 'and',
         binary_search_fields       :: [binary()],
         deref_aliases = never      :: never | searching | finding | always,
         matches = 0                :: non_neg_integer()}).

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
%% mod_vcards callbacks
%%--------------------------------------------------------------------

init(VHost, Options) ->
    start_link(VHost, Options),
    ok.

remove_user(_LUser, _LServer) ->
    %% no need to handle this - in ldap
    %% removing user = delete all user info
    ok.

get_vcard(LUser, LServer) ->
    Proc = gen_mod:get_module_proc(LServer, ?PROCNAME),
    {ok, State} = gen_server:call(Proc, get_state),
    LServer = State#state.serverhost,
    case ejabberd_auth:is_user_exists(LUser, LServer) of
        true ->
            VCardMap = State#state.vcard_map,
            case find_ldap_user(LUser, State) of
                #eldap_entry{attributes = Attributes} ->
                    Vcard = ldap_attributes_to_vcard(Attributes, VCardMap, {LUser, LServer}),
                    {ok, Vcard};
                _ ->
                    {ok, []}
            end;
        _ ->
            {ok, []}
    end.

set_vcard(_User, _VHost, _VCard, _VCardSearch) ->
    {error, mongoose_xmpp_errors:not_allowed()}.

search(LServer, Data) ->
    Proc = gen_mod:get_module_proc(LServer, ?PROCNAME),
    {ok, State} = gen_server:call(Proc, get_state),
    search_internal(State, Data).

search_fields(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    {ok, State} = gen_server:call(Proc, get_state),
    State#state.search_fields.

search_reported_fields(Host, Lang) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    {ok, State} = gen_server:call(Proc, get_state),
    SearchReported = State#state.search_reported,
    #xmlel{name = <<"reported">>, attrs = [],
           children =
           [?TLFIELD(<<"text-single">>, <<"Jabber ID">>,
                     <<"jid">>)]
           ++
           lists:map(fun ({Name, Value}) ->
                             ?TLFIELD(<<"text-single">>, Name,
                                      Value)
                     end,
                     SearchReported)}.


%%--------------------------------------------------------------------
%% gen server callbacks
%%--------------------------------------------------------------------

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
                          [Host, Opts], []).

init([Host, Opts]) ->
    process_flag(trap_exit, true),
    State = parse_options(Host, Opts),
    eldap_pool:start_link(State#state.eldap_id,
                          State#state.servers, State#state.backups,
                          State#state.port, State#state.dn,
                          State#state.password, State#state.tls_options),
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, State) ->
    eldap_pool:stop(State#state.eldap_id).

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
          case eldap_pool:search(EldapID,
                                 [{base, Base}, {filter, EldapFilter},
                                  {deref_aliases, State#state.deref_aliases},
                                  {attributes, VCardAttrs}])
              of
            #eldap_search_result{entries = [E | _]} -> E;
            _ -> false
          end;
      _ -> false
    end.

ldap_attributes_to_vcard(Attributes, VCardMap, UD) ->
    Attrs = lists:map(fun ({VCardName, _, _}) ->
                              {stringprep:tolower(VCardName),
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
            attrs = [{<<"xmlns">>, ?NS_VCARD}],
            children =
                lists:append([X || X <- Elts, X /= none],
                             [#xmlel{name = <<"N">>, attrs = [],
                                     children = [X || X <- NElts, X /= none]},
                              #xmlel{name = <<"ORG">>, attrs = [],
                                     children = [X || X <- OElts, X /= none]},
                              #xmlel{name = <<"ADR">>, attrs = [],
                                     children =
                                         [X || X <- AElts, X /= none]}])}].

ldap_attribute_to_vcard(vCard, {<<"fn">>, Value}) ->
    #xmlel{name = <<"FN">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard,
                        {<<"nickname">>, Value}) ->
    #xmlel{name = <<"NICKNAME">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"title">>, Value}) ->
    #xmlel{name = <<"TITLE">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"bday">>, Value}) ->
    #xmlel{name = <<"BDAY">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"url">>, Value}) ->
    #xmlel{name = <<"URL">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"desc">>, Value}) ->
    #xmlel{name = <<"DESC">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"role">>, Value}) ->
    #xmlel{name = <<"ROLE">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"tel">>, Value}) ->
    #xmlel{name = <<"TEL">>, attrs = [],
           children =
               [#xmlel{name = <<"VOICE">>, attrs = [], children = []},
                #xmlel{name = <<"WORK">>, attrs = [], children = []},
                #xmlel{name = <<"NUMBER">>, attrs = [],
                       children = [{xmlcdata, Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"email">>, Value}) ->
    #xmlel{name = <<"EMAIL">>, attrs = [],
           children =
               [#xmlel{name = <<"INTERNET">>, attrs = [],
                       children = []},
                #xmlel{name = <<"PREF">>, attrs = [], children = []},
                #xmlel{name = <<"USERID">>, attrs = [],
                       children = [{xmlcdata, Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"photo">>, Value}) ->
    #xmlel{name = <<"PHOTO">>, attrs = [],
           children =
               [#xmlel{name = <<"TYPE">>, attrs = [],
                       children = [{xmlcdata, <<"image/jpeg">>}]},
                #xmlel{name = <<"BINVAL">>, attrs = [],
                       children = [{xmlcdata, jlib:encode_base64(Value)}]}]};
ldap_attribute_to_vcard(vCardN,
                        {<<"family">>, Value}) ->
    #xmlel{name = <<"FAMILY">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardN, {<<"given">>, Value}) ->
    #xmlel{name = <<"GIVEN">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardN,
                        {<<"middle">>, Value}) ->
    #xmlel{name = <<"MIDDLE">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardO,
                        {<<"orgname">>, Value}) ->
    #xmlel{name = <<"ORGNAME">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardO,
                        {<<"orgunit">>, Value}) ->
    #xmlel{name = <<"ORGUNIT">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
                        {<<"locality">>, Value}) ->
    #xmlel{name = <<"LOCALITY">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
                        {<<"street">>, Value}) ->
    #xmlel{name = <<"STREET">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA, {<<"ctry">>, Value}) ->
    #xmlel{name = <<"CTRY">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
                        {<<"region">>, Value}) ->
    #xmlel{name = <<"REGION">>, attrs = [],
           children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA, {<<"pcode">>, Value}) ->
    #xmlel{name = <<"PCODE">>, attrs = [],
           children = [{xmlcdata, Value}]};
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
    Filter = eldap:'and'([SearchFilter,
                          eldap_utils:make_filter(Data, UIDs, Op)]),
    case eldap_pool:search(EldapID,
                           [{base, Base}, {filter, Filter}, {limit, Limit},
                            {deref_aliases, State#state.deref_aliases},
                            {attributes, ReportedAttrs}])
        of
      #eldap_search_result{entries = E} ->
          search_items(E, State);
      _ -> error
    end.

search_items(Entries, State) ->
    lists:flatmap(fun(#eldap_entry{attributes = Attrs}) -> attrs_to_item_xml(Attrs, State) end,
                  Entries).

attrs_to_item_xml(Attrs, #state{uids = UIDs} = State) ->
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
    case ejabberd_auth:is_user_exists(Username, LServer) of
        true ->
            RFields = lists:map(fun ({_, VCardName}) ->
                                        {VCardName, map_vcard_attr(VCardName, Attrs, VCardMap,
                                                                   {Username, ?MYNAME})}
                                end,
                                SearchReported),
            Result = [?FIELD(<<"jid">>, <<Username/binary, "@", LServer/binary>>)] ++
            [?FIELD(Name, search_item_value(Name, Value, BinFields)) || {Name, Value} <- RFields],
            [#xmlel{name = <<"item">>, attrs = [], children = Result}];
        _ -> []
    end.

%%%-----------------------
%%% Auxiliary functions.
%%%-----------------------
search_item_value(Name, Value, BinaryFields) ->
    case lists:member(Name, BinaryFields) of
        true  -> jlib:encode_base64(Value);
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

parse_options(Host, Opts) ->
    MyHost = gen_mod:get_opt_subhost(Host, Opts, mod_vcard:default_host()),
    Matches = eldap_utils:get_mod_opt(matches, Opts,
                              fun(infinity) -> 0;
                                 (I) when is_integer(I), I>0 -> I
                              end, 30),
    EldapID = atom_to_binary(gen_mod:get_module_proc(Host, ?PROCNAME), utf8),
    Cfg = eldap_utils:get_config(Host, Opts),
    UIDsTemp = eldap_utils:get_opt(
                 {ldap_uids, Host}, Opts,
                 fun(Us) ->
                         lists:map(
                           fun({U, P}) ->
                                   {iolist_to_binary(U),
                                    iolist_to_binary(P)};
                              ({U}) ->
                                   {iolist_to_binary(U)}
                           end, Us)
                 end, [{<<"uid">>, <<"%u">>}]),
    UIDs = eldap_utils:uids_domain_subst(Host, UIDsTemp),
    SubFilter = eldap_utils:generate_subfilter(UIDs),
    UserFilter = case eldap_utils:get_opt(
                        {ldap_filter, Host}, Opts,
                        fun check_filter/1, <<"">>) of
                     <<"">> ->
                         SubFilter;
                     F ->
                         <<"(&", SubFilter/binary, F/binary, ")">>
                 end,
    {ok, SearchFilter} =
        eldap_filter:parse(eldap_filter:do_sub(UserFilter,
                                               [{<<"%u">>, <<"*">>}])),
    VCardMap = eldap_utils:get_mod_opt(ldap_vcard_map, Opts,
                               fun(Ls) ->
                                       lists:map(
                                         fun({S, P, L}) ->
                                                 {iolist_to_binary(S),
                                                  iolist_to_binary(P),
                                                  [iolist_to_binary(E)
                                                   || E <- L]}
                                         end, Ls)
                               end, ?VCARD_MAP),
    SearchFields = eldap_utils:get_mod_opt(ldap_search_fields, Opts,
                                   fun(Ls) ->
                                           [{iolist_to_binary(S),
                                             iolist_to_binary(P)}
                                            || {S, P} <- Ls]
                                   end, ?SEARCH_FIELDS),
    SearchReported = eldap_utils:get_mod_opt(ldap_search_reported, Opts,
                                     fun(Ls) ->
                                             [{iolist_to_binary(S),
                                               iolist_to_binary(P)}
                                              || {S, P} <- Ls]
                                     end, ?SEARCH_REPORTED),
    UIDAttrs = [UAttr || {UAttr, _} <- UIDs],
    VCardMapAttrs = lists:usort(lists:append([A
                                              || {_, _, A} <- VCardMap])
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
    SearchOperatorFun = fun
        ('or') -> 'or';
        (_)    -> 'and'
    end,
    SearchOperator = eldap_utils:get_mod_opt(ldap_search_operator, Opts,
                                             SearchOperatorFun, 'and'),
    BinaryFields = eldap_utils:get_mod_opt(ldap_binary_search_fields, Opts,
                                           fun(X) -> X end, []),
    #state{serverhost = Host, myhost = MyHost,
           eldap_id = EldapID,
           servers = Cfg#eldap_config.servers,
           backups = Cfg#eldap_config.backups,
           port = Cfg#eldap_config.port,
           tls_options = Cfg#eldap_config.tls_options,
           dn = Cfg#eldap_config.dn,
           password = Cfg#eldap_config.password,
           base = Cfg#eldap_config.base,
           deref_aliases = Cfg#eldap_config.deref_aliases,
           uids = UIDs, vcard_map = VCardMap,
           vcard_map_attrs = VCardMapAttrs,
           user_filter = UserFilter, search_filter = SearchFilter,
           search_fields = SearchFields,
           binary_search_fields = BinaryFields,
           search_reported = SearchReported,
           search_reported_attrs = SearchReportedAttrs,
           search_operator = SearchOperator,
           matches = Matches}.

check_filter(F) ->
    NewF = iolist_to_binary(F),
    {ok, _} = eldap_filter:parse(NewF),
    NewF.
