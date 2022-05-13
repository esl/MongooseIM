%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_auth_ldap).
-author('alexey@process-one.net').

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3,
         handle_cast/2, terminate/2, code_change/3]).

%% External exports
-behaviour(mongoose_gen_auth).

-export([start/1,
         stop/1,
         config_spec/0,
         start_link/1,
         set_password/4,
         authorize/1,
         try_register/4,
         get_registered_users/3,
         get_registered_users_number/3,
         does_user_exist/3,
         remove_user/3,
         supports_sasl_module/2,
         supported_features/0
        ]).

%% Internal
-export([check_password/4,
         check_password/6]).

-ignore_xref([start_link/1]).

-include("mongoose_config_spec.hrl").
-include_lib("eldap/include/eldap.hrl").

-record(state,
       {host_type              :: mongooseim:host_type(),
        eldap_id               :: eldap_utils:eldap_id(),
        bind_eldap_id          :: eldap_utils:eldap_id(),
        base = <<>>            :: binary(),
        uids = []              :: [{binary()} | {binary(), binary()}],
        ufilter = <<>>         :: binary(),
        sfilter = <<>>         :: binary(),
        lfilter                :: {any(), any()} | undefined,
        deref = neverDerefAliases  :: eldap_utils:deref(),
        dn_filter              :: eldap_utils:dn() | undefined,
        dn_filter_attrs = []   :: [binary()]
       }).
-type state() :: #state{}.

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

-define(LDAP_SEARCH_TIMEOUT, 5).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type()) -> ok.
start(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [HostType]},
                 transient, 1000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(Proc, stop),
    ejabberd_sup:stop_child(Proc),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    CommonLDAPSpec = mongoose_ldap_config:spec(),
    Items = #{<<"bind_pool_tag">> => #option{type = atom,
                                             validate = non_empty},
              <<"uids">> => #list{items = mongoose_ldap_config:uids()},
              <<"dn_filter">> => mongoose_ldap_config:dn_filter(),
              <<"local_filter">> => mongoose_ldap_config:local_filter()},
    Defaults = #{<<"bind_pool_tag">> => bind,
                 <<"base">> => <<>>,
                 <<"uids">> => [{<<"uid">>, <<"%u">>}],
                 <<"dn_filter">> => {undefined, []},
                 <<"local_filter">> => undefined},
    CommonLDAPSpec#section{items = maps:merge(CommonLDAPSpec#section.items, Items),
                           defaults = maps:merge(CommonLDAPSpec#section.defaults, Defaults)}.

-spec start_link(HostType :: mongooseim:host_type()) -> {ok, pid()} | {error, any()}.
start_link(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, HostType, []).

terminate(_Reason, _State) -> ok.

-spec init(HostType :: mongooseim:host_type()) -> {'ok', state()}.
init(HostType) ->
    State = parse_options(HostType),
    {ok, State}.

-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, cyrsasl_plain) -> true;
supports_sasl_module(_, cyrsasl_external) -> true;
supports_sasl_module(_, _) -> false.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    case mongoose_credentials:get(Creds, cert_file, false) of
        true -> verify_user_exists(Creds);
        false -> ejabberd_auth:authorize_with_check_password(?MODULE, Creds)
    end.

-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(_HostType, _LUser, _LServer, <<"">>) -> false;
check_password(HostType, LUser, LServer, Password) ->
    case catch check_password_ldap(HostType, LUser, LServer, Password) of
        {'EXIT', _} -> false;
        Result -> Result
    end.

-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(HostType, LUser, LServer, Password, _Digest,
               _DigestGen) ->
    check_password(HostType, LUser, LServer, Password).


-spec set_password(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary())
      -> ok | {error, not_allowed | invalid_jid | user_not_found}.
set_password(HostType, LUser, LServer, Password) ->
    {ok, State} = eldap_utils:get_state(HostType, ?MODULE),
    case find_user_dn(LUser, LServer, State) of
      false -> {error, user_not_found};
      DN ->
          eldap_pool:modify_passwd(State#state.eldap_id, DN, Password)
    end.

%% TODO Support multiple domains
-spec try_register(HostType :: mongooseim:host_type(), LUser :: jid:luser(),
                   LServer :: jid:lserver(), Password :: binary()) ->
    ok | {error, exists}.
try_register(HostType, LUser, _LServer, Password) ->
    {ok, State} = eldap_utils:get_state(HostType, ?MODULE),
    UserStr = binary_to_list(LUser),
    DN = "cn=" ++ UserStr ++ ", " ++ binary_to_list(State#state.base),
    Attrs =   [{"objectclass", ["inetOrgPerson"]},
              {"cn", [UserStr]},
              {"sn", [UserStr]},
              {"userPassword", [binary_to_list(Password)]},
              {"uid", [UserStr]}],
    case eldap_pool:add(State#state.eldap_id, DN, Attrs) of
        ok -> ok;
        _ -> {error, exists}
    end.

-spec get_registered_users(HostType :: mongooseim:host_type(),
                           LServer :: jid:lserver(),
                           Opts :: list()) -> [jid:simple_bare_jid()].
get_registered_users(HostType, LServer, _) ->
    case catch get_registered_users_ldap(HostType, LServer) of
      {'EXIT', _} -> [];
      Result -> Result
    end.


-spec get_registered_users_number(HostType :: mongooseim:host_type(),
                                  LServer :: jid:lserver(),
                                  Opts :: list()) -> non_neg_integer().
get_registered_users_number(HostType, LServer, Opts) ->
    length(get_registered_users(HostType, LServer, Opts)).


-spec does_user_exist(HostType :: mongooseim:host_type(),
                      LUser :: jid:luser(),
                      LServer :: jid:lserver()) -> boolean() | {error, atom()}.
does_user_exist(HostType, LUser, LServer) ->
    case catch does_user_exist_in_ldap(HostType, LUser, LServer) of
      {'EXIT', Error} -> {error, Error};
      Result -> Result
    end.


-spec remove_user(HostType :: mongooseim:host_type(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver()) -> ok | {error, not_allowed}.
remove_user(HostType, LUser, LServer) ->
    {ok, State} = eldap_utils:get_state(HostType, ?MODULE),
    case find_user_dn(LUser, LServer, State) of
      false -> {error, not_allowed};
      DN -> eldap_pool:delete(State#state.eldap_id, DN)
    end.

%% Multiple domains are not supported for in-band registration
-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec verify_user_exists(mongoose_credentials:t()) ->
                                {ok, mongoose_credentials:t()} | {error, not_authorized}.
verify_user_exists(Creds) ->
    User = mongoose_credentials:get(Creds, username),
    case jid:nodeprep(User) of
        error ->
            error({nodeprep_error, User});
        LUser ->
            LServer = mongoose_credentials:lserver(Creds),
            HostType = mongoose_credentials:host_type(Creds),
            case does_user_exist(HostType, LUser, LServer) of
                true -> {ok, mongoose_credentials:extend(Creds, [{auth_module, ?MODULE}])};
                false -> {error, not_authorized}
            end
    end.

-spec check_password_ldap(HostType :: mongooseim:host_type(),
                          LUser :: jid:luser(),
                          LServer :: jid:lserver(),
                          Password :: binary()) -> boolean().
check_password_ldap(HostType, LUser, LServer, Password) ->
    {ok, State} = eldap_utils:get_state(HostType, ?MODULE),
    case find_user_dn(LUser, LServer, State) of
      false -> false;
      DN ->
          case eldap_pool:bind(State#state.bind_eldap_id, DN, Password) of
            ok -> true;
            _ -> false
          end
    end.


-spec get_registered_users_ldap(mongooseim:host_type(), jid:lserver()) -> [jid:simple_bare_jid()].
get_registered_users_ldap(HostType, LServer) ->
    {ok, State} = eldap_utils:get_state(HostType, ?MODULE),
    UIDs = State#state.uids,
    EldapID = State#state.eldap_id,
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(State#state.sfilter) of
      {ok, EldapFilter} ->
          case eldap_pool:search(EldapID,
                                 [{base, State#state.base},
                                  {filter, EldapFilter},
                                  {timeout, ?LDAP_SEARCH_TIMEOUT},
                                  {deref, State#state.deref},
                                  {attributes, ResAttrs}]) of
              #eldap_search_result{entries = Entries} ->
                  get_users_from_ldap_entries(Entries, UIDs, LServer, State);
              _ -> []
          end;
      _ -> []
    end.

-spec get_users_from_ldap_entries(list(), [{binary()} | {binary(), binary()}],
                                  jid:lserver(), state()) -> list().
get_users_from_ldap_entries(LDAPEntries, UIDs, LServer, State) ->
    lists:flatmap(
      fun(#eldap_entry{attributes = Attrs,
                       object_name = DN}) ->
              case is_valid_dn(DN, LServer, Attrs, State) of
                  false -> [];
                  true ->
                      get_user_from_ldap_attributes(UIDs, Attrs, LServer)
              end
      end,
      LDAPEntries).

-spec get_user_from_ldap_attributes([{binary()} | {binary(), binary()}],
                                    [{binary(), [binary()]}], jid:lserver())
                                   -> list().
get_user_from_ldap_attributes(UIDs, Attributes, LServer) ->
    case eldap_utils:find_ldap_attrs(UIDs, Attributes) of
        <<"">> -> [];
        {User, UIDFormat} ->
            case eldap_utils:get_user_part(User, UIDFormat) of
                {ok, U} ->
                    [{U, LServer}];
                _ -> []
            end
    end.

-spec does_user_exist_in_ldap(HostType :: mongooseim:host_type(),
                              LUser :: jid:luser(),
                              LServer :: jid:lserver()) -> boolean().
does_user_exist_in_ldap(HostType, LUser, LServer) ->
    {ok, State} = eldap_utils:get_state(HostType, ?MODULE),
    case find_user_dn(LUser, LServer, State) of
      false -> false;
      _DN -> true
    end.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

-spec find_user_dn(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   State :: state()) -> false | eldap_utils:dn().
find_user_dn(LUser, LServer, State) ->
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(State#state.ufilter, [{<<"%u">>, LUser}]) of
      {ok, Filter} ->
          SearchOpts = find_user_opts(Filter, ResAttrs, State),
          case eldap_pool:search(State#state.eldap_id, SearchOpts) of
            #eldap_search_result{entries =
                                     [#eldap_entry{attributes = Attrs,
                                                   object_name = DN}
                                      | _]} ->
                dn_filter(DN, LServer, Attrs, State);
            _ -> false
          end;
      _ -> false
    end.

find_user_opts(Filter, ResAttrs, State) ->
    [{base, State#state.base}, {filter, Filter},
     {deref, State#state.deref}, {attributes, ResAttrs}].


%% @doc apply the dn filter and the local filter:
-spec dn_filter(DN :: eldap_utils:dn(),
                LServer :: jid:lserver(),
                Attrs :: [{binary(), [any()]}],
                State :: state()) -> false | eldap_utils:dn().
dn_filter(DN, LServer, Attrs, State) ->
    case check_local_filter(Attrs, State) of
      false -> false;
      true ->
            case is_valid_dn(DN, LServer, Attrs, State) of
                true -> DN;
                false -> false
            end
    end.

%% @doc Check that the DN is valid, based on the dn filter
-spec is_valid_dn(DN :: eldap_utils:dn(),
                  LServer :: jid:lserver(),
                  Attrs :: [{binary(), [any()]}],
                  State :: state()) -> boolean().
is_valid_dn(_DN, _LServer, _, #state{dn_filter = undefined}) -> true;
is_valid_dn(DN, LServer, Attrs, State) ->
    DNAttrs = State#state.dn_filter_attrs,
    UIDs = State#state.uids,
    Values = [{<<"%s">>, eldap_utils:get_ldap_attr(Attr, Attrs), 1}
              || Attr <- DNAttrs],
    SubstValues = case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
                    <<>> -> Values;
                    {S, UAF} ->
                        case eldap_utils:get_user_part(S, UAF) of
                          {ok, U} -> [{<<"%u">>, U} | Values];
                          _ -> Values
                        end
                  end ++ [{<<"%d">>, LServer}, {<<"%D">>, DN}],
    case eldap_filter:parse(State#state.dn_filter, SubstValues) of
      {ok, EldapFilter} ->
          case eldap_pool:search(State#state.eldap_id,
                                 [{base, State#state.base},
                                  {filter, EldapFilter},
                                  {deref, State#state.deref},
                                  {attributes, [<<"dn">>]}])
              of
            #eldap_search_result{entries = [_ | _]} -> true;
            _ -> false
          end;
      _ -> false
    end.


%% @doc The local filter is used to check an attribute in ejabberd
%% and not in LDAP to limit the load on the LDAP directory.
%% A local rule can be either:
%%    {equal, {"accountStatus", ["active"]}}
%%    {notequal, {"accountStatus", ["disabled"]}}
%% {ldap_local_filter, {notequal, {"accountStatus", ["disabled"]}}}
-spec check_local_filter(Attrs :: [{binary(), [any()]}],
                         State :: state()) -> boolean().
check_local_filter(_Attrs,
                   #state{lfilter = undefined}) ->
    true;
check_local_filter(Attrs,
                   #state{lfilter = LocalFilter}) ->
    {Operation, FilterMatch} = LocalFilter,
    local_filter(Operation, Attrs, FilterMatch).


-spec local_filter('equal' | 'notequal',
                   Attrs :: [{binary(), [any()]}],
                   FilterMatch :: {_, _}) -> boolean().
local_filter(equal, Attrs, FilterMatch) ->
    {Attr, Value} = FilterMatch,
    case lists:keysearch(Attr, 1, Attrs) of
      false -> false;
      {value, {Attr, Value}} -> true;
      _ -> false
    end;
local_filter(notequal, Attrs, FilterMatch) ->
    not local_filter(equal, Attrs, FilterMatch).


-spec result_attrs(state()) -> maybe_improper_list().
result_attrs(#state{uids = UIDs,
                    dn_filter_attrs = DNFilterAttrs}) ->
    lists:foldl(fun ({UID}, Acc) -> [UID | Acc];
                    ({UID, _}, Acc) -> [UID | Acc]
                end,
                DNFilterAttrs, UIDs).

%%%----------------------------------------------------------------------
%%% Auxiliary functions
%%%----------------------------------------------------------------------

-spec parse_options(HostType :: mongooseim:host_type()) -> state().
parse_options(HostType) ->
    Opts = mongoose_config:get_opt([{auth, HostType}, ldap]),
    #{pool_tag := PoolTag,
      bind_pool_tag := BindPoolTag,
      base := Base,
      deref := Deref,
      uids := RawUIDs,
      filter := RawUserFilter,
      dn_filter := {DNFilter, DNFilterAttrs},
      local_filter := LocalFilter} = Opts,
    EldapID = {HostType, PoolTag},
    BindEldapID = {HostType, BindPoolTag},
    DerefAliases = eldap_utils:deref_aliases(Deref),
    UIDs = eldap_utils:uids_domain_subst(HostType, RawUIDs),
    UserFilter = eldap_utils:process_user_filter(UIDs, RawUserFilter),
    SearchFilter = eldap_utils:get_search_filter(UserFilter),
    #state{host_type = HostType,
           eldap_id = EldapID,
           bind_eldap_id = BindEldapID,
           base = Base,
           deref = DerefAliases,
           uids = UIDs,
           ufilter = UserFilter,
           sfilter = SearchFilter,
           lfilter = LocalFilter,
           dn_filter = DNFilter,
           dn_filter_attrs = DNFilterAttrs}.
