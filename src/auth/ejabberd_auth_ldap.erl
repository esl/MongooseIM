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
-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         start_link/1,
         set_password/3,
         authorize/1,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         does_user_exist/2,
         remove_user/2,
         remove_user/3,
         supports_sasl_module/2
        ]).

%% Internal
-export([check_password/3,
         check_password/5]).

-export([config_change/4]).

-include("mongoose.hrl").
-include("eldap.hrl").

-record(state,
       {host = <<"">>          :: jid:lserver(),
        eldap_id               :: {jid:lserver(), binary()},
        bind_eldap_id          :: {jid:lserver(), binary()},
        base = <<"">>          :: binary(),
        uids = []              :: [{binary()} | {binary(), binary()}],
        ufilter = <<"">>       :: binary(),
        sfilter = <<"">>       :: binary(),
        lfilter                :: {any(), any()} | undefined,
        deref =                neverDerefAliases  :: neverDerefAliases |
                                                     derefInSearching |
                                                     derefFindingBaseObj |
                                                     derefAlways,
        dn_filter              :: binary() | undefined,
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

-spec start(Host :: jid:lserver()) -> ok.
start(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host]},
                 transient, 1000, worker, [?MODULE]},
    ejabberd_hooks:add(host_config_update, Host, ?MODULE, config_change, 50),
    ejabberd_sup:start_child(ChildSpec),
    ok.

-spec stop(Host :: jid:lserver()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(host_config_update, Host, ?MODULE, config_change, 50),
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    ejabberd_sup:stop_child(Proc),
    ok.

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

terminate(_Reason, _State) -> ok.

-spec init(Host :: jid:lserver()) -> {'ok', state()}.
init(Host) ->
    State = parse_options(Host),
    {ok, State}.

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, cyrsasl_plain) -> true;
supports_sasl_module(_, cyrsasl_external) -> true;
supports_sasl_module(_, _) -> false.

config_change(Acc, Host, ldap, _NewConfig) ->
    stop(Host),
    start(Host),
    Acc;
config_change(Acc, _, _, _) ->
    Acc.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    case mongoose_credentials:get(Creds, cert_file, false) of
        true -> verify_user_exists(Creds);
        false -> ejabberd_auth:authorize_with_check_password(?MODULE, Creds)
    end.

-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(_LUser, _LServer, <<"">>) -> false;
check_password(LUser, LServer, Password) ->
    case catch check_password_ldap(LUser, LServer, Password) of
        {'EXIT', _} -> false;
        Result -> Result
    end.

-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(LUser, LServer, Password, _Digest,
               _DigestGen) ->
    check_password(LUser, LServer, Password).


-spec set_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary())
      -> ok | {error, not_allowed | invalid_jid}.
set_password(LUser, LServer, Password) ->
    {ok, State} = eldap_utils:get_state(LServer, ?MODULE),
    case find_user_dn(LUser, State) of
      false -> {error, user_not_found};
      DN ->
          eldap_pool:modify_passwd(State#state.eldap_id, DN,
                                   Password)
    end.


-spec try_register(LUser :: jid:luser(), LServer :: jid:lserver(),
                   Password :: binary()) -> ok | {error, exists}.
try_register(LUser, LServer, Password) ->
    {ok, State} = eldap_utils:get_state(LServer, ?MODULE),
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


-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    LServers = ejabberd_config:get_vh_by_auth_method(ldap),
    lists:flatmap(fun (LServer) ->
                          get_vh_registered_users(LServer)
                  end,
                  LServers).


-spec get_vh_registered_users(LServer :: jid:lserver()
                             ) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer) ->
    case catch get_vh_registered_users_ldap(LServer) of
      {'EXIT', _} -> [];
      Result -> Result
    end.


-spec get_vh_registered_users(LServer :: jid:lserver(),
                              Opts :: list()) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer, _) ->
    get_vh_registered_users(LServer).


-spec get_vh_registered_users_number(LServer :: jid:lserver()) -> integer().
get_vh_registered_users_number(LServer) ->
    length(get_vh_registered_users(LServer)).


-spec get_vh_registered_users_number(LServer :: jid:lserver(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(LServer, _) ->
    get_vh_registered_users_number(LServer).


-spec get_password(LUser :: jid:luser(),
                   LServer :: jid:lserver()) -> binary() | false.
get_password(_LUser, _LServer) -> false.


-spec get_password_s(LUser :: jid:luser(),
                     LServer :: jid:lserver()) -> binary().
get_password_s(_LUser, _LServer) -> <<"">>.


-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver()
                     ) -> boolean() | {error, atom()}.
does_user_exist(LUser, LServer) ->
    case catch is_user_exists_ldap(LUser, LServer) of
      {'EXIT', Error} -> {error, Error};
      Result -> Result
    end.


-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver()
                  ) -> ok | {error, not_allowed}.
remove_user(LUser, LServer) ->
    {ok, State} = eldap_utils:get_state(LServer, ?MODULE),
    case find_user_dn(LUser, State) of
      false -> {error, not_allowed};
      DN -> eldap_pool:delete(State#state.eldap_id, DN)
    end.


-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver(),
                  Password :: binary()
                  ) -> ok | {error, not_exists | not_allowed}.
remove_user(LUser, LServer, Password) ->
    {ok, State} = eldap_utils:get_state(LServer, ?MODULE),
    case find_user_dn(LUser, State) of
      false -> {error, not_exists};
      DN ->
            case eldap_pool:bind(State#state.bind_eldap_id, DN, Password) of
                ok -> ok = eldap_pool:delete(State#state.eldap_id, DN);
                _ -> {error, not_allowed}
            end
    end.


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
            case does_user_exist(LUser, LServer) of
                true -> {ok, mongoose_credentials:extend(Creds, [{auth_module, ?MODULE}])};
                false -> {error, not_authorized}
            end
    end.

-spec check_password_ldap(LUser :: jid:luser(),
                          LServer :: jid:lserver(),
                          Password :: binary()) -> boolean().
check_password_ldap(LUser, LServer, Password) ->
    {ok, State} = eldap_utils:get_state(LServer, ?MODULE),
    case find_user_dn(LUser, State) of
      false -> false;
      DN ->
          case eldap_pool:bind(State#state.bind_eldap_id, DN, Password) of
            ok -> true;
            _ -> false
          end
    end.


-spec get_vh_registered_users_ldap(LServer :: jid:lserver()
                                  ) -> [jid:simple_bare_jid()].
get_vh_registered_users_ldap(LServer) ->
    {ok, State} = eldap_utils:get_state(LServer, ?MODULE),
    UIDs = State#state.uids,
    EldapID = State#state.eldap_id,
    LServer = State#state.host,
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
              case is_valid_dn(DN, Attrs, State) of
                  false -> [];
                  _ ->
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

-spec is_user_exists_ldap(LUser :: jid:luser(),
                          LServer :: jid:lserver()) -> boolean().
is_user_exists_ldap(LUser, LServer) ->
    {ok, State} = eldap_utils:get_state(LServer, ?MODULE),
    case find_user_dn(LUser, State) of
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
                   State :: state()) -> 'false' | binary().
find_user_dn(LUser, State) ->
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(State#state.ufilter,
                            [{<<"%u">>, LUser}])
        of
      {ok, Filter} ->
          case eldap_pool:search(State#state.eldap_id,
                                 [{base, State#state.base}, {filter, Filter},
                                  {deref, State#state.deref},
                                  {attributes, ResAttrs}])
              of
            #eldap_search_result{entries =
                                     [#eldap_entry{attributes = Attrs,
                                                   object_name = DN}
                                      | _]} ->
                dn_filter(DN, Attrs, State);
            _ -> false
          end;
      _ -> false
    end.


%% @doc apply the dn filter and the local filter:
-spec dn_filter(DN :: binary(),
                Attrs :: [{binary(), [any()]}],
                State :: state()) -> 'false' | binary().
dn_filter(DN, Attrs, State) ->
    case check_local_filter(Attrs, State) of
      false -> false;
      true -> is_valid_dn(DN, Attrs, State)
    end.


%% @doc Check that the DN is valid, based on the dn filter
-spec is_valid_dn(DN :: binary(),
                  Attrs :: [{binary(), [any()]}],
                  State :: state()) -> 'false' | binary().
is_valid_dn(DN, _, #state{dn_filter = undefined}) -> DN;
is_valid_dn(DN, Attrs, State) ->
    DNAttrs = State#state.dn_filter_attrs,
    UIDs = State#state.uids,
    Values = [{<<"%s">>,
               eldap_utils:get_ldap_attr(Attr, Attrs), 1}
              || Attr <- DNAttrs],
    SubstValues = case eldap_utils:find_ldap_attrs(UIDs,
                                                   Attrs)
                      of
                    <<"">> -> Values;
                    {S, UAF} ->
                        case eldap_utils:get_user_part(S, UAF) of
                          {ok, U} -> [{<<"%u">>, U} | Values];
                          _ -> Values
                        end
                  end
                    ++ [{<<"%d">>, State#state.host}, {<<"%D">>, DN}],
    case eldap_filter:parse(State#state.dn_filter,
                            SubstValues)
        of
      {ok, EldapFilter} ->
          case eldap_pool:search(State#state.eldap_id,
                                 [{base, State#state.base},
                                  {filter, EldapFilter},
                                  {deref, State#state.deref},
                                  {attributes, [<<"dn">>]}])
              of
            #eldap_search_result{entries = [_ | _]} -> DN;
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

-spec parse_options(Host :: jid:lserver()) -> state().
parse_options(Host) ->
    Opts = ejabberd_config:get_local_option_or_default({auth_opts, Host}, []),
    EldapID = eldap_utils:get_mod_opt(ldap_pool_tag, Opts,
                                      fun(A) when is_atom(A) -> A end, default),
    BindEldapID = eldap_utils:get_mod_opt(ldap_bind_pool_tag, Opts,
                                          fun(A) when is_atom(A) -> A end, bind),
    Base = eldap_utils:get_base(Opts),
    DerefAliases = eldap_utils:get_deref_aliases(Opts),
    UIDs = eldap_utils:get_uids(Host, Opts),
    UserFilter = eldap_utils:get_user_filter(UIDs, Opts),
    SearchFilter = eldap_utils:get_search_filter(UserFilter),
    {DNFilter, DNFilterAttrs} = eldap_utils:get_dn_filter_with_attrs(Opts),
    LocalFilter = eldap_utils:get_mod_opt(ldap_local_filter, Opts),
    #state{host = Host,
           eldap_id = {Host, EldapID},
           bind_eldap_id = {Host, BindEldapID},
           base = Base,
           deref = DerefAliases,
           uids = UIDs, ufilter = UserFilter,
           sfilter = SearchFilter, lfilter = LocalFilter,
           dn_filter = DNFilter, dn_filter_attrs = DNFilterAttrs}.
