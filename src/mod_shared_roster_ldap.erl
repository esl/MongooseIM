%%%-------------------------------------------------------------------
%%% File    : mod_shared_roster_ldap.erl
%%% Author  : Realloc <realloc@realloc.spb.ru>
%%%           Marcin Owsiany <marcin@owsiany.pl>
%%%           Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : LDAP shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_shared_roster_ldap).

-behaviour(gen_server).

-behaviour(gen_mod).

-behaviour(mongoose_module_metrics).

%% API
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([get_user_roster/2, get_subscription_lists/3,
         get_jid_info/4, process_item/2, in_subscription/6,
         out_subscription/5]).

-export([config_change/4]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").

-include("eldap.hrl").

-define(CACHE_SIZE, 1000).

-define(USER_CACHE_VALIDITY, 300).

-define(GROUP_CACHE_VALIDITY, 300).

-define(LDAP_SEARCH_TIMEOUT, 5).

-record(state,
        {host = <<"">>                                :: binary(),
         eldap_id                                     :: {jid:lserver(), binary()},
         base = <<"">>                                :: binary(),
         uid = <<"">>                                 :: binary(),
         deref =                                         neverDerefAliases  :: neverDerefAliases |
                                                         derefInSearching |
                                                         derefFindingBaseObj |
                                                         derefAlways,
         group_attr = <<"">>                          :: binary(),
         group_desc = <<"">>                          :: binary(),
         user_desc = <<"">>                           :: binary(),
         user_uid = <<"">>                            :: binary(),
         uid_format = <<"">>                          :: binary(),
         uid_format_re = <<"">>                       :: binary(),
         filter = <<"">>                              :: binary(),
         ufilter = <<"">>                             :: binary(),
         rfilter = <<"">>                             :: binary(),
         gfilter = <<"">>                             :: binary(),
         auth_check = true                            :: boolean(),
         user_cache_size = ?CACHE_SIZE                :: non_neg_integer(),
         group_cache_size = ?CACHE_SIZE               :: non_neg_integer(),
         user_cache_validity = ?USER_CACHE_VALIDITY   :: non_neg_integer(),
         group_cache_validity = ?GROUP_CACHE_VALIDITY :: non_neg_integer()}).

-record(group_info, {desc, members}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
                          [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
                 permanent, 1000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ejabberd_sup:stop_child(Proc).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
get_user_roster(Acc, {U, S} = US) ->
    Items = mongoose_acc:get(roster, items, [], Acc),
    SRUsers = get_user_to_groups_map(US, true),
    {NewItems1, SRUsersRest} =
        lists:mapfoldl(
          fun (Item, SRUsers1) ->
                  {_, _, {U1, S1, _}} = Item#roster.usj,
                  US1 = {U1, S1},
                  case dict:find(US1, SRUsers1) of
                      {ok, _GroupNames} ->
                          {Item#roster{subscription = both, ask = none},
                           dict:erase(US1, SRUsers1)};
                      error ->
                          {Item, SRUsers1}
                  end
          end,
          SRUsers, Items),
    SRItems = [#roster{usj = {U, S, {U1, S1, <<"">>}},
                       us = US, jid = {U1, S1, <<"">>},
                       name = get_user_name(U1, S1), subscription = both,
                       ask = none, groups = GroupNames}
               || {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    mongoose_acc:set(roster, items, SRItems ++ NewItems1, Acc).

%% This function in use to rewrite the roster entries when moving or renaming
%% them in the user contact list.
process_item(RosterItem, _Host) ->
    USFrom = RosterItem#roster.us,
    {User, Server, _Resource} = RosterItem#roster.jid,
    USTo = {User, Server},
    Map = get_user_to_groups_map(USFrom, false),
    case dict:find(USTo, Map) of
        error -> RosterItem;
        {ok, []} -> RosterItem;
        {ok, GroupNames}
          when RosterItem#roster.subscription == remove ->
            RosterItem#roster{subscription = both, ask = none,
                              groups = GroupNames};
        _ -> RosterItem#roster{subscription = both, ask = none}
    end.

get_subscription_lists(Acc, User, Server) ->
    {F, T, P} = mongoose_acc:get(roster, subscription_lists, {[], [], []}, Acc),
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:usort(lists:flatmap(fun (Group) ->
                                                get_group_users(LServer, Group)
                                        end,
                                        DisplayedGroups)),
    SRJIDs = [{U1, S1, <<"">>} || {U1, S1} <- SRUsers],
    NewLists = {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T), P},
    mongoose_acc:set(roster, subscription_lists, NewLists, Acc).

get_jid_info({Subscription, Groups}, User, Server, JID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jid:to_lower(JID),
    US1 = {U1, S1},
    SRUsers = get_user_to_groups_map(US, false),
    case dict:find(US1, SRUsers) of
        {ok, GroupNames} ->
            NewGroups = case Groups of
                            [] -> GroupNames;
                            _ -> Groups
                        end,
            {both, NewGroups};
        error -> {Subscription, Groups}
    end.

-spec in_subscription(Acc:: mongoose_acc:t(),
                      User :: binary(),
                      Server :: binary(),
                      JID ::jid:jid(),
                      Type :: mod_roster:sub_presence(),
                      _Reason :: any()) ->
    mongoose_acc:t() | {stop, mongoose_acc:t()}.
in_subscription(Acc, User, Server, JID, Type, _Reason) ->
    case process_subscription(in, User, Server, JID, Type) of
        stop ->
            {stop, Acc};
        {stop, false} ->
            {stop, mongoose_acc:set(hook, result, false, Acc)};
        _ -> Acc
    end.

-spec out_subscription(Acc:: mongoose_acc:t(),
                      User :: binary(),
                      Server :: binary(),
                      JID ::jid:jid(),
                      Type :: mod_roster:sub_presence()) ->
    mongoose_acc:t() | {stop, mongoose_acc:t()}.
out_subscription(Acc, User, Server, JID, Type) ->
    case process_subscription(out, User, Server, JID, Type) of
        stop ->
            {stop, Acc};
        {stop, false} ->
            {stop, Acc};
         false -> Acc
    end.

process_subscription(Direction, User, Server, JID, _Type) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jid:to_lower(jid:to_bare(JID)),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:usort(lists:flatmap(
                            fun (Group) ->
                                    get_group_users(LServer, Group)
                            end,
                            DisplayedGroups)),
    case lists:member(US1, SRUsers) of
        true ->
            case Direction of
                in -> {stop, false};
                out -> stop
            end;
        false -> false
    end.

%%====================================================================
%% config change hook
%%====================================================================
%% react to "global" config change
config_change(Acc, Host, ldap, _NewConfig) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    Mods = ejabberd_config:get_local_option({modules, Host}),
    Opts = proplists:get_value(?MODULE, Mods, []),
    ok = gen_server:call(Proc, {new_config, Host, Opts}),
    Acc;
config_change(Acc, _, _, _) ->
    Acc.


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    State = parse_options(Host, Opts),
    process_flag(trap_exit, true),
    cache_tab:new(shared_roster_ldap_user,
                  [{max_size, State#state.user_cache_size}, {lru, false},
                   {life_time, State#state.user_cache_validity}]),
    cache_tab:new(shared_roster_ldap_group,
                  [{max_size, State#state.group_cache_size}, {lru, false},
                   {life_time, State#state.group_cache_validity}]),
    ejabberd_hooks:add(host_config_update, Host, ?MODULE,
                       config_change, 50),
    ejabberd_hooks:add(roster_get, Host, ?MODULE,
                       get_user_roster, 70),
    ejabberd_hooks:add(roster_in_subscription, Host, ?MODULE,
                       in_subscription, 30),
    ejabberd_hooks:add(roster_out_subscription, Host, ?MODULE,
                       out_subscription, 30),
    ejabberd_hooks:add(roster_get_subscription_lists, Host, ?MODULE,
                       get_subscription_lists, 70),
    ejabberd_hooks:add(roster_get_jid_info, Host, ?MODULE,
                       get_jid_info, 70),
    ejabberd_hooks:add(roster_process_item, Host, ?MODULE,
                       process_item, 50),
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(host_config_update, Host, ?MODULE, config_change, 50),
    ejabberd_hooks:delete(roster_get, Host, ?MODULE,
                          get_user_roster, 70),
    ejabberd_hooks:delete(roster_in_subscription, Host,
                          ?MODULE, in_subscription, 30),
    ejabberd_hooks:delete(roster_out_subscription, Host,
                          ?MODULE, out_subscription, 30),
    ejabberd_hooks:delete(roster_get_subscription_lists,
                          Host, ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
                          ?MODULE, get_jid_info, 70),
    ejabberd_hooks:delete(roster_process_item, Host,
                          ?MODULE, process_item, 50).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% For a given user, map all his shared roster contacts to groups they are
%% members of. Skip the user himself iff SkipUS is true.
get_user_to_groups_map({_, Server} = US, SkipUS) ->
    DisplayedGroups = get_user_displayed_groups(US),
    %% Pass given FilterParseArgs to eldap_filter:parse, and if successful, run and
    %% return the resulting filter, retrieving given AttributesList. Return the
    %% result entries. On any error silently return an empty list of results.
    %%
    %% Eldap server ID and base DN for the query are both retrieved from the State
    %% record.
    lists:foldl(fun (Group, Dict1) ->
                        GroupName = get_group_name(Server, Group),
                        lists:foldl(fun (Contact, Dict) when SkipUS, Contact == US ->
                                            Dict;
                                        (Contact, Dict) ->
                                            dict:append(Contact, GroupName, Dict)
                                    end,
                                    Dict1, get_group_users(Server, Group))
                end,
                dict:new(), DisplayedGroups).

eldap_search(State, FilterParseArgs, AttributesList) ->
    case apply(eldap_filter, parse, FilterParseArgs) of
        {ok, EldapFilter} ->
            case eldap_pool:search(State#state.eldap_id,
                                   [{base, State#state.base},
                                    {filter, EldapFilter},
                                    {timeout, ?LDAP_SEARCH_TIMEOUT},
                                    {deref, State#state.deref},
                                    {attributes, AttributesList}])
            of
                #eldap_search_result{entries = Es} ->
                    %% A result with entries. Return their list.
                    Es;
                _ ->
                    %% Something else. Pretend we got no results.
                    []
            end;
        _->
            %% Filter parsing failed. Pretend we got no results.
            []
    end.

get_user_displayed_groups({User, Host}) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    GroupAttr = State#state.group_attr,
    Entries = eldap_search(State,
                           [eldap_filter:do_sub(State#state.rfilter, [{<<"%u">>, User}])],
                           [GroupAttr]),
    Reply = lists:flatmap(fun (#eldap_entry{attributes = Attrs}) ->
                                  case eldap_utils:singleton_value(Attrs) of
                                      {GroupAttr, Value} -> [eldap_utils:maybe_list2b(Value)];
                                      _ -> []
                                  end
                          end,
                          Entries),
    lists:usort(Reply).

get_group_users(Host, Group) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case cache_tab:dirty_lookup(shared_roster_ldap_group,
                                {Group, Host},
                                fun () -> search_group_info(State, Group) end)
    of
        {ok, #group_info{members = Members}}
          when Members /= undefined ->
            Members;
        _ -> []
    end.

get_group_name(Host, Group) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case cache_tab:dirty_lookup(shared_roster_ldap_group,
                                {Group, Host},
                                fun () -> search_group_info(State, Group) end)
    of
        {ok, #group_info{desc = GroupName}}
          when GroupName /= undefined ->
            GroupName;
        _ -> Group
    end.

get_user_name(User, Host) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case cache_tab:dirty_lookup(shared_roster_ldap_user,
                                {User, Host},
                                fun () -> search_user_name(State, User) end)
    of
        {ok, UserName} -> UserName;
        error -> User
    end.

search_group_info(State, Group) ->
    Extractor = case State#state.uid_format_re of
                    <<"">> ->
                        fun (UID) ->
                                catch eldap_utils:get_user_part(
                                        UID,
                                        State#state.uid_format)
                        end;
                    _ ->
                        fun (UID) ->
                                catch get_user_part_re(
                                        UID,
                                        State#state.uid_format_re)
                        end
                end,
    AuthChecker = case State#state.auth_check of
                      true -> fun ejabberd_auth:is_user_exists/2;
                      _ -> fun (_U, _S) -> true end
                  end,
    Host = State#state.host,
    case eldap_search(State,
                      [eldap_filter:do_sub(State#state.gfilter,
                                           [{<<"%g">>, Group}])],
                      [State#state.group_attr, State#state.group_desc,
                       State#state.uid]) of
        [] ->
            error;
        LDAPEntries ->
            {GroupDesc, MembersLists} = ldap_entries_to_group(LDAPEntries, Host, Group, State,
                                                             Extractor, AuthChecker),
            {ok, #group_info{desc = GroupDesc, members = lists:usort(MembersLists)}}
    end.

ldap_entries_to_group(LDAPEntries, Host, Group, State, Extractor, AuthChecker) ->
    ldap_entries_to_group(LDAPEntries, Host, Group, [], State, Extractor, AuthChecker).

ldap_entries_to_group([#eldap_entry{ attributes = Attrs } | REntries], Host,
                      DescAcc, JIDsAcc, State, Extractor, AuthChecker) ->
    UID = lists:keysearch(State#state.uid, 1, Attrs),
    ListUID = State#state.uid,
    case {eldap_utils:get_ldap_attr(State#state.group_attr, Attrs),
          eldap_utils:get_ldap_attr(State#state.group_desc, Attrs), UID} of
        {ID, Desc, {value, {GroupMemberAttr, MemberIn}}}
          when ID /= <<"">>, GroupMemberAttr == ListUID ->
            Member = case MemberIn of
                         [M] -> M;
                         _ -> MemberIn
                     end,
            Extracted = Extractor(eldap_utils:maybe_list2b(Member)),
            NewJIDsAcc = check_and_accumulate_member(Extracted, AuthChecker, Host, JIDsAcc),
            ldap_entries_to_group(REntries, Host, Desc, NewJIDsAcc, State, Extractor, AuthChecker);
        _ ->
            ldap_entries_to_group(REntries, Host, DescAcc, JIDsAcc, State, Extractor, AuthChecker)
    end;
ldap_entries_to_group([], _Host, DescAcc, JIDsAcc, _State, _Extractor, _AuthChecker) ->
    {DescAcc, JIDsAcc}.

check_and_accumulate_member({ok, UID}, AuthChecker, Host, JIDsAcc) ->
    PUID = jid:nodeprep(UID),
    case PUID of
        error ->
            JIDsAcc;
        _ ->
            case AuthChecker(PUID, Host) of
                true ->
                    [{PUID, Host} | JIDsAcc];
                _ ->
                    JIDsAcc
            end
    end;
check_and_accumulate_member(_, _AuthChecker, _Host, JIDsAcc) ->
    JIDsAcc.

search_user_name(State, User) ->
    case eldap_search(State,
                      [eldap_filter:do_sub(State#state.ufilter,
                                           [{<<"%u">>, User}])],
                      [State#state.user_desc, State#state.user_uid])
    of
        [#eldap_entry{attributes = Attrs} | _] ->
            case {eldap_utils:get_ldap_attr(State#state.user_uid, Attrs),
                  eldap_utils:get_ldap_attr(State#state.user_desc, Attrs)}
            of
                {UID, Desc} when UID /= <<"">> -> {ok, Desc};
                _ -> error
            end;
        [] -> error
    end.

%% Getting User ID part by regex pattern
get_user_part_re(String, Pattern) ->
    case catch re:run(String, Pattern) of
        {match, Captured} ->
            {First, Len} = lists:nth(2, Captured),
            Result = binary:part(String, First, Len),
            {ok, Result};
        _ -> {error, badmatch}
    end.

parse_options(Host, Opts) ->
    EldapID = eldap_utils:get_mod_opt(ldap_pool_tag, Opts,
                                      fun(A) when is_atom(A) -> A end, default),
    Base = eldap_utils:get_base(Opts),
    DerefAliases = eldap_utils:get_deref_aliases(Opts),
    GroupAttr = eldap_utils:get_mod_opt(ldap_groupattr, Opts,
                                        fun iolist_to_binary/1,
                                        <<"cn">>),
    GroupDesc = eldap_utils:get_mod_opt(ldap_groupdesc, Opts,
                                        fun iolist_to_binary/1,
                                        GroupAttr),
    UserDesc = eldap_utils:get_mod_opt(ldap_userdesc, Opts,
                                       fun iolist_to_binary/1,
                                       <<"cn">>),
    UserUID = eldap_utils:get_mod_opt(ldap_useruid, Opts,
                                      fun iolist_to_binary/1,
                                      <<"cn">>),
    UIDAttr = eldap_utils:get_mod_opt(ldap_memberattr, Opts,
                                      fun iolist_to_binary/1,
                                      <<"memberUid">>),
    UIDAttrFormat = eldap_utils:get_mod_opt(ldap_memberattr_format, Opts,
                                            fun iolist_to_binary/1,
                                            <<"%u">>),
    UIDAttrFormatRe = eldap_utils:get_mod_opt(ldap_memberattr_format_re, Opts,
                                              fun(S) ->
                                                      Re = iolist_to_binary(S),
                                                      {ok, MP} = re:compile(Re),
                                                      MP
                                              end, <<"">>),
    AuthCheck = eldap_utils:get_mod_opt(ldap_auth_check, Opts,
                                        fun(on) -> true;
                                           (off) -> false;
                                           (false) -> false;
                                           (true) -> true
                                        end, true),
    UserCacheValidity = eldap_utils:get_mod_opt(
                          ldap_user_cache_validity, Opts,
                          fun(I) when is_integer(I), I>0 -> I end,
                          ?USER_CACHE_VALIDITY),
    GroupCacheValidity = eldap_utils:get_mod_opt(
                           ldap_group_cache_validity, Opts,
                           fun(I) when is_integer(I), I>0 -> I end,
                           ?GROUP_CACHE_VALIDITY),
    UserCacheSize = eldap_utils:get_mod_opt(
                      ldap_user_cache_size, Opts,
                      fun(I) when is_integer(I), I>0 -> I end,
                      ?CACHE_SIZE),
    GroupCacheSize = eldap_utils:get_mod_opt(
                       ldap_group_cache_size, Opts,
                       fun(I) when is_integer(I), I>0 -> I end,
                       ?CACHE_SIZE),
    ConfigFilter = eldap_utils:get_mod_opt(ldap_filter, Opts,
                                       fun check_filter/1, <<"">>),
    ConfigUserFilter = eldap_utils:get_mod_opt(ldap_ufilter, Opts,
                                           fun check_filter/1, <<"">>),
    ConfigGroupFilter = eldap_utils:get_mod_opt(ldap_gfilter, Opts,
                                            fun check_filter/1, <<"">>),
    RosterFilter = eldap_utils:get_mod_opt(ldap_rfilter, Opts,
                                       fun check_filter/1, <<"">>),
    SubFilter = <<"(&(", UIDAttr/binary, "=", UIDAttrFormat/binary,
                  ")(", GroupAttr/binary, "=%g))">>,
    UserSubFilter = case ConfigUserFilter of
                        <<"">> ->
                            eldap_filter:do_sub(SubFilter, [{<<"%g">>, <<"*">>}]);
                        UString -> UString
                    end,
    GroupSubFilter = case ConfigGroupFilter of
                         <<"">> ->
                             eldap_filter:do_sub(SubFilter, [{<<"%u">>, <<"*">>}]);
                         GString -> GString
                     end,
    Filter = case ConfigFilter of
                 <<"">> -> SubFilter;
                 _ ->
                     <<"(&", SubFilter/binary, ConfigFilter/binary, ")">>
             end,
    UserFilter = case ConfigFilter of
                     <<"">> -> UserSubFilter;
                     _ ->
                         <<"(&", UserSubFilter/binary, ConfigFilter/binary, ")">>
                 end,
    GroupFilter = case ConfigFilter of
                      <<"">> -> GroupSubFilter;
                      _ ->
                          <<"(&", GroupSubFilter/binary, ConfigFilter/binary, ")">>
                  end,
    #state{host = Host,
           eldap_id = {Host, EldapID},
           base = Base,
           deref = DerefAliases,
           uid = UIDAttr,
           group_attr = GroupAttr, group_desc = GroupDesc,
           user_desc = UserDesc, user_uid = UserUID,
           uid_format = UIDAttrFormat,
           uid_format_re = UIDAttrFormatRe, filter = Filter,
           ufilter = UserFilter, rfilter = RosterFilter,
           gfilter = GroupFilter, auth_check = AuthCheck,
           user_cache_size = UserCacheSize,
           user_cache_validity = UserCacheValidity,
           group_cache_size = GroupCacheSize,
           group_cache_validity = GroupCacheValidity}.

check_filter(F) ->
    NewF = iolist_to_binary(F),
    {ok, _} = eldap_filter:parse(NewF),
    NewF.
