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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%-------------------------------------------------------------------
-module(mod_shared_roster_ldap).

-behaviour(gen_server).

-behaviour(gen_mod).

-behaviour(mongoose_module_metrics).

%% API
-export([start_link/2, start/2, stop/1, config_spec/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Hook handlers
-export([get_user_roster/3, get_subscription_lists/3,
         get_jid_info/3, process_item/3, in_subscription/3,
         out_subscription/3]).

-ignore_xref([start_link/2]).

-include("jlib.hrl").
-include("mod_roster.hrl").
-include("mongoose_config_spec.hrl").
-include_lib("eldap/include/eldap.hrl").

-define(CACHE_SIZE, 1000).

-define(USER_CACHE_VALIDITY, 300).

-define(GROUP_CACHE_VALIDITY, 300).

-define(LDAP_SEARCH_TIMEOUT, 5).

%% re:mp() type (it is not exprted in the re module)
-type re_mp() :: {re_pattern, _, _, _, _}.

-record(state,
        {host = <<>>                                  :: binary(),
         eldap_id                                     :: eldap_utils:eldap_id(),
         base = <<>>                                  :: binary(),
         uid = <<>>                                   :: binary(),
         deref = neverDerefAliases                    :: eldap_utils:deref(),
         group_attr = <<>>                            :: binary(),
         group_desc = <<>>                            :: binary(),
         user_desc = <<>>                             :: binary(),
         user_uid = <<>>                              :: binary(),
         uid_format = <<>>                            :: binary(),
         uid_format_re = <<>>                         :: binary() | re_mp(),
         filter = <<>>                                :: binary(),
         ufilter = <<>>                               :: binary(),
         rfilter = <<>>                               :: binary(),
         gfilter = <<>>                               :: binary(),
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

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    CommonLDAPSpec = mongoose_ldap_config:spec(),
    Items = #{<<"groupattr">> => #option{type = binary},
              <<"groupdesc">> => #option{type = binary},
              <<"userdesc">> => #option{type = binary},
              <<"useruid">> => #option{type = binary},
              <<"memberattr">> => #option{type = binary},
              <<"memberattr_format">> => #option{type = binary},
              <<"memberattr_format_re">> => #option{type = binary},
              <<"auth_check">> => #option{type = boolean},
              <<"user_cache_validity">> => #option{type = integer,
                                                   validate = positive},
              <<"group_cache_validity">> => #option{type = integer,
                                                    validate = positive},
              <<"user_cache_size">> => #option{type = integer,
                                               validate = positive},
              <<"group_cache_size">> => #option{type = integer,
                                                validate = positive},
              <<"rfilter">> => #option{type = binary},
              <<"gfilter">> => #option{type = binary},
              <<"ufilter">> => #option{type = binary}
    },
    Defaults = #{<<"groupattr">> => <<"cn">>,
                 <<"userdesc">> => <<"cn">>,
                 <<"useruid">> => <<"cn">>,
                 <<"memberattr">> => <<"memberUid">>,
                 <<"memberattr_format">> => <<"%u">>,
                 <<"memberattr_format_re">> => <<>>,
                 <<"auth_check">> => true,
                 <<"user_cache_validity">> => ?USER_CACHE_VALIDITY,
                 <<"group_cache_validity">> => ?GROUP_CACHE_VALIDITY,
                 <<"user_cache_size">> => ?CACHE_SIZE,
                 <<"group_cache_size">> => ?CACHE_SIZE,
                 <<"rfilter">> => <<>>,
                 <<"gfilter">> => <<>>,
                 <<"ufilter">> => <<>>},
    CommonLDAPSpec#section{items = maps:merge(CommonLDAPSpec#section.items, Items),
                           defaults = maps:merge(CommonLDAPSpec#section.defaults, Defaults),
                           process = fun process_ldap_options/1}.

process_ldap_options(Opts = #{groupattr := GroupAttr}) ->
    GroupDesc = maps:get(groupdesc, Opts, GroupAttr),
    Opts#{groupdesc => GroupDesc}.

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
-spec get_user_roster(Acc, Params, Extra) -> {ok, Acc} when
     Acc :: [mod_roster:roster()],
     Params :: #{mongoose_acc := mongoose_acc:t(), jid := jid:jid()},
     Extra :: gen_hook:extra().
get_user_roster(Items, #{jid := JID}, _) ->
    US = jid:to_lus(JID),
    SRUsers = get_user_to_groups_map(US, true),
    {NewItems1, SRUsersRest} =
        lists:mapfoldl(
          fun (Item, SRUsers1) ->
                  Contact = Item#roster.jid,
                  US1 = jid:to_lus(Contact),
                  case dict:find(US1, SRUsers1) of
                      {ok, _GroupNames} ->
                          {Item#roster{subscription = both, ask = none},
                           dict:erase(US1, SRUsers1)};
                      error ->
                          {Item, SRUsers1}
                  end
          end,
          SRUsers, Items),
    SRItems = [#roster{usj = {US, {U1, S1, <<>>}},
                       us = US, jid = {U1, S1, <<>>},
                       name = get_user_name(U1, S1), subscription = both,
                       ask = none, groups = GroupNames}
               || {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    {ok, SRItems ++ NewItems1}.

%% This function in use to rewrite the roster entries when moving or renaming
%% them in the user contact list.
-spec process_item(Acc, Params, Extra) -> {ok, Acc} when
     Acc :: mod_roster:roster(),
     Params :: map(),
     Extra :: gen_hook:extra().
process_item(RosterItem, _, _) ->
    USFrom = RosterItem#roster.us,
    {User, Server, _Resource} = RosterItem#roster.jid,
    USTo = {User, Server},
    Map = get_user_to_groups_map(USFrom, false),
    NewRosterItem = case dict:find(USTo, Map) of
        error -> RosterItem;
        {ok, []} -> RosterItem;
        {ok, GroupNames}
          when RosterItem#roster.subscription == remove ->
            RosterItem#roster{subscription = both, ask = none,
                              groups = GroupNames};
        _ -> RosterItem#roster{subscription = both, ask = none}
    end,
    {ok, NewRosterItem}.

-spec get_subscription_lists(Acc, Params, Extra) -> {ok, Acc} when
     Acc ::mongoose_acc:t(),
     Params :: #{jid := jid:jid()},
     Extra :: gen_hook:extra().
get_subscription_lists(Acc, #{jid := #jid{lserver = LServer} = JID}, _) ->
    {F, T, P} = mongoose_acc:get(roster, subscription_lists, {[], [], []}, Acc),
    US = jid:to_lus(JID),
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:usort(lists:flatmap(fun (Group) ->
                                                get_group_users(LServer, Group)
                                        end,
                                        DisplayedGroups)),
    SRJIDs = [{U1, S1, <<>>} || {U1, S1} <- SRUsers],
    NewLists = {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T), P},
    {ok, mongoose_acc:set(roster, subscription_lists, NewLists, Acc)}.

-spec get_jid_info(Acc, Params, Extra) -> {ok, Acc} when
     Acc :: {mod_roster:subscription_state(), [binary()]},
     Params :: #{to := jid:jid(), remote := jid:jid() | jid:simple_jid()},
     Extra :: gen_hook:extra().
get_jid_info({Subscription, Groups}, #{to := ToJID, remote := JID}, _) ->
    ToUS = jid:to_lus(ToJID),
    US1 = jid:to_lus(JID),
    SRUsers = get_user_to_groups_map(ToUS, false),
    NewAcc = case dict:find(US1, SRUsers) of
        {ok, GroupNames} ->
            NewGroups = case Groups of
                            [] -> GroupNames;
                            _ -> Groups
                        end,
            {both, NewGroups};
        error -> {Subscription, Groups}
    end,
    {ok, NewAcc}.

-spec in_subscription(Acc, Params, Extra) -> {ok | stop, Acc} when
     Acc :: mongoose_acc:t(),
     Params :: #{to := jid:jid(),
                 from := jid:jid(),
                 type := mod_roster:sub_presence()},
     Extra :: gen_hook:extra().
in_subscription(Acc, #{to := ToJID, from := FromJID, type := Type}, _) ->
    case process_subscription(in, ToJID, FromJID, Type) of
        stop ->
            {stop, Acc};
        {stop, false} ->
            {stop, mongoose_acc:set(hook, result, false, Acc)};
        _ -> {ok, Acc}
    end.

-spec out_subscription(Acc, Params, Extra) -> {ok | stop, Acc} when
     Acc :: mongoose_acc:t(),
     Params :: #{to := jid:jid(),
                 from := jid:jid(),
                 type := mod_roster:sub_presence()},
     Extra :: gen_hook:extra().
 out_subscription(Acc, #{to := ToJID, from := FromJID, type := Type}, _) ->
    case process_subscription(out, FromJID, ToJID, Type) of
        stop ->
            {stop, Acc};
        {stop, false} ->
            {stop, Acc};
        false -> {ok, Acc}
    end.

process_subscription(Direction, #jid{luser = LUser, lserver = LServer}, ToJID, _Type) ->
    US = {LUser, LServer},
    {U1, S1, _} = jid:to_lower(jid:to_bare(ToJID)),
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
    gen_hook:add_handlers(hooks(Host)),
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    gen_hook:delete_handlers(hooks(Host)).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
        {roster_get, HostType, fun ?MODULE:get_user_roster/3, #{}, 70},
        {roster_in_subscription, HostType, fun ?MODULE:in_subscription/3, #{}, 70},
        {roster_out_subscription, HostType, fun ?MODULE:out_subscription/3, #{}, 70},
        {roster_get_subscription_lists, HostType, fun ?MODULE:get_subscription_lists/3, #{}, 70},
        {roster_get_jid_info, HostType, fun ?MODULE:get_jid_info/3, #{}, 70},
        {roster_process_item, HostType, fun ?MODULE:process_item/3, #{}, 70}
    ].

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
            SearchOpts = search_opts(EldapFilter, AttributesList, State),
            case eldap_pool:search(State#state.eldap_id, SearchOpts) of
                #eldap_search_result{entries = Es} ->
                    %% A result with entries. Return their list.
                    Es;
                _ ->
                    %% Something else. Pretend we got no results.
                    []
            end;
        _ ->
            %% Filter parsing failed. Pretend we got no results.
            []
    end.

search_opts(EldapFilter, AttributesList, State) ->
    [{base, State#state.base},
     {filter, EldapFilter},
     {timeout, ?LDAP_SEARCH_TIMEOUT},
     {deref, State#state.deref},
     {attributes, AttributesList}].

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
                                fun() -> search_group_info(State, Group) end)
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
                                fun() -> search_group_info(State, Group) end)
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
                                fun() -> search_user_name(State, User) end)
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
                      true -> fun ejabberd_auth:does_user_exist/1;
                      false -> fun(_JID) -> true end
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
            JID = jid:make_bare(PUID, Host),
            case AuthChecker(JID) of
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


parse_options(Host, #{base := Base, pool_tag := EldapID, deref := Deref, filter := FilterIn,
                      groupattr := GroupAttr, groupdesc := GroupDesc, userdesc := UserDesc,
                      useruid := UserUID, memberattr := UIDAttr, memberattr_format := UIDAttrFormat,
                      memberattr_format_re := UIDAttrFormatReIn, auth_check := AuthCheck,
                      user_cache_validity := UserCacheValidity, group_cache_validity := GroupCacheValidity,
                      user_cache_size := UserCacheSize, group_cache_size := GroupCacheSize,
                      ufilter := UFilterIn, gfilter := GFilterIn, rfilter := RFilterIn}) ->
    DerefAliases = eldap_utils:deref_aliases(Deref),
    ConfigFilter = check_filter(FilterIn),
    ConfigUserFilter = check_filter(UFilterIn),
    ConfigGroupFilter = check_filter(GFilterIn),
    RosterFilter = check_filter(RFilterIn),
    SubFilter = <<"(&(", UIDAttr/binary, "=", UIDAttrFormat/binary,
                  ")(", GroupAttr/binary, "=%g))">>,
    UIDAttrFormatRe = case UIDAttrFormatReIn of
                          <<>> -> UIDAttrFormatReIn;
                          RE ->
                              {ok, MP} = re:compile(RE),
                              MP
                      end,
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

check_filter(<<>>) -> <<>>;
check_filter(F) ->
    {ok, _} = eldap_filter:parse(F),
    F.
