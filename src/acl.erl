%%%----------------------------------------------------------------------
%%% File    : acl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ACL support
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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

-module(acl).
-author('alexey@process-one.net').

-export([match_rule/3, match_rule/4, match_rule/5,
         merge_access_rules/2]).

-include("jlib.hrl").
-include("mongoose.hrl").

-export_type([rule_name/0]).

%% A rule consists of clauses matched from top to bottom
%% Each clause returns specific values (results) for specific acl's
%% There are two predefined rules: 'all' (always allow) and 'none' (always deny)
-type rule_name() :: all | none | atom().
-type rule_clause() :: #{acl := acl_name(), value := acl_result()}.

%% Rules can allow/deny access or return other (e.g. numerical) values
-type acl_result() :: allow | deny | term().

%% An acl is a union of specs that decide which users belong to it
%% There are two predefined acl's: 'all' (match everyone) and 'none' (match nobody)
-type acl_name() :: all | none | atom().
-type acl_spec() :: #{match := all | none | current_domain | any_hosted_domain,
                      acl_spec_key() => binary()}.
-type acl_spec_key() :: user | user_regexp | user_glob
                      | server | server_regexp | server_glob
                      | resource | resource_regexp | resource_glob.

%% Skips the domain check for the 'match => current_domain' condition
-spec match_rule(mongooseim:host_type_or_global(), rule_name(), jid:jid()) -> acl_result().
match_rule(HostType, RuleName, JID) ->
    match_rule(HostType, JID#jid.lserver, RuleName, JID).

-spec match_rule(mongooseim:host_type_or_global(), jid:lserver(), rule_name(), jid:jid()) ->
          acl_result().
match_rule(_HostType, _Domain, all, _JID) ->
    allow;
match_rule(_HostType, _Domain, none, _JID) ->
    deny;
match_rule(HostType, Domain, RuleName, JID) ->
    match_rule(HostType, Domain, RuleName, JID, deny).

-spec match_rule(mongooseim:host_type_or_global(), jid:lserver(), rule_name(), jid:jid(),
                 Default :: acl_result()) ->
          acl_result().
match_rule(HostType, Domain, RuleName, JID, Default) ->
    case mongoose_config:lookup_opt([{access, HostType}, RuleName]) of
        {error, not_found} ->
            Default;
        {ok, RuleSpec} ->
            match_rule_clauses(RuleSpec, JID, HostType, Domain)
    end.

%% Merge host-type rules with global ones on startup
-spec merge_access_rules([rule_clause()], [rule_clause()]) -> [rule_clause()].
merge_access_rules(Global, HostLocal) ->
    case lists:reverse(Global) of
        [#{acl := all, value := allow} = AllowAll | Rest] ->
            lists:reverse(Rest) ++ HostLocal ++ [AllowAll];
        _ ->
            Global ++ HostLocal
    end.

-spec match_rule_clauses([rule_clause()], jid:jid(), mongooseim:host_type_or_global(),
                         jid:lserver()) ->
          acl_result().
match_rule_clauses([], _, _HostType, _Domain) ->
    deny;
match_rule_clauses([#{acl := ACLName, value := Value} | RemainingClauses], JID, HostType, Domain) ->
    case match_acl(ACLName, JID, HostType, Domain) of
        true ->
            Value;
        _ ->
            match_rule_clauses(RemainingClauses, JID, HostType, Domain)
    end.

-spec match_acl(acl_name(), jid:jid(), mongooseim:host_type_or_global(), jid:lserver()) ->
          boolean().
match_acl(all, _JID, _HostType, _Domain) ->
    true;
match_acl(none, _JID, _HostType, _Domain) ->
    false;
match_acl(ACLName, JID, HostType, Domain) ->
    lists:any(fun(ACLSpec) -> match(ACLSpec, Domain, JID) end, get_acl_specs(ACLName, HostType)).

-spec get_acl_specs(acl_name(), mongooseim:host_type_or_global()) -> [acl_spec()].
get_acl_specs(ACLName, HostType) ->
    mongoose_config:get_opt([{acl, HostType}, ACLName], []).

%% @doc Check if all conditions from ACLSpec are satisfied by JID
-spec match(acl_spec(), jid:lserver(), jid:jid()) -> boolean().
match(ACLSpec, Domain, JID) when map_size(ACLSpec) > 0 ->
    match_step(maps:next(maps:iterator(ACLSpec)), Domain, JID).

match_step({K, V, I}, Domain, JID) ->
    check(K, V, Domain, JID) andalso match_step(maps:next(I), Domain, JID);
match_step(none, _Domain, _JID) ->
    true.

-spec check(acl_spec_key(), binary(), jid:lserver(), jid:jid()) -> boolean().
check(match, all, _, _) -> true;
check(match, none, _, _) -> false;
check(match, any_hosted_domain, _, JID) ->
    mongoose_domain_api:get_host_type(JID#jid.lserver) =/= {error, not_found};
check(match, current_domain, Domain, JID) -> JID#jid.lserver =:= Domain;
check(user, User, _, JID) -> JID#jid.luser =:= User;
check(user_regexp, Regexp, _, JID) -> is_regexp_match(JID#jid.luser, Regexp);
check(user_glob, Glob, _, JID) -> is_glob_match(JID#jid.luser, Glob);
check(server, Server, _, JID) -> JID#jid.lserver =:= Server;
check(server_regexp, Regexp, _, JID) -> is_regexp_match(JID#jid.lserver, Regexp);
check(server_glob, Glob, _, JID) -> is_glob_match(JID#jid.lserver, Glob);
check(resource, Resource, _, JID) -> JID#jid.lresource =:= Resource;
check(resource_regexp, Regexp, _, JID) -> is_regexp_match(JID#jid.lresource, Regexp);
check(resource_glob, Glob, _, JID) -> is_glob_match(JID#jid.lresource, Glob).

-spec is_regexp_match(binary(), RegExp :: iodata()) -> boolean().
is_regexp_match(String, RegExp) ->
    try re:run(String, RegExp, [{capture, none}]) of
        nomatch ->
            false;
        match ->
            true
    catch _:ErrDesc ->
              ?LOG_ERROR(#{what => acl_regexp_match_failed,
                           string => String, regex => RegExp, reason => ErrDesc}),
            false
    end.

-spec is_glob_match(binary(), Glob :: binary()) -> boolean().
is_glob_match(String, Glob) ->
    is_regexp_match(String, xmerl_regexp:sh_to_awk(binary_to_list(Glob))).
