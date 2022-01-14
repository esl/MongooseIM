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

-export([match_rule/4, match_rule/5]).

-include("jlib.hrl").
-include("mongoose.hrl").

-export_type([rule/0]).

-type rule() :: atom().
-type acl_spec() :: #{match := all | none | current_domain,
                      acl_spec_key() => binary()}.
-type acl_spec_key() :: user | user_regexp | user_glob
                      | server | server_regexp | server_glob
                      | resource | resource_regexp | resource_glob.

-type acl_result() :: allow | deny | term().

-spec match_rule(mongooseim:host_type_or_global(), jid:lserver(), rule(), jid:jid()) ->
          acl_result().
match_rule(_HostType, _Domain, all, _JID) ->
    allow;
match_rule(_HostType, _Domain, none, _JID) ->
    deny;
match_rule(HostType, Domain, Rule, JID) ->
    match_rule(HostType, Domain, Rule, JID, deny).

-spec match_rule(mongooseim:host_type_or_global(), jid:lserver(), rule(), jid:jid(),
                 Default :: acl_result()) ->
          acl_result().
match_rule(global, Domain, Rule, JID, Default) ->
    case mongoose_config:lookup_opt({access, Rule, global}) of
        {error, not_found} ->
            Default;
        {ok, ACLs} ->
            match_acls(ACLs, JID, global, Domain)
    end;
match_rule(HostType, Domain, Rule, JID, Default) ->
    case {mongoose_config:lookup_opt({access, Rule, global}),
          mongoose_config:lookup_opt({access, Rule, HostType})} of
        {{error, not_found}, {error, not_found}} ->
            Default;
        {{error, not_found}, {ok, HostACLs}} ->
            match_acls(HostACLs, JID, HostType, Domain);
        {{ok, GlobalACLs}, {error, not_found}} ->
            match_acls(GlobalACLs, JID, HostType, Domain);
        {{ok, GlobalACLs}, {ok, HostACLs}} ->
            match_acls(merge_acls(GlobalACLs, HostACLs), JID, HostType, Domain)
    end.

-spec merge_acls([{any(), rule()}], [{any(), rule()}]) -> [{any(), rule()}].
merge_acls(Global, HostLocal) ->
    case lists:reverse(Global) of
        [{allow, all} | Rest] ->
            lists:reverse(Rest) ++ HostLocal ++ [{allow, all}];
        _ ->
            Global ++ HostLocal
    end.

-spec match_acls(ACLs :: [{any(), rule()}], jid:jid(), mongooseim:host_type_or_global(),
                 jid:lserver()) ->
          acl_result().
match_acls([], _, _HostType, _Domain) ->
    deny;
match_acls([{Value, Rule} | ACLs], JID, HostType, Domain) ->
    case match_acl(Rule, JID, HostType, Domain) of
        true ->
            Value;
        _ ->
            match_acls(ACLs, JID, HostType, Domain)
    end.

-spec match_acl(rule(), jid:jid(), mongooseim:host_type_or_global(), jid:lserver()) -> boolean().
match_acl(all, _JID, _HostType, _Domain) ->
    true;
match_acl(none, _JID, _HostType, _Domain) ->
    false;
match_acl(Rule, JID, HostType, Domain) ->
    AllSpecs = case HostType of
                   global -> get_acl_specs(Rule, global);
                   _ -> get_acl_specs(Rule, HostType) ++ get_acl_specs(Rule, global)
               end,
    Pred = fun(ACLSpec) -> match(ACLSpec, Domain, JID) end,
    lists:any(Pred, AllSpecs).

-spec get_acl_specs(rule(), mongooseim:host_type_or_global()) -> [acl_spec()].
get_acl_specs(Rule, HostType) ->
    mongoose_config:get_opt({acl, Rule, HostType}, []).

%% @doc Check if all conditions from ACLSpec are satisfied by JID
-spec match(acl_spec(), jid:lserver(), jid:jid()) -> boolean().
match(ACLSpec, Domain, JID) ->
    match_step(maps:next(maps:iterator(ACLSpec)), Domain, JID).

match_step({K, V, I}, Domain, JID) ->
    check(K, V, Domain, JID) andalso match_step(maps:next(I), Domain, JID);
match_step(none, _Domain, _JID) ->
    true.

-spec check(acl_spec_key(), binary(), jid:lserver(), jid:jid()) -> boolean().
check(match, all, _, _) -> true;
check(match, none, _, _) -> false;
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
