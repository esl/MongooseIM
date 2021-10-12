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

-export([start/0,
         to_record/3,
         add/3,
         delete/3,
         match_rule/3,
         match_rule/4,
         match_rule_for_host_type/4,
         match_rule_for_host_type/5]).

-ignore_xref([add/3, delete/3, match_rule/4]).

-include("mongoose.hrl").

-export_type([rule/0, domain/0]).

-type rule() :: 'all' | 'none' | atom().
-type domain() :: jid:lserver() | global.
-type host_type() :: mongooseim:host_type() | global.
-type acl_name() :: {atom(), host_type()}.
-type regexp() :: iolist() | binary().
-type aclspec() :: all
                | none
                | {user, jid:user()}
                | {user, jid:user(), jid:server()}
                | {server, jid:server()}
                | {resource, jid:resource()}
                | {user_regexp, regexp()}
                | {user_regexp, regexp(), jid:server()}
                | {server_regexp, regexp()}
                | {resource_regexp, regexp()}
                | {node_regexp, regexp(), regexp()}
                | {user_glob, regexp()}
                | {user_glob, regexp(), jid:server()}
                | {server_glob, regexp()}
                | {resource_glob, regexp()}
                | {node_glob, regexp(), regexp()}.

-record(acl, {aclname :: acl_name(),
              aclspec :: aclspec()
             }).
-type acl() :: #acl{}.
-type acl_result() :: allow | deny | term().

start() ->
    mnesia:create_table(acl,
                        [{ram_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, acl)}]),
    mnesia:add_table_copy(acl, node(), ram_copies),
    ok.

-spec to_record(HostType :: host_type(),
                ACLName :: atom(),
                ACLSpec :: aclspec()) -> acl().
to_record(HostType, ACLName, ACLSpec) ->
    #acl{aclname = {ACLName, HostType}, aclspec = normalize_spec(ACLSpec)}.

-spec add(HostType :: host_type(),
          ACLName :: atom(),
          ACLSpec :: aclspec()) -> {atomic, term()} | {aborted, term()}.
add(HostType, ACLName, ACLSpec) ->
    F = fun() ->
                ACLRecord = to_record(HostType, ACLName, ACLSpec),
                mnesia:write(ACLRecord)
        end,
    mnesia:transaction(F).

-spec delete(HostType :: host_type(),
             ACLName :: atom(),
             ACLSpec :: aclspec()) -> {atomic, term()} | {aborted, term()}.
delete(HostType, ACLName, ACLSpec) ->
    F = fun() ->
                ACLRecord = to_record(HostType, ACLName, ACLSpec),
                mnesia:delete_object(ACLRecord)
        end,
    mnesia:transaction(F).

-spec normalize(_) -> 'error' | binary() | tuple().
normalize(A) when is_list(A) ->
    normalize(list_to_binary(A));
normalize(A) ->
    jid:nodeprep(A).

-spec normalize_spec(aclspec())
      -> aclspec()
         | {_, 'error' | binary() | tuple()}
         | {_, 'error' | binary() | tuple(), 'error' | binary() | tuple()}.
normalize_spec({A, B}) ->
    {A, normalize(B)};
normalize_spec({A, B, C}) ->
    {A, normalize(B), normalize(C)};
normalize_spec(all) ->
    all;
normalize_spec(none) ->
    none.


%% legacy API, use match_rule_for_host_type instead
-spec match_rule(Domain :: domain(),
                 Rule :: rule(),
                 JID :: jid:jid()) -> acl_result().
match_rule(Domain, Rule, JID) ->
    match_rule(Domain, Rule, JID, deny).

-spec match_rule_for_host_type(HostType :: host_type(),
                               Domain :: domain(),
                               Rule :: rule(),
                               JID :: jid:jid()) -> acl_result().
match_rule_for_host_type(HostType, Domain, Rule, JID) ->
    match_rule_for_host_type(HostType, Domain, Rule, JID, deny).

%% legacy API, use match_rule_for_host_type instead
-spec match_rule(Domain :: domain(),
                 Rule :: rule(),
                 JID :: jid:jid(),
                 Default :: acl_result()) -> acl_result().
match_rule(Domain, Rule, JID, Default) ->
    %% We don't want to cast Domain to HostType here.
    %% Developers should start using match_rule_for_host_type explicetly.
    match_rule_for_host_type(Domain, Domain, Rule, JID, Default).

%% HostType determines which rules and ACLs are checked:
%%   - 'global' - only global ones
%%   - a specific host type - both global and per-host-type ones
%% Domain is only used for validating the user's domain name in the {user, U} pattern:
%%   - 'global' - any domain is accepted
%%   - a specific domain name - only the provided name is accepted
-spec match_rule_for_host_type(HostType :: host_type(),
                               Domain :: domain(),
                               Rule :: rule(),
                               JID :: jid:jid(),
                               Default :: acl_result()) -> acl_result().
match_rule_for_host_type(_HostType, _, all, _, _Default) ->
    allow;
match_rule_for_host_type(_HostType, _, none, _, _Default) ->
    deny;
match_rule_for_host_type(global, Domain, Rule, JID, Default) ->
    case ejabberd_config:get_global_option({access, Rule, global}) of
        undefined ->
            Default;
        GACLs ->
            match_acls(GACLs, JID, global, Domain)
    end;
match_rule_for_host_type(HostType, Domain, Rule, JID, Default) ->
    GlobalACLs = ejabberd_config:get_global_option({access, Rule, global}),
    HostACLs = ejabberd_config:get_global_option({access, Rule, HostType}),
    case {GlobalACLs, HostACLs} of
        {undefined, undefined} ->
            Default;
        {undefined, HostACLs} ->
            match_acls(HostACLs, JID, HostType, Domain);
        {GlobalACLs, undefined} ->
            match_acls(GlobalACLs, JID, HostType, Domain);
        {GlobalACLs, HostACLs} ->
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

-spec match_acls(ACLs :: [{any(), rule()}],
                 JID :: jid:jid(),
                 HostType :: host_type(),
                 Domain :: domain()) -> deny | term().
match_acls([], _, _HostType, _Domain) ->
    deny;
match_acls([{Value, ACL} | ACLs], JID, HostType, Domain) ->
    case match_acl(ACL, JID, HostType, Domain) of
        true ->
            Value;
        _ ->
            match_acls(ACLs, JID, HostType, Domain)
    end.

-spec match_acl(ACL :: rule(),
                JID :: jid:jid(),
                HostType :: host_type(),
                Domain :: domain()) -> boolean().
match_acl(all, _JID, _HostType, _Domain) ->
    true;
match_acl(none, _JID, _HostType, _Domain) ->
    false;
match_acl(ACL, JID, HostType, Domain) ->
    LJID = jid:to_lower(JID),
    AllSpecs = lookup_acl(ACL, HostType),
    Pred = fun(#acl{aclspec = S}) -> match(S, LJID, Domain) end,
    lists:any(Pred, AllSpecs).

-spec lookup_acl(rule(), host_type()) -> [aclspec()].
lookup_acl(ACL, global) ->
    ets:lookup(acl, {ACL, global});
lookup_acl(ACL, HostType) ->
    ets:lookup(acl, {ACL, global}) ++ ets:lookup(acl, {ACL, HostType}).

-spec is_server_valid(domain(), jid:lserver()) -> boolean().
is_server_valid(Domain, Domain) ->
    true;
is_server_valid(global, JIDServer) ->
    case mongoose_domain_api:get_domain_host_type(JIDServer) of
        {ok, _HostType} ->
            true;
        _ ->
            false
    end;
is_server_valid(_Domain, _JIDServer) ->
    false.

-spec match(aclspec(), jid:simple_jid(), domain()) -> boolean().
match(all, _LJID, _Domain) ->
    true;
match({user, U}, {User, Server, _Resource}, Domain) ->
    U == User andalso is_server_valid(Domain, Server);
match({user, U, S}, {User, Server, _Resource}, _Domain) ->
    U == User andalso S == Server;
match({server, S}, {_User, Server, _Resource}, _Domain) ->
    S == Server;
match({resource, Res}, {_User, _Server, Resource}, _Domain) ->
    Resource == Res;
match({user_regexp, UserReg}, {User, Server, _Resource}, Domain) ->
    is_server_valid(Domain, Server) andalso is_regexp_match(User, UserReg);
match({user_regexp, UserReg, MServer}, {User, Server, _Resource}, _Domain) ->
    MServer == Server andalso is_regexp_match(User, UserReg);
match({server_regexp, ServerReg}, {_User, Server, _Resource}, _Domain) ->
    is_regexp_match(Server, ServerReg);
match({resource_regexp, ResourceReg}, {_User, _Server, Resource}, _Domain) ->
    is_regexp_match(Resource, ResourceReg);
match({node_regexp, UserReg, ServerReg}, {User, Server, _Resource}, _Domain) ->
    is_regexp_match(Server, ServerReg) andalso
    is_regexp_match(User, UserReg);
match({user_glob, UserGlob}, {User, Server, _Resource}, Domain) ->
    is_server_valid(Domain, Server) andalso is_glob_match(User, UserGlob);
match({user_glob, UserGlob, MServer}, {User, Server, _Resource}, _Domain) ->
    MServer == Server andalso is_glob_match(User, UserGlob);
match({server_glob, ServerGlob}, {_User, Server, _Resource}, _Domain) ->
    is_glob_match(Server, ServerGlob);
match({resource_glob, ResourceGlob}, {_User, _Server, Resource}, _Domain) ->
    is_glob_match(Resource, ResourceGlob);
match({node_glob, UserGlob, ServerGlob}, {User, Server, _Resource}, _Domain) ->
    is_glob_match(Server, ServerGlob) andalso is_glob_match(User, UserGlob);
match(WrongSpec, _LJID, _Domain) ->
    ?LOG_ERROR(#{what => wrong_acl_expression,
                 text => <<"Wrong ACL expression in the configuration file">>,
                 wrong_spec => WrongSpec}),
    false.

-spec is_regexp_match(binary(), Regex :: regexp()) -> boolean().
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

-spec is_glob_match(binary(), Glob :: regexp()) -> boolean().
is_glob_match(String, Glob) ->
    is_regexp_match(String, xmerl_regexp:sh_to_awk(binary_to_list(Glob))).

