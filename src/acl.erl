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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(acl).
-author('alexey@process-one.net').

-export([start/0,
         to_record/3,
         add/3,
         delete/3,
         match_rule/3,
         match_rule/4]).

-include("mongoose.hrl").

-export_type([rule/0, host/0]).

-type rule() :: 'all' | 'none' | atom().
-type host() :: jid:server() | 'global'.
-type acl_name() :: {atom(), host()}.
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

start() ->
    mnesia:create_table(acl,
                        [{ram_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, acl)}]),
    mnesia:add_table_copy(acl, node(), ram_copies),
    ok.

-spec to_record(Host :: host(),
                ACLName :: atom(),
                ACLSpec :: aclspec()) -> acl().
to_record(Host, ACLName, ACLSpec) ->
    #acl{aclname = {ACLName, Host}, aclspec = normalize_spec(ACLSpec)}.

-spec add(Host :: host(),
          ACLName :: atom(),
          ACLSpec :: aclspec()) -> {atomic, term()} | {aborted, term()}.
add(Host, ACLName, ACLSpec) ->
    F = fun() ->
                ACLRecord = to_record(Host, ACLName, ACLSpec),
                mnesia:write(ACLRecord)
        end,
    mnesia:transaction(F).

-spec delete(Host :: host(),
          ACLName :: atom(),
          ACLSpec :: aclspec()) -> {atomic, term()} | {aborted, term()}.
delete(Host, ACLName, ACLSpec) ->
    F = fun() ->
                ACLRecord = to_record(Host, ACLName, ACLSpec),
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


-spec match_rule(Host :: host(),
                 Rule :: rule(),
                 JID :: jid:jid()) -> allow | deny | term().
match_rule(Host, Rule, JID) ->
    match_rule(Host, Rule, JID, deny).

match_rule(_, all, _, _Default) ->
    allow;
match_rule(_, none, _, _Default) ->
    deny;
match_rule(global, Rule, JID, Default) ->
    case ejabberd_config:get_global_option({access, Rule, global}) of
        undefined ->
            Default;
        GACLs ->
            match_acls(GACLs, JID, global)
    end;
match_rule(Host, Rule, JID, Default) ->
    GlobalACLs = ejabberd_config:get_global_option({access, Rule, global}),
    HostACLs = ejabberd_config:get_global_option({access, Rule, Host}),

    case {GlobalACLs, HostACLs} of
        {undefined, undefined} ->
            Default;
        {undefined, HostACLs} ->
            match_acls(HostACLs, JID, Host);
        {GlobalACLs, undefined} ->
            match_acls(GlobalACLs, JID, Host);
        {GlobalACLs, HostACLs} ->
            match_acls(merge_acls(GlobalACLs, HostACLs), JID, Host)
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
                 Host :: host()) -> deny | term().
match_acls([], _, _Host) ->
    deny;
match_acls([{Value, ACL} | ACLs], JID, Host) ->
    case match_acl(ACL, JID, Host) of
        true ->
            Value;
        _ ->
            match_acls(ACLs, JID, Host)
    end.

-spec match_acl(ACL :: rule(),
                JID :: jid:jid(),
                Host :: host()) -> boolean().
match_acl(all, _JID, _Host) ->
    true;
match_acl(none, _JID, _Host) ->
    false;
match_acl(ACL, JID, Host) ->
    LJID = jid:to_lower(JID),
    AllSpecs = ets:lookup(acl, {ACL, global}) ++ ets:lookup(acl, {ACL, Host}),
    Pred = fun(#acl{aclspec = S}) -> match(S, LJID, Host) end,
    lists:any(Pred, AllSpecs).

-spec is_server_valid(host(), jid:server()) -> boolean().
is_server_valid(Host, Host) ->
    true;
is_server_valid(global, JIDServer) ->
    lists:member(JIDServer, ?MYHOSTS);
is_server_valid(_Host, _JIDServer) ->
    false.

-spec match(aclspec(), jid:simple_jid(), host()) -> boolean().
match(all, _LJID, _Host) ->
    true;
match({user, U}, {User, Server, _Resource}, Host) ->
    U == User andalso is_server_valid(Host, Server);
match({user, U, S}, {User, Server, _Resource}, _Host) ->
    U == User andalso S == Server;
match({server, S}, {_User, Server, _Resource}, _Host) ->
    S == Server;
match({resource, Res}, {_User, _Server, Resource}, _Host) ->
    Resource == Res;
match({user_regexp, UserReg}, {User, Server, _Resource}, Host) ->
    is_server_valid(Host, Server) andalso is_regexp_match(User, UserReg);
match({user_regexp, UserReg, MServer}, {User, Server, _Resource}, _Host) ->
    MServer == Server andalso is_regexp_match(User, UserReg);
match({server_regexp, ServerReg}, {_User, Server, _Resource}, _Host) ->
    is_regexp_match(Server, ServerReg);
match({resource_regexp, ResourceReg}, {_User, _Server, Resource}, _Host) ->
    is_regexp_match(Resource, ResourceReg);
match({node_regexp, UserReg, ServerReg}, {User, Server, _Resource}, _Host) ->
    is_regexp_match(Server, ServerReg) andalso
    is_regexp_match(User, UserReg);
match({user_glob, UserGlob}, {User, Server, _Resource}, Host) ->
    is_server_valid(Host, Server) andalso is_glob_match(User, UserGlob);
match({user_glob, UserGlob, MServer}, {User, Server, _Resource}, _Host) ->
    MServer == Server andalso is_glob_match(User, UserGlob);
match({server_glob, ServerGlob}, {_User, Server, _Resource}, _Host) ->
    is_glob_match(Server, ServerGlob);
match({resource_glob, ResourceGlob}, {_User, _Server, Resource}, _Host) ->
    is_glob_match(Resource, ResourceGlob);
match({node_glob, UserGlob, ServerGlob}, {User, Server, _Resource}, _Host) ->
    is_glob_match(Server, ServerGlob) andalso is_glob_match(User, UserGlob);
match(WrongSpec, _LJID, _Host) ->
    ?ERROR_MSG(
       "Wrong ACL expression: ~p~n"
       "Check your config file and reload it with the override_acls option enabled",
       [WrongSpec]),
    false.

-spec is_regexp_match(binary(), Regex :: regexp()) -> boolean().
is_regexp_match(String, RegExp) ->
    try re:run(String, RegExp, [{capture, none}]) of
        nomatch ->
            false;
        match ->
            true
    catch _:ErrDesc ->
            ?ERROR_MSG("Wrong regexp ~p in ACL: ~p", [RegExp, ErrDesc]),
            false
    end.

-spec is_glob_match(binary(), Glob :: regexp()) -> boolean().
is_glob_match(String, Glob) ->
    is_regexp_match(String, xmerl_regexp:sh_to_awk(binary_to_list(Glob))).

