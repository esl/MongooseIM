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
         add_list/3,
         match_rule/3,
         % for debugging only
         match_acl/3]).

-include("ejabberd.hrl").

-export_type([rule/0, host/0]).

-type rule() :: 'all' | 'none' | atom().
-type host() :: ejabberd:server() | 'global'.
-type acl_name() :: {atom(), host()}.
-type regexp() :: iolist() | binary().
-type aclspec() :: all
                | none
                | {user, ejabberd:user()}
                | {user, ejabberd:user(), ejabberd:server()}
                | {server, ejabberd:server()}
                | {resource, ejabberd:resource()}
                | {user_regexp, regexp()}
                | {shared_group, _}
                | {shared_group, _, _}
                | {user_regexp, regexp(), ejabberd:server()}
                | {server_regexp, regexp()}
                | {resource_regexp, regexp()}
                | {node_regexp, regexp(), regexp()}
                | {user_glob, regexp()}
                | {user_glob, regexp(), ejabberd:server()}
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


add_list(Host, ACLs, Clear) ->
    F = fun() ->
                if
                    Clear ->
                        Ks = mnesia:select(
                               acl, [{{acl, {'$1', Host}, '$2'}, [], ['$1']}]),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({acl, {K, Host}})
                                      end, Ks);
                    true ->
                        ok
                end,
                lists:foreach(fun(ACL) ->
                                      case ACL of
                                          #acl{aclname = ACLName,
                                               aclspec = ACLSpec} ->
                                              mnesia:write(
                                                #acl{aclname = {ACLName, Host},
                                                     aclspec = normalize_spec(ACLSpec)})
                                      end
                              end, ACLs)
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            ok;
        _ ->
            false
    end.

-spec normalize(_) -> 'error' | binary() | tuple().
normalize(A) when is_list(A) ->
    normalize(list_to_binary(A));
normalize(A) ->
    jlib:nodeprep(A).

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
                 JID :: ejabberd:jid()) -> allow | deny | term().
match_rule(global, Rule, JID) ->
    case Rule of
        all -> allow;
        none -> deny;
        _ ->
            case ejabberd_config:get_global_option({access, Rule, global}) of
                undefined ->
                    deny;
                GACLs ->
                    match_acls(GACLs, JID, global)
            end
    end;
match_rule(Host, Rule, JID) ->
    case Rule of
        all -> allow;
        none -> deny;
        _ ->
            case ejabberd_config:get_global_option({access, Rule, global}) of
                undefined ->
                    case ejabberd_config:get_global_option({access, Rule, Host}) of
                        undefined ->
                            deny;
                        ACLs ->
                            match_acls(ACLs, JID, Host)
                    end;
                GACLs ->
                    case ejabberd_config:get_global_option({access, Rule, Host}) of
                        undefined ->
                            match_acls(GACLs, JID, Host);
                        ACLs ->
                            case lists:reverse(GACLs) of
                                [{allow, all} | Rest] ->
                                    match_acls(
                                      lists:reverse(Rest) ++ ACLs ++
                                      [{allow, all}],
                                      JID, Host);
                                _ ->
                                    match_acls(GACLs ++ ACLs, JID, Host)
                            end
                    end
            end
    end.

-spec match_acls(ACLs :: [{boolean(), rule()}],
                 JID :: ejabberd:jid(),
                 Host :: host()) -> deny | term().
match_acls([], _, _Host) ->
    deny;
match_acls([{Access, ACL} | ACLs], JID, Host) ->
    case match_acl(ACL, JID, Host) of
        true ->
            Access;
        _ ->
            match_acls(ACLs, JID, Host)
    end.

-spec match_acl(ACL :: rule(),
                JID :: ejabberd:jid(),
                Host :: host()) -> boolean().
match_acl(ACL, JID, Host) ->
    case ACL of
        all -> true;
        none -> false;
        _ ->
            {User, Server, Resource} = jlib:jid_tolower(JID),
            lists:any(fun(#acl{aclspec = Spec}) ->
                              case Spec of
                                  all ->
                                      true;
                                  {user, U} ->
                                      (U == User)
                                          andalso
                                            ((Host == Server) orelse
                                             ((Host == global) andalso
                                              lists:member(Server, ?MYHOSTS)));
                                  {user, U, S} ->
                                      (U == User) andalso (S == Server);
                                  {server, S} ->
                                      S == Server;
                                  {resource, R} ->
                                      R == Resource;
                                  {user_regexp, UR} ->
                                      ((Host == Server) orelse
                                       ((Host == global) andalso
                                        lists:member(Server, ?MYHOSTS)))
                                          andalso is_regexp_match(User, UR);
                                  {shared_group, G} ->
                                      mod_shared_roster:is_user_in_group({User, Server}, G, Host);
                                  {shared_group, G, H} ->
                                      mod_shared_roster:is_user_in_group({User, Server}, G, H);
                                  {user_regexp, UR, S} ->
                                      (S == Server) andalso
                                          is_regexp_match(User, UR);
                                  {server_regexp, SR} ->
                                      is_regexp_match(Server, SR);
                                  {resource_regexp, RR} ->
                                      is_regexp_match(Resource, RR);
                                  {node_regexp, UR, SR} ->
                                      is_regexp_match(Server, SR) andalso
                                          is_regexp_match(User, UR);
                                  {user_glob, UR} ->
                                      ((Host == Server) orelse
                                       ((Host == global) andalso
                                        lists:member(Server, ?MYHOSTS)))
                                          andalso
                                          is_glob_match(User, UR);
                                  {user_glob, UR, S} ->
                                      (S == Server) andalso
                                          is_glob_match(User, UR);
                                  {server_glob, SR} ->
                                      is_glob_match(Server, SR);
                                  {resource_glob, RR} ->
                                      is_glob_match(Resource, RR);
                                  {node_glob, UR, SR} ->
                                      is_glob_match(Server, SR) andalso
                                          is_glob_match(User, UR);
                                  WrongSpec ->
                                      ?ERROR_MSG(
                                         "Wrong ACL expression: ~p~n"
                                         "Check your config file and reload it with the override_acls option enabled",
                                         [WrongSpec]),
                                      false
                              end
                      end,
                      ets:lookup(acl, {ACL, global}) ++
                      ets:lookup(acl, {ACL, Host}))
    end.

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
    is_regexp_match(String, xmerl_regexp:sh_to_awk(Glob)).

