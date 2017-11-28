%%%-------------------------------------------------------------------
%%% File    : eldap_pool.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : LDAP connections pool
%%% Created : 12 Nov 2006 by Evgeniy Khramtsov <xram@jabber.ru>
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

-module(eldap_pool).

-author('xram@jabber.ru').

%% API
-export([start_link/7, stop/1, bind/3, search/2, delete/2, add/3,
         modify_passwd/3]).

-include("ejabberd.hrl").

%%====================================================================
%% API
%%====================================================================

-spec bind(binary(), _, _) -> any().
bind(PoolName, DN, Passwd) ->
    do_request(PoolName, {bind, [DN, Passwd]}).


-spec search(binary(), _) -> any().
search(PoolName, Opts) ->
    do_request(PoolName, {search, [Opts]}).


-spec modify_passwd(binary(), _, _) -> any().
modify_passwd(PoolName, DN, Passwd) ->
    do_request(PoolName, {modify_passwd, [DN, Passwd]}).


-spec delete(binary(), _) -> any().
delete(PoolName, DN) ->
    case do_request(PoolName, {delete, [DN]}) of
        false -> not_exists;
        R -> R
    end.


-spec add(binary(), _, _) -> any().
add(PoolName, DN, Attrs) ->
    do_request(PoolName, {add, [DN, Attrs]}).


-spec start_link(Name :: binary(), Hosts :: [any()], _, _, _, _, _) -> 'ok'.
start_link(Name, Hosts, Backups, Port, Rootdn, Passwd, Opts) ->
    PoolName = make_id(Name),
    pg2:create(PoolName),
    lists:foreach(fun (Host) ->
                          ID = list_to_binary(erlang:ref_to_list(make_ref())),
                          case catch eldap:start_link(ID, [Host | Backups],
                                                      Port, Rootdn, Passwd,
                                                      Opts)
                              of
                            {ok, Pid} -> pg2:join(PoolName, Pid);
                            Err ->
                                  ?INFO_MSG("Err = ~p", [Err]),
                                  error
                          end
                  end,
                  Hosts).


-spec stop(binary()) -> 'ok'.
stop(Name) ->
    Pids = pg2:get_local_members(make_id(Name)),
    lists:foreach(fun (P) ->
                          ok = pg2:leave(make_id(Name), P),
                          eldap:close(P)
                  end, Pids).


%%====================================================================
%% Internal functions
%%====================================================================

-type f() :: add | bind | delete | modify_passwd | search.
-spec do_request(Name :: binary(), {f(), [any(), ...]}) -> any().
do_request(Name, {F, Args}) ->
    case pg2:get_closest_pid(make_id(Name)) of
      Pid when is_pid(Pid) ->
          case catch apply(eldap, F, [Pid | Args]) of
            {'EXIT', {timeout, _}} ->
                ?ERROR_MSG("LDAP request failed: timed out", []);
            {'EXIT', Reason} ->
                ?ERROR_MSG("LDAP request failed: eldap:~p(~p)~nReason: ~p",
                           [F, Args, Reason]),
                {error, Reason};
            Reply -> Reply
          end;
      Err -> Err
    end.


-spec make_id(binary()) -> atom().
make_id(Name) ->
    binary_to_atom(<<"eldap_pool_", Name/binary>>, utf8).
