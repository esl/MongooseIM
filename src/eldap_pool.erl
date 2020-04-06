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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%-------------------------------------------------------------------

-module(eldap_pool).

-author('xram@jabber.ru').

%% API
-export([bind/3, search/2, delete/2, add/3, modify_passwd/3]).
-import(eldap_utils, [maybe_b2list/1]).
-include("mongoose.hrl").
-include("eldap.hrl").

%%====================================================================
%% API
%%====================================================================

bind(PoolName, DN, Passwd) ->
  do_request(PoolName, {simple_bind, [maybe_b2list(DN), maybe_b2list(Passwd)]}).

parse_search_opts(Opts) ->
  [parse_opt(O) || O <- Opts].

parse_opt({base, Bin}) -> {base, maybe_b2list(Bin)};
parse_opt({attributes, BinList}) -> {attributes, [maybe_b2list(B) || B <- BinList]};
parse_opt({Atom, List}) -> {Atom, List}.

search(PoolName, Opts) ->
  parse_search_result(do_request(PoolName, {search, [parse_search_opts(Opts)]})).

parse_search_result({ok, #eldap_search_result{entries = Entries, referrals = Refs}}) ->
  #eldap_search_result{entries = parse_entries(Entries), referrals = parse_refs(Refs)};
parse_search_result(R) ->
   R.

parse_entries(Entries) ->
  [#eldap_entry{object_name = list_to_binary(Obj), attributes = parse_attrs(Attrs)} ||
    #eldap_entry{object_name = Obj, attributes = Attrs} <- Entries].

parse_attrs(Attrs) ->
  [{list_to_binary(Name), parse_values(Values)} || {Name, Values} <- Attrs].

parse_values(Values) ->
  [list_to_binary(V) || V <- Values].

parse_refs(R) -> R.


modify_passwd(PoolName, DN, Passwd) ->
  do_request(PoolName, {modify_password, [maybe_b2list(DN), maybe_b2list(Passwd)]}).


delete(PoolName, DN) ->
  case do_request(PoolName, {delete, [maybe_b2list(DN)]}) of
    false -> not_exists;
    R -> R
  end.

add(PoolName, DN, Attrs) ->
  do_request(PoolName, {add, [maybe_b2list(DN), parse_add_atrs(Attrs)]}).

parse_add_atrs(Attrs) ->
  [parse_add_attr(A) || A <- Attrs].

parse_add_attr({N, List}) ->
  {maybe_b2list(N), [maybe_b2list(L) || L <- List]}.


%%====================================================================
%% Internal functions
%%====================================================================

do_request({Host, PoolTag}, Request) ->
    mongoose_wpool:call(ldap, Host, PoolTag, Request).
