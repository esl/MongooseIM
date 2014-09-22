%%%----------------------------------------------------------------------
%%% File    : mod_muc_bc_c2s.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Simple mod_muc_light broadcaster, works in c2s context
%%% Created : 9 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
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

-module(mod_muc_light_bc_c2s).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([broadcast/3]).

-include("jlib.hrl").
-include("mod_muc_light.hrl").

%%====================================================================
%% API
%%====================================================================

-spec broadcast(#jid{}, #xmlel{}, affiliations()) -> ok.
broadcast(From, Packet, Affiliations) ->
    lists:foreach(
      fun({User, _}) ->
              Packet1 = xml:replace_tag_attr(
                          <<"to">>, jlib:jid_to_binary(User), Packet),
              ejabberd_router:route(From, jlib:make_jid(User), Packet1)
      end, Affiliations).

%%====================================================================
%% Internal functions
%%====================================================================


