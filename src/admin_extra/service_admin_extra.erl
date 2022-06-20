%%%-------------------------------------------------------------------
%%% File    : service_admin_extra.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(service_admin_extra).
-author('badlop@process-one.net').

-behaviour(mongoose_service).

-include("mongoose_config_spec.hrl").

-export([start/1, stop/0, config_spec/0]).

-define(SUBMODS, [node, accounts, sessions, vcard, roster, last,
                  private, stanza, stats, gdpr, upload, domain
                  %, srg %% Disabled until we add mod_shared_roster
                 ]).

%%%
%%% gen_mod
%%%

-spec start(mongoose_service:options()) -> ok.
start(#{submods := Submods}) ->
    lists:foreach(fun(Submod) ->
                ejabberd_commands:register_commands((mod_name(Submod)):commands())
        end, Submods).

-spec stop() -> ok.
stop() ->
    lists:foreach(fun(Submod) ->
                ejabberd_commands:unregister_commands((mod_name(Submod)):commands())
        end, ?SUBMODS).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"submods">> => #list{items = #option{type = atom,
                                                        validate = {enum, ?SUBMODS}},
                                        validate = unique}
                },
       defaults = #{<<"submods">> => ?SUBMODS}
      }.

mod_name(ModAtom) ->
    list_to_existing_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(ModAtom)).
