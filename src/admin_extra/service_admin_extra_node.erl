%%%-------------------------------------------------------------------
%%% File    : service_admin_extra_node.erl
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

-module(service_admin_extra_node).
-author('badlop@process-one.net').

-export([
     commands/0,

         load_config/1,
         get_cookie/0,
         remove_node/1
        ]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").

%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    [
        #ejabberd_commands{name = load_config, tags = [server],
                           desc = "Load ejabberd configuration file",
                           module = ?MODULE, function = load_config,
                           args = [{file, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = get_cookie, tags = [erlang],
                           desc = "Get the Erlang cookie of this node",
                           module = ?MODULE, function = get_cookie,
                           args = [],
                           result = {cookie, string}},
        #ejabberd_commands{name = remove_node, tags = [erlang],
                           desc = "Remove an ejabberd node from Mnesia clustering config",
                           module = ?MODULE, function = remove_node,
                           args = [{node, string}],
                           result = {res, rescode}}
        ].


%%%
%%% Node
%%%

-spec load_config(string()) -> 'ok'.
load_config(Path) ->
    ok = ejabberd_config:load_file(Path).


-spec get_cookie() -> string().
get_cookie() ->
    atom_to_list(erlang:get_cookie()).


-spec remove_node(string()) -> 'ok'.
remove_node(Node) ->
    mnesia:del_table_copy(schema, list_to_atom(Node)),
    ok.

