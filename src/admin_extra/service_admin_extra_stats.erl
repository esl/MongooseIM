%%%-------------------------------------------------------------------
%%% File    : service_admin_extra_stats.erl
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

-module(service_admin_extra_stats).
-author('badlop@process-one.net').


-export([commands/0]).

-ignore_xref([commands/0, stats/1, stats/2]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").

%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    [
        #ejabberd_commands{name = stats, tags = [stats],
                           desc = "Get statistical value:"
                                  " registeredusers onlineusers onlineusersnode uptimeseconds",
                           module = stats_api, function = stats,
                           args = [{name, binary}],
                           result = {stat, integer}},
        #ejabberd_commands{name = stats_host, tags = [stats],
                           desc = "Get statistical value for this host:"
                                  " registeredusers onlineusers",
                           module = stats_api, function = stats,
                           args = [{name, binary}, {host, binary}],
                           result = {stat, integer}}
        ].
