%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_last.erl
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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_admin_extra_last).
-author('badlop@process-one.net').

-export([
         commands/0,
         set_last/4
        ]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    [
     #ejabberd_commands{name = set_last, tags = [last],
                        desc = "Set last activity information",
                        longdesc = "Timestamp is the seconds since"
                        "1970-01-01 00:00:00 UTC, for example: date +%s",
                        module = ?MODULE, function = set_last,
                        args = [{user, binary}, {host, binary},
                                {timestamp, integer}, {status, binary}],
                        result = {res, restuple}}
    ].

%%%
%%% Last Activity
%%%

-spec set_last(jid:user(), jid:server(), _, _) -> {Res, string()} when
    Res :: ok | user_does_not_exist.
set_last(User, Server, Timestamp, Status) ->
    case ejabberd_auth:is_user_exists(User, Server) of
        true ->
            mod_last:store_last_info(jid:nodeprep(User), jid:nameprep(Server), Timestamp, Status),
            {ok, io_lib:format("Last activity for user ~s@~s is set as ~B with status ~s",
                               [User, Server, Timestamp, Status])};
        false ->
            String = io_lib:format("User ~s@~s does not exist", [User, Server]),
            {user_does_not_exist, String}
    end.

