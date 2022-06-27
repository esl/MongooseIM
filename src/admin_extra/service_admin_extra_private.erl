%%%-------------------------------------------------------------------
%%% File    : service_admin_extra_private.erl
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

-module(service_admin_extra_private).
-author('badlop@process-one.net').

-export([
    commands/0,
    private_get/4,
    private_set/3
    ]).

-ignore_xref([
    commands/0, private_get/4, private_set/3
]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    [
        #ejabberd_commands{name = private_get, tags = [private],
                           desc = "Get some information from a user private storage",
                           module = ?MODULE, function = private_get,
                           args = [{user, binary}, {host, binary}, {element, binary}, {ns, binary}],
                           result = {content, string}},
        #ejabberd_commands{name = private_set, tags = [private],
                           desc = "Set to the user private storage",
                           module = ?MODULE, function = private_set,
                           args = [{user, binary}, {host, binary}, {element, binary}],
                           result = {res, restuple}}
        ].

%%%
%%% Private Storage
%%%

%% Example usage:
%% $ mongooseimctl private_set badlop localhost "\<aa\ xmlns=\'bb\'\>Cluth\</aa\>"
%% $ mongooseimctl private_get badlop localhost aa bb
%% <aa xmlns='bb'>Cluth</aa>

-spec private_get(jid:user(), jid:server(), binary(), binary()) ->
    {not_found, string()} | string().
private_get(Username, Host, Element, Ns) ->
    JID = jid:make(Username, Host, <<>>),
    case mod_private_api:private_get(JID, Element, Ns) of
        {ok, String} -> String;
        Error -> Error
    end.

-spec private_set(jid:user(), jid:server(),
                  ElementString :: binary()) -> {Res, string()} when
    Res :: ok | not_found | not_loaded | parse_error.
private_set(Username, Host, ElementString) ->
    JID = jid:make(Username, Host, <<>>),
    mod_private_api:private_set(JID, ElementString).
