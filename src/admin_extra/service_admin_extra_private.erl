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
    {error, string()} | string().
private_get(Username, Host, Element, Ns) ->
    case ejabberd_auth:is_user_exists(Username, Host) of
        true ->
            do_private_get(Username, Host, Element, Ns);
        false ->
            {error, io_lib:format("User ~s@~s does not exist", [Username, Host])}
    end.

do_private_get(Username, Host, Element, Ns) ->
    From = jid:make(Username, Host, <<"">>),
    To = jid:make(Username, Host, <<"">>),
    IQ = {iq, <<"">>, get, ?NS_PRIVATE, <<"">>,
          #xmlel{ name = <<"query">>,
                  attrs = [{<<"xmlns">>, ?NS_PRIVATE}],
                  children = [#xmlel{ name = Element, attrs = [{<<"xmlns">>, Ns}]}] } },
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              from_jid => From,
                              to_jid => To,
                              lserver => From#jid.lserver,
                              element => jlib:iq_to_xml(IQ) }),
    {_, ResIq} = mod_private:process_sm_iq(From, To, Acc, IQ),
    [#xmlel{ name = <<"query">>,
             attrs = [{<<"xmlns">>, ?NS_PRIVATE}],
             children = [SubEl] }] = ResIq#iq.sub_el,
    exml:to_binary(SubEl).

-spec private_set(jid:user(), jid:server(),
                  ElementString :: binary()) -> {Res, string()} when
    Res :: ok | user_does_not_exist | user_does_not_exist | not_loaded.
private_set(Username, Host, ElementString) ->
    case exml:parse(ElementString) of
        {error, Error} ->
            String = io_lib:format("Error found parsing the element:~n  ~p~nError: ~p~n",
                      [ElementString, Error]),
            {parse_error, String};
        {ok, Xml} ->
            private_set2(Username, Host, Xml)
    end.


private_set2(Username, Host, Xml) ->
    case ejabberd_auth:is_user_exists(Username, Host) of
        true ->
            do_private_set2(Username, Host, Xml);
        false ->
            {user_does_not_exist, io_lib:format("User ~s@~s does not exist", [Username, Host])}
    end.

do_private_set2(Username, Host, Xml) ->
    case is_private_module_loaded(Host) of
        true ->
            From = jid:make(Username, Host, <<"">>),
            To = jid:make(Username, Host, <<"">>),
            IQ = {iq, <<"">>, set, ?NS_PRIVATE, <<"">>,
                  #xmlel{ name = <<"query">>,
                          attrs = [{<<"xmlns">>, ?NS_PRIVATE}],
                          children = [Xml]}},
            Acc = mongoose_acc:new(#{ location => ?LOCATION,
                                      from_jid => From,
                                      to_jid => To,
                                      lserver => From#jid.lserver,
                                      element => jlib:iq_to_xml(IQ) }),
            mod_private:process_sm_iq(From, To, Acc, IQ),
            {ok, ""};
        false ->
            {not_loaded, io_lib:format("Module mod_private is not loaded on host ~s", [Host])}
    end.

-spec is_private_module_loaded(jid:server()) -> true | false.
is_private_module_loaded(Server) ->
    lists:member(mod_private, gen_mod:loaded_modules(Server)).
