%%%-------------------------------------------------------------------
%%% File    : service_admin_extra_stanza.erl
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

-module(service_admin_extra_stanza).
-author('badlop@process-one.net').

-export([
    commands/0,

    send_message_headline/4,
    send_message_chat/3,
    send_stanza_c2s/4
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
        #ejabberd_commands{name = send_message_chat, tags = [stanza],
                           desc = "Send a chat message to a local or remote bare of full JID",
                           module = ?MODULE, function = send_message_chat,
                           args = [{from, binary}, {to, binary}, {body, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = send_message_headline, tags = [stanza],
                           desc = "Send a headline message to a local or remote bare of full JID",
                           module = ?MODULE, function = send_message_headline,
                           args = [{from, binary}, {to, binary},
                                   {subject, binary}, {body, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = send_stanza_c2s, tags = [stanza],
                           desc = "Send a stanza as if sent from a c2s session",
                           module = ?MODULE, function = send_stanza_c2s,
                           args = [{user, binary}, {host, binary},
                                   {resource, binary}, {stanza, binary}],
                           result = {res, restuple}}
        ].

%%%
%%% Stanza
%%%

%% @doc Send a chat message to a Jabber account.
-spec send_message_chat(From :: binary(), To :: binary(),
                        Body :: binary() | string()) -> {Res, string()} when
    Res :: bad_jid | ok.
send_message_chat(From, To, Body) ->
    Packet = build_packet(message_chat, [Body]),
    send_packet_all_resources(From, To, Packet).


%% @doc Send a headline message to a Jabber account.
-spec send_message_headline(From :: binary(), To :: binary(),
                            Subject:: binary() | string(),
                            Body :: binary() | string()) ->  {Res, string()} when
    Res :: ok | bad_jid.
send_message_headline(From, To, Subject, Body) ->
    Packet = build_packet(message_headline, [Subject, Body]),
    send_packet_all_resources(From, To, Packet).


%% @doc Send a packet to a Jabber account.
%% If a resource was specified in the JID, the packet is sent only to that
%%      specific resource.
%% If no resource was specified in the JID, and the user is remote or local but
%%      offline, the packet is sent to the bare JID.
%% If the user is local and is online in several resources, the packet is sent
%%      to all its resources.
-spec send_packet_all_resources(
        FromJIDStr :: binary() | jid:jid(),
        ToJIDString :: binary() | jid:jid(),
        exml:element()) ->
    {Res, string()} when
    Res :: bad_jid | ok.
send_packet_all_resources(FromJIDString, ToJIDString, Packet) when is_binary(FromJIDString) ->
    case jid:from_binary(FromJIDString) of
        error ->
            {bad_jid, "Sender JID is invalid"};
        FromJID ->
            send_packet_all_resources(FromJID, ToJIDString, Packet),
            {ok, ""}
    end;
send_packet_all_resources(FromJID, ToJIDString, Packet) when is_binary(ToJIDString) ->
    case jid:from_binary(ToJIDString) of
        error ->
            {bad_jid, "Receiver JID is invalid"};
        ToJID ->
            send_packet_all_resources(FromJID, ToJID, Packet),
            {ok, ""}
    end;
send_packet_all_resources(#jid{} = FromJID, #jid{} = ToJID, Packet) ->
    case ToJID#jid.resource of
        <<"">> ->
            send_packet_all_resources_2(FromJID, ToJID, Packet),
            {ok, ""};
        _Res ->
            route_packet(FromJID, ToJID, Packet),
            {ok, ""}
    end.


-spec send_packet_all_resources_2(FromJID :: jid:jid(),
                                  ToJID :: jid:jid(),
                                  exml:element()) -> ok.
send_packet_all_resources_2(FromJID, ToJID, Packet) ->
    case ejabberd_sm:get_user_resources(ToJID) of
        [] ->
            route_packet(FromJID, ToJID, Packet);
        ToResources ->
            lists:foreach(
              fun(ToResource) ->
                      route_packet(FromJID, jid:replace_resource(ToJID, ToResource), Packet)
              end,
              ToResources)
    end.


-spec route_packet(jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
route_packet(FromJID, ToJID, Packet) ->
    ejabberd_router:route(FromJID, ToJID, Packet).


-spec build_packet('message_chat' | 'message_headline',
                  SubjectBody :: [binary() | string(), ...]) -> exml:element().
build_packet(message_chat, [Body]) ->
    #xmlel{ name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>}, {<<"id">>, mongoose_bin:gen_from_crypto()}],
           children = [#xmlel{ name = <<"body">>, children = [#xmlcdata{content = Body}]}]
          };
build_packet(message_headline, [Subject, Body]) ->
    #xmlel{ name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}, {<<"id">>, mongoose_bin:gen_from_crypto()}],
           children = [#xmlel{ name = <<"subject">>, children = [#xmlcdata{content = Subject}]},
                       #xmlel{ name = <<"body">>, children = [#xmlcdata{content = Body}]}
                      ]
          }.


-spec send_stanza_c2s(jid:user(), jid:server(), jid:resource(),
                      Stanza :: binary()) -> {Res, string()} when
    Res :: user_does_not_exist | bad_stanza | ok.
send_stanza_c2s(Username, Host, Resource, Stanza) ->
    C2sPid = ejabberd_sm:get_session_pid(jid:make(Username, Host, Resource)),
    case C2sPid of
        none ->
            {user_does_not_exist,
             io_lib:format("User ~s@~s/~s does not exist", [Username, Host, Resource])};
        _ ->
            case exml:parse(Stanza) of
                {ok, XmlEl} ->
                    p1_fsm_old:send_event(C2sPid, {xmlstreamelement, XmlEl}),
                    {ok, "Stanza has been sent"};
                {error, _} ->
                    {bad_stanza, "Stanza is incorrect"}
            end
    end.

