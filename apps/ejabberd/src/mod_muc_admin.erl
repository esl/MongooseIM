%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Author: Joseph Yiasemides <joseph.yiasemides@erlang-solutions.com>
%% Description: Administration commands for Mult-user Chat (MUC)
%%==============================================================================

-module(mod_muc_admin).

-behaviour(gen_mod).
-export([start/2, stop/1]).

-export([create_instant_room/4]).
-export([invite_to_room/5]).
-export([send_message_to_room/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).

commands() ->
    [
     [{name, create_muc_room},
      {category, mucs},
      {desc, "Create a MUC room."},
      {module, ?MODULE},
      {function, create_instant_room},
      {action, create},
      {identifiers, [host]},
      {args,
       %% The argument `host' is what we normally term the XMPP
       %% host, `name' is the room name, `owner' is the XMPP entity
       %% that would normally request an instant MUC room.
       [{host, binary},
        {name, binary},
        {owner, binary},
        {nick, binary}]},
      {result, {name, binary}}],

     [{name, invite_to_muc_room},
      {category, mucs},
      {desc, "Send a MUC room invite from one user to another."},
      {module, ?MODULE},
      {function, invite_to_room},
      {action, update},
      {identifiers, [host, name]},
      {args,
       [{host, binary},
        {name, binary},
        {sender, binary},
        {recipient, binary},
        {reason, binary}
       ]},
      {result, ok}],

     %% This breaks the module because we have two HTTP end-points
     %% with the same URL and method.
     [{name, send_message_to_room},
      {category, mucs},
      {subcategory, <<"messages">>},
      {desc, "Send a message to a MUC room from a given user."},
      {module, ?MODULE},
      {function, send_message_to_room},
      {action, create},
      {identifiers, [host, name]},
      {args,
       [{host, binary},
        {name, binary},
        {sender, binary},
        {message, binary}
       ]},
      {result, ok}]

    ].

create_instant_room(Host, Name, Owner, Nick) ->
    %% Because these stanzas are sent on the owner's behalf, they will
    %% certainly recieve stanzas as a consequence, even if their
    %% client(s) did not initiate anything.
    OwnerJID = jid:from_binary(Owner),
    MUCHost = gen_mod:get_module_opt_host(Host, mod_muc,
                                            <<"muc.@HOST@">>),
    UserRoomJID = jid:make(Name, MUCHost, Nick),
    BareRoomJID = jid:make(Name, MUCHost, <<"">>),
    %% Send presence to create a room.
    ejabberd_router:route(OwnerJID, UserRoomJID, presence()),
    %% Send IQ set to unlock the room.
    ejabberd_router:route(OwnerJID, BareRoomJID, declination()),
    case mod_muc_room:can_access_room(BareRoomJID, OwnerJID) of
        {ok, true} ->
            Name;
        {ok, false} ->
            {error, room_remains_locked};
        {error, not_found} = E ->
            E
    end.

invite_to_room(Host, Name, Sender, Recipient, Reason) ->
    S = jid:from_binary(Sender),
    R = jid:from_binary(Recipient),
    %% Direct invitation: i.e. not mediated by MUC room. See XEP 0249.
    X = #xmlel{
           name = <<"x">>,
           attrs =
               [ {<<"xmlns">>, ?NS_CONFERENCE},
                 {<<"jid">> , room_jid(Name, Host)},
                 {<<"reason">>, Reason}
               ]
          },
    Invite = #xmlel{name = <<"message">>, children = [ X ]},
    ejabberd_router:route(S, R, Invite).

send_message_to_room(Host, Name, Sender, Message) ->
    S = jid:from_binary(Sender),
    Room = jid:from_binary(room_jid(Name, Host)), %% Go for jid:make/3 instead.
    X = #xmlel{
           name = <<"body">>,
           children =
               [ #xmlcdata{ content = Message } ]
          },
    Stanza = #xmlel{
                 name = <<"message">>, 
                 attrs = [{<<"type">>, <<"groupchat">>}],
                 children = [ X ]},
    ejabberd_router:route(S, Room, Stanza).


%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

room_jid(Name, Host) ->
    MUCHost = gen_mod:get_module_opt_host(Host, mod_muc,
                                            <<"muc.@HOST@">>),
    <<Name/binary, $@, MUCHost/binary>>.

presence() ->
    #xmlel{
       name = <<"presence">>,
       children = [ #xmlel{
                       name = <<"x">>,
                       attrs = [{<<"xmlns">>, ?NS_MUC}]} ]}.

declination() ->
    #xmlel{
       name = <<"iq">>,
       attrs = [{<<"type">>, <<"set">>}],
       children = [ data_submission() ]}.

data_submission() ->
    #xmlel{
       name = <<"query">>,
       attrs = [{<<"xmlns">>, ?NS_MUC_OWNER}],
       children = [ #xmlel{
                       name = <<"x">>,
                       attrs = [{<<"xmlns">>, ?NS_XDATA},
                                {<<"type">>, <<"submit">>}]} ]}.
