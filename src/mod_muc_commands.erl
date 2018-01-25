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

-module(mod_muc_commands).

-behaviour(gen_mod).
-export([start/2, stop/1]).

-export([create_instant_room/4]).
-export([invite_to_room/5]).
-export([send_message_to_room/4]).
-export([kick_user_from_room/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).

commands() ->
    [
     [{name, create_muc_room},
      {category, <<"mucs">>},
      {desc, <<"Create a MUC room.">>},
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
      {category, <<"mucs">>},
      {subcategory, <<"participants">>},
      {desc, <<"Send a MUC room invite from one user to another.">>},
      {module, ?MODULE},
      {function, invite_to_room},
      {action, create},
      {identifiers, [host, name]},
      {args,
       [{host, binary},
        {name, binary},
        {sender, binary},
        {recipient, binary},
        {reason, binary}
       ]},
      {result, ok}],

     [{name, send_message_to_room},
      {category, <<"mucs">>},
      {subcategory, <<"messages">>},
      {desc, <<"Send a message to a MUC room from a given user.">>},
      {module, ?MODULE},
      {function, send_message_to_room},
      {action, create},
      {identifiers, [host, name]},
      {args,
       [{host, binary},
        {name, binary},
        {from, binary},
        {body, binary}
       ]},
      {result, ok}],

     [{name, kick_user_from_room},
      {category, <<"mucs">>},
      {desc,
       <<"Kick a user from a MUC room (on behalf of a moderator).">>},
      {module, ?MODULE},
      {function, kick_user_from_room},
      {action, delete},
      {identifiers, [host, name, nick]},
      {args,
       [{host, binary},
        {name, binary},
        {nick, binary}
       ]},
      {result, ok}]

    ].

create_instant_room(Host, Name, Owner, Nick) ->
    %% Because these stanzas are sent on the owner's behalf through
    %% the HTTP API, they will certainly recieve stanzas as a
    %% consequence, even if their client(s) did not initiate this.
    OwnerJID = jid:binary_to_bare(Owner),
    MUCHost = gen_mod:get_module_opt_subhost(Host, mod_muc, mod_muc:default_host()),
    UserRoomJID = jid:make(Name, MUCHost, Nick),
    BareRoomJID = jid:make(Name, MUCHost, <<"">>),
    %% Send presence to create a room.
    ejabberd_router:route(OwnerJID, UserRoomJID,
                          presence(OwnerJID, UserRoomJID)),
    %% Send IQ set to unlock the room.
    ejabberd_router:route(OwnerJID, BareRoomJID,
                          declination(OwnerJID, BareRoomJID)),
    case mod_muc_room:can_access_room(BareRoomJID, OwnerJID) of
        {ok, true} ->
            Name;
        {ok, false} ->
            {error, room_remains_locked};
        {error, not_found} = E ->
            E
    end.

invite_to_room(Host, Name, Sender, Recipient, Reason) ->
    S = jid:binary_to_bare(Sender),
    R = jid:binary_to_bare(Recipient),
    %% Direct invitation: i.e. not mediated by MUC room. See XEP 0249.
    X = #xmlel{name = <<"x">>,
               attrs = [{<<"xmlns">>, ?NS_CONFERENCE},
                        {<<"jid">>, room_address(Name, Host)},
                        {<<"reason">>, Reason}]
              },
    Invite = message(S, R, <<>>, [ X ]),
    ejabberd_router:route(S, R, Invite).

send_message_to_room(Host, Name, Sender, Message) ->
    S = jid:binary_to_bare(Sender),
    Room = jid:from_binary(room_address(Name, Host)),
    B = #xmlel{name = <<"body">>,
               children = [ #xmlcdata{ content = Message } ]
              },
    Stanza = message(S, Room, <<"groupchat">>, [ B ]),
    ejabberd_router:route(S, Room, Stanza).

kick_user_from_room(Host, Name, Nick) ->
    %% All the machinery which is already deeply embedden in the MUC
    %% modules will perform the neccessary checking.
    R = jid:from_binary(room_address(Name, Host)),
    S = room_moderator(R),
    Reason = #xmlel{name = <<"reason">>,
                    children = [ #xmlcdata{ content = reason() } ]
                   },
    K = #xmlel{name = <<"item">>,
               attrs = [{<<"nick">>, Nick},
                        {<<"role">>, <<"none">>}],
               children = [ Reason ]
              },
    IQ = iq(<<"set">>, S, R, [ query(?NS_MUC_ADMIN, [ K ]) ]),
    ejabberd_router:route(S, R, IQ).


%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

room_address(Name, Host) ->
    MUCHost = gen_mod:get_module_opt_subhost(Host, mod_muc, mod_muc:default_host()),
    <<Name/binary, $@, MUCHost/binary>>.

iq(Type, Sender, Recipient, Children)
  when is_binary(Type), is_list(Children) ->
    Addresses = address_attributes(Sender, Recipient),
    #xmlel{name = <<"iq">>,
           attrs = Addresses ++ [{<<"type">>, Type}],
           children = Children
          }.

message(Sender, Recipient, Type, Contents)
  when is_binary(Type), is_list(Contents) ->
    Addresses = address_attributes(Sender, Recipient),
    Attributes = case Type of
                     <<>> -> Addresses;
                     _ -> [{<<"type">>, Type}|Addresses]
                 end,
    #xmlel{name = <<"message">>,
           attrs = Attributes,
           children = Contents
          }.

query(XMLNameSpace, Children)
  when is_binary(XMLNameSpace), is_list(Children) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, XMLNameSpace}],
           children = Children
          }.

presence(Sender, Recipient) ->
    #xmlel{name = <<"presence">>,
           attrs = address_attributes(Sender, Recipient),
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, ?NS_MUC}]}]
          }.

declination(Sender, Recipient) ->
    iq(<<"set">>, Sender, Recipient, [ data_submission() ]).

data_submission() ->
    query(?NS_MUC_OWNER, [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, ?NS_XDATA},
                                       {<<"type">>, <<"submit">>}]}]).

address_attributes(Sender, Recipient) ->
    [
     {<<"from">>, jid:to_binary(Sender)},
     {<<"to">>, jid:to_binary(Recipient)}
    ].

reason() ->
    <<"Kicked through HTTP Administration API.">>.


room_moderator(RoomJID) ->
    [JIDStruct|_] =
        [ UserJID
          || #user{ jid = UserJID,
                    role = moderator } <- room_users(RoomJID) ],
    JIDStruct.

room_users(RoomJID) ->
    {ok, Affiliations} = mod_muc_room:get_room_users(RoomJID),
    Affiliations.
