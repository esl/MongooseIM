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
-behaviour(mongoose_module_metrics).

-export([start/2, stop/1, supported_features/0]).

-export([create_instant_room/4]).
-export([invite_to_room/5]).
-export([send_message_to_room/4]).
-export([kick_user_from_room/3]).

-ignore_xref([create_instant_room/4, invite_to_room/5, kick_user_from_room/3,
              send_message_to_room/4]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

commands() ->
    [
     [{name, create_muc_room},
      {category, <<"mucs">>},
      {desc, <<"Create a MUC room.">>},
      {module, ?MODULE},
      {function, create_instant_room},
      {action, create},
      {identifiers, [domain]},
      {args,
       %% The argument `domain' is what we normally term the XMPP
       %% domain, `name' is the room name, `owner' is the XMPP entity
       %% that would normally request an instant MUC room.
       [{domain, binary},
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
      {identifiers, [domain, name]},
      {args,
       [{domain, binary},
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
      {identifiers, [domain, name]},
      {args,
       [{domain, binary},
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
      {identifiers, [domain, name, nick]},
      {args,
       [{domain, binary},
        {name, binary},
        {nick, binary}
       ]},
      {result, ok}]

    ].

create_instant_room(Domain, Name, Owner, Nick) ->
    %% Because these stanzas are sent on the owner's behalf through
    %% the HTTP API, they will certainly recieve stanzas as a
    %% consequence, even if their client(s) did not initiate this.
    OwnerJID = jid:binary_to_bare(Owner),
    BareRoomJID = #jid{lserver = MUCDomain} = room_jid(Domain, Name),
    UserRoomJID = jid:make(Name, MUCDomain, Nick),
    %% Send presence to create a room.
    ejabberd_router:route(OwnerJID, UserRoomJID,
                          presence(OwnerJID, UserRoomJID)),
    %% Send IQ set to unlock the room.
    ejabberd_router:route(OwnerJID, BareRoomJID,
                          declination(OwnerJID, BareRoomJID)),
    case verify_room(BareRoomJID, OwnerJID) of
        ok ->
            Name;
        Error ->
            Error
    end.

invite_to_room(Domain, Name, Sender, Recipient, Reason) ->
    case mod_commands:parse_from_to(Sender, Recipient) of
        {ok, SenderJid, RecipientJid} ->
            RoomJid = room_jid(Domain, Name),
            case verify_room(RoomJid, SenderJid) of
                ok ->
                    %% Direct invitation: i.e. not mediated by MUC room. See XEP 0249.
                    X = #xmlel{name = <<"x">>,
                               attrs = [{<<"xmlns">>, ?NS_CONFERENCE},
                                        {<<"jid">>, jid:to_binary(RoomJid)},
                                        {<<"reason">>, Reason}]
                    },
                    Invite = message(SenderJid, RecipientJid, <<>>, [ X ]),
                    ejabberd_router:route(SenderJid, RecipientJid, Invite);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

send_message_to_room(Domain, Name, Sender, Message) ->
    RoomJid = room_jid(Domain, Name),
    case jid:from_binary(Sender) of
        error ->
            error;
        SenderJid ->
            Body = #xmlel{name = <<"body">>,
                          children = [ #xmlcdata{ content = Message } ]
                         },
            Stanza = message(SenderJid, RoomJid, <<"groupchat">>, [ Body ]),
            ejabberd_router:route(SenderJid, RoomJid, Stanza)
    end.

kick_user_from_room(Domain, Name, Nick) ->
    %% All the machinery which is already deeply embedded in the MUC
    %% modules will perform the neccessary checking.
    RoomJid = room_jid(Domain, Name),
    SenderJid = room_moderator(RoomJid),
    Reason = #xmlel{name = <<"reason">>,
                    children = [ #xmlcdata{ content = reason() } ]
                   },
    Item = #xmlel{name = <<"item">>,
                  attrs = [{<<"nick">>, Nick},
                           {<<"role">>, <<"none">>}],
                  children = [ Reason ]
                 },
    IQ = iq(<<"set">>, SenderJid, RoomJid, [ query(?NS_MUC_ADMIN, [ Item ]) ]),
    ejabberd_router:route(SenderJid, RoomJid, IQ).

%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

-spec room_jid(jid:lserver(), binary()) -> jid:jid() | error.
room_jid(Domain, Name) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(Domain),
    MUCDomain = mod_muc:server_host_to_muc_host(HostType, Domain),
    jid:make(Name, MUCDomain, <<>>).

-spec verify_room(jid:jid(), jid:jid()) ->
    ok | {error, internal | not_found, term()}.
verify_room(BareRoomJID, OwnerJID) ->
    case mod_muc_room:can_access_room(BareRoomJID, OwnerJID) of
        {ok, true} ->
            ok;
        {ok, false} ->
            {error, internal, "room is locked"};
        {error, not_found} ->
            {error, not_found, "room does not exist"}
    end.

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
