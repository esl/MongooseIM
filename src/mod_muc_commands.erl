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

-include_lib("jid/include/jid.hrl").

-ignore_xref([create_instant_room/4, invite_to_room/5, kick_user_from_room/3,
              send_message_to_room/4]).

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
    case jid:binary_to_bare(Owner) of
        error ->
            error;
        OwnerJID ->
            #jid{luser = RName, lserver = MUCServer} = room_jid(Domain, Name),
            case mod_muc_api:create_instant_room(MUCServer, RName, OwnerJID, Nick) of
                {ok, #{title := RName}} -> RName;
                Error -> make_rest_error(Error)
            end
    end.

invite_to_room(Domain, Name, Sender, Recipient, Reason) ->
    case mongoose_stanza_helper:parse_from_to(Sender, Recipient) of
        {ok, SenderJID, RecipientJID} ->
            RoomJID = room_jid(Domain, Name),
            case mod_muc_api:invite_to_room(RoomJID, SenderJID, RecipientJID, Reason) of
                {ok, _} ->
                    ok;
                Error ->
                    make_rest_error(Error)
            end;
        Error ->
            Error
    end.

send_message_to_room(Domain, Name, Sender, Message) ->
    RoomJID = room_jid(Domain, Name),
    case jid:from_binary(Sender) of
        error ->
            error;
        SenderJID ->
            mod_muc_api:send_message_to_room(RoomJID, SenderJID, Message)
    end.

kick_user_from_room(Domain, Name, Nick) ->
    Reason = <<"User kicked from the admin REST API">>,
    RoomJID = room_jid(Domain, Name),
    case mod_muc_api:kick_user_from_room(RoomJID, Nick, Reason) of
        {ok, _} ->
            ok;
        Error ->
            make_rest_error(Error)
    end.

%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

-spec room_jid(jid:lserver(), binary()) -> jid:jid() | error.
room_jid(Domain, Name) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(Domain),
    MUCDomain = mod_muc:server_host_to_muc_host(HostType, Domain),
    jid:make(Name, MUCDomain, <<>>).

make_rest_error({room_not_found, ErrMsg}) -> {error, not_found, ErrMsg};
make_rest_error({user_not_found, ErrMsg}) -> {error, not_found, ErrMsg};
make_rest_error({moderator_not_found, ErrMsg}) -> {error, not_found, ErrMsg};
make_rest_error({internal, ErrMsg}) -> {error, internal, ErrMsg}.
