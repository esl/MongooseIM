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
%% Description: Administration commands for MUC Light
%%==============================================================================

-module(mod_muc_light_commands).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2, stop/1, supported_features/0]).

-export([create_unique_room/4]).
-export([create_identifiable_room/5]).
-export([send_message/4]).
-export([invite_to_room/4]).
-export([change_affiliation/5]).
-export([delete_room/3]).
-export([change_room_config/5]).

-ignore_xref([delete_room/3, invite_to_room/4, send_message/4, change_room_config/5]).

%%--------------------------------------------------------------------
%% `gen_mod' callbacks
%%--------------------------------------------------------------------

start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%--------------------------------------------------------------------
%% Interface descriptions
%%--------------------------------------------------------------------

commands() ->

    [
     [{name, create_muc_light_room},
      {category, <<"muc-lights">>},
      {desc, <<"Create a MUC Light room with unique username part in JID.">>},
      {module, ?MODULE},
      {function, create_unique_room},
      {action, create},
      {identifiers, [domain]},
      {args,
       [
        %% The parent `domain' under which MUC Light is
        %% configured.
        {domain, binary},
        {name, binary},
        {owner, binary},
        {subject, binary}
       ]},
      {result, {name, binary}}],

     [{name, create_identifiable_muc_light_room},
      {category, <<"muc-lights">>},
      {desc, <<"Create a MUC Light room with user-provided username part in JID">>},
      {module, ?MODULE},
      {function, create_identifiable_room},
      {action, update},
      {identifiers, [domain]},
      {args,
       [{domain, binary},
        {id, binary},
        {name, binary},
        {owner, binary},
        {subject, binary}
       ]},
      {result, {id, binary}}],

     [{name, change_muc_light_room_configuration},
      {category, <<"muc-lights">>},
      {subcategory, <<"config">>},
      {desc, <<"Change configuration of MUC Light room.">>},
      {module, ?MODULE},
      {function, change_room_config},
      {action, update},
      {identifiers, [domain]},
      {args,
       [
        {domain, binary},
        {id, binary},
        {name, binary},
        {user, binary},
        {subject, binary}
       ]},
      {result, ok}],

     [{name, invite_to_room},
      {category, <<"muc-lights">>},
      {subcategory, <<"participants">>},
      {desc, <<"Invite to a MUC Light room.">>},
      {module, ?MODULE},
      {function, invite_to_room},
      {action, create},
      {identifiers, [domain, name]},
      {args,
       [{domain, binary},
        {name, binary},
        {sender, binary},
        {recipient, binary}
       ]},
      {result, ok}],

     [{name, send_message_to_muc_light_room},
      {category, <<"muc-lights">>},
      {subcategory, <<"messages">>},
      {desc, <<"Send a message to a MUC Light room.">>},
      {module, ?MODULE},
      {function, send_message},
      {action, create},
      {identifiers, [domain, name]},
      {args,
       [{domain, binary},
        {name, binary},
        {from, binary},
        {body, binary}
       ]},
      {result, ok}],

     [{name, delete_room},
      {category, <<"muc-lights">>},
      {subcategory, <<"management">>},
      {desc, <<"Delete a MUC Light room.">>},
      {module, ?MODULE},
      {function, delete_room},
      {action, delete},
      {identifiers, [domain, name, owner]},
      {args,
       [{domain, binary},
        {name, binary},
        {owner, binary}]},
      {result, ok}]
    ].


%%--------------------------------------------------------------------
%% Internal procedures
%%--------------------------------------------------------------------

create_unique_room(Domain, RoomName, Creator, Subject) ->
    CreatorJID = jid:from_binary(Creator),
    case mod_muc_light_api:create_room(Domain, <<>>, RoomName, CreatorJID, Subject) of
        {ok, #{jid := JID}} -> jid:to_binary(JID);
        Error -> format_err_result(Error)
    end.

create_identifiable_room(Domain, Identifier, RoomName, Creator, Subject) ->
    CreatorJID = jid:from_binary(Creator),
    case mod_muc_light_api:create_room(Domain, Identifier, RoomName, CreatorJID, Subject) of
        {ok, #{jid := JID}} -> jid:to_binary(JID);
        Error -> format_err_result(Error)
    end.

invite_to_room(Domain, RoomName, Sender, Recipient) ->
    SenderJID = jid:from_binary(Sender),
    RecipientJID = jid:from_binary(Recipient),
    Result = mod_muc_light_api:invite_to_room(Domain, RoomName, SenderJID, RecipientJID),
    format_result_no_msg(Result).

change_affiliation(Domain, RoomID, Sender, Recipient, Affiliation) ->
    % FIXME use mod_muc_light_api instead of this in the client api
    SenderJID = jid:from_binary(Sender),
    RecipientJID = jid:from_binary(Recipient),
    mod_muc_light_api:change_affiliation(Domain, RoomID, SenderJID, RecipientJID, Affiliation).

change_room_config(Domain, RoomID, RoomName, User, Subject) ->
    UserJID = jid:from_binary(User),
    Result = mod_muc_light_api:change_room_config(Domain, RoomID, RoomName, UserJID, Subject),
    format_result_no_msg(Result).

send_message(Domain, RoomName, Sender, Message) ->
    SenderJID = jid:from_binary(Sender),
    Result = mod_muc_light_api:send_message(Domain, RoomName, SenderJID, Message),
    format_result_no_msg(Result).

-spec delete_room(jid:lserver(), binary(), jid:literal_jid()) -> ok | {error, atom(), term()}.
delete_room(DomainName, RoomName, Owner) ->
    OwnerJID = jid:from_binary(Owner),
    Result = mod_muc_light_api:delete_room(DomainName, RoomName, OwnerJID),
    format_result_no_msg(Result).

format_result_no_msg({ok, _}) -> ok;
format_result_no_msg(Res) -> format_err_result(Res).

format_err_result({ResStatus, Msg}) when not_exists =:= ResStatus;
                                         room_not_found =:= ResStatus;
                                         domain_not_found =:= ResStatus ->
    {error, not_found, Msg};
format_err_result({ResStatus, Msg}) when exist =:= ResStatus;
                                         not_allowed =:= ResStatus;
                                         user_without_room =:= ResStatus ->
    {error, denied, Msg};
format_err_result({_, Reason}) -> {error, internal, Reason}.
