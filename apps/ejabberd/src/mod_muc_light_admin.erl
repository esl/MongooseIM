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

-module(mod_muc_light_admin).

-behaviour(gen_mod).
-export([start/2, stop/1]).

-export([create_unique_room/4]).
-export([send_message/4]).
-export([invite_to_room/4]).

-include("mod_muc_light.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").


%%--------------------------------------------------------------------
%% `gen_mod' callbacks
%%--------------------------------------------------------------------

start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).


%%--------------------------------------------------------------------
%% Interface descriptions
%%--------------------------------------------------------------------

commands() ->

    [
     [{name, create_muc_light_room},
      {category, <<"muc-lights">>},
      {desc, <<"Create a MUC Light room.">>},
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
        {creator, binary},
        {subject, binary}
       ]},
      {result, {name, binary}}],

     [{name, invite_to_room},
      {category, <<"muc-lights">>},
      {desc, <<"Invite to a MUC Light room.">>},
      {module, ?MODULE},
      {function, invite_to_room},
      {action, update},
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
        {sender, binary},
        {message, binary}
       ]},
      {result, ok}]
    ].


%%--------------------------------------------------------------------
%% Internal procedures
%%--------------------------------------------------------------------

create_unique_room(Domain, RoomName, Creator, Subject) ->
    C = jid:to_lus(jid:from_binary(Creator)),
    MUCLightDomain = gen_mod:get_module_subhost(Domain, mod_muc_light),
    MUCService = jid:make(<<>>, MUCLightDomain, <<>>),
    Config = make_room_config(RoomName, Subject),
    case mod_muc_light:try_to_create_room(C, MUCService, Config) of
        {ok, RoomUS, _} ->
            jid:to_binary(RoomUS);
        {error, _Reason} = E ->
            E
    end.

invite_to_room(Domain, RoomName, Sender, Recipient0) ->
    Recipient1 = jid:binary_to_bare(Recipient0),
    R = muc_light_room_name_to_jid(jid:from_binary(Sender), RoomName, Domain),
    S = jid:binary_to_bare(Sender),
    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                    [affiliate(jid:to_binary(Recipient1), <<"member">>)]),
    ejabberd_router:route(S, R, iq(jid:to_binary(S), jid:to_binary(R),
                                   <<"set">>, [Changes])).

send_message(Domain, RoomName, Sender, Message) ->
    Body = #xmlel{name = <<"body">>,
                  children = [ #xmlcdata{ content = Message } ]
                 },
    Stanza = #xmlel{name = <<"message">>,
                    attrs = [{<<"type">>, <<"groupchat">>}],
                    children = [ Body ]
                   },
    S = jid:binary_to_bare(Sender),
    case get_user_rooms(S) of
        [] ->
            {error, given_user_does_not_occupy_any_room};
        RoomJIDs when is_list(RoomJIDs) ->
            {RU, RS} = lists:foldl(find_room_with_name(RoomName),
                                   none, RoomJIDs),
            true = is_subdomain(RS, Domain),
            R = jid:make(RU, RS, <<>>),
            ejabberd_router:route(S, R, Stanza)
    end.

%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

make_room_config(Name, Subject) ->
    #create{raw_config = [{<<"roomname">>, Name},
                          {<<"subject">>, Subject}]
           }.

muc_light_room_name_to_jid(Participant, RoomName, Domain) ->
    case get_user_rooms(Participant) of
        [] ->
            {error, given_user_does_not_occupy_any_room};
        RoomJIDs when is_list(RoomJIDs) ->
            {RU, RS} = lists:foldl(find_room_with_name(RoomName),
                                   none, RoomJIDs),
            true = is_subdomain(RS, Domain),
            jid:make(RU, RS, <<>>)
    end.

get_user_rooms(UserJID) ->
    ?BACKEND:get_user_rooms(jid:to_lus(UserJID)).

name_of_room_with_jid(RoomJID) ->
    case ?BACKEND:get_info(RoomJID) of
        {ok, Cfg, _, _} ->
            {roomname, N} = lists:keyfind(roomname, 1, Cfg),
            N
    end.

find_room_with_name(RoomName) ->
    fun (RoomJID, none) ->
            case name_of_room_with_jid(RoomJID) of
                RoomName ->
                    RoomJID;
                _ ->
                    none
            end;
        (_, Acc) when Acc =/= none ->
            Acc
    end.

is_subdomain(Child, Parent) ->
    %% Example input Child = <<"muclight.localhost">> and Parent =
    %% <<"localhost">>
    case binary:match(Child, Parent) of
        nomatch -> false;
        {_, _} -> true
    end.

iq(S, R, T, C) when is_binary(S),
                    is_binary(R), is_binary(T), is_list(C) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"from">>, S},
                    {<<"to">>, R},
                    {<<"type">>, T}],
           children = C
          }.

query(NS, C) when is_binary(NS), is_list(C) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, NS}],
           children = C
          }.

affiliate(JID, Kind) when is_binary(JID), is_binary(Kind) ->
    #xmlel{name = <<"user">>,
           attrs = [{<<"affiliation">>, Kind}],
           children = [ #xmlcdata{ content = JID } ]
          }.

