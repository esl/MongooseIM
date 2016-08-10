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
      {category, 'muc-lights'},
      {desc, "Create a MUC Light room."},
      {module, ?MODULE},
      {function, create_unique_room},
      {action, create},
      {identifiers, [domain]},
      {args,
       [{domain, binary}, %% The `domain' under which MUC Light is configured.
        {name, binary},
        {creator, binary},
        {subject, binary}
       ]},
      {result, {name, binary}}],

     [{name, send_message_to_muc_light_room},
      {category, 'muc-lights'},
      {subcategory, <<"messages">>},
      {desc, "Send a message to a MUC Light room."},
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
    MUCLightDomain = gen_mod:get_module_opt_host(Domain, mod_muc,
                                            <<"muclight.@HOST@">>),
    MUCService = jid:make(<<>>, MUCLightDomain, <<>>),
    Config = make_room_config(RoomName, Subject),
    case mod_muc_light:try_to_create_room(C, MUCService, Config) of
        {ok, RoomUS, _} ->
            jid:to_binary(RoomUS);
        {error, _Reason} = E ->
            E
    end.

send_message(Domain, RoomName, Sender, Message) ->
    Body = #xmlel{ name = <<"body">>,
                   children = [ #xmlcdata{ content = Message } ] },
    Stanza = #xmlel{ name = <<"message">>,
                     attrs = [{<<"type">>, <<"groupchat">>}],
                     children = [ Body ] },
    S = jid:from_binary(Sender),
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
    #create{ raw_config = [ {<<"roomname">>, Name},
                            {<<"subject">>, Subject} ] }.

get_user_rooms(UserJID) ->
    mod_muc_light_db_mnesia:get_user_rooms(jid:to_lus(UserJID)).

name_of_room_with_jid(RoomJID) ->
    case mod_muc_light_db_mnesia:get_info(RoomJID) of
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
