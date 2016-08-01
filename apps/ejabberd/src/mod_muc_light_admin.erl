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

-export([create_room/3]).

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
     [{name, create_muc_room},
      {category, 'muc-lights'},
      {desc, "Create a MUC room."},
      {module, ?MODULE},
      {function, create_room},
      {action, create},
      {identifiers, [domain]},
      {args,
       [{domain, binary}, %% The `domain' under which MUC Light is configured.
        {name, binary},
        {creator, binary}
       ]},
      {result, {name, binary}}]
    ].


%%--------------------------------------------------------------------
%% Internal procedures
%%--------------------------------------------------------------------

create_room(Domain, RoomName, Creator) ->
    C = jid:to_lus(jid:from_binary(Creator)),
    RoomJID = jid:from_binary(room_jid(RoomName, Domain)),
    try mod_muc_light:create_room_just_with_validation(C, RoomJID, #create{}) of
        {RoomName, _} ->
                 RoomName;
        {error, Reason} = E ->
            E
    catch
        Class:Reason ->
             {error, {Class, Reason}}
    end.


%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

room_jid(Name, Domain) ->
    MUCLightDomain = gen_mod:get_module_opt_host(Domain, mod_muc,
                                            <<"muclight.@HOST@">>),
        <<Name/binary, $@, MUCLightDomain/binary>>.
