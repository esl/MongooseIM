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
      {function, create_unique_room},
      {action, create},
      {identifiers, [domain]},
      {args,
       [{domain, binary}, %% The `domain' under which MUC Light is configured.
        {name, binary},
        {creator, binary},
        {subject, binary}
       ]},
      {result, {name, binary}}]
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

%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

make_room_config(Name, Subject) ->
    #create{ raw_config = [ {<<"roomname">>, Name},
                            {<<"subject">>, Subject} ] }.
