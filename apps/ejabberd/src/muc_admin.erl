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

-module(muc_admin).

-behaviour(gen_mod).
-export([start/2, stop/1]).

-export([create_room/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).

commands() ->
    [
     [{name, create_muc_room},
      {category, muc},
      {desc, "Create a MUC room."},
      {module, ?MODULE},
      {function, create_room},
      {action, create},
      {args,
       %% The argument `domain' is what we normally term the XMPP
       %% host, `name' is the room name, `owner' is the XMPP entity
       %% that would normally request an instant MUC room.
       [{domain, binary},
        {name, binary},
        {owner, binary},
        {nick, binary}]},
      {result, {name, binary}}]
    ].

create_room(Domain, Name, Owner, Nick) ->
    try mod_muc:create_instant_room(Domain, Name, jid:from_binary(Owner), Nick, default) of
        ok -> Name
    catch
        Kind:Reason -> {error, {Kind, Reason}}
    end.
