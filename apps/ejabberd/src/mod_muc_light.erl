%%%----------------------------------------------------------------------
%%% File    : mod_muc_light.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Lightweight MUC support (subset of XEP-0045)
%%% Created : 8 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_mod).

%% API
-export([backend/0, bcaster/0, default_configuration/0]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% Router export
-export([route/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DEFAULT_HOST, <<"muc.@HOST@">>).
-define(lower(J), jlib:jid_to_lower(J)).

%%====================================================================
%% API
%%====================================================================

backend() ->
    mod_muc_light_db_mnesia.

bcaster() ->
    mod_muc_light_bc_c2s.

default_configuration() ->
    [{title, <<"Untitled">>}].

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    MyDomain = gen_mod:get_opt_host(Host, Opts, ?DEFAULT_HOST),
    ejabberd_router:register_route(MyDomain, {apply, ?MODULE, route}),
    ok.

stop(Host) ->
    MyDomain = gen_mod:get_module_opt_host(Host, ?MODULE, ?DEFAULT_HOST),
    ejabberd_router:unregister_route(MyDomain),
    ok.

%%====================================================================
%% Routing
%%====================================================================

route(From, To, Packet) ->
    case (backend()):room_exists() of
        true ->
            mod_muc_light_room:handle_packet(From, To, Packet);
        false ->
            create_room(From, To, Packet)
    end.

create_room(From, To, #xmlel{ name = <<"iq">> } = Packet) ->
    Config = mod_muc_light_utils:iq_to_config(Packet, default_configuration()),
    (backend()):create_room(To, ?lower(From), Config).
