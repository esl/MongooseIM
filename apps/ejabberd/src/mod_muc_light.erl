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
-include("mod_muc_light.hrl").

-define(DEFAULT_HOST, <<"muc.@HOST@">>).

%%====================================================================
%% API
%%====================================================================

-spec backend() -> atom().
backend() ->
    mod_muc_light_db_mnesia.

-spec bcaster() -> atom().
bcaster() ->
    mod_muc_light_bc_c2s.

-spec default_configuration() -> configuration().
default_configuration() ->
    [{roomname, <<"Untitled">>}].

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(ejabberd:server(), list()) -> any().
start(Host, Opts) ->
    MyDomain = gen_mod:get_opt_host(Host, Opts, ?DEFAULT_HOST),
    ejabberd_router:register_route(MyDomain, {apply, ?MODULE, route}),
    (backend()):start(Host, MyDomain),
    ok.

-spec stop(ejabberd:server()) -> any().
stop(Host) ->
    MyDomain = gen_mod:get_module_opt_host(Host, ?MODULE, ?DEFAULT_HOST),
    ejabberd_router:unregister_route(MyDomain),
    (backend()):stop(Host, MyDomain),
    ok.

%%====================================================================
%% Routing
%%====================================================================

-spec route(#jid{}, #jid{}, #xmlel{}) -> ok.
route(From, #jid{ luser = <<>> } = To, Packet) ->
    ejabberd_router:route(
      To, From, jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST));
route(From, To, Packet) ->
    case (backend()):room_exists(To) of
        true ->
            mod_muc_light_room:handle_packet(From, To, Packet);
        false when To#jid.lresource =:= <<>> ->
            create_room(From, To, Packet)
    end.

-spec create_room(#jid{}, #jid{}, #xmlel{}) -> ok.
create_room(From, To, #xmlel{ name = <<"iq">> } = IQ) ->
    case mod_muc_light_utils:iq_to_config(IQ, default_configuration()) of
        {ok, Config} ->
            case (backend()):create_room(To, lower_nores(From), Config) of
                ok ->
                    IQRes = jlib:make_result_iq_reply(IQ),
                    ejabberd_router:route(
                      To, From, IQRes#xmlel{ children = [] });
                {error, exists} ->
                    ErrorPacket = jlib:make_error_reply(IQ, ?ERR_FORBIDDEN),
                    ejabberd_router:route(To, From, ErrorPacket)
            end;
        {error, Error} ->
            ErrorText = format_config_error(Error),
            ErrorPacket = jlib:make_error_reply(
                            IQ, ?ERRT_NOT_ACCEPTABLE("en", ErrorText)),
            ejabberd_router:route(To, From, ErrorPacket)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec lower_nores(#jid{}) -> {binary(), binary(), <<>>}.
lower_nores(JID) -> jlib:jid_remove_resource(jlib:jid_to_lower(JID)).

-spec format_config_error({binary(), atom()}) -> iolist().
format_config_error({Key, Error}) ->
    io_lib:format("~s:~p", [Key, Error]).
