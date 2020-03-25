%%%----------------------------------------------------------------------
%%% File    : cyrsasl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Cyrus SASL-like library
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(cyrsasl).
-author('alexey@process-one.net').

-export([listmech/1,
         server_new/5,
         server_start/3,
         server_step/2]).

-include("mongoose.hrl").

-type sasl_module() :: module().
-type mechanism() :: binary().

-record(sasl_state, {service :: binary(),
                     myname :: jid:server(),
                     realm :: binary(),
                     mech_mod :: sasl_module(),
                     mech_state :: any(),
                     creds :: mongoose_credentials:t()
                     }).
-type sasl_state() :: #sasl_state{}.

% Either a simple error tag or an error tag + <text> field
-type error() :: {error, binary() | {binary(), binary()}}
               | {error, binary() | {binary(), binary()}, jid:user()}.

-export_type([mechanism/0,
              error/0]).

-callback mechanism() -> mechanism().

-callback mech_new(Host :: jid:server(),
                   Creds :: mongoose_credentials:t()) -> {ok, tuple()}.

-callback mech_step(State :: tuple(),
                    ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                           | cyrsasl:error().

-spec check_credentials(sasl_state(), mongoose_credentials:t()) -> R when
      R :: {'ok', mongoose_credentials:t()}
         | {'error', binary()}.
check_credentials(_State, Creds) ->
    case jid:nodeprep(mongoose_credentials:get(Creds, username, <<>>)) of
        error ->
            {error, <<"not-authorized">>};
        <<>> ->
            {error, <<"not-authorized">>};
        _LUser ->
            {ok, Creds}
    end.

-spec listmech(jid:server()) -> [mechanism()].
listmech(Host) ->
    [M:mechanism() || M <- get_modules(Host), is_module_supported(Host, M)].

-spec server_new(Service :: binary(),
                 ServerFQDN :: jid:server(),
                 UserRealm :: binary(),
                 _SecFlags :: [any()],
                 Creds :: mongoose_credentials:t()) -> sasl_state().
server_new(Service, ServerFQDN, UserRealm, _SecFlags, Creds) ->
    #sasl_state{service = Service,
                myname = ServerFQDN,
                realm = UserRealm,
                creds = Creds}.

-spec server_start(sasl_state(), Mech :: mechanism(), ClientIn :: binary()) -> Result when
      Result :: {ok, mongoose_credentials:t()}
              | {'continue', _, sasl_state()}
              | error().

server_start(State, Mech, ClientIn) ->
    Host = State#sasl_state.myname,
    case [M || M <- get_modules(Host), M:mechanism() =:= Mech, is_module_supported(Host, M)] of
        [Module] ->
            {ok, MechState} = Module:mech_new(Host, State#sasl_state.creds),
            server_step(State#sasl_state{mech_mod = Module,
                                         mech_state = MechState},
                        ClientIn);
        [] ->
            {error, <<"no-mechanism">>}
    end.

is_module_supported(Host, cyrsasl_oauth) ->
    gen_mod:is_loaded(Host, mod_auth_token);
is_module_supported(Host, Module) ->
    mongoose_fips:supports_sasl_module(Module) andalso ejabberd_auth:supports_sasl_module(Host, Module).

-spec server_step(State :: sasl_state(), ClientIn :: binary()) -> Result when
      Result :: {ok, mongoose_credentials:t()}
              | {'continue', _, sasl_state()}
              | error().
server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
        {ok, Creds} ->
            check_credentials(State, Creds);
        {continue, ServerOut, NewMechState} ->
            {continue, ServerOut,
             State#sasl_state{mech_state = NewMechState}};
        {error, Error, Username} ->
            {error, Error, Username};
        {error, Error} ->
            {error, Error}
    end.

-spec get_modules(jid:server()) -> [sasl_module()].
get_modules(Host) ->
    case ejabberd_config:get_local_option({sasl_mechanisms, Host}) of
        undefined -> ejabberd_config:get_local_option_or_default(sasl_mechanisms, default_modules());
        Modules -> Modules
    end.

default_modules() ->
    [cyrsasl_plain,
     cyrsasl_digest,
     cyrsasl_scram,
     cyrsasl_scram_sha256,
     cyrsasl_anonymous,
     cyrsasl_oauth].
