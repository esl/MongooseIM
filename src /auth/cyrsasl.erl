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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(cyrsasl).
-author('alexey@process-one.net').

-export([start/0,
         register_mechanism/3,
         listmech/1,
         server_new/5,
         server_start/3,
         server_step/2]).

-include("mongoose.hrl").

-record(sasl_mechanism, {
          mechanism,
          module,
          password_type
         }).
-type sasl_module() :: module().
-type sasl_mechanism() :: #sasl_mechanism{
                             mechanism :: mechanism(),
                             module :: sasl_module(),
                             password_type :: password_type()
                            }.
-type mechanism() :: binary().
-type password_type() :: plain | digest | scram.

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
              password_type/0,
              error/0]).

-callback mech_new(Host :: jid:server(),
                   Creds :: mongoose_credentials:t()) -> {ok, tuple()}.

-callback mech_step(State :: tuple(),
                    ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                           | cyrsasl:error().

-spec start() -> 'ok'.
start() ->
    ets:new(sasl_mechanism, [named_table,
                             public,
                             {keypos, #sasl_mechanism.mechanism}]),
    MechOpts = [],
    [ Mech:start(MechOpts) || Mech <- get_mechanisms() ],
    ok.

-spec register_mechanism(Mechanism :: mechanism(),
                         Module :: sasl_module(),
                         PasswordType :: password_type()) -> true.
register_mechanism(Mechanism, Module, PasswordType) ->
    ets_insert_mechanism(#sasl_mechanism{mechanism = Mechanism,
                                         module = Module,
                                         password_type = PasswordType}).

-spec ets_insert_mechanism(MechanismRec :: sasl_mechanism()) -> true.
ets_insert_mechanism(MechanismRec) ->
    ets:insert(sasl_mechanism, MechanismRec).

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
    Mechs = ets:select(sasl_mechanism,
                       [{#sasl_mechanism{mechanism = '$1',
                                         password_type = '$2',
                                         _ = '_'},
                         case catch ejabberd_auth:store_type(Host) of
                             external ->
                                 [{'==', '$2', plain}];
                             scram ->
                                 [{'/=', '$2', digest}];
                             {'EXIT', {undef, [{Module, store_type, []} | _]}} ->
                                 ?WARNING_MSG("~p doesn't implement the function store_type/0",
                                              [Module]),
                                 [];
                             _Else ->
                                 []
                         end,
                         ['$1']}]),
    filter_mechanisms(Host, Mechs,
                      [{<<"ANONYMOUS">>,
                        fun(H)-> ejabberd_auth_anonymous:is_sasl_anonymous_enabled(H) end},
                       {<<"X-OAUTH">>,
                        fun(H) -> gen_mod:is_loaded(H, mod_auth_token) end}]).

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

-spec server_start(sasl_state(),
                 Mech :: mechanism(),
                 ClientIn :: binary()) -> {ok, _}
                                        | {ok, term(), term()}
                                        | {error, binary()}
                                        | {'continue', _, sasl_state()}
                                        | {'error', binary(), jid:user()}.
server_start(State, Mech, ClientIn) ->
    case lists:member(Mech, listmech(State#sasl_state.myname)) of
        true ->
            case lookup_mech(Mech) of
                [#sasl_mechanism{module = Module}] ->
                    {ok, MechState} = Module:mech_new(State#sasl_state.myname,
                                                      State#sasl_state.creds),
                    server_step(State#sasl_state{mech_mod = Module,
                                                 mech_state = MechState},
                                ClientIn);
                _ ->
                    {error, <<"no-mechanism">>}
            end;
        false ->
            {error, <<"no-mechanism">>}
    end.

-spec lookup_mech(Mech :: mechanism()) -> [sasl_mechanism()].
lookup_mech(Mech) ->
    ets:lookup(sasl_mechanism, Mech).

-spec server_step(State :: sasl_state(), ClientIn :: binary()) ->
                                          {'error', _}
                                          | {'ok', [any()]}
                                          | {'ok', [any()], term()}
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

%% @doc Remove the anonymous mechanism from the list if not enabled for the
%% given host
-spec filter_mechanisms(jid:server(), [mechanism()],
                        [{mechanism(), fun()}]) -> [mechanism()].
filter_mechanisms(Host, Mechanisms, UnwantedMechanisms) ->
    lists:foldl(fun({Mechanism, FilterFun}, Acc) ->
                        case FilterFun(Host) of
                            true -> Acc;
                            false -> Acc -- [Mechanism]
                        end
                end, Mechanisms, UnwantedMechanisms).

get_mechanisms() ->
    Default = [cyrsasl_plain,
               cyrsasl_digest,
               cyrsasl_scram,
               cyrsasl_anonymous,
               cyrsasl_oauth],
    ejabberd_config:get_local_option_or_default(sasl_mechanisms, Default).
