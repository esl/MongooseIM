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
         server_new/7,
         server_start/3,
         server_step/2]).

-include("ejabberd.hrl").

-record(sasl_mechanism, {mechanism :: mechanism(),
                         module :: sasl_module(),
                         password_type :: plain | digest | scram
                        }).
-type sasl_module() :: cyrsasl_anonymous
                     | cyrsasl_digest
                     | cyrsasl_plain.
-type mechanism() :: binary().
-type sasl_mechanism() :: #sasl_mechanism{}.

-record(sasl_state, {service :: binary(),
                     myname :: ejabberd:server(),
                     realm :: binary(),
                     get_password :: get_password_fun(),
                     check_password :: check_password_fun(),
                     check_password_digest :: check_pass_digest_fun(),
                     mech_mod :: sasl_module(),
                     mech_state :: tuple()
                     }).
-type sasl_state() :: #sasl_state{}.

-type get_password_fun() :: fun((ejabberd:user()) ->
                          {binary(), ejabberd_auth:authmodule()} | {false, none}
                        ).
-type check_password_fun() :: fun((User :: ejabberd:user(),
                                   Password :: binary()) ->
                                      'false' | {'true', ejabberd_auth:authmodule()}
                                 ).
-type check_pass_digest_fun() :: fun((User :: ejabberd:user(),
                                    Server :: ejabberd:server(),
                                    Password :: binary(),
                                    Digest :: binary(),
                                    DigestGen :: fun()) ->
                                      'false' | {'true', ejabberd_auth:authmodule()}
                                  ).
-export_type([get_password_fun/0,
              check_password_fun/0,
              check_pass_digest_fun/0]).

-callback mech_new(Host :: ejabberd:server(),
                   GetPassword :: get_password_fun(),
                   CheckPassword :: check_password_fun(),
                   CheckPasswordDigest :: check_pass_digest_fun()
                   ) -> {ok, tuple()}.
-callback mech_step(State :: tuple(),
                    ClientIn :: binary()
                    ) -> {ok, proplists:proplist()} | {error, binary()}.

-spec start() -> 'ok'.
start() ->
    ets:new(sasl_mechanism, [named_table,
                             public,
                             {keypos, #sasl_mechanism.mechanism}]),
    cyrsasl_plain:start([]),
    cyrsasl_digest:start([]),
    cyrsasl_scram:start([]),
    cyrsasl_anonymous:start([]),
    ok.

-spec register_mechanism(Mechanism :: mechanism(),
                         Module :: sasl_module(),
                         PasswordType :: plain | digest | scram) -> 'true'.
register_mechanism(Mechanism, Module, PasswordType) ->
    ets:insert(sasl_mechanism,
	       #sasl_mechanism{mechanism = Mechanism,
			       module = Module,
			       password_type = PasswordType}).

%%% TODO: use callbacks
%%-include("ejabberd.hrl").
%%-include("jlib.hrl").
%%check_authzid(_State, Props) ->
%%    AuthzId = xml:get_attr_s(authzid, Props),
%%    case jlib:binary_to_jid(AuthzId) of
%%      error ->
%%          {error, "invalid-authzid"};
%%      JID ->
%%          LUser = jlib:nodeprep(xml:get_attr_s(username, Props)),
%%          {U, S, R} = jlib:jid_tolower(JID),
%%          case R of
%%              "" ->
%%                  {error, "invalid-authzid"};
%%              _ ->
%%                  case {LUser, ?MYNAME} of
%%                      {U, S} ->
%%                          ok;
%%                      _ ->
%%                          {error, "invalid-authzid"}
%%                  end
%%          end
%%    end.

-spec check_credentials(sasl_state(), list()) -> 'ok' | {'error', binary()}.
check_credentials(_State, Props) ->
    User = xml:get_attr_s(username, Props),
    case jlib:nodeprep(User) of
        error ->
            {error, <<"not-authorized">>};
        <<>> ->
            {error, <<"not-authorized">>};
        _LUser ->
            ok
    end.

-spec listmech(ejabberd:server()) -> [sasl_mechanism()].
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
                             {'EXIT',{undef,[{Module,store_type,[]} | _]}} ->
                                 ?WARNING_MSG("~p doesn't implement the function store_type/0", [Module]),
                                 [];
                             _Else ->
                                 []
                         end,
                         ['$1']}]),
    filter_anonymous(Host, Mechs).

-spec server_new(Service :: binary(),
                 ServerFQDN :: ejabberd:server(),
                 UserRealm :: binary(),
                 _SecFlags :: [any()],
                 GetPassword :: get_password_fun(),
                 CheckPassword :: check_password_fun(),
                 CheckPasswordDigest :: check_pass_digest_fun()) -> sasl_state().
server_new(Service, ServerFQDN, UserRealm, _SecFlags,
           GetPassword, CheckPassword, CheckPasswordDigest) ->
    #sasl_state{service = Service,
                myname = ServerFQDN,
                realm = UserRealm,
                get_password = GetPassword,
                check_password = CheckPassword,
                check_password_digest= CheckPasswordDigest}.

-spec server_start(sasl_state(),
                 Mech :: any(),
                 ClientIn :: binary()) -> {ok, _}
                                        | {error, binary()}
                                        | {'continue',_,sasl_state()}
                                        | {'error',binary(),ejabberd:user()}.
server_start(State, Mech, ClientIn) ->
    case lists:member(Mech, listmech(State#sasl_state.myname)) of
        true ->
            case ets:lookup(sasl_mechanism, Mech) of
                [#sasl_mechanism{module = Module}] ->
                    {ok, MechState} = Module:mech_new(
                                        State#sasl_state.myname,
                                        State#sasl_state.get_password,
                                        State#sasl_state.check_password,
                                        State#sasl_state.check_password_digest),
                    server_step(State#sasl_state{mech_mod = Module,
                                                 mech_state = MechState},
                                ClientIn);
                _ ->
                    {error, <<"no-mechanism">>}
            end;
        false ->
            {error, <<"no-mechanism">>}
    end.

-spec server_step(State :: sasl_state(), ClientIn :: binary()) ->
                                          {'error',_}
                                          | {'ok',[any()]}
                                          | {'continue',_,sasl_state()}
                                          | {'error',binary(),ejabberd:user()}.
server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
	{ok, Props} ->
	    case check_credentials(State, Props) of
		ok ->
		    {ok, Props};
		{error, Error} ->
		    {error, Error}
	    end;
	{ok, Props, ServerOut} ->
	    case check_credentials(State, Props) of
		ok ->
		    {ok, Props, ServerOut};
		{error, Error} ->
		    {error, Error}
	    end;
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
-spec filter_anonymous(ejabberd:server(), [mechanism()]) -> [mechanism()].
filter_anonymous(Host, Mechs) ->
    case ejabberd_auth_anonymous:is_sasl_anonymous_enabled(Host) of
        true  -> Mechs;
        false -> Mechs -- [<<"ANONYMOUS">>]
    end.
