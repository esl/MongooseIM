%%%----------------------------------------------------------------------
%%% File    : cyrsasl_plain.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : PLAIN SASL mechanism
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

-module(cyrsasl_plain).
-author('alexey@process-one.net').

-export([mechanism/0, mech_new/3, mech_step/2, parse/1]).
-behaviour(cyrsasl).

-include("mongoose.hrl").

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"PLAIN">>.

-spec mech_new(Host   :: jid:server(),
               Creds  :: mongoose_credentials:t(),
               Socket :: term()) -> {ok, tuple()}.
mech_new(_Host, Creds, _Socket) ->
    {ok, Creds}.

-spec mech_step(Creds :: mongoose_credentials:t(),
                ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                       | {error, binary()}.
mech_step(Creds, ClientIn) ->
    case prepare(ClientIn) of
        [AuthzId, User, Password] ->
            Request = mongoose_credentials:extend(Creds,
                                                  [{username, User},
                                                   {password, Password},
                                                   {authzid, AuthzId}]),
            authorize(Request, User);
        _ ->
            {error, <<"bad-protocol">>}
    end.

authorize(Request, User) ->
    case ejabberd_auth:authorize(Request) of
        {ok, Result} ->
            {ok, Result};
        {error, not_authorized} ->
            {error, <<"not-authorized">>, User};
        {error, {no_auth_modules, _}} ->
            {error, <<"not-authorized">>, User};
        {error, R} ->
            ?LOG_DEBUG(#{what => unauthorized_login, reason => R, user => User}),
            {error, <<"internal-error">>}
    end.

-spec prepare(binary()) -> 'error' | [binary(), ...].
prepare(ClientIn) ->
    case parse(ClientIn) of
        [<<>>, UserMaybeDomain, Password] ->
            case parse_domain(UserMaybeDomain) of
                %% <NUL>login@domain<NUL>pwd
                [User, _Domain] ->
                    [UserMaybeDomain,
                     User,
                     Password];
                %% <NUL>login<NUL>pwd
                [User] ->
                    [<<>>, User, Password]
            end;
        %% login@domain<NUL>login<NUL>pwd
        [AuthzId, User, Password] ->
            [AuthzId, User, Password];
        _ ->
            error
    end.


-spec parse(binary()) -> [binary(), ...].
parse(S) ->
    binary:split(S, <<0>>, [global, trim]).

-spec parse_domain(binary()) -> [binary(), ...].
parse_domain(S) ->
    binary:split(S, <<$@>>, [global, trim]).
