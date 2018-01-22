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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(cyrsasl_plain).
-author('alexey@process-one.net').

-export([start/1, stop/0, mech_new/2, mech_step/2, parse/1]).
-xep([{xep, 78}, {version, "2.5"}]).
-behaviour(cyrsasl).

-include("mongoose.hrl").

start(_Opts) ->
    cyrsasl:register_mechanism(<<"PLAIN">>, ?MODULE, plain),
    ok.

stop() ->
    ok.

-spec mech_new(Host :: jid:server(),
               Creds :: mongoose_credentials:t()) -> {ok, tuple()}.
mech_new(_Host, Creds) ->
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
            case ejabberd_auth:authorize(Request) of
                {ok, Result} ->
                    {ok, Result};
                {error, not_authorized} ->
                    {error, <<"not-authorized">>, User};
                {error, {no_auth_modules, _}} ->
                    {error, <<"not-authorized">>, User};
                {error, _} ->
                    {error, <<"internal-error">>}
            end;
        _ ->
            {error, <<"bad-protocol">>}
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
    parse1(S, <<>>, []).

-spec parse1(binary(), binary(), [binary()]) -> [binary(), ...].
parse1(<<0, Cs/binary>>, S, T) ->
    parse1(Cs, <<>>, [binary_reverse(S)| T]);
parse1(<<C, Cs/binary>>, S, T) ->
    parse1(Cs, <<C, S/binary>>, T);
%parse1([], [], T) ->
%    lists:reverse(T);
parse1(<<>>, S, T) ->
    lists:reverse([binary_reverse(S)| T]).


-spec parse_domain(binary()) -> [binary(), ...].
parse_domain(S) ->
    parse_domain1(S, <<>>, []).

-spec parse_domain1(binary(), binary(), [binary()]) -> [binary(), ...].
parse_domain1(<<$@, Cs/binary>>, S, T) ->
    parse_domain1(Cs, <<>>, [binary_reverse(S) | T]);
parse_domain1(<<C, Cs/binary>>, S, T) ->
    parse_domain1(Cs, <<C, S/binary>>, T);
parse_domain1(<<>>, S, T) ->
    lists:reverse([binary_reverse(S) | T]).


-spec binary_reverse(binary()) -> binary().
binary_reverse(<<>>) ->
    <<>>;
binary_reverse(<<H, T/binary>>) ->
    <<(binary_reverse(T))/binary, H>>.
