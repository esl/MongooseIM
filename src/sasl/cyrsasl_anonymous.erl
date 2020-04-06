%%%----------------------------------------------------------------------
%%% File    : cyrsasl_anonymous.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : ANONYMOUS SASL mechanism
%%%  See http://www.ietf.org/internet-drafts/draft-ietf-sasl-anon-05.txt
%%% Created : 23 Aug 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
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

-module(cyrsasl_anonymous).
-xep([{xep, 175}, {version, "1.2"}]).
-export([mechanism/0, mech_new/2, mech_step/2]).

-behaviour(cyrsasl).

-record(state, {creds}).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"ANONYMOUS">>.

-spec mech_new(Host :: jid:server(),
               Creds :: mongoose_credentials:t()) -> {ok, tuple()}.
mech_new(_Host, Creds) ->
    {ok, #state{creds = Creds}}.

-spec mech_step(State :: tuple(), ClientIn :: binary()) -> R when
      R :: {ok, mongoose_credentials:t()} | {error, binary()}.
mech_step(#state{creds = Creds}, _ClientIn) ->
    %% We generate a random username:
    User = <<(mongoose_bin:gen_from_crypto())/binary,
             (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% Checks that the username is available
    case ejabberd_auth:is_user_exists(User, mongoose_credentials:lserver(Creds)) of
        true  -> {error, <<"not-authorized">>};
        false -> {ok, mongoose_credentials:extend(Creds, [{username, User},
                                                          {auth_module, ?MODULE}])}
    end.
