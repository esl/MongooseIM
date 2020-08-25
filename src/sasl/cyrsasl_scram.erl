%%%----------------------------------------------------------------------
%%% File    : cyrsasl_scram.erl
%%% Author  : Stephen Röttger <stephen.roettger@googlemail.com>
%%% Purpose : SASL SCRAM authentication
%%% Created : 7 Aug 2011 by Stephen Röttger <stephen.roettger@googlemail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-module(cyrsasl_scram).

-author('stephen.roettger@googlemail.com').

-export([mech_new/3, mech_step/2]).

-include("mongoose.hrl").

-behaviour(cyrsasl).

-type sha() :: sha | sha224 | sha256 | sha384 | sha512.
-type scram_att() :: {module(), scram_keys()}.
-type scram_keys() :: term().
-type error() :: {error, binary()} | {error, binary(), binary()}.

-define(SALT_LENGTH, 16).
-define(NONCE_LENGTH, 16).

mech_new(LServer, Creds, #{sha := Sha,
                           socket := Socket,
                           auth_mech := AuthMech,
                           scram_plus := ScramPlus}) ->
    ChannelBinding = calculate_channel_binding(Socket, ScramPlus, Sha, AuthMech),
    Fun = fun(Username, St0) ->
                  JID = jid:make(Username, LServer, <<>>),
                  case get_scram_attributes(JID, Sha) of
                      {AuthModule, {StoredKey, ServerKey, Salt, ItCount}} ->
                          Creds1 = fast_scram:mech_get(creds, St0, Creds),
                          R = [{username, Username}, {auth_module, AuthModule}],
                          Creds2 = mongoose_credentials:extend(Creds1, R),
                          St1 = fast_scram:mech_set(creds, Creds2, St0),
                          ExtraConfig = #{it_count => ItCount, salt => Salt,
                                          auth_data => #{stored_key => StoredKey,
                                                         server_key => ServerKey}},
                          {St1, ExtraConfig};
                      {error, Reason, User} ->
                          {error, {Reason, User}}
                  end
          end,
    {ok, St0} = fast_scram:mech_new(
                        #{entity => server,
                          hash_method => Sha,
                          retrieve_mechanism => Fun,
                          nonce_size => ?NONCE_LENGTH,
                          channel_binding => ChannelBinding}),
    St1 = fast_scram:mech_set(creds, Creds, St0),
    {ok, St1}.

mech_step(State, ClientIn) ->
    case fast_scram:mech_step(State, ClientIn) of
        {continue, Msg, NewState} ->
            {continue, Msg, NewState};
        {ok, Msg, FinalState} ->
            Creds0 = fast_scram:mech_get(creds, FinalState),
            R = [{sasl_success_response, Msg}],
            Creds1 = mongoose_credentials:extend(Creds0, R),
            {ok, Creds1};
        {error, Reason, _} ->
            {error, Reason}
    end.

-spec get_scram_attributes(jid:jid(), sha()) -> scram_att() | error().
get_scram_attributes(JID, Sha) ->
    case ejabberd_auth:get_passterm_with_authmodule(JID) of
        {false, _} ->
            {UserName, _} = jid:to_lus(JID),
            {error, <<"not-authorized">>, UserName};
        {Params, AuthModule} ->
            {AuthModule, do_get_scram_attributes(Params, Sha)}
    end.

do_get_scram_attributes(#{iteration_count := IterationCount} = Params, Sha) ->
    #{Sha := #{salt := Salt, stored_key := StoredKey, server_key := ServerKey}} = Params,
    {base64:decode(StoredKey), base64:decode(ServerKey),
     base64:decode(Salt), IterationCount};
do_get_scram_attributes(Password, Sha) ->
    TempSalt = crypto:strong_rand_bytes(?SALT_LENGTH),
    SaltedPassword = mongoose_scram:salted_password(Sha, Password, TempSalt,
                                                    mongoose_scram:iterations()),
    {fast_scram:stored_key(Sha, fast_scram:client_key(Sha, SaltedPassword)),
     fast_scram:server_key(Sha, SaltedPassword), TempSalt, mongoose_scram:iterations()}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
calculate_channel_binding(Socket, ScramPlus, Sha, AuthMech) ->
    {CBVariant, CBData} = maybe_get_tls_last_message(Socket, ScramPlus),
    Advertised = is_scram_plus_advertised(Sha, AuthMech),
    case {Advertised, CBVariant} of
        {true, _} -> {CBVariant, CBData};
        {false, none} -> {undefined, <<>>};
        {false, CBVariant} -> {CBVariant, CBData}
    end.

maybe_get_tls_last_message(Socket, true) ->
    case ejabberd_tls:get_tls_last_message(ejabberd_socket:get_socket(Socket)) of
        {error, _Error} ->
            {none, <<>>};
        {ok, Msg} ->
            {<<"tls-unique">>, Msg}
    end;
maybe_get_tls_last_message(_, _) ->
    {none, <<>>}.

is_scram_plus_advertised(sha, Mech)    -> lists:member(<<"SCRAM-SHA-1-PLUS">>, Mech);
is_scram_plus_advertised(sha224, Mech) -> lists:member(<<"SCRAM-SHA-224-PLUS">>, Mech);
is_scram_plus_advertised(sha256, Mech) -> lists:member(<<"SCRAM-SHA-256-PLUS">>, Mech);
is_scram_plus_advertised(sha384, Mech) -> lists:member(<<"SCRAM-SHA-384-PLUS">>, Mech);
is_scram_plus_advertised(sha512, Mech) -> lists:member(<<"SCRAM-SHA-512-PLUS">>, Mech).
