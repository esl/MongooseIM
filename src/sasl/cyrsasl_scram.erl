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

-export([mechanism/0, mech_new/2, mech_step/2]).

-include("mongoose.hrl").

-include("jlib.hrl").

-behaviour(cyrsasl).

-record(state,
        {step = 2              :: 2 | 4,
         stored_key = <<"">>   :: binary(),
         server_key = <<"">>   :: binary(),
         username = <<"">>     :: binary(),
         creds                 :: mongoose_credentials:t(),
         auth_message = <<"">> :: binary(),
         client_nonce = <<"">> :: binary(),
         server_nonce = <<"">> :: binary(),
         auth_module           :: ejabberd_gen_auth:t()}).

-define(SALT_LENGTH, 16).

-define(NONCE_LENGTH, 16).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"SCRAM-SHA-1">>.

mech_new(_Host, Creds) ->
    {ok, #state{step = 2, creds = Creds}}.

mech_step(#state{step = 2} = State, ClientIn) ->
    case re:split(ClientIn, <<",">>, [{return, binary}]) of
        [_CBind, _AuthorizationIdentity, _UserNameAttribute, _ClientNonceAttribute, ExtensionAttribute | _]
          when ExtensionAttribute /= [] ->
            {error, <<"protocol-error-extension-not-supported">>};
        [CBind, _AuthorizationIdentity, UserNameAttribute, ClientNonceAttribute | _]
          when (CBind == <<"y">>) or (CBind == <<"n">>) ->
            case parse_attribute(UserNameAttribute) of
                {error, Reason} -> {error, Reason};
                {_, EscapedUserName} ->
                    case unescape_username(EscapedUserName) of
                        error -> {error, <<"protocol-error-bad-username">>};
                        UserName ->
                            case parse_attribute(ClientNonceAttribute) of
                                {$r, ClientNonce} ->
                                    Creds = State#state.creds,
                                    LServer = mongoose_credentials:lserver(Creds),
                                    case ejabberd_auth:get_passterm_with_authmodule(UserName, LServer) of
                                        {false, _} -> {error, <<"not-authorized">>, UserName};
                                        {Ret, AuthModule} ->
                                            {StoredKey, ServerKey, Salt, IterationCount} =
                                            if is_tuple(Ret) -> Ret;
                                               true ->
                                                   TempSalt =
                                                   crypto:strong_rand_bytes(?SALT_LENGTH),
                                                   SaltedPassword =
                                                   mongoose_scram:salted_password(Ret,
                                                                                  TempSalt,
                                                                                  mongoose_scram:iterations()),
                                                   {mongoose_scram:stored_key(mongoose_scram:client_key(SaltedPassword)),
                                                    mongoose_scram:server_key(SaltedPassword),
                                                    TempSalt,
                                                    mongoose_scram:iterations()}
                                            end,
                                            {NStart, _} = binary:match(ClientIn, <<"n=">>),
                                            ClientFirstMessageBare = binary:part(ClientIn,
                                                                                 {NStart,
                                                                                  byte_size(ClientIn)-NStart}),
                                            ServerNonce =
                                            jlib:encode_base64(crypto:strong_rand_bytes(?NONCE_LENGTH)),
                                            ServerFirstMessage =
                                            iolist_to_binary(
                                              [<<"r=">>,
                                               ClientNonce,
                                               ServerNonce,
                                               <<",s=">>,
                                               jlib:encode_base64(Salt),
                                               <<",i=">>,
                                               integer_to_list(IterationCount)]),
                                            {continue, ServerFirstMessage,
                                             State#state{step = 4, stored_key = StoredKey,
                                                         server_key = ServerKey,
                                                         auth_message =
                                                         <<ClientFirstMessageBare/binary,
                                                           ",", ServerFirstMessage/binary>>,
                                                         client_nonce = ClientNonce,
                                                         server_nonce = ServerNonce,
                                                         username = UserName,
                                                         auth_module = AuthModule}}
                                    end;
                                _Else -> {error, <<"not-supported">>}
                            end
                    end
            end;
        _Else -> {error, <<"bad-protocol">>}
    end;
mech_step(#state{step = 4} = State, ClientIn) ->
    case re:split(ClientIn, <<",">>) of
        [GS2ChannelBindingAttribute, NonceAttribute,
         ClientProofAttribute] ->
            case parse_attribute(GS2ChannelBindingAttribute) of
                {$c, CVal} ->
                    ChannelBindingSupport = binary:at(jlib:decode_base64(CVal), 0),
                    if (ChannelBindingSupport == $n)
                       or (ChannelBindingSupport == $y) ->
                           Nonce = <<(State#state.client_nonce)/binary,
                                     (State#state.server_nonce)/binary>>,
                           case parse_attribute(NonceAttribute) of
                               {$r, CompareNonce} when CompareNonce == Nonce ->
                                   case parse_attribute(ClientProofAttribute) of
                                       {$p, ClientProofB64} ->
                                           ClientProof = jlib:decode_base64(ClientProofB64),
                                           {PStart, _} = binary:match(ClientIn, <<",p=">>),
                                           AuthMessage = iolist_to_binary(
                                                           [State#state.auth_message,
                                                            ",",
                                                            binary:part(ClientIn, 0, PStart)
                                                           ]),
                                           ClientSignature =
                                           mongoose_scram:client_signature(State#state.stored_key,
                                                                           AuthMessage),
                                           ClientKey = mongoose_scram:client_key(ClientProof,
                                                                                 ClientSignature),
                                           CompareStoredKey = mongoose_scram:stored_key(ClientKey),
                                           if CompareStoredKey == State#state.stored_key ->
                                                  ServerSignature =
                                                  mongoose_scram:server_signature(State#state.server_key,
                                                                                  AuthMessage),
                                                  R = [{username, State#state.username},
                                                       {sasl_success_response,
                                                        <<"v=", (jlib:encode_base64(ServerSignature))/binary>>},
                                                       {auth_module, State#state.username}],
                                                  {ok, mongoose_credentials:extend(State#state.creds, R)};
                                              true -> {error, <<"bad-auth">>}
                                           end;
                                       _Else -> {error, <<"bad-protocol">>}
                                   end;
                               {$r, _} -> {error, <<"bad-nonce">>};
                               _Else -> {error, <<"bad-protocol">>}
                           end;
                       true -> {error, <<"bad-channel-binding">>}
                    end;
                _Else -> {error, <<"bad-protocol">>}
            end;
        _Else -> {error, <<"bad-protocol">>}
    end.

parse_attribute(Attribute) ->
    AttributeLen = byte_size(Attribute),
    case AttributeLen >= 3 of
        true ->
            parse_attribute(Attribute, 0);
        _ ->
            {error, <<"bad-format attribute too short">>}
    end.

parse_attribute(<<Char, Rest/binary>>, 0) ->
    case is_alpha(Char) of
        true ->
            parse_attribute(Rest, Char);
        _ ->
            {error, <<"bad-format attribute too short">>}
    end;
parse_attribute(<<Char, Rest/binary>>, PName) ->
    case Char == $= of
        true ->
            {PName, Rest};
        _ ->
            {error, <<"bad-format first char not a letter">>}
    end.


unescape_username(<<"">>) -> <<"">>;
unescape_username(UnescapedUsername) ->
    case binary:match(UnescapedUsername, <<"=">>) of
        nomatch ->
            UnescapedUsername;
        _ ->
            EscapedUsername = binary:replace(
                                binary:replace(UnescapedUsername,
                                               <<"=3D">>, <<"=">>,
                                               [global]),
                                <<"=2C">>, <<",">>, [global]),
            case binary:match(EscapedUsername, <<"=">>) of
                nomatch ->
                    EscapedUsername;
                _ ->
                    error
            end
    end.

is_alpha(Char) when Char >= $a, Char =< $z -> true;
is_alpha(Char) when Char >= $A, Char =< $Z -> true;
is_alpha(_) -> false.
