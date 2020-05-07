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

-include("jlib.hrl").

-behaviour(cyrsasl).


-record(state,
        {step = 2                  :: 2 | 4,
         stored_key = <<"">>       :: binary(),
         server_key = <<"">>       :: binary(),
         username = <<"">>         :: binary(),
         creds                     :: mongoose_credentials:t(),
         auth_message = <<"">>     :: binary(),
         client_nonce = <<"">>     :: binary(),
         server_nonce = <<"">>     :: binary(),
         auth_module               :: ejabberd_gen_auth:t(),
         sha                       :: sha(),
         plus_variant = none       :: plus_variant(),
         plus_advertised           :: boolean(),
         tls_last_message = <<"">> :: binary()
        }).

-type state() :: #state{}.
-type sha() :: sha | sha224 | sha256 | sha384 | sha512.
-type client_in() :: [binary()].
-type username_att() :: {term(), binary()}.
-type nonce_attr() :: {term(), binary()}.
-type nonce() :: binary().
-type scram_att() :: {module(), scram_keys()}.
-type scram_keys() :: term().
-type channel_binding() :: term().
-type client_proof() :: term().
-type error() :: {error, binary()} | {error, binary(), binary()}.
-type plus_variant() :: none | tls_unique.

-define(SALT_LENGTH, 16).

-define(NONCE_LENGTH, 16).

mech_new(_Host, Creds, #{sha := Sha,
                         socket := Socket,
                         auth_mech := AuthMech,
                         scram_plus := ScramPlus}) ->
    {PlusVariant, TlsLastMessage} = maybe_get_tls_last_message(Socket, ScramPlus),
    {ok, #state{step = 2,
                creds = Creds,
                sha = Sha,
                plus_variant = PlusVariant,
                plus_advertised = is_scram_plus_advertised(Sha, AuthMech),
                tls_last_message = TlsLastMessage}}.

mech_step(#state{step = 2} = State, ClientIn) ->
    ClientInList = binary:split(ClientIn, <<",">>, [global]),
    ClientInSteps = [ fun parse_step2_client_in/1,
                      fun parse_username_attribute/1,
                      fun unescape_username_attribute/1 ],
    case parse_attributes({ClientInList, State}, ClientInSteps) of
        {ok, {UserName, ClientNonce}} ->
            Creds = State#state.creds,
            LServer = mongoose_credentials:lserver(Creds),
            Sha = State#state.sha,
            case get_scram_attributes(UserName, LServer, Sha) of
                {AuthModule, {StoredKey, ServerKey, Salt, IterationCount}} ->
                     {NStart, _} = binary:match(ClientIn, <<"n=">>),
                     ClientFirstMessageBare =
                        binary:part(ClientIn, {NStart, byte_size(ClientIn) - NStart}),
                     ServerNonce = jlib:encode_base64(
                                    crypto:strong_rand_bytes(?NONCE_LENGTH)),
                     ServerFirstMessage =
                        create_server_first_message(ClientNonce, ServerNonce,
                                                    Salt, IterationCount),
                     {continue, ServerFirstMessage,
                     State#state{step = 4, stored_key = StoredKey, server_key = ServerKey,
                                 auth_message = <<ClientFirstMessageBare/binary,
                                                ",", ServerFirstMessage/binary>>,
                                 client_nonce = ClientNonce, server_nonce = ServerNonce,
                                 username = UserName, auth_module = AuthModule}};
                {error, Reason, User} ->
                    {error, Reason, User}
            end;
        {error, Reason} -> {error, Reason}
    end;
mech_step(#state{step = 4} = State, ClientIn) ->
    ClientInList = binary:split(ClientIn, <<",">>, [global]),
    ClientInSteps = [ fun parse_step4_client_in/1,
                      fun parse_channel_binding_attribute/1,
                      fun verify_channel_binding/1],
    case parse_attributes({ClientInList, State}, ClientInSteps) of
        {ok, {NonceAtt, ClientProofAtt}} ->
            Nonce = <<(State#state.client_nonce)/binary,
                      (State#state.server_nonce)/binary>>,
            {PStart, _} = binary:match(ClientIn, <<",p=">>),
            AuthMessage = iolist_to_binary( [State#state.auth_message,",",
                                    binary:part(ClientIn, 0, PStart)]),
            AuthSteps = [fun verify_nonce/1,
                         fun get_client_proof/1,
                         fun get_stored_key/1,
                         fun verify_stored_key/1],
            AuthArgs = [{NonceAtt, Nonce}, ClientProofAtt, AuthMessage, State],
            case parse_attributes(AuthArgs, AuthSteps) of
                {ok, auth_successful} ->
                    ServerSignature =
                        mongoose_scram:server_signature(State#state.sha,
                                                        State#state.server_key,
                                                        AuthMessage),
                    R = [{username, State#state.username}, {sasl_success_response,
                         <<"v=", (jlib:encode_base64(ServerSignature))/binary>>},
                         {auth_module, State#state.auth_module}],
                    {ok, mongoose_credentials:extend(State#state.creds, R)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

 -spec parse_attributes(any(), [fun()]) -> {ok, term()} | error().
parse_attributes(ClientInList, Steps) ->
    lists:foldl(fun
                    (_ , {error, Reason}) -> {error, Reason};
                    (F, Args) -> F(Args)
                end, ClientInList, Steps).

-spec parse_step2_client_in({client_in(), state()}) -> {username_att(), nonce_attr()} | error().
parse_step2_client_in({[_,_,_,_, Extension | _], _}) when Extension /= [] ->
    {error, <<"protocol-error-extension-not-supported">>};
parse_step2_client_in({[CBind, _AuthIdentity, UserNameAtt, ClientNonceAtt | _], State}) ->
    case supported_channel_binding_flag(CBind, State#state.plus_variant, State#state.plus_advertised) of
        true ->
            {parse_attribute(UserNameAtt), parse_attribute(ClientNonceAtt)};
        false ->
            {error, <<"bad-protocol">>}
    end.

supported_channel_binding_flag(<<"p=tls-unique">>, tls_unique, true) -> true;
supported_channel_binding_flag(<<"y">>, none, false) -> true;
supported_channel_binding_flag(<<"n">>, _, _) -> true;
supported_channel_binding_flag(_, _, _) -> false.

-spec parse_username_attribute({username_att(), nonce_attr()}) ->
    {jid:username(), nonce()} | error().
parse_username_attribute({{error, Reason}, _}) ->
    {errror, Reason};
parse_username_attribute({{_, EscapedUserName}, {$r, ClientNonce}}) ->
    {EscapedUserName, ClientNonce};
parse_username_attribute({_, _}) ->
    {errror, <<"not-supported">>}.

-spec unescape_username_attribute({jid:username(), nonce()}) ->
    {ok, {jid:username(), nonce()}} | error().
unescape_username_attribute({EscapedUserName, ClientNonce}) ->
    case unescape_username(EscapedUserName) of
        error -> {error, <<"protocol-error-bad-username">>};
        UserName -> {ok, {UserName, ClientNonce}}
    end.

-spec get_scram_attributes(jid:username(), jid:lserver(), sha()) -> scram_att() | error().
get_scram_attributes(UserName, LServer, Sha) ->
    case ejabberd_auth:get_passterm_with_authmodule(UserName, LServer) of
        {false, _} ->
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
    {mongoose_scram:stored_key(Sha, mongoose_scram:client_key(Sha, SaltedPassword)),
     mongoose_scram:server_key(Sha, SaltedPassword), TempSalt, mongoose_scram:iterations()}.

create_server_first_message(ClientNonce, ServerNonce, Salt, IterationCount) ->
    iolist_to_binary([<<"r=">>, ClientNonce, ServerNonce, <<",s=">>,
        jlib:encode_base64(Salt), <<",i=">>, integer_to_list(IterationCount)]).

-spec parse_step4_client_in({client_in(), state()}) ->
    {channel_binding(), nonce_attr(), client_proof(), state()} | error().
parse_step4_client_in({AttributesList, State}) when length(AttributesList) == 3 ->
    [CBind,
     NonceAtt,
     ClientProofAtt] = [parse_attribute(Attribute) || Attribute <-AttributesList],
     {CBind, NonceAtt, ClientProofAtt, State};
parse_step4_client_in(_) ->
    {error, <<"bad-protocol">>}.

-spec parse_channel_binding_attribute({channel_binding(), nonce_attr(), client_proof(), state()}) ->
    {ok, {nonce(), client_proof()}} | error().
parse_channel_binding_attribute({{$c, CVal}, NonceAtt, ClientProofAtt, State}) ->
    PlusVariant = State#state.plus_variant,
    TlsLastMsg = State#state.tls_last_message,
    IsPlusListed = State#state.plus_advertised,
    {base64:decode(CVal), PlusVariant, IsPlusListed, TlsLastMsg, NonceAtt, ClientProofAtt};
parse_channel_binding_attribute(_) ->
    {error, <<"bad-protocol">>}.

verify_channel_binding({<<"p=tls-unique,,", TlsLastMsg/binary>>,
                        tls_unique, true, TlsLastMsg, NonceAtt, ClientProofAtt}) ->
    {ok, {NonceAtt, ClientProofAtt}};
verify_channel_binding({<<"y", _/binary>>,
                        none, false, _TlsLastMsg, NonceAtt, ClientProofAtt}) ->
    {ok, {NonceAtt, ClientProofAtt}};
verify_channel_binding({<<"n", _/binary>>,
                        none, _, _TlsLastMsg, NonceAtt, ClientProofAtt}) ->
    {ok, {NonceAtt, ClientProofAtt}};
verify_channel_binding(_) ->
    {error, <<"bad-channel-binding">>}.

verify_nonce([{{$r, Nonce}, Nonce} | Args]) ->
    Args;
verify_nonce([{{$r, _ClientNonce}, _Nonce} | _Args]) ->
    {error, <<"bad-nonce">>};
verify_nonce(_) ->
    {error, <<"bad-protocol">>}.

get_client_proof([{$p, ClientProofB64}, AuthMessage, State]) ->
    {jlib:decode_base64(ClientProofB64), AuthMessage, State};
get_client_proof(_) ->
    {error,<<"bad-protocol">>}.

get_stored_key({ClientProof, AuthMessage, State}) ->
    Sha = State#state.sha,
    ClientSignature = mongoose_scram:client_signature(Sha, State#state.stored_key,
                                                      AuthMessage),
    ClientKey = mongoose_scram:client_proof_key(ClientProof, ClientSignature),
    CompareStoredKey = mongoose_scram:stored_key(Sha, ClientKey),
    {CompareStoredKey, State#state.stored_key}.

-spec verify_stored_key({binary(), binary()}) -> {ok, auth_successful} | error().
verify_stored_key({StoredKey, StoredKey}) ->
    {ok, auth_successful};
verify_stored_key(_) ->
    {error, <<"bad-auth">>}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
maybe_get_tls_last_message(Socket, true) ->
    case ejabberd_tls:get_tls_last_message(ejabberd_socket:get_socket(Socket)) of
        {error, _Error} ->
            {none, <<"">>};
        {ok, Msg} ->
            {tls_unique, Msg}
    end;
maybe_get_tls_last_message(_, _) ->
    {none, <<"">>}.

is_scram_plus_advertised(sha, Mech)    -> lists:member(<<"SCRAM-SHA-1-PLUS">>, Mech);
is_scram_plus_advertised(sha224, Mech) -> lists:member(<<"SCRAM-SHA-224-PLUS">>, Mech);
is_scram_plus_advertised(sha256, Mech) -> lists:member(<<"SCRAM-SHA-256-PLUS">>, Mech);
is_scram_plus_advertised(sha384, Mech) -> lists:member(<<"SCRAM-SHA-384-PLUS">>, Mech);
is_scram_plus_advertised(sha512, Mech) -> lists:member(<<"SCRAM-SHA-512-PLUS">>, Mech).

parse_attribute(Attribute) when byte_size(Attribute) >= 3 ->
    parse_attribute(Attribute, 0);
parse_attribute(_) ->
    {error, <<"bad-format attribute too short">>}.

parse_attribute(<<Char, Rest/binary>>, 0) ->
    case is_alpha(Char) of
        true ->
            parse_attribute(Rest, Char);
        _ ->
            {error, <<"bad-format attribute too short">>}
    end;
parse_attribute(<<$=, Rest/binary>>, PName) ->
    {PName, Rest};
parse_attribute(_,_) ->
   {error, <<"bad-format first char not a letter">>}.

unescape_username(<<"">>) -> <<"">>;
unescape_username(UnescapedUsername) ->
    case binary:match(UnescapedUsername, <<"=">>) of
        nomatch ->
            UnescapedUsername;
        _ ->
            EscapedUsername = do_escape_username(UnescapedUsername),
            check_escaped_username(EscapedUsername)
    end.

do_escape_username(UnescapedUsername) ->
    ReplacedEqual = binary:replace(UnescapedUsername, <<"=3D">>, <<"=">>, [global]),
    binary:replace(ReplacedEqual, <<"=2C">>, <<",">>, [global]).

check_escaped_username(EscapedUsername) ->
    case binary:match(EscapedUsername, <<"=">>) of
        nomatch ->
            EscapedUsername;
        _ ->
            error
    end.

is_alpha(Char) when Char >= $a, Char =< $z -> true;
is_alpha(Char) when Char >= $A, Char =< $Z -> true;
is_alpha(_) -> false.
