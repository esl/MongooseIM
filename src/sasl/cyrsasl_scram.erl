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
    try
        % Check received attributes
        ClientInList = re:split(ClientIn, <<",">>, [{return, binary}]),
        ok = check_extension_attribute(ClientInList),
        [CBind, _AuthIdentity, UserNameAtt, ClientNonceAtt | _] = ClientInList,
        ok = check_message_headers(CBind),
        EscapedUserName = check_username(parse_attribute(UserNameAtt)),
        UserName = get_username(EscapedUserName),
        ClientNonce = get_client_nonce(parse_attribute(ClientNonceAtt)),
        % Authenticate and prepare response message
        Creds = State#state.creds,
        LServer = mongoose_credentials:lserver(Creds),
        {AuthModule, {StoredKey, ServerKey,
                      Salt, IterationCount}} = get_scram_attributes(UserName, LServer),
        {NStart, _} = binary:match(ClientIn, <<"n=">>),
        ClientFirstMessageBare = binary:part(ClientIn,
                                            {NStart, byte_size(ClientIn) - NStart}),
        ServerNonce = jlib:encode_base64(crypto:strong_rand_bytes(?NONCE_LENGTH)),
        ServerFirstMessage = create_server_first_message(ClientNonce, ServerNonce,
                                                         Salt, IterationCount),

        {continue, ServerFirstMessage,
        State#state{step = 4, stored_key = StoredKey, server_key = ServerKey,
                    auth_message = <<ClientFirstMessageBare/binary,
                                   ",", ServerFirstMessage/binary>>,
                    client_nonce = ClientNonce, server_nonce = ServerNonce,
                    username = UserName, auth_module = AuthModule}}
    catch
        error:Error ->
            case Error of
                {Reason, User} -> {error, Reason, User};
                Error -> {error, Error}
            end;
        Type:Error -> {Type, Error}

    end;
mech_step(#state{step = 4} = State, ClientIn) ->
    try
        % Checked received arrtibutes
        ClientInList = re:split(ClientIn, <<",">>),
        [GS2ChannelBindingAtt,
         NonceAtt,
         ClientProofAtt] = check_attributes(ClientInList),
        ok = check_channel_binding_attribute(GS2ChannelBindingAtt),
        Nonce = <<(State#state.client_nonce)/binary,
                    (State#state.server_nonce)/binary>>,
        ok = check_nonce(NonceAtt, Nonce),
        % Authenticate and prepare response message
        ClientProof = get_client_proof(ClientProofAtt),
        {PStart, _} = binary:match(ClientIn, <<",p=">>),
        AuthMessage = iolist_to_binary(
                        [State#state.auth_message,",",
                        binary:part(ClientIn, 0, PStart)]),
        ClientSignature = mongoose_scram:client_signature(State#state.stored_key,
                                                          AuthMessage),
        ClientKey = mongoose_scram:client_key(ClientProof, ClientSignature),
        CompareStoredKey = mongoose_scram:stored_key(ClientKey),
        ok = verify_stored_key(CompareStoredKey, State#state.stored_key),
        ServerSignature = mongoose_scram:server_signature(State#state.server_key,
                                                          AuthMessage),
        R = [{username, State#state.username}, {sasl_success_response,
             <<"v=", (jlib:encode_base64(ServerSignature))/binary>>},
             {auth_module, State#state.username}],
        {ok, mongoose_credentials:extend(State#state.creds, R)}
    catch
        error:Error ->
            case Error of
                {Reason, User} -> {error, Reason, User};
                Error -> {error, Error}
            end;
        Type:Error -> {Type, Error}

    end.

check_extension_attribute([_,_,_,_, Extension | _]) when Extension /= [] ->
    erlang:error(<<"protocol-error-extension-not-supported">>);
check_extension_attribute(_) ->
    ok.

check_message_headers(CBind) when (CBind == <<"y">>) orelse (CBind == <<"n">>) ->
    ok;
check_message_headers(_) ->
    erlang:error(<<"bad-protocol">>).

check_username({error, Reason}) ->
    erlang:error(Reason);
check_username({_, EscapedUserName}) ->
    EscapedUserName.

get_username(EscapedUserName) ->
    case unescape_username(EscapedUserName) of
        error -> erlang:error(<<"protocol-error-bad-username">>);
        UserName -> UserName
    end.

get_client_nonce({$r, ClientNonce}) ->
    ClientNonce;
get_client_nonce(_) ->
    erlang:error(<<"not-supported">>).

get_scram_attributes(UserName, LServer) ->
    case ejabberd_auth:get_passterm_with_authmodule(UserName, LServer) of
        {false, _} ->
            erlang:error({<<"not-authorized">>, UserName});
        {Params, AuthModule} ->
            {AuthModule, do_get_scram_attributes(Params)}
    end.

do_get_scram_attributes(Params) when is_tuple(Params) ->
    Params;
do_get_scram_attributes(Params) ->
    TempSalt = crypto:strong_rand_bytes(?SALT_LENGTH),
    SaltedPassword = mongoose_scram:salted_password(Params, TempSalt,
                                                    mongoose_scram:iterations()),
    {mongoose_scram:stored_key(mongoose_scram:client_key(SaltedPassword)),
     mongoose_scram:server_key(SaltedPassword), TempSalt, mongoose_scram:iterations()}.

create_server_first_message(ClientNonce, ServerNonce, Salt, IterationCount) ->
    iolist_to_binary([<<"r=">>, ClientNonce, ServerNonce, <<",s=">>,
        jlib:encode_base64(Salt), <<",i=">>, integer_to_list(IterationCount)]).

check_attributes(AttributesList) when length(AttributesList) == 3 ->
    [parse_attribute(Attribute) || Attribute <-AttributesList];
check_attributes(_) ->
    erlang:error(<<"bad-protocol">>).

check_channel_binding_attribute({$c, CVal}) ->
    check_channel_binding_support(binary:at(jlib:decode_base64(CVal), 0));
check_channel_binding_attribute(_) ->
    erlang:error(<<"bad-protocol">>).

check_channel_binding_support(Binding) when (Binding == $n) orelse (Binding ==$y) ->
    ok;
check_channel_binding_support(_) ->
    erlang:error(<<"bad-channel-binding">>).

check_nonce({$r, Nonce}, Nonce) ->
    ok;
check_nonce({$r, _ClientNonce}, _Nonce) ->
    erlang:error(<<"bad-nonce">>);
check_nonce(_, _) ->
    erlang:error(<<"bad-protocol">>).

get_client_proof({$p, ClientProofB64}) ->
    jlib:decode_base64(ClientProofB64);
get_client_proof(_) ->
    erlang:error(<<"bad-protocol">>).

verify_stored_key(StoredKey, StoredKey) ->
    ok;
verify_stored_key(_, _) ->
    erlang:error(<<"bad-auth">>).

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
parse_attribute(<<Char, Rest/binary>>, PName) when Char == $= ->
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
