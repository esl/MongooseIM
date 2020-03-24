-module(cyrsasl_scram_sha256).

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

-type client_in() :: [binary()].
-type username_att() :: {term(), binary()}.
-type nonce_attr() :: {term(), binary()}.
-type nonce() :: binary().
-type scram_att() :: {module(), scram_keys()}.
-type scram_keys() :: term().
-type channel_binding() :: term().
-type client_proof() :: term().
-type error() :: {error, binary()} | {error, binary(), binary()}.

-define(SALT_LENGTH, 16).

-define(NONCE_LENGTH, 16).

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"SCRAM-SHA-256">>.

mech_new(_Host, Creds) ->
    {ok, #state{step = 2, creds = Creds}}.

mech_step(#state{step = 2} = State, ClientIn) ->
    ClientInList = binary:split(ClientIn, <<",">>, [global]),
    ClientInSteps = [ fun parse_step2_client_in/1,
                      fun parse_username_attribute/1,
                      fun unescape_username_attribute/1 ],
    case parse_attributes(ClientInList, ClientInSteps) of
        {ok, {UserName, ClientNonce}} ->
            Creds = State#state.creds,
            LServer = mongoose_credentials:lserver(Creds),
            case get_scram_attributes(UserName, LServer) of
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
                      fun parse_channel_binding_attribute/1],
    case parse_attributes(ClientInList, ClientInSteps) of
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
                        mongoose_scram_sha256:server_signature(State#state.server_key,
                                                        AuthMessage),
                    R = [{username, State#state.username}, {sasl_success_response,
                         <<"v=", (jlib:encode_base64(ServerSignature))/binary>>},
                         {auth_module, State#state.username}],
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

-spec parse_step2_client_in(client_in()) -> {username_att(), nonce_attr()} | error().
parse_step2_client_in([_,_,_,_, Extension | _]) when Extension /= [] ->
    {error, <<"protocol-error-extension-not-supported">>};
parse_step2_client_in([CBind | _]) when (CBind =/= <<"y">>) andalso (CBind =/= <<"n">>) ->
    {error, <<"bad-protocol">>};
parse_step2_client_in([_CBind, _AuthIdentity, UserNameAtt, ClientNonceAtt | _]) ->
    {parse_attribute(UserNameAtt), parse_attribute(ClientNonceAtt)}.

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

-spec get_scram_attributes(jid:username(), jid:lserver()) -> scram_att() | error().
get_scram_attributes(UserName, LServer) ->
    case ejabberd_auth:get_passterm_with_authmodule(UserName, LServer) of
        {false, _} ->
            {error, <<"not-authorized">>, UserName};
        {Params, AuthModule} ->
            {AuthModule, do_get_scram_attributes(Params)}
    end.

do_get_scram_attributes(Params) when is_tuple(Params) ->
    Params;
do_get_scram_attributes(Params) ->
    TempSalt = crypto:strong_rand_bytes(?SALT_LENGTH),
    SaltedPassword = mongoose_scram_sha256:salted_password(Params, TempSalt,
                                                    mongoose_scram_sha256:iterations()),
    {mongoose_scram_sha256:stored_key(mongoose_scram_sha256:client_key(SaltedPassword)),
     mongoose_scram_sha256:server_key(SaltedPassword), TempSalt, mongoose_scram_sha256:iterations()}.

create_server_first_message(ClientNonce, ServerNonce, Salt, IterationCount) ->
    iolist_to_binary([<<"r=">>, ClientNonce, ServerNonce, <<",s=">>,
        jlib:encode_base64(Salt), <<",i=">>, integer_to_list(IterationCount)]).

-spec parse_step4_client_in(client_in()) ->
    {channel_binding(), nonce_attr(), client_proof()} | error().
parse_step4_client_in(AttributesList) when length(AttributesList) == 3 ->
    [CBind,
     NonceAtt,
     ClientProofAtt] = [parse_attribute(Attribute) || Attribute <-AttributesList],
     {CBind, NonceAtt, ClientProofAtt};
parse_step4_client_in(_) ->
    {error, <<"bad-protocol">>}.

-spec parse_channel_binding_attribute({channel_binding(), nonce_attr(), client_proof()}) ->
    {ok, {nonce(), client_proof()}} | error().
parse_channel_binding_attribute({{$c, CVal}, NonceAtt, ClientProofAtt}) ->
    CBind = binary:at(jlib:decode_base64(CVal), 0),
    check_channel_binding({CBind, NonceAtt, ClientProofAtt});
parse_channel_binding_attribute(_) ->
    {error, <<"bad-protocol">>}.

check_channel_binding({CBind, _, _ }) when (CBind =/= $n) andalso (CBind =/=$y) ->
    {error, <<"bad-channel-binding">>};
check_channel_binding({_CBind, NonceAtt, ClientProofAtt}) ->
    {ok, {NonceAtt, ClientProofAtt}}.

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
    ClientSignature = mongoose_scram_sha256:client_signature(State#state.stored_key,
                                                      AuthMessage),
    ClientKey = mongoose_scram_sha256:client_key(ClientProof, ClientSignature),
    CompareStoredKey = mongoose_scram_sha256:stored_key(ClientKey),
    {CompareStoredKey, State#state.stored_key}.

-spec verify_stored_key({binary(), binary()}) -> {ok, auth_successful} | error().
verify_stored_key({StoredKey, StoredKey}) ->
    {ok, auth_successful};
verify_stored_key(_) ->
    {error, <<"bad-auth">>}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

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
