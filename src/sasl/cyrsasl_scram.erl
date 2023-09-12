%%%=============================================================================
%%% @copyright (C) 1999-2021, Erlang Solutions Ltd
%%% @doc SASL SCRAM implementation
%%% @end
%%%=============================================================================
-module(cyrsasl_scram).

-export([mech_new/3, mech_step/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

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
    {ok, St0} = fast_scram:mech_new(
                        #{entity => server,
                          hash_method => Sha,
                          retrieve_mechanism => retrieve_mechanism_fun(LServer, Creds, Sha),
                          nonce_size => ?NONCE_LENGTH,
                          channel_binding => ChannelBinding}),
    St1 = fast_scram:mech_set(creds, Creds, St0),
    {ok, St1}.

retrieve_mechanism_fun(LServer, Creds, Sha) ->
    fun(Username, St0) ->
            case jid:make_bare(Username, LServer) of
                error -> {error, {invalid_username, Username}};
                JID ->
                    retrieve_mechanism_continue(JID, Creds, Sha, St0)
            end
    end.

retrieve_mechanism_continue(#jid{luser = Username} = JID, Creds, Sha, St0) ->
    HostType = mongoose_credentials:host_type(Creds),
    case get_scram_attributes(HostType, JID, Sha) of
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
    end.

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
            ?LOG_INFO(#{what => scram_authentication_failed, reason => Reason}),
            {error, <<"not-authorized">>}
    end.

-spec get_scram_attributes(mongooseim:host_type(), jid:jid(), sha()) -> scram_att() | error().
get_scram_attributes(HostType, JID, Sha) ->
    case ejabberd_auth:get_passterm_with_authmodule(HostType, JID) of
        false ->
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
    case mongoose_c2s_socket:get_tls_last_message(Socket) of
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
