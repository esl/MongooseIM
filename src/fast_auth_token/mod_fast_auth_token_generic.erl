-module(mod_fast_auth_token_generic).

-export([mech_new/4, mech_step/2]).

-record(state, {creds, agent_id, mechanism}).
-include("mongoose.hrl").
-type state() :: #state{}.

-spec mech_new(Host   :: jid:server(),
               Creds  :: mongoose_credentials:t(),
               SocketData :: term(),
               Mech :: mod_fast_auth_token:mechanism()) -> {ok, state()} | {error, binary()}.
mech_new(_Host, Creds, SocketData = #{sasl_state := SaslState}, Mech) ->
    SaslModState = mod_sasl2:get_mod_state(SaslState),
    case SaslModState of
        #{encoded_id := AgentId} ->
            {ok, #state{creds = Creds, agent_id = AgentId, mechanism = Mech}};
        _ ->
            {error, <<"not-sasl2">>}
    end;
mech_new(_Host, _Creds, _SocketData, _Mech) ->
    {error, <<"not-sasl2">>}.

-spec mech_step(State :: #state{},
                ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                       | {error, binary()}.
mech_step(#state{creds = Creds, agent_id = AgentId, mechanism = Mech}, SerializedToken) ->
    %% SerializedToken is base64 decoded.
    Parts = binary:split(SerializedToken, <<0>>),
    [Username, InitiatorHashedToken] = Parts,
    HostType = mongoose_credentials:host_type(Creds),
    LServer = mongoose_credentials:lserver(Creds),
    LUser = jid:nodeprep(Username),
    case mod_fast_auth_token:read_tokens(HostType, LServer, LUser, AgentId) of
        {ok, TokenData} ->
            ?LOG_ERROR(#{what => mech_step, token_data => TokenData}),
            CBData = <<>>,
            case handle_auth(TokenData, InitiatorHashedToken, CBData, Mech) of
                true ->
                    {ok, mongoose_credentials:extend(Creds,
                                                     [{username, LUser},
                                                      {auth_module, ?MODULE}])};
                false ->
                    {error, <<"not-authorized">>}
            end;
        {error, _Reason} ->
            {error, <<"not-authorized">>}
    end.


%% For every client using FAST, have two token slots - 'current' and 'new'.
%% Whenever generating a new token, always place it into the 'new' slot.
%% During authentication, first check against the token
%% in the 'new' slot (if any).
%% If successful, move the token from the 'new' slot to the 'current' slot
%% (overwrite any existing token in that slot).
%%
%% If the client's provided token does not match the token in the 'new' slot,
%% or if the 'new' slot is empty, compare against the token
%% in the 'current' slot (if any).
handle_auth(#{
        now_timestamp := NowTimestamp,
        current_token := CurrentToken,
        current_expire := CurrentExpire,
        current_count := CurrentCount,
        current_mech := CurrentMech,
        new_token := NewToken,
        new_expire := NewExpire,
        new_count := NewCount,
        new_mech := NewMech
    }, InitiatorHashedToken, CBData, Mech) ->
    ToHash = <<"Initiator", CBData/binary>>,
    Token1 = {NewToken, NewExpire, NewCount, NewMech},
    Token2 = {CurrentToken, CurrentExpire, CurrentCount, CurrentMech},
    Shared = {NowTimestamp, ToHash, InitiatorHashedToken, Mech},
    case check_token(Token1, Shared) of
        true ->
            true;
        false ->
            check_token(Token2, Shared)
    end.

%% Mech of the token in DB should match the mech the client is using.
check_token({Token, Expire, Count, Mech},
            {NowTimestamp, ToHash, InitiatorHashedToken, Mech})
    when is_binary(Token) ->
    crypto:mac(hmac, sha256, Token, ToHash) =:= InitiatorHashedToken;
check_token(_, _) ->
    false.
