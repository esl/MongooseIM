-module(cyrsasl_ht_sha256_none).

-export([mechanism/0, mech_new/3, mech_step/2]).

-ignore_xref([mech_new/3]).

-behaviour(cyrsasl).

-record(state, {creds, agent_id}).

-include("mongoose.hrl").

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"HT-SHA-256-NONE">>.

-spec mech_new(Host   :: jid:server(),
               Creds  :: mongoose_credentials:t(),
               SocketData :: term()) -> {ok, tuple()}.
mech_new(_Host, Creds, SocketData = #{sasl_state := SaslState}) ->
    SaslModState = mod_sasl2:get_mod_state(SaslState),
    case SaslModState of
        #{encoded_id := AgentId} ->
            {ok, #state{creds = Creds, agent_id = AgentId}};
        _ ->
            {error, <<"not-sasl2">>}
    end;
mech_new(_Host, _Creds, _SocketData) ->
    {error, <<"not-sasl2">>}.

format_term(X) -> iolist_to_binary(io_lib:format("~0p", [X])).

-spec mech_step(State :: #state{},
                ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                       | {error, binary()}.
mech_step(#state{creds = Creds, agent_id = AgentId}, SerializedToken) ->
    %% SerializedToken is base64 decoded.
    Parts = binary:split(SerializedToken, <<0>>),
    ?LOG_ERROR(#{what => cyrsasl_ht_sha256_none, creds => Creds, ser_token => SerializedToken, parts => Parts, agent_id => AgentId}),
    [Username, InitiatorHashedToken] = Parts,
    HostType = mongoose_credentials:host_type(Creds),
    LServer = mongoose_credentials:lserver(Creds),
    LUser = jid:nodeprep(Username),
    case mod_fast:read_tokens(HostType, LServer, LUser, AgentId) of
        {ok, TokenData} ->
            ?LOG_ERROR(#{what => mech_step, token_data => TokenData}),
            CBData = <<>>,
            case handle_auth(TokenData, InitiatorHashedToken, CBData) of
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
        new_token := NewToken,
        new_expire := NewExpire,
        new_count := NewCount
    }, InitiatorHashedToken, CBData) ->
    ToHash = <<"Initiator", CBData/binary>>,
    Token1 = {NewToken, NewExpire, NewCount},
    Token2 = {CurrentToken, CurrentExpire, CurrentCount},
    Shared = {NowTimestamp, ToHash, InitiatorHashedToken},
    case check_token(Token1, Shared) of
        true ->
            true;
        false ->
            check_token(Token2, Shared)
    end.

check_token({Token, Expire, Count}, {NowTimestamp, ToHash, InitiatorHashedToken}) ->
    crypto:mac(hmac, sha256, Token, ToHash) =:= InitiatorHashedToken.
