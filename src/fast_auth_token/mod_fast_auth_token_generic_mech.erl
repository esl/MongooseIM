-module(mod_fast_auth_token_generic_mech).

-export([mech_new/4, mech_step/2]).
%% Called from mongoose_c2s
-export([skip_announce_mechanism/1]).
%% Called from mod_fast_auth_token
-export([mechanisms/0]).
%% Called from mod_fast_auth_token_rdbms
-export([mech_id/1, mech_name/1]).
%% Called from cyrsasl
-export([supports_sasl_module/2, sasl_modules/0]).

-record(fast_info, {creds, agent_id, mechanism}).
-include("mongoose.hrl").
-type fast_info() :: #fast_info{}.

-spec mech_new(Host   :: jid:server(),
               Creds  :: mongoose_credentials:t(),
               SocketData :: term(),
               Mech :: mod_fast_auth_token:mechanism()) -> {ok, fast_info()} | {error, binary()}.
mech_new(_Host, Creds, _SocketData = #{sasl_state := SaslState}, Mech) ->
    SaslModState = mod_sasl2:get_mod_state(SaslState),
    case SaslModState of
        #{encoded_id := AgentId} ->
            {ok, #fast_info{creds = Creds, agent_id = AgentId, mechanism = Mech}};
        _ ->
            {error, <<"not-sasl2">>}
    end;
mech_new(_Host, _Creds, _SocketData, _Mech) ->
    {error, <<"not-sasl2">>}.

-spec mech_step(State :: fast_info(),
                ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                       | {error, binary()}.
mech_step(#fast_info{creds = Creds, agent_id = AgentId, mechanism = Mech}, SerializedToken) ->
    %% SerializedToken is base64 decoded.
    Parts = binary:split(SerializedToken, <<0>>),
    [Username, InitiatorHashedToken] = Parts,
    HostType = mongoose_credentials:host_type(Creds),
    LServer = mongoose_credentials:lserver(Creds),
    LUser = jid:nodeprep(Username),
    case mod_fast_auth_token:read_tokens(HostType, LServer, LUser, AgentId) of
        {ok, TokenData} ->
            CBData = <<>>,
            case handle_auth(TokenData, InitiatorHashedToken, CBData, Mech) of
                {true, TokenSlot} ->
                    {ok, mongoose_credentials:extend(Creds,
                                                     [{username, LUser},
                                                      {auth_module, ?MODULE},
                                                      {fast_token_slot_used, TokenSlot},
                                                      {fast_token_data, TokenData}])};
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
-spec handle_auth(Data :: mod_fast_auth_token:tokens_data(),
                  InitiatorHashedToken :: binary(),
                  CBData :: binary(),
                  Mech :: mod_fast_auth_token:mechanism()) ->
      {true, Slot :: mod_fast_auth_token:token_slot()} | false.
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
    TokenNew = {NewToken, NewExpire, NewCount, NewMech},
    TokenCur = {CurrentToken, CurrentExpire, CurrentCount, CurrentMech},
    Shared = {NowTimestamp, ToHash, InitiatorHashedToken, Mech},
    case check_token(TokenNew, Shared) of
        true ->
            {true, new};
        false ->
            case check_token(TokenCur, Shared) of
                true ->
                    {true, current};
                false ->
                    false
            end
    end.

%% Mech of the token in DB should match the mech the client is using.
check_token({Token, Expire, _Count, Mech},
            {NowTimestamp, ToHash, InitiatorHashedToken, Mech})
    when is_binary(Token), Expire > NowTimestamp ->
    Algo = mech_to_algo(Mech),
    ComputedToken = crypto:mac(hmac, Algo, Token, ToHash),
    %% To be theoretically safe against timing attacks (attacks that measure
    %% the time it take to compare to binaries to guess how many bytes were
    %% guessed correctly when the comparison is executed byte-by-byte and
    %% shortcircuit upon the first difference)
    crypto:hash_equals(ComputedToken, InitiatorHashedToken);
check_token(_, _) ->
    false.

%% List:
%% https://www.iana.org/assignments/named-information/named-information.xhtml#hash-alg
%% 1 	sha-256 	256 bits 	[RFC6920] 	current
%% 2 	sha-256-128 	128 bits 	[RFC6920] 	current
%% 3 	sha-256-120 	120 bits 	[RFC6920] 	current
%% 4 	sha-256-96 	96 bits 	[RFC6920] 	current
%% 5 	sha-256-64 	64 bits 	[RFC6920] 	current
%% 6 	sha-256-32 	32 bits 	[RFC6920] 	current
%% 7 	sha-384 	384 bits 	[FIPS 180-4] 	current
%% 8 	sha-512 	512 bits 	[FIPS 180-4] 	current
%% 9 	sha3-224 	224 bits 	[FIPS 202] 	current
%% 10 	sha3-256 	256 bits 	[FIPS 202] 	current
%% 11 	sha3-384 	384 bits 	[FIPS 202] 	current
%% 12 	sha3-512 	512 bits 	[FIPS 202] 	current
%% 	blake2s-256 	256 bits 	[RFC7693] 	current
%% 	blake2b-256 	256 bits 	[RFC7693] 	current
%% 	blake2b-512 	512 bits 	[RFC7693] 	current
%% 	k12-256 	256 bits 	[draft-irtf-cfrg-kangarootwelve-06] 	current
%% 	k12-512 	512 bits 	[draft-irtf-cfrg-kangarootwelve-06] 	current
-spec mech_to_algo(mod_fast_auth_token:mechanism()) -> atom().
mech_to_algo(<<"HT-SHA-256-NONE">>) -> sha256;
mech_to_algo(<<"HT-SHA-384-NONE">>) -> sha384;
mech_to_algo(<<"HT-SHA-512-NONE">>) -> sha512;

mech_to_algo(<<"HT-SHA-3-256-NONE">>) -> sha3_256;
mech_to_algo(<<"HT-SHA-3-384-NONE">>) -> sha3_384;
mech_to_algo(<<"HT-SHA-3-512-NONE">>) -> sha3_512;

mech_to_algo(_) -> unknown.

-spec skip_announce_mechanism(mod_fast_auth_token:mechanism()) -> boolean().
skip_announce_mechanism(Mech) ->
    mech_to_algo(Mech) =/= unknown.

-spec mechanisms() -> [mod_fast_auth_token:mechanism()].
mechanisms() ->
    %% Mechanisms described in
    %% https://www.ietf.org/archive/id/draft-schmaus-kitten-sasl-ht-09.html
    [
        <<"HT-SHA-256-NONE">>,
        <<"HT-SHA-384-NONE">>,
        <<"HT-SHA-512-NONE">>,

        <<"HT-SHA-3-256-NONE">>,
        <<"HT-SHA-3-384-NONE">>,
        <<"HT-SHA-3-512-NONE">>
    ].

-spec mech_id(mod_fast_auth_token:mechanism()) -> non_neg_integer().
mech_id(<<"HT-SHA-256-NONE">>) -> 1;
mech_id(<<"HT-SHA-384-NONE">>) -> 7;
mech_id(<<"HT-SHA-512-NONE">>) -> 8;

mech_id(<<"HT-SHA-3-256-NONE">>) -> 10;
mech_id(<<"HT-SHA-3-384-NONE">>) -> 11;
mech_id(<<"HT-SHA-3-512-NONE">>) -> 12.

-spec mech_name(non_neg_integer()) -> mod_fast_auth_token:mechanism().
mech_name(1) -> <<"HT-SHA-256-NONE">>;
mech_name(7) -> <<"HT-SHA-384-NONE">>;
mech_name(8) -> <<"HT-SHA-512-NONE">>;

mech_name(10) -> <<"HT-SHA-3-256-NONE">>;
mech_name(11) -> <<"HT-SHA-3-384-NONE">>;
mech_name(12) -> <<"HT-SHA-3-512-NONE">>;

mech_name(_) -> <<"UNKNOWN-MECH">>. %% Just in case DB has an unknown mech_id

-spec supports_sasl_module(mongooseim:host_type(), module()) -> boolean().
supports_sasl_module(HostType, Module) ->
    lists:member(Module, sasl_modules())
        andalso gen_mod:is_loaded(HostType, mod_fast_auth_token).

sasl_modules() ->
    [cyrsasl_ht_sha256_none,
     cyrsasl_ht_sha384_none,
     cyrsasl_ht_sha512_none,

     cyrsasl_ht_sha3_256_none,
     cyrsasl_ht_sha3_384_none,
     cyrsasl_ht_sha3_512_none].
