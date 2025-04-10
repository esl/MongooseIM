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

-define(CB_LABEL, <<"EXPORTER-Channel-Binding">>).

-record(fast_info, {
        creds :: mongoose_credentials:t(),
        agent_id :: mod_fast_auth_token:agent_id(),
        mechanism :: mod_fast_auth_token:mechanism(),
        count :: mod_fast_auth_token:counter() | undefined,
        cb_data :: binary()
    }).

-type fast_info() :: #fast_info{}.
-type cb_data() :: binary().

-spec mech_new(Host   :: jid:server(),
               Creds  :: mongoose_credentials:t(),
               SocketData :: term(),
               Mech :: mod_fast_auth_token:mechanism()) -> {ok, fast_info()} | {error, binary()}.
mech_new(_Host, Creds, _SocketData = #{sasl_state := SaslState, socket := Socket}, Mech) ->
    SaslModState = mod_sasl2:get_mod_state(SaslState),
    case SaslModState of
        #{encoded_id := AgentId} ->
            Count = mongoose_acc:get(mod_fast_auth_token, fast_count, undefined, SaslState),
            CBData = mech_to_cb_data(Mech, Socket),
            {ok, #fast_info{creds = Creds, agent_id = AgentId, mechanism = Mech,
                            count = Count, cb_data = CBData}};
        _ ->
            {error, <<"not-sasl2">>}
    end;
mech_new(_Host, _Creds, _SocketData, _Mech) ->
    {error, <<"not-sasl2">>}.

-spec mech_step(State :: fast_info(),
                ClientIn :: binary()) -> {ok, mongoose_credentials:t()}
                                       | {error, binary()}.
mech_step(#fast_info{creds = Creds, agent_id = AgentId, mechanism = Mech,
                     count = Count, cb_data = CBData},
          SerializedToken) ->
    %% SerializedToken is base64 decoded.
    Parts = binary:split(SerializedToken, <<0>>),
    [Username, InitiatorHashedToken] = Parts,
    HostType = mongoose_credentials:host_type(Creds),
    LServer = mongoose_credentials:lserver(Creds),
    LUser = jid:nodeprep(Username),
    case mod_fast_auth_token:read_tokens(HostType, LServer, LUser, AgentId) of
        {ok, TokenData} ->
            case handle_auth(TokenData, InitiatorHashedToken, CBData, Mech, Count) of
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

mech_to_cb_type(Mech) ->
    Type = lists:last(binary:split(Mech, <<"-">>, [global])),
    cb_type_to_atom(Type).

cb_type_to_atom(<<"NONE">>) -> none;
cb_type_to_atom(<<"EXPR">>) -> expr.

-spec mech_to_cb_data(
    Mech :: mod_fast_auth_token:mechanism(),
    Socket :: mongoose_xmpp_socket:socket()) -> cb_data().
mech_to_cb_data(Mech, Socket) ->
    case mech_to_cb_type(Mech) of
        none ->
            <<>>;
        expr ->
            %% The 'tls-exporter' Channel Binding Type
            %% Described in:
            %% RFC 9266 Channel Bindings for TLS 1.3
            %% https://www.ietf.org/rfc/rfc9266.html
            %%
            %% uses Exported Keying Material (EKM).
            %% https://www.rfc-editor.org/rfc/rfc5705.html#section-4
            %%
            %% Arguments:
            %% A disambiguating label string:
            %%    The ASCII string "EXPORTER-Channel-Binding" with no terminating NUL.
            %% Context value: Zero-length string (no_context).
            %% Length: 32 bytes
            %% Calls function with one label:
            %% export_key_materials(Scoket, Labels, Contexts, WantedLengths, ConsumeSecret)
            case mongoose_xmpp_socket:export_key_materials(
                    Socket, [?CB_LABEL], [no_context], [32], true) of
                {ok, [Msg | _]} when is_binary(Msg) ->
                    Msg;
                Other ->
                    error({failed_to_get_channel_binding_data, Other, Socket})
            end
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
                  CBData :: cb_data(),
                  Mech :: mod_fast_auth_token:mechanism(),
                  Count :: mod_fast_auth_token:counter() | undefined) ->
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
    }, InitiatorHashedToken, CBData, Mech, Count) ->
    ToHash = <<"Initiator", CBData/binary>>,
    TokenNew = {NewToken, NewExpire, NewCount, NewMech},
    TokenCur = {CurrentToken, CurrentExpire, CurrentCount, CurrentMech},
    Shared = {NowTimestamp, ToHash, InitiatorHashedToken, Mech, Count},
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
%% Sadly, we cannot check if data comes from 0-RTT or not.
check_token(_DbToken = {Token, Expire, DbCount, Mech},
            _ProvidedToken = {NowTimestamp, ToHash, InitiatorHashedToken, Mech, ProvidedCount})
    when is_binary(Token), Expire > NowTimestamp,
         (ProvidedCount =:= undefined orelse DbCount < ProvidedCount) ->
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

mech_to_algo(<<"HT-SHA-256-EXPR">>) -> sha256;
mech_to_algo(<<"HT-SHA-384-EXPR">>) -> sha384;
mech_to_algo(<<"HT-SHA-512-EXPR">>) -> sha512;

mech_to_algo(<<"HT-SHA-3-256-EXPR">>) -> sha3_256;
mech_to_algo(<<"HT-SHA-3-384-EXPR">>) -> sha3_384;
mech_to_algo(<<"HT-SHA-3-512-EXPR">>) -> sha3_512;

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
        <<"HT-SHA-3-512-NONE">>,

        <<"HT-SHA-256-EXPR">>,
        <<"HT-SHA-384-EXPR">>,
        <<"HT-SHA-512-EXPR">>,

        <<"HT-SHA-3-256-EXPR">>,
        <<"HT-SHA-3-384-EXPR">>,
        <<"HT-SHA-3-512-EXPR">>
    ].

-spec mech_id(mod_fast_auth_token:mechanism()) -> non_neg_integer().
mech_id(<<"HT-SHA-256-NONE">>) -> 1;
mech_id(<<"HT-SHA-384-NONE">>) -> 7;
mech_id(<<"HT-SHA-512-NONE">>) -> 8;

mech_id(<<"HT-SHA-3-256-NONE">>) -> 10;
mech_id(<<"HT-SHA-3-384-NONE">>) -> 11;
mech_id(<<"HT-SHA-3-512-NONE">>) -> 12;

%% Use ids from unassigned block
mech_id(<<"HT-SHA-256-EXPR">>) -> 13;
mech_id(<<"HT-SHA-384-EXPR">>) -> 14;
mech_id(<<"HT-SHA-512-EXPR">>) -> 15;

mech_id(<<"HT-SHA-3-256-EXPR">>) -> 16;
mech_id(<<"HT-SHA-3-384-EXPR">>) -> 17;
mech_id(<<"HT-SHA-3-512-EXPR">>) -> 18.

-spec mech_name(non_neg_integer()) -> mod_fast_auth_token:mechanism().
mech_name(1) -> <<"HT-SHA-256-NONE">>;
mech_name(7) -> <<"HT-SHA-384-NONE">>;
mech_name(8) -> <<"HT-SHA-512-NONE">>;

mech_name(10) -> <<"HT-SHA-3-256-NONE">>;
mech_name(11) -> <<"HT-SHA-3-384-NONE">>;
mech_name(12) -> <<"HT-SHA-3-512-NONE">>;

mech_name(13) -> <<"HT-SHA-256-EXPR">>;
mech_name(14) -> <<"HT-SHA-384-EXPR">>;
mech_name(15) -> <<"HT-SHA-512-EXPR">>;

mech_name(16) -> <<"HT-SHA-3-256-EXPR">>;
mech_name(17) -> <<"HT-SHA-3-384-EXPR">>;
mech_name(18) -> <<"HT-SHA-3-512-EXPR">>;

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
     cyrsasl_ht_sha3_512_none,

     cyrsasl_ht_sha256_expr,
     cyrsasl_ht_sha384_expr,
     cyrsasl_ht_sha512_expr,

     cyrsasl_ht_sha3_256_expr,
     cyrsasl_ht_sha3_384_expr,
     cyrsasl_ht_sha3_512_expr].
