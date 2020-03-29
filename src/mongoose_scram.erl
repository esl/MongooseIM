-module(mongoose_scram).

-include("mongoose.hrl").
-include("scram.hrl").

% Core SCRAM functions
%% ejabberd doesn't implement SASLPREP, so we use the similar RESOURCEPREP instead
-export([
         salted_password/3,
         salted_password/4,
         stored_key/1,
         stored_key/2,
         server_key/1,
         server_key/2,
         server_signature/2,
         server_signature/3,
         client_signature/2,
         client_signature/3,
         client_key/1,
         client_key/2,
         client_proof_key/2]).

-export([
         enabled/1,
         iterations/0,
         iterations/1,
         password_to_scram/1,
         password_to_scram/2,
         check_password/2,
         check_digest/4
        ]).

-export([serialize/1, deserialize/1]).

-export([scram_to_tuple/1]).

-type sha_type() :: crypto:sha1() | crypto:sha2().

-type scram_tuple() :: { StoredKey :: binary(), ServerKey :: binary(),
                         Salt :: binary(), Iterations :: non_neg_integer() }.

-type scram() :: #scram{}.

-export_type([scram_tuple/0, scram/0]).

-define(SALT_LENGTH, 16).
-define(SCRAM_DEFAULT_ITERATION_COUNT, 4096).
-define(SCRAM_SERIAL_PREFIX, "==SCRAM==,").

-spec salted_password(binary(), binary(), non_neg_integer()) -> binary().
salted_password(Password, Salt, IterationCount) ->
    hi(sha, jid:resourceprep(Password), Salt, IterationCount).
-spec salted_password(sha_type(), binary(), binary(), non_neg_integer()) -> binary().
salted_password(Sha, Password, Salt, IterationCount) ->
    hi(Sha, jid:resourceprep(Password), Salt, IterationCount).

-spec client_key(binary()) -> binary().
client_key(SaltedPassword) ->
    crypto:hmac(sha, SaltedPassword, <<"Client Key">>).
-spec client_key(sha_type(), binary()) -> binary().
client_key(Sha, SaltedPassword) when Sha == sha orelse Sha == sha256 ->
    crypto:hmac(Sha, SaltedPassword, <<"Client Key">>).

-spec stored_key(binary()) -> binary().
stored_key(ClientKey) -> crypto:hash(sha, ClientKey).
-spec stored_key(sha_type(), binary()) -> binary().
stored_key(Sha, ClientKey) -> crypto:hash(Sha, ClientKey).

-spec server_key(binary()) -> binary().
server_key(SaltedPassword) ->
    crypto:hmac(sha, SaltedPassword, <<"Server Key">>).
-spec server_key(sha_type(), binary()) -> binary().
server_key(Sha, SaltedPassword) ->
    crypto:hmac(Sha, SaltedPassword, <<"Server Key">>).

-spec client_signature(binary(), binary()) -> binary().
client_signature(StoredKey, AuthMessage) ->
    crypto:hmac(sha, StoredKey, AuthMessage).
-spec client_signature(sha_type(), binary(), binary()) -> binary().
client_signature(Sha, StoredKey, AuthMessage) ->
    crypto:hmac(Sha, StoredKey, AuthMessage).

-spec client_proof_key(binary(), binary()) -> binary().
client_proof_key(ClientProof, ClientSignature) ->
    mask(ClientProof, ClientSignature).

-spec server_signature(binary(), binary()) -> binary().
server_signature(ServerKey, AuthMessage) ->
    crypto:hmac(sha, ServerKey, AuthMessage).
-spec server_signature(sha_type(), binary(), binary()) -> binary().
server_signature(Sha, ServerKey, AuthMessage) ->
    crypto:hmac(Sha, ServerKey, AuthMessage).

-spec hi(sha_type(), binary(), binary(), non_neg_integer()) -> binary().
hi(Sha, Password, Salt, IterationCount) ->
    U1 = crypto:hmac(Sha, Password, <<Salt/binary, 0, 0, 0, 1>>),
    mask(U1, hi_round(Sha, Password, U1, IterationCount - 1)).

-spec hi_round(sha_type(), binary(), binary(), non_neg_integer()) -> binary().
hi_round(Sha, Password, UPrev, 1) ->
    crypto:hmac(Sha, Password, UPrev);
hi_round(Sha, Password, UPrev, IterationCount) ->
    U = crypto:hmac(Sha, Password, UPrev),
    mask(U, hi_round(Sha, Password, U, IterationCount - 1)).

-spec mask(binary(), binary()) -> binary().
mask(Key, Data) ->
    KeySize = size(Key) * 8,
    <<A:KeySize>> = Key,
    <<B:KeySize>> = Data,
    C = A bxor B,
    <<C:KeySize>>.

enabled(Host) ->
    ejabberd_auth:get_opt(Host, password_format) == scram.

%% This function is exported and used from other modules
iterations() -> ?SCRAM_DEFAULT_ITERATION_COUNT.

iterations(Host) ->
    ejabberd_auth:get_opt(Host, scram_iterations, ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(Password) ->
    password_to_scram(Password, ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(#scram{} = Password, _) ->
    Password;
password_to_scram(Password, IterationCount) ->
    Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
    SaltedPassword = salted_password(Password, Salt, IterationCount),
    StoredKey = stored_key(client_key(SaltedPassword)),
    ServerKey = server_key(SaltedPassword),
    #scram{storedkey = base64:encode(StoredKey),
           serverkey = base64:encode(ServerKey),
           salt = base64:encode(Salt),
           iterationcount = IterationCount}.

check_password(Password, Scram) ->
    IterationCount = Scram#scram.iterationcount,
    Salt = base64:decode(Scram#scram.salt),
    SaltedPassword = salted_password(Password, Salt, IterationCount),
    StoredKey = stored_key(client_key(SaltedPassword)),
    (base64:decode(#scram.storedkey) == StoredKey).

serialize(#scram{storedkey = StoredKey, serverkey = ServerKey,
                     salt = Salt, iterationcount = IterationCount})->
    IterationCountBin = integer_to_binary(IterationCount),
    << <<?SCRAM_SERIAL_PREFIX>>/binary,
       StoredKey/binary, $,, ServerKey/binary,
       $,, Salt/binary, $,, IterationCountBin/binary>>.

deserialize(<<?SCRAM_SERIAL_PREFIX, Serialized/binary>>) ->
    case catch binary:split(Serialized, <<",">>, [global]) of
        [StoredKey, ServerKey, Salt, IterationCount] ->
            {ok, #scram{storedkey = StoredKey,
                        serverkey = ServerKey,
                        salt = Salt,
                        iterationcount = binary_to_integer(IterationCount)}};
        _ ->
            ?WARNING_MSG("Incorrect serialized SCRAM: ~p, ~p", [Serialized]),
            {error, incorrect_scram}
    end;
deserialize(Bin) ->
    ?WARNING_MSG("Corrupted serialized SCRAM: ~p", [Bin]),
    {error, corrupted_scram}.

-spec scram_to_tuple(scram()) -> scram_tuple().
scram_to_tuple(Scram) ->
    {base64:decode(Scram#scram.storedkey),
     base64:decode(Scram#scram.serverkey),
     base64:decode(Scram#scram.salt),
     Scram#scram.iterationcount}.

-spec check_digest(scram(), binary(), fun(), binary()) -> boolean().
check_digest(#scram{storedkey = StoredKey}, Digest, DigestGen, Password) ->
    Passwd = base64:decode(StoredKey),
    ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd).
