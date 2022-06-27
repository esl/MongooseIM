-module(mongoose_scram).

-include("mongoose.hrl").
-include("scram.hrl").

% Core SCRAM functions
-export([salted_password/4]).

-export([
         enabled/1,
         enabled/2,
         iterations/0,
         iterations/1,
         password_to_scram/2,
         password_to_scram/3,
         password_to_scram_sha/3,
         check_password/2,
         check_digest/4
        ]).

-export([serialize/1, deserialize/1]).

-export([scram_to_tuple/1, scram_record_to_map/1]).

-ignore_xref([password_to_scram/2, scram_to_tuple/1]).

-type scram_tuple() :: { StoredKey :: binary(), ServerKey :: binary(),
                         Salt :: binary(), Iterations :: non_neg_integer() }.

-type scram_map() ::
    #{iteration_count := non_neg_integer(),
      sha_key() := server_and_stored_key_type()}.

-type sha_key() :: sha | sha224 | sha256 | sha384 | sha512.

-type server_and_stored_key_type() :: #{salt       := binary(),
                                        server_key := binary(),
                                        stored_key := binary()}.

-type scram() :: #scram{}.

-export_type([scram_tuple/0, scram/0, scram_map/0]).

-define(SALT_LENGTH, 16).
-define(SCRAM_DEFAULT_ITERATION_COUNT, 10000).
-define(SCRAM_SERIAL_PREFIX, "==SCRAM==,").
-define(MULTI_SCRAM_SERIAL_PREFIX, "==MULTI_SCRAM==,").
-define(SCRAM_SHA1_PREFIX, "===SHA1===").
-define(SCRAM_SHA224_PREFIX, "==SHA224==").
-define(SCRAM_SHA256_PREFIX, "==SHA256==").
-define(SCRAM_SHA384_PREFIX, "==SHA384==").
-define(SCRAM_SHA512_PREFIX, "==SHA512==").

%% ejabberd doesn't implement SASLPREP, so we use the similar RESOURCEPREP instead
salted_password(Sha, Password, Salt, IterationCount) ->
    fast_scram:salted_password(Sha, jid:resourceprep(Password), Salt, IterationCount).

enabled(HostType) ->
    mongoose_config:get_opt([{auth, HostType}, password, format]) =:= scram.

enabled(HostType, cyrsasl_scram_sha1)   -> is_password_format_allowed(HostType, sha);
enabled(HostType, cyrsasl_scram_sha224) -> is_password_format_allowed(HostType, sha224);
enabled(HostType, cyrsasl_scram_sha256) -> is_password_format_allowed(HostType, sha256);
enabled(HostType, cyrsasl_scram_sha384) -> is_password_format_allowed(HostType, sha384);
enabled(HostType, cyrsasl_scram_sha512) -> is_password_format_allowed(HostType, sha512);
enabled(HostType, cyrsasl_scram_sha1_plus) -> is_password_format_allowed(HostType, sha);
enabled(HostType, cyrsasl_scram_sha224_plus) -> is_password_format_allowed(HostType, sha224);
enabled(HostType, cyrsasl_scram_sha256_plus) -> is_password_format_allowed(HostType, sha256);
enabled(HostType, cyrsasl_scram_sha384_plus) -> is_password_format_allowed(HostType, sha384);
enabled(HostType, cyrsasl_scram_sha512_plus) -> is_password_format_allowed(HostType, sha512);
enabled(_HostType, _Mechanism) -> false.

is_password_format_allowed(HostType, Sha) ->
    case mongoose_config:get_opt([{auth, HostType}, password]) of
        #{format := scram, hash := ConfiguredSha} -> lists:member(Sha, ConfiguredSha);
        #{format := _PlainOrScram} -> true
    end.

%% This function is exported and used from other modules
iterations() -> ?SCRAM_DEFAULT_ITERATION_COUNT.

iterations(HostType) ->
    mongoose_config:get_opt([{auth, HostType}, password, scram_iterations]).

password_to_scram_sha(Password, IterationCount, HashType) ->
    ScramHash = do_password_to_scram(Password, IterationCount, HashType),
    maps:from_list([{iteration_count, IterationCount}, ScramHash]).

password_to_scram(HostType, Password) ->
    password_to_scram(HostType, Password, ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(_, #scram{} = Password, _) ->
    scram_record_to_map(Password);
password_to_scram(HostType, Password, IterationCount) ->
    ServerStoredKeys = [do_password_to_scram(Password, IterationCount, HashType)
                            || {HashType, _Prefix} <- configured_sha_types(HostType)],
    ResultList = lists:merge([{iteration_count, IterationCount}], ServerStoredKeys),
    maps:from_list(ResultList).

do_password_to_scram(Password, IterationCount, HashType) ->
    Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
    SaltedPassword = salted_password(HashType, Password, Salt, IterationCount),
    StoredKey = fast_scram:stored_key(HashType, fast_scram:client_key(HashType, SaltedPassword)),
    ServerKey = fast_scram:server_key(HashType, SaltedPassword),
    {HashType, #{salt       => base64:encode(Salt),
                 server_key => base64:encode(ServerKey),
                 stored_key => base64:encode(StoredKey)}}.

check_password(Password, Scram) when is_record(Scram, scram)->
    ScramMap = scram_record_to_map(Scram),
    check_password(Password, ScramMap);
check_password(Password, ScramMap) when is_map(ScramMap) ->
    #{iteration_count := IterationCount} = ScramMap,
    [Sha | _] = [ShaKey || {ShaKey, _Prefix} <- supported_sha_types(),
                                                maps:is_key(ShaKey, ScramMap)],
    #{Sha := #{salt := Salt, stored_key := StoredKey}} = ScramMap,
    SaltedPassword = salted_password(Sha, Password, base64:decode(Salt), IterationCount),
    ClientStoredKey = fast_scram:stored_key(Sha, fast_scram:client_key(Sha, SaltedPassword)),
    ClientStoredKey == base64:decode(StoredKey).

serialize(#scram{storedkey = StoredKey, serverkey = ServerKey,
                 salt = Salt, iterationcount = IterationCount})->
    IterationCountBin = integer_to_binary(IterationCount),
    << <<?SCRAM_SERIAL_PREFIX>>/binary,
       StoredKey/binary, $,, ServerKey/binary,
       $,, Salt/binary, $,, IterationCountBin/binary>>;
serialize(#{iteration_count := IterationCount} = ScramMap) ->
    IterationCountBin = integer_to_binary(IterationCount),
    ConfigedSha = [{ShaKey, Prefix} || {ShaKey, Prefix} <- supported_sha_types(),
                                                           maps:is_key(ShaKey, ScramMap)],
    Header = [?MULTI_SCRAM_SERIAL_PREFIX, IterationCountBin],
    do_serialize(Header, ScramMap, ConfigedSha).

do_serialize(Serialized, _ ,[]) ->
    erlang:iolist_to_binary(Serialized);
do_serialize(Header, ScramMap, [{Sha, Prefix} | RemainingSha]) ->
    #{Sha := #{salt := Salt,
               server_key := ServerKey,
               stored_key := StoredKey}} = ScramMap,
    ShaSerialization = [$, , Prefix, Salt, $|, StoredKey, $|, ServerKey],
    NewHeader = [Header | ShaSerialization],
    do_serialize(NewHeader, ScramMap, RemainingSha).

deserialize(<<?SCRAM_SERIAL_PREFIX, Serialized/binary>>) ->
    case catch binary:split(Serialized, <<",">>, [global]) of
        [StoredKey, ServerKey, Salt, IterationCount] ->
            {ok, #{iteration_count => binary_to_integer(IterationCount),
                   sha => #{salt       => Salt,
                            stored_key => StoredKey,
                            server_key => ServerKey}}};
        _ ->
            ?LOG_WARNING(#{what => scram_serialisation_incorrect}),
            {error, incorrect_scram}
    end;
deserialize(<<?MULTI_SCRAM_SERIAL_PREFIX, Serialized/binary>>) ->
    case catch binary:split(Serialized, <<",">>, [global]) of
        [IterationCountBin | ListOfShaSpecificDetails] ->
            IterationCount = binary_to_integer(IterationCountBin),
            DeserializedKeys = [deserialize(supported_sha_types(), ShaDetails)
                                             || ShaDetails <- ListOfShaSpecificDetails],
            ResultList = lists:merge([{iteration_count, IterationCount}],
                                     lists:flatten(DeserializedKeys)),
            {ok, maps:from_list(ResultList)};
        _ ->
            ?LOG_WARNING(#{what => scram_serialisation_incorrect}),
            {error, incorrect_scram}
    end;
deserialize(_) ->
    ?LOG_WARNING(#{what => scram_serialisation_corrupted}),
    {error, corrupted_scram}.

deserialize([], _) ->
    [];
deserialize([{Sha, Prefix} | _RemainingSha],
    <<Prefix:10/binary, ShaDetails/binary>>) ->
    case catch binary:split(ShaDetails, <<"|">>, [global]) of
        [Salt, StoredKey, ServerKey] ->
            {Sha, #{salt => Salt, server_key => ServerKey, stored_key => StoredKey}};
        _ ->
            ?LOG_WARNING(#{what => scram_serialisation_incorrect})
    end;
deserialize([_CurrentSha | RemainingSha], ShaDetails) ->
    deserialize(RemainingSha, ShaDetails).

-spec scram_to_tuple(scram()) -> scram_tuple().
scram_to_tuple(Scram) ->
    {base64:decode(Scram#scram.storedkey),
     base64:decode(Scram#scram.serverkey),
     base64:decode(Scram#scram.salt),
     Scram#scram.iterationcount}.

-spec scram_record_to_map(scram()) -> scram_map().
scram_record_to_map(Scram) ->
    #{iteration_count => Scram#scram.iterationcount,
      sha => #{salt       => Scram#scram.salt,
               stored_key => Scram#scram.storedkey,
               server_key => Scram#scram.serverkey}}.

-spec check_digest(Scram, binary(), fun(), binary()) -> boolean() when
    Scram :: scram_map() | scram().
check_digest(Scram, Digest, DigestGen, Password) when is_record(Scram, scram) ->
    ScramMap = scram_record_to_map(Scram),
    check_digest(ScramMap, Digest, DigestGen, Password);
check_digest(ScramMap, Digest, DigestGen, Password) ->
    do_check_digest(supported_sha_types(), ScramMap, Digest, DigestGen, Password).

do_check_digest([] , _, _, _, _) ->
    false;
do_check_digest([{Sha,_Prefix} | RemainingSha], ScramMap, Digest, DigestGen, Password) ->
    #{Sha := #{stored_key := StoredKey}} = ScramMap,
    Passwd = base64:decode(StoredKey),
    case ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd) of
        true  -> true;
        false -> do_check_digest(RemainingSha, Digest, DigestGen, Password, Passwd)
    end.

supported_sha_types() ->
    [{sha,      <<?SCRAM_SHA1_PREFIX>>},
     {sha224,   <<?SCRAM_SHA224_PREFIX>>},
     {sha256,   <<?SCRAM_SHA256_PREFIX>>},
     {sha384,   <<?SCRAM_SHA384_PREFIX>>},
     {sha512,   <<?SCRAM_SHA512_PREFIX>>}].

configured_sha_types(HostType) ->
    case mongoose_config:lookup_opt([{auth, HostType}, password, hash]) of
        {ok, ScramSha} ->
            lists:filter(fun({Sha, _Prefix}) ->
                            lists:member(Sha, ScramSha) end, supported_sha_types());
        _ -> supported_sha_types()
    end.
