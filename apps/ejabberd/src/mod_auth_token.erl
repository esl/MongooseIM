-module(mod_auth_token).

-behavior(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_auth_token.hrl").

%% gen_mod callbacks
-export([start/2,
         stop/1]).

-export([process_iq/3]).

-export_type([token_type/0]).

%% TODO: TEMP! remove before merge to master!
-compile([export_all]).

-type token() :: #token{}.
-type token_type() :: access | refresh | provision.
-type serialized() :: binary().

-define(a2b(A), atom_to_binary(A, utf8)).
-define(b2a(B), binary_to_atom(B, utf8)).

-define(i2b(I), integer_to_binary(I)).
-define(b2i(B), binary_to_integer(B)).

-define(l2b(L), list_to_binary(L)).
-define(b2l(B), binary_to_list(B)).

start(Host, Opts) ->
    mod_disco:register_feature(Host, ?NS_AUTH_TOKEN),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_AUTH_TOKEN, ?MODULE, process_iq, IQDisc),
    ok.

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_AUTH_TOKEN),
    ok.

-spec serialize(token()) -> serialized().
serialize(#token{mac_signature = undefined} = T) -> error(incomplete_token, [T]);
serialize(#token{token_body = undefined} = T)    -> error(incomplete_token, [T]);
serialize(#token{token_body = Body, mac_signature = MAC}) ->
    <<Body/bytes, (field_separator()), (base16:encode(MAC))/bytes>>.

token_with_mac(#token{mac_signature = undefined, token_body = undefined} = T) ->
    Body = join_fields(T),
    HMACOpts = lists:keystore(key, 1, hmac_opts(),
                              {key, acquire_key_for_user(T#token.user_jid)}),
    MAC = keyed_hash(Body, HMACOpts),
    T#token{token_body = Body, mac_signature = MAC}.

field_separator() -> 0.

join_fields(T) ->
    Sep = field_separator(),
    #token{type = Type, expiry_datetime = Expiry, user_jid = JID, sequence_no = SeqNo} = T,
    case {Type, SeqNo} of
        {access, undefined} ->
            <<(?a2b(Type))/bytes, Sep,
              JID/bytes, Sep,
              (?i2b(datetime_to_seconds(Expiry)))/bytes>>;
        {refresh, _} ->
            <<(?a2b(Type))/bytes, Sep,
              JID/bytes, Sep,
              (?i2b(datetime_to_seconds(Expiry)))/bytes, Sep,
              (?i2b(SeqNo))/bytes>>
    end.

keyed_hash(Data, Opts) ->
    Type = proplists:get_value(hmac_type, Opts, sha384),
    {key, Key} = lists:keyfind(key, 1, Opts),
    crypto:hmac(Type, Key, Data).

hmac_opts() ->
    [].

-spec deserialize(serialized()) -> token().
deserialize(Serialized) when is_binary(Serialized) ->
    get_token_as_record(Serialized).

validate_token(TokenIn) ->
    %%io:format("~n ==== Token Raws ====  ~n~p~n ", [TokenIn]),
    TokenReceivedRec = get_token_as_record(TokenIn),
    % io:format("~n ==== Token Parsed as ====  ~n~p~n ", [TokenReceivedRec]),
    #token{user_jid = TokenOwnerUser,
           mac_signature = MACReceived,
           token_body = RecvdTokenBody} = TokenReceivedRec,

    UsersKey = acquire_key_for_user(TokenOwnerUser),
    MACreference = get_token_mac(RecvdTokenBody, UsersKey, get_hash_algorithm()),

    %% validation criteria

    MACCheckResult = MACReceived =:= MACreference,
    ValidityCheckResult =  is_token_valid(TokenReceivedRec),

    %% validation results processing

    ValidationResult = case MACCheckResult and ValidityCheckResult of
                           true -> ok;
                           _  -> error
                       end,

    ValidationResultBase = {ValidationResult, mod_auth_token, get_username_from_jid(TokenOwnerUser)},

    #token{type = TokenType} = TokenReceivedRec,

    case TokenType of
        access ->
            ValidationResultBase;
        refresh ->
            io:format(" validate_token: refresh tokens case "),
            erlang:append_element(ValidationResultBase, get_requested_tokens());
        _Other -> {error, <<"token-type-not-supported">>}
    end.

get_requested_tokens() ->
    <<"test response">>.

%% args: binary() -> binary()
get_username_from_jid(User) when is_binary(User) ->
    hd(binary:split(User,[<<"@">>])).

%% args: #token -> true | false
is_token_valid(#token{expiry_datetime = Expiry}) ->
    utc_now_as_seconds() < datetime_to_seconds(Expiry).

process_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml:get_tag_attr(<<"xmlns">>, SubEl) of
        false ->
            {error, ?ERR_BAD_REQUEST};
        {value, Ns} ->
            case Ns of
                ?NS_AUTH_TOKEN ->
                    create_token_response(From, IQ);
                _OTHER  ->
                    {error, ?ERR_BAD_REQUEST}
                end
    end.

create_token_response(From, IQ) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"items">>,
                           attrs = [{<<"xmlns">>, ?NS_AUTH_TOKEN}],
                           children = tokens_body(From)}]}.

%% DateTime -> integer()
datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

%% integer -> DateTime
seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds).

%% returns tokens validity periods expressed in days
get_expiry_dates_from_config(User) ->
    UserBareJid = get_bare_jid_binary(User),
    UsersHost = get_users_host(UserBareJid),
    ValidityOpts = gen_mod:get_module_opt(UsersHost, ?MODULE, validity_durations, []),

    case ValidityOpts of
        [] -> error(missing_token_validity_configuration);
        _ ->
            Unit = proplists:get_value(duration_unit, ValidityOpts, seconds),
            AccessTokenValidityPeriod = proplists:get_value(access_token_validity_days, ValidityOpts, 1),
            RefreshTokenValidityPeriod = proplists:get_value(refresh_token_validity_days, ValidityOpts, 1),
            AccessTokeExpirationDateTime = get_expiry_date(AccessTokenValidityPeriod, Unit),
            RefreshTokenExpirationDateTime = get_expiry_date(RefreshTokenValidityPeriod, Unit),
            [{access_token_expiry, AccessTokeExpirationDateTime},
             {refresh_token_expiry, RefreshTokenExpirationDateTime}]
    end.

get_expiry_date(ValidityPeriod, days) ->
    SecondsInDay = 86400,
    seconds_to_datetime(utc_now_as_seconds() + (SecondsInDay * ValidityPeriod));

get_expiry_date(ValidityPeriod, seconds) ->
    seconds_to_datetime(utc_now_as_seconds() + ValidityPeriod).


utc_now_as_seconds() ->
    datetime_to_seconds(calendar:universal_time()).

tokens_body(User) ->
    Halgo = get_hash_algorithm(),
    UserBareJid = get_bare_jid_binary(User),

    %% todo: handle revocation!
    SeqNo = 555,
    ExpiryDates = get_expiry_dates_from_config(User),


    AccessTokenExpiryDateTime = proplists:get_value(access_token_expiry, ExpiryDates),
    RefreshTokenExpiryDateTime = proplists:get_value(refresh_token_expiry, ExpiryDates),

    UserKey  = acquire_key_for_user(UserBareJid),

    {AccessToken, MacAccess}  = generate_access_token(UserBareJid, AccessTokenExpiryDateTime, UserKey, Halgo),
    {RefreshToken, MacRefresh} = generate_refresh_token(UserBareJid, RefreshTokenExpiryDateTime, UserKey, Halgo, SeqNo),

    AccessTokenMac = concat_token_mac(AccessToken, MacAccess),
    RefreshTokenMac = concat_token_mac(RefreshToken, MacRefresh),

    [
     #xmlel{ name = <<"access_token">>,
             children=[#xmlcdata{content = encode_for_transport(AccessTokenMac)}] },
     #xmlel{ name = <<"refresh_token">>,
             children=[#xmlcdata{content = encode_for_transport(RefreshTokenMac)}]}
    ].

% ----------------------------- tokens assembly routines -----------------
%% returns binary Token + Mac glued together using + separator.
%% args: binary(), binary() -> binary()
concat_token_mac(TokenRaw, Mac) when is_binary(TokenRaw), is_binary(Mac) ->
    <<TokenRaw/binary, "+", Mac/binary>>.

%% returns Token body and the MAC as tuple {Token, MAC}
%% args: binary() -> {binary(), binary()}
token_mac_split(Token) when is_binary(Token) ->
    R = binary:split(Token, <<"+">>, [global]),
    {hd(R), lists:last(R)}.

%% split decoded (eg from base64) token to list of binaries
%% args: binary() -> [binary()]
token_body_split(Token) when is_binary(Token) ->
    binary:split(Token, <<"&">>, [global]).


%% generate Message Authentication Code based on content and secret key
get_token_mac(TokenBin, SecretKey, Method) ->
    crypto:hmac(Method, SecretKey, TokenBin).

assemble_token_from_params(TokenParams) ->
    lists:foldl(fun(X,Sum) -> <<Sum/binary, X/binary,"&">> end, <<>> , TokenParams).
    % https://developers.google.com/talk/jep_extensions/oauth

%% args: binary(), datetime(), binary(), atom() -> {binary(), binary()}
generate_access_token(UserBareJid, ExpiryDateTime, Key, HashAlgorithm) ->
    RawAccessToken  = generate_access_token_body(UserBareJid, ExpiryDateTime),
    Mac = get_token_mac(RawAccessToken, Key, HashAlgorithm),
    %% io:format("~n Access Token (raw) ~p ~n ", [RawAccessToken]),
    %% io:format("~n MAC ~p ~n ", [Mac]),
    {RawAccessToken, Mac}.

generate_access_token_body(UserBareJid, ExpiryDateTime) ->
    UserAccessTokenParams = [
                             term_to_binary(access),    %% eg. access
                             UserBareJid,               %% <<"bob@host.com">>
                             term_to_binary(datetime_to_seconds(ExpiryDateTime)) %% {{2015,9,21},{12,29,51}}
                            ],

    assemble_token_from_params(UserAccessTokenParams).


generate_refresh_token(UserBareJid, ExpiryDateTime, Key, HashAlgorithm, SeqNo) ->
    RawRefreshToken = generate_refresh_token_body(
                               UserBareJid, ExpiryDateTime, SeqNo),

    Mac = get_token_mac(RawRefreshToken, Key, HashAlgorithm),
    {RawRefreshToken, Mac}.

generate_refresh_token_body(UserBareJid, ExpiryDateTime, SeqNo) ->
    UserRefreshTokenParams = [
                              term_to_binary(refresh),
                              UserBareJid,
                              term_to_binary(datetime_to_seconds(ExpiryDateTime)),
                              <<SeqNo>>
                             ],

    assemble_token_from_params(UserRefreshTokenParams).

%% args: Token with Mac decoded from transport, #token
%% is shared between tokens. Introduce other container types if
%% they start to differ more than a few fields.
%% args: {binary(),binary()} -> #token()
get_token_as_record(BToken) ->
    [BType, User, Expiry | Rest] = binary:split(BToken, <<(field_separator())>>, [global]),
    T = #token{type = ?b2a(BType),
               expiry_datetime = seconds_to_datetime(binary_to_integer(Expiry)),
               user_jid = User},
    {SeqNo, MAC} = case {BType, Rest} of
                             {<<"access">>, [BMAC]} ->
                                 {undefined, base16:decode(BMAC)};
                             {<<"refresh">>, [BSeqNo, BMAC]} ->
                                 {?b2i(BSeqNo), base16:decode(BMAC)}
                         end,
    T1 = T#token{sequence_no = SeqNo, mac_signature = MAC},
    T1#token{token_body = join_fields(T1)}.

acquire_key_for_user(User) ->
    UsersHost = get_users_host(User),
    %% todo : extract key name from config (possible resolution by host)
    [{{asdqwe_access_secret, UsersHost}, RawKey}] = ejabberd_hooks:run_fold(
                                         get_key, UsersHost, [], [{asdqwe_access_secret, UsersHost}]),
    RawKey.

get_users_host(User) when is_binary(User) ->
    #jid{lserver = UsersHost} = jlib:binary_to_jid(User),
    UsersHost.

%% consider reading it out of config file
get_hash_algorithm() ->
    sha384.

%% args: binary() -> binary()
decode_from_transport(Data) ->
    base64:decode(Data).

%% args: binary() -> binary()
encode_for_transport(Data) ->
    base64:encode(Data).

%% args: -record #jid
get_bare_jid_binary(User) ->
    U = User#jid.luser,
    S = User#jid.lserver,
    <<U/binary,"@",S/binary>>.
