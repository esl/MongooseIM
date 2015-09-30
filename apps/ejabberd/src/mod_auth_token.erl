-module(mod_auth_token).

-behavior(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_auth_token.hrl").

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% gen_iq_handler callbacks
-export([process_iq/3]).

%% Public API
-export([validate_token/1]).

%% Token serialization
-export([deserialize/1,
         serialize/1]).

%% Test only!
-export([datetime_to_seconds/1,
         seconds_to_datetime/1]).
-export([expiry_datetime/3,
         token_with_mac/1]).

-export_type([period/0,
              token/0,
              token_type/0]).

-type error() :: error | {error, any()}.
-type period() :: {Count :: non_neg_integer(),
                   Unit  :: 'days' | 'hours' | 'minutes' | 'seconds'}.
-type serialized() :: binary().
-type token() :: #token{}.
-type token_type() :: access | refresh | provision.
-type validation_result() :: {ok, module(), ejabberd:user()}
                           | {ok, module(), ejabberd:user(), binary()}
                           | error().

-define(a2b(A), atom_to_binary(A, utf8)).
-define(b2a(B), binary_to_atom(B, utf8)).

-define(i2b(I), integer_to_binary(I)).
-define(b2i(B), binary_to_integer(B)).

-define(l2b(L), list_to_binary(L)).
-define(b2l(B), binary_to_list(B)).

-spec start(ejabberd:server(), list()) -> ok.
start(Host, Opts) ->
    mod_disco:register_feature(Host, ?NS_AUTH_TOKEN),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_AUTH_TOKEN, ?MODULE, process_iq, IQDisc),
    ok.

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_AUTH_TOKEN),
    ok.

-spec serialize(token()) -> serialized().
serialize(#token{mac_signature = undefined} = T) -> error(incomplete_token, [T]);
serialize(#token{token_body = undefined} = T)    -> error(incomplete_token, [T]);
serialize(#token{token_body = Body, mac_signature = MAC}) ->
    <<Body/bytes, (field_separator()), (base16:encode(MAC))/bytes>>.

%% #token{} contains fields which are:
%% - primary - these have to be supplied on token creation,
%% - dependent - these are computed based on the primary fields.
%% `token_with_mac/1` computes dependent fields and stores them in the record
%% based on a record with just the primary fields.
-spec token_with_mac(token()) -> token().
token_with_mac(#token{mac_signature = undefined, token_body = undefined} = T) ->
    Body = join_fields(T),
    MAC = keyed_hash(Body, user_hmac_opts(T#token.user_jid)),
    T#token{token_body = Body, mac_signature = MAC}.

user_hmac_opts(User) ->
    lists:keystore(key, 1, hmac_opts(), {key, acquire_key_for_user(User)}).

field_separator() -> 0.

join_fields(T) ->
    Sep = field_separator(),
    #token{type = Type, expiry_datetime = Expiry, user_jid = JID, sequence_no = SeqNo} = T,
    case {Type, SeqNo} of
        {access, undefined} ->
            <<(?a2b(Type))/bytes, Sep,
              (jlib:jid_to_binary(JID))/bytes, Sep,
              (?i2b(datetime_to_seconds(Expiry)))/bytes>>;
        {refresh, _} ->
            <<(?a2b(Type))/bytes, Sep,
              (jlib:jid_to_binary(JID))/bytes, Sep,
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

-spec validate_token(serialized()) -> validation_result().
validate_token(TokenIn) ->
    TokenReceivedRec = deserialize(TokenIn),
    #token{user_jid = TokenOwner,
           mac_signature = MACReceived,
           token_body = RecvdTokenBody} = TokenReceivedRec,
    MACreference = keyed_hash(RecvdTokenBody, user_hmac_opts(TokenOwner)),

    %% validation criteria

    MACCheckResult = MACReceived =:= MACreference,
    ValidityCheckResult = is_token_valid(TokenReceivedRec),

    %% validation results processing

    ValidationResult = case MACCheckResult and ValidityCheckResult of
                           true -> ok;
                           _  -> error
                       end,

    ValidationResultBase = {ValidationResult, mod_auth_token, TokenOwner#jid.luser},
    #token{type = TokenType} = TokenReceivedRec,

    case TokenType of
        access ->
            ValidationResultBase;
        refresh ->
            Token = token(access, TokenOwner),
            erlang:append_element(ValidationResultBase, serialize(Token))
    end.

-spec is_token_valid(token()) -> boolean().
is_token_valid(#token{expiry_datetime = Expiry}) ->
    utc_now_as_seconds() < datetime_to_seconds(Expiry).

-spec process_iq(jid(), jid(), iq()) -> iq() | error().
process_iq(From, _To, #iq{xmlns = ?NS_AUTH_TOKEN} = IQ) ->
    create_token_response(From, IQ);
process_iq(_From, _To, #iq{}) ->
    {error, ?ERR_BAD_REQUEST}.

create_token_response(From, IQ) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"items">>,
                           attrs = [{<<"xmlns">>, ?NS_AUTH_TOKEN}],
                           children = [token_to_xmlel(token(access, From)),
                                       token_to_xmlel(token(refresh, From))]}]}.

-spec datetime_to_seconds(calendar:datetime()) -> non_neg_integer().
datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

-spec seconds_to_datetime(non_neg_integer()) -> calendar:datetime().
seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds).

utc_now_as_seconds() ->
    datetime_to_seconds(calendar:universal_time()).

token(Type, User) ->
    T = #token{type = Type,
               expiry_datetime = expiry_datetime(User#jid.lserver, Type, utc_now_as_seconds()),
               user_jid = User,
               sequence_no = case Type of
                                 access -> undefined;
                                 %% TODO: this is just a stub
                                 refresh -> 666
                             end},
    token_with_mac(T).

%% {modules, [
%%            {mod_auth_token, [{{validity_period, access}, {13, minutes}},
%%                              {{validity_period, refresh}, {13, days}}]}
%%           ]}.
-spec expiry_datetime(Domain, Type, UTCSeconds) -> ExpiryDatetime when
      Domain :: ejabberd:server(),
      Type :: token_type(),
      UTCSeconds :: non_neg_integer(),
      ExpiryDatetime :: calendar:datetime().
expiry_datetime(Domain, Type, UTCSeconds) ->
    Period = get_validity_period(Domain, Type),
    seconds_to_datetime(UTCSeconds + period_to_seconds(Period)).

-spec get_validity_period(ejabberd:server(), token_type()) -> period().
get_validity_period(Domain, Type) ->
    gen_mod:get_module_opt(Domain, ?MODULE, {validity_period, Type},
                           default_validity_period(Type)).

period_to_seconds({Days, days}) -> milliseconds_to_seconds(timer:hours(24 * Days));
period_to_seconds({Hours, hours}) -> milliseconds_to_seconds(timer:hours(Hours));
period_to_seconds({Minutes, minutes}) -> milliseconds_to_seconds(timer:minutes(Minutes));
period_to_seconds({Seconds, seconds}) -> milliseconds_to_seconds(timer:seconds(Seconds)).

milliseconds_to_seconds(Millis) -> erlang:round(Millis / 1000).

token_to_xmlel(#token{type = Type} = T) ->
    #xmlel{name = case Type of
                      access -> <<"access_token">>;
                      refresh -> <<"refresh_token">>
                  end,
           attrs = [{<<"xmlns">>, ?NS_AUTH_TOKEN}],
           children = [#xmlcdata{content = jlib:encode_base64(serialize(T))}]}.

default_validity_period(access) -> {1, hours};
default_validity_period(refresh) -> {25, days}.

%% args: Token with Mac decoded from transport, #token
%% is shared between tokens. Introduce other container types if
%% they start to differ more than a few fields.
-spec get_token_as_record(BToken) -> Token when
      BToken :: serialized(),
      Token :: token().
get_token_as_record(BToken) ->
    [BType, User, Expiry | Rest] = binary:split(BToken, <<(field_separator())>>, [global]),
    T = #token{type = ?b2a(BType),
               expiry_datetime = seconds_to_datetime(binary_to_integer(Expiry)),
               user_jid = jlib:binary_to_jid(User)},
    {SeqNo, MAC} = case {BType, Rest} of
                             {<<"access">>, [BMAC]} ->
                                 {undefined, base16:decode(BMAC)};
                             {<<"refresh">>, [BSeqNo, BMAC]} ->
                                 {?b2i(BSeqNo), base16:decode(BMAC)}
                         end,
    T1 = T#token{sequence_no = SeqNo, mac_signature = MAC},
    T1#token{token_body = join_fields(T1)}.

acquire_key_for_user(User) ->
    UsersHost = User#jid.lserver,
    %% todo : extract key name from config (possible resolution by host)
    [{{asdqwe_access_secret, UsersHost}, RawKey}] = ejabberd_hooks:run_fold(
                                         get_key, UsersHost, [], [{asdqwe_access_secret, UsersHost}]),
    RawKey.
