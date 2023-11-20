-module(mod_auth_token).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_auth_token.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod callbacks
-export([start/2]).
-export([stop/1]).
-export([hooks/1]).
-export([supported_features/0]).
-export([config_spec/0]).

%% Hook handlers
-export([clean_tokens/3,
         disco_local_features/3]).

%% gen_iq_handler handlers
-export([process_iq/5]).

%% Public API
-export([authenticate/2,
         revoke/2,
         token/3]).

%% Token serialization
-export([deserialize/1,
         serialize/1]).

%% Test only!
-export([datetime_to_seconds/1,
         seconds_to_datetime/1]).
-export([expiry_datetime/3,
         get_key_for_host_type/2,
         token_with_mac/2]).

-export([config_metrics/1]).

-export_type([period/0,
              sequence_no/0,
              token/0,
              token_type/0]).

-ignore_xref([
    behaviour_info/1, datetime_to_seconds/1, deserialize/1,
    expiry_datetime/3, get_key_for_host_type/2, process_iq/5,
    revoke/2, seconds_to_datetime/1,
    serialize/1, token/3, token_with_mac/2
]).

-type error() :: error | {error, any()}.
-type period() :: #{value := non_neg_integer(),
                    unit := days | hours | minutes | seconds}.
-type sequence_no() :: integer().
-type serialized() :: binary().
-type token() :: #token{}.
-type token_type() :: access | refresh | provision.
-type validation_result() :: {ok, module(), jid:user()}
                           | {ok, module(), jid:user(), binary()}
                           | error().

-define(A2B(A), atom_to_binary(A, utf8)).

-define(I2B(I), integer_to_binary(I)).
-define(B2I(B), binary_to_integer(B)).

%%
%% gen_mod callbacks
%%

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc} = Opts) ->
    mod_auth_token_backend:start(HostType, Opts),
    gen_iq_handler:add_iq_handler_for_domain(
      HostType, ?NS_ESL_TOKEN_AUTH, ejabberd_sm,
      fun ?MODULE:process_iq/5, #{}, IQDisc),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_ESL_TOKEN_AUTH, ejabberd_sm),
    ok.

hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 90},
     {remove_user, HostType, fun ?MODULE:clean_tokens/3, #{}, 50}].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"backend">> => #option{type = atom,
                                          validate = {module, mod_auth_token}},
                 <<"validity_period">> => validity_periods_spec(),
                 <<"iqdisc">> => mongoose_config_spec:iqdisc()},
       defaults = #{<<"backend">> => rdbms,
                    <<"iqdisc">> => no_queue}
      }.

validity_periods_spec() ->
    #section{
       items = #{<<"access">> => validity_period_spec(),
                 <<"refresh">> => validity_period_spec()},
       defaults = #{<<"access">> => #{value => 1, unit => hours},
                    <<"refresh">> => #{value => 25, unit => days}},
       include = always
      }.

validity_period_spec() ->
    #section{
       items = #{<<"value">> => #option{type = integer,
                                        validate = non_negative},
                 <<"unit">> => #option{type = atom,
                                       validate = {enum, [days, hours, minutes, seconds]}}
                },
       required = all
      }.

%%
%% Other stuff
%%

-spec serialize(token()) -> serialized().
serialize(#token{mac_signature = undefined} = T) -> error(incomplete_token, [T]);
serialize(#token{token_body = undefined} = T)    -> error(incomplete_token, [T]);
serialize(#token{token_body = Body, mac_signature = MAC}) ->
    <<Body/bytes, (field_separator()), (base16:encode(MAC))/bytes>>.

%% #token{} contains fields which are:
%% - primary - these have to be supplied on token creation,
%% - dependent - these are computed based on the primary fields.
%% `token_with_mac/2` computes dependent fields and stores them in the record
%% based on a record with just the primary fields.
-spec token_with_mac(mongooseim:host_type(), token()) -> token().
token_with_mac(HostType, #token{mac_signature = undefined, token_body = undefined} = T) ->
    Body = join_fields(T),
    MAC = keyed_hash(Body, hmac_opts(HostType, T#token.type)),
    T#token{token_body = Body, mac_signature = MAC}.

-spec hmac_opts(mongooseim:host_type(), token_type()) -> [{any(), any()}].
hmac_opts(HostType, TokenType) ->
    lists:keystore(key, 1, hmac_opts(),
                   {key, get_key_for_host_type(HostType, TokenType)}).

field_separator() -> 0.

join_fields(T) ->
    Sep = field_separator(),
    #token{type = Type, expiry_datetime = Expiry, user_jid = JID,
           sequence_no = SeqNo, vcard = VCard} = T,
    case {Type, SeqNo} of
        {access, undefined} ->
            <<(?A2B(Type))/bytes, Sep,
              (jid:to_binary(JID))/bytes, Sep,
              (?I2B(datetime_to_seconds(Expiry)))/bytes>>;
        {refresh, _} ->
            <<(?A2B(Type))/bytes, Sep,
              (jid:to_binary(JID))/bytes, Sep,
              (?I2B(datetime_to_seconds(Expiry)))/bytes, Sep,
              (?I2B(SeqNo))/bytes>>;
        {provision, undefined} ->
            <<(?A2B(Type))/bytes, Sep,
              (jid:to_binary(JID))/bytes, Sep,
              (?I2B(datetime_to_seconds(Expiry)))/bytes, Sep,
              (exml:to_binary(VCard))/bytes>>
    end.

keyed_hash(Data, Opts) ->
    Type = proplists:get_value(hmac_type, Opts, sha384),
    {key, Key} = lists:keyfind(key, 1, Opts),
    crypto:mac(hmac, Type, Key, Data).

hmac_opts() ->
    [].

-spec deserialize(serialized()) -> token().
deserialize(Serialized) when is_binary(Serialized) ->
    get_token_as_record(Serialized).

-spec revoke(mongooseim:host_type(), jid:jid()) -> ok | not_found | error.
revoke(HostType, Owner) ->
    try
        mod_auth_token_backend:revoke(HostType, Owner)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => auth_token_revoke_failed,
                         user => Owner#jid.luser, server => Owner#jid.lserver,
                         class => Class, reason => Reason, stacktrace => Stacktrace}),
            error
    end.

-spec authenticate(mongooseim:host_type(), serialized()) -> validation_result().
authenticate(HostType, SerializedToken) ->
    try
        do_authenticate(HostType, SerializedToken)
    catch
        _:_ -> {error, internal_server_error}
    end.

do_authenticate(HostType, SerializedToken) ->
    #token{user_jid = Owner} = Token = deserialize(SerializedToken),
    {Criteria, Result} = validate_token(HostType, Token),
    ?LOG_INFO(#{what => auth_token_validate,
                user => Owner#jid.luser, server => Owner#jid.lserver,
                criteria => Criteria, result => Result}),
    case {Result, Token#token.type} of
        {ok, access} ->
            {ok, mod_auth_token, Owner#jid.luser};
        {ok, refresh} ->
            case token(HostType, Owner, access) of
                #token{} = T ->
                    {ok, mod_auth_token, Owner#jid.luser, serialize(T)};
                {error, R} ->
                    {error, R}
            end;
        {ok, provision} ->
            case set_vcard(HostType, Owner, Token#token.vcard) of
                {error, Reason} ->
                    ?LOG_WARNING(#{what => auth_token_set_vcard_failed,
                                   reason => Reason, token_vcard => Token#token.vcard,
                                   user => Owner#jid.luser, server => Owner#jid.lserver,
                                   criteria => Criteria, result => Result}),
                    {ok, mod_auth_token, Owner#jid.luser};
                ok ->
                    {ok, mod_auth_token, Owner#jid.luser}
            end;
        {error, _} ->
            {error, {Owner#jid.luser, [ Criterion
                                        || {_, false} = Criterion <- Criteria ]}}
    end.

set_vcard(HostType, #jid{} = User, #xmlel{} = VCard) ->
    mongoose_hooks:set_vcard(HostType, User, VCard).

validate_token(HostType, Token) ->
    Criteria = [{mac_valid, is_mac_valid(HostType, Token)},
                {not_expired, is_not_expired(Token)},
                {not_revoked, not is_revoked(Token, HostType)}],
    Result = case Criteria of
                 [{_, true}, {_, true}, {_, true}] -> ok;
                 _ -> error
             end,
    {Criteria, Result}.

is_mac_valid(HostType, #token{type = Type, token_body = Body, mac_signature = ReceivedMAC}) ->
    ComputedMAC = keyed_hash(Body, hmac_opts(HostType, Type)),
    ReceivedMAC =:= ComputedMAC.

is_not_expired(#token{expiry_datetime = Expiry}) ->
    utc_now_as_seconds() < datetime_to_seconds(Expiry).

is_revoked(#token{type = T}, _) when T =:= access;
                                  T =:= provision ->
    false;
is_revoked(#token{type = refresh, sequence_no = TokenSeqNo} = T, HostType) ->
    try
        ValidSeqNo = mod_auth_token_backend:get_valid_sequence_number(HostType, T#token.user_jid),
        TokenSeqNo < ValidSeqNo
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => auth_token_revocation_check_failed,
                         text => <<"Error checking revocation status">>,
                         token_seq_no => TokenSeqNo,
                         class => Class, reason => Reason, stacktrace => Stacktrace}),
            true
    end.

-spec process_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), any()) ->
    {mongoose_acc:t(), jlib:iq()} | error().
process_iq(Acc, From, To, #iq{xmlns = ?NS_ESL_TOKEN_AUTH} = IQ, _Extra) ->
    IQResp = process_local_iq(Acc, From, To, IQ),
    {Acc, IQResp};
process_iq(Acc, _From, _To, #iq{} = IQ, _Extra) ->
    {Acc, iq_error(IQ, [mongoose_xmpp_errors:bad_request()])}.

process_local_iq(Acc, From, _To, IQ) ->
    try create_token_response(Acc, From, IQ) of
        #iq{} = Response -> Response;
        {error, Reason} -> iq_error(IQ, [Reason])
    catch
        _:_ -> iq_error(IQ, [mongoose_xmpp_errors:internal_server_error()])
    end.

iq_error(IQ, SubElements) when is_list(SubElements) ->
    IQ#iq{type = error, sub_el = SubElements}.

create_token_response(Acc, From, IQ) ->
    HostType = mongoose_acc:host_type(Acc),
    case {token(HostType, From, access), token(HostType, From, refresh)} of
        {#token{} = AccessToken, #token{} = RefreshToken} ->
            IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"items">>,
                                   attrs = [{<<"xmlns">>, ?NS_ESL_TOKEN_AUTH}],
                                   children = [token_to_xmlel(AccessToken),
                                               token_to_xmlel(RefreshToken)]}]};
        {_, _} -> {error, mongoose_xmpp_errors:internal_server_error()}
    end.

-spec datetime_to_seconds(calendar:datetime()) -> non_neg_integer().
datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

-spec seconds_to_datetime(non_neg_integer()) -> calendar:datetime().
seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds).

utc_now_as_seconds() ->
    datetime_to_seconds(calendar:universal_time()).

-spec token(mongooseim:host_type(), jid:jid(), token_type()) -> token() | error().
token(HostType, User, Type) ->
    ExpiryTime = expiry_datetime(HostType, Type, utc_now_as_seconds()),
    T = #token{type = Type, expiry_datetime = ExpiryTime, user_jid = User},
    try
        T2 = case Type of
            access -> T;
            refresh ->
                ValidSeqNo = mod_auth_token_backend:get_valid_sequence_number(HostType, User),
                T#token{sequence_no = ValidSeqNo}
        end,
        token_with_mac(HostType, T2)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => auth_token_revocation_check_failed,
                         text => <<"Error creating token sequence number">>,
                         token_type => Type, expiry_datetime => ExpiryTime,
                         user => User#jid.luser, server => User#jid.lserver,
                         class => Class, reason => Reason, stacktrace => Stacktrace}),
            {error, {Class, Reason}}
    end.

-spec expiry_datetime(mongooseim:host_type(), token_type(), non_neg_integer()) ->
      calendar:datetime().
expiry_datetime(HostType, Type, UTCSeconds) ->
    #{value := Value, unit := Unit} = get_validity_period(HostType, Type),
    seconds_to_datetime(UTCSeconds + period_to_seconds(Value, Unit)).

-spec get_validity_period(mongooseim:host_type(), token_type()) -> period().
get_validity_period(HostType, Type) ->
    gen_mod:get_module_opt(HostType, ?MODULE, [validity_period, Type]).

period_to_seconds(Days, days) -> 24 * 3600 * Days;
period_to_seconds(Hours, hours) -> 3600 * Hours;
period_to_seconds(Minutes, minutes) -> 60 * Minutes;
period_to_seconds(Seconds, seconds) -> Seconds.

token_to_xmlel(#token{type = Type} = T) ->
    #xmlel{name = case Type of
                      access -> <<"access_token">>;
                      refresh -> <<"refresh_token">>
                  end,
           attrs = [{<<"xmlns">>, ?NS_ESL_TOKEN_AUTH}],
           children = [#xmlcdata{content = jlib:encode_base64(serialize(T))}]}.

%% args: Token with Mac decoded from transport, #token
%% is shared between tokens. Introduce other container types if
%% they start to differ more than a few fields.
-spec get_token_as_record(BToken) -> Token when
      BToken :: serialized(),
      Token :: token().
get_token_as_record(BToken) ->
    [BType, User, Expiry | Rest] = binary:split(BToken, <<(field_separator())>>, [global]),
    T = #token{type = decode_token_type(BType),
               expiry_datetime = seconds_to_datetime(binary_to_integer(Expiry)),
               user_jid = jid:from_binary(User)},
    T1 = case {BType, Rest} of
             {<<"access">>, [BMAC]} ->
                 T#token{mac_signature = base16:decode(BMAC)};
             {<<"refresh">>, [BSeqNo, BMAC]} ->
                 T#token{sequence_no = ?B2I(BSeqNo),
                         mac_signature = base16:decode(BMAC)};
             {<<"provision">>, [BVCard, BMAC]} ->
                 {ok, VCard} = exml:parse(BVCard),
                 T#token{vcard = VCard,
                         mac_signature = base16:decode(BMAC)}
         end,
    T1#token{token_body = join_fields(T1)}.

-spec decode_token_type(binary()) -> token_type().
decode_token_type(<<"access">>) ->
    access;
decode_token_type(<<"refresh">>) ->
    refresh;
decode_token_type(<<"provision">>) ->
    provision.

-spec get_key_for_host_type(mongooseim:host_type(), token_type()) -> binary().
get_key_for_host_type(HostType, TokenType) ->
    KeyName = key_name(TokenType),
    [{{KeyName, _UsersHost}, RawKey}] = mongoose_hooks:get_key(HostType, KeyName),
    RawKey.

-spec key_name(token_type()) -> token_secret | provision_pre_shared.
key_name(access)    -> token_secret;
key_name(refresh)   -> token_secret;
key_name(provision) -> provision_pre_shared.

-spec clean_tokens(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
clean_tokens(Acc, #{jid := Owner}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    try
        mod_auth_token_backend:clean_tokens(HostType, Owner)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => auth_token_clean_tokens_failed,
                         text => <<"Error in clean_tokens backend">>,
                         jid => jid:to_binary(Owner), acc => Acc, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
               {error, {Class, Reason}}
    end,
    {ok, Acc}.

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).

-spec disco_local_features(mongoose_disco:feature_acc(),
                           map(),
                           map()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features([?NS_ESL_TOKEN_AUTH], Acc)};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.
