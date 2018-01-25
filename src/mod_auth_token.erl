-module(mod_auth_token).

-behavior(gen_mod).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").
-include("mod_auth_token.hrl").

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([clean_tokens/2]).

%% gen_iq_handler handlers
-export([process_iq/4]).

%% Public API
-export([authenticate/1,
         revoke/1,
         token/2]).

%% Token serialization
-export([deserialize/1,
         serialize/1]).

%% Command-line interface
-export([revoke_token_command/1]).

%% Test only!
-export([datetime_to_seconds/1,
         seconds_to_datetime/1]).
-export([expiry_datetime/3,
         get_key_for_user/2,
         token_with_mac/1]).

-export_type([period/0,
              sequence_no/0,
              token/0,
              token_type/0]).

-type error() :: error | {error, any()}.
-type period() :: {Count :: non_neg_integer(),
                   Unit  :: 'days' | 'hours' | 'minutes' | 'seconds'}.
-type sequence_no() :: integer().
-type serialized() :: binary().
-type token() :: #token{}.
-type token_type() :: access | refresh | provision.
-type validation_result() :: {ok, module(), jid:user()}
                           | {ok, module(), jid:user(), binary()}
                           | error().

-callback revoke(Owner) -> ok | not_found when
      Owner :: jid:jid().

-callback get_valid_sequence_number(Owner) -> integer() when
      Owner :: jid:jid().

-callback clean_tokens(Owner) -> ok when
      Owner :: jid:jid().

-define(A2B(A), atom_to_binary(A, utf8)).
-define(B2A(B), binary_to_atom(B, utf8)).

-define(I2B(I), integer_to_binary(I)).
-define(B2I(B), binary_to_integer(B)).

%%
%% gen_mod callbacks
%%

-spec start(jid:server(), list()) -> ok.
start(Domain, Opts) ->
    gen_mod:start_backend_module(?MODULE, default_opts(Opts)),
    mod_disco:register_feature(Domain, ?NS_ESL_TOKEN_AUTH),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    [ ejabberd_hooks:add(Hook, Domain, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    gen_iq_handler:add_iq_handler(ejabberd_sm, Domain, ?NS_ESL_TOKEN_AUTH,
                                  ?MODULE, process_iq, IQDisc),
    ejabberd_commands:register_commands(commands()),
    ok.

-spec stop(jid:server()) -> ok.
stop(Domain) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Domain, ?NS_ESL_TOKEN_AUTH),
    [ ejabberd_hooks:delete(Hook, Domain, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    ok.

default_opts(Opts) ->
    [{backend, odbc} || not proplists:is_defined(backend, Opts)] ++ Opts.

hook_handlers() ->
    [{remove_user, clean_tokens, 50}].

-spec commands() -> [ejabberd_commands:cmd()].
commands() ->
    [#ejabberd_commands{ name = revoke_token, tags = [tokens],
                         desc = "Revoke REFRESH token",
                         module = ?MODULE, function = revoke_token_command,
                         args = [{owner, binary}], result = {res, restuple} }].

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
%% `token_with_mac/1` computes dependent fields and stores them in the record
%% based on a record with just the primary fields.
-spec token_with_mac(token()) -> token().
token_with_mac(#token{mac_signature = undefined, token_body = undefined} = T) ->
    Body = join_fields(T),
    MAC = keyed_hash(Body, user_hmac_opts(T#token.type, T#token.user_jid)),
    T#token{token_body = Body, mac_signature = MAC}.

-spec user_hmac_opts(token_type(), jid:jid()) -> [{any(), any()}].
user_hmac_opts(TokenType, User) ->
    lists:keystore(key, 1, hmac_opts(),
                   {key, get_key_for_user(TokenType, User)}).

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
    crypto:hmac(Type, Key, Data).

hmac_opts() ->
    [].

-spec deserialize(serialized()) -> token().
deserialize(Serialized) when is_binary(Serialized) ->
    get_token_as_record(Serialized).

-spec revoke(Owner) -> ok | not_found | error when
      Owner :: jid:jid().
revoke(Owner) ->
    try
        mod_auth_token_backend:revoke(Owner)
    catch
        E:R -> ?ERROR_MSG("backend error! ~p", [{E, R}]),
               error
    end.

-spec authenticate(serialized()) -> validation_result().
authenticate(SerializedToken) ->
    try
        do_authenticate(SerializedToken)
    catch
        _:_ -> {error, internal_server_error}
    end.

do_authenticate(SerializedToken) ->
    #token{user_jid = Owner} = Token = deserialize(SerializedToken),
    {Criteria, Result} = validate_token(Token),
    ?INFO_MSG("result: ~p, criteria: ~p", [Result, Criteria]),
    case {Result, Token#token.type} of
        {ok, access} ->
            {ok, mod_auth_token, Owner#jid.luser};
        {ok, refresh} ->
            case token(access, Owner) of
                #token{} = T ->
                    {ok, mod_auth_token, Owner#jid.luser, serialize(T)};
                {error, R} ->
                    {error, R}
            end;
        {ok, provision} ->
            case set_vcard(Owner#jid.lserver, Owner, Token#token.vcard) of
                {error, Reason} ->
                    ?WARNING_MSG("can't set embedded provision token vCard: ~p", [Reason]),
                    {ok, mod_auth_token, Owner#jid.luser};
                ok ->
                    {ok, mod_auth_token, Owner#jid.luser}
            end;
        {error, _} ->
            {error, {Owner#jid.luser, [ Criterion
                                        || {_, false} = Criterion <- Criteria ]}}
    end.

set_vcard(Domain, #jid{} = User, #xmlel{} = VCard) ->
    Acc0 = {error, no_handler_defined},
    ejabberd_hooks:run_fold(set_vcard, Domain, Acc0, [User, VCard]).

validate_token(Token) ->
    Criteria = [{mac_valid, is_mac_valid(Token)},
                {not_expired, is_not_expired(Token)},
                {not_revoked, not is_revoked(Token)}],
    Result = case Criteria of
                 [{_, true}, {_, true}, {_, true}] -> ok;
                 _ -> error
             end,
    {Criteria, Result}.

is_mac_valid(#token{type = Type, user_jid = Owner,
                    token_body = Body, mac_signature = ReceivedMAC}) ->
    ComputedMAC = keyed_hash(Body, user_hmac_opts(Type, Owner)),
    ReceivedMAC =:= ComputedMAC.

is_not_expired(#token{expiry_datetime = Expiry}) ->
    utc_now_as_seconds() < datetime_to_seconds(Expiry).

is_revoked(#token{type = T}) when T =:= access;
                                  T =:= provision ->
    false;
is_revoked(#token{type = refresh, sequence_no = TokenSeqNo} = T) ->
    try
        ValidSeqNo = mod_auth_token_backend:get_valid_sequence_number(T#token.user_jid),
        TokenSeqNo < ValidSeqNo
    catch
        E:R -> ?ERROR_MSG("error checking revocation status: ~p", [{E, R}]),
               true
    end.

-spec process_iq(jid:jid(), mongoose_acc:t(), jid:jid(), jlib:iq()) -> {mongoose_acc:t(), jlib:iq()} | error().
process_iq(From, To, Acc, #iq{xmlns = ?NS_ESL_TOKEN_AUTH} = IQ) ->
    IQResp = case lists:member(From#jid.lserver, ?MYHOSTS) of
        true -> process_local_iq(From, To, IQ);
        false -> iq_error(IQ, [mongoose_xmpp_errors:item_not_found()])
    end,
    {Acc, IQResp};
process_iq(_From, _To, Acc, #iq{} = IQ) ->
    {Acc, iq_error(IQ, [mongoose_xmpp_errors:bad_request()])}.

process_local_iq(From, _To, IQ) ->
    try create_token_response(From, IQ) of
        #iq{} = Response -> Response;
        {error, Reason} -> iq_error(IQ, [Reason])
    catch
        _:_ -> iq_error(IQ, [mongoose_xmpp_errors:internal_server_error()])
    end.

iq_error(IQ, SubElements) when is_list(SubElements) ->
    IQ#iq{type = error, sub_el = SubElements}.

create_token_response(From, IQ) ->
    case {token(access, From), token(refresh, From)} of
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

-spec token(token_type(), jid:jid()) -> token() | error().
token(Type, User) ->
    T = #token{type = Type,
               expiry_datetime = expiry_datetime(User#jid.lserver, Type, utc_now_as_seconds()),
               user_jid = User},
    try
        token_with_mac(case Type of
                           access -> T;
                           refresh ->
                               ValidSeqNo = mod_auth_token_backend:get_valid_sequence_number(User),
                               T#token{sequence_no = ValidSeqNo}
                       end)
    catch
        E:R -> ?ERROR_MSG("error creating token sequence number ~p~nstacktrace: ~p",
                          [{E, R}, erlang:get_stacktrace()]),
               {error, {E, R}}
    end.

%% {modules, [
%%            {mod_auth_token, [{{validity_period, access}, {13, minutes}},
%%                              {{validity_period, refresh}, {13, days}}]}
%%           ]}.
-spec expiry_datetime(Domain, Type, UTCSeconds) -> ExpiryDatetime when
      Domain :: jid:server(),
      Type :: token_type(),
      UTCSeconds :: non_neg_integer(),
      ExpiryDatetime :: calendar:datetime().
expiry_datetime(Domain, Type, UTCSeconds) ->
    Period = get_validity_period(Domain, Type),
    seconds_to_datetime(UTCSeconds + period_to_seconds(Period)).

-spec get_validity_period(jid:server(), token_type()) -> period().
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
           attrs = [{<<"xmlns">>, ?NS_ESL_TOKEN_AUTH}],
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
    T = #token{type = ?B2A(BType),
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

-spec get_key_for_user(token_type(), jid:jid()) -> binary().
get_key_for_user(TokenType, User) ->
    UsersHost = User#jid.lserver,
    KeyName = key_name(TokenType),
    [{{KeyName, UsersHost},
      RawKey}] = ejabberd_hooks:run_fold(get_key, UsersHost, [],
                                         [{KeyName, UsersHost}]),
    RawKey.

-spec key_name(token_type()) -> token_secret | provision_pre_shared.
key_name(access)    -> token_secret;
key_name(refresh)   -> token_secret;
key_name(provision) -> provision_pre_shared.

-spec revoke_token_command(Owner) -> ResTuple when
      Owner :: binary(),
      ResCode :: ok | not_found | error,
      ResTuple :: {ResCode, string()}.
revoke_token_command(Owner) ->
    try revoke(jid:from_binary(Owner)) of
        not_found ->
            {not_found, "User or token not found."};
        ok ->
            {ok, "Revoked."};
        error ->
            {error, "Internal server error"}
    catch _:_ ->
            {error, "Internal server error"}
    end.

-spec clean_tokens(User :: jid:user(), Server :: jid:server()) -> ok.
clean_tokens(User, Server) ->
    try
        Owner = jid:make(User, Server, <<>>),
        mod_auth_token_backend:clean_tokens(Owner)
    catch
        E:R -> ?ERROR_MSG("clean_tokens backend error: ~p", [{E, R}]),
               ok
    end.
