-module(auth_tokens_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("jlib.hrl").
-include("mod_auth_token.hrl").

-import(prop_helper, [prop/2]).

-define(TESTED, mod_auth_token).
-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

-define(l2b(List), list_to_binary(List)).
-define(i2b(I), integer_to_binary(I)).

all() ->
    [{group, creation},
     {group, revocation}].

groups() ->
    [
     {creation, [],
      [
       expiry_date_roundtrip_test,
       join_and_split_with_base16_and_zeros_are_reversible_property,
       serialize_deserialize_property,
       validity_period_test
      ]},
      {revocation, [],
       [
        revoked_token_is_not_valid
       ]}
    ].

init_per_suite(C) ->
    stringprep:start(),
    xml:start(),
    C.

init_per_testcase(serialize_deserialize_property, Config) ->
    mock_mongoose_metrics(),
    Config1 = async_helper:start(Config, ejabberd_hooks, start_link, []),
    mock_keystore(),
    Config1;

init_per_testcase(validity_period_test, Config) ->
    mock_gen_iq_handler(),
    mock_ejabberd_commands(),
    async_helper:start(Config, gen_mod, start, []);

init_per_testcase(revoked_token_is_not_valid, Config) ->
    mock_mongoose_metrics(),
    mock_tested_backend(),
    async_helper:start(Config, gen_mod, start, []);

init_per_testcase(_, C) -> C.

end_per_testcase(serialize_deserialize_property, C) ->
    meck:unload(mongoose_metrics),
    async_helper:stop_all(C),
    C;

end_per_testcase(validity_period_test, C) ->
    meck:unload(gen_iq_handler),
    meck:unload(ejabberd_commands),
    async_helper:stop_all(C),
    C;

end_per_testcase(revoked_token_is_not_valid, C) ->
    meck:unload(mongoose_metrics),
    meck:unload(mod_auth_token_odbc),
    async_helper:stop_all(C),
    C;

end_per_testcase(_, C) -> C.

%%
%% Tests
%%

expiry_date_roundtrip_test(_) ->
    D = {{2015,9,17},{20,28,21}}, %% DateTime
    S =  mod_auth_token:datetime_to_seconds(D),
    ResD = mod_auth_token:seconds_to_datetime(S),
    ?ae(D, ResD).

join_and_split_with_base16_and_zeros_are_reversible_property(_) ->
    prop(join_and_split_are_reversible_property,
         ?FORALL(RawToken, token(<<0>>),
                 is_join_and_split_with_base16_and_zeros_reversible(RawToken))).

serialize_deserialize_property(_) ->
    prop(serialize_deserialize_property,
         ?FORALL(Token, token(), is_serialization_reversible(Token))).

validity_period_test(_) ->
    %% given
    ok = ?TESTED:start(<<"localhost">>,
                       validity_period_cfg(access, {13, hours})),
    UTCSeconds = utc_now_as_seconds(),
    ExpectedSeconds = UTCSeconds + (    13 %% hours
                                    * 3600 %% seconds per hour
                                   ),
    %% when
    ActualDT = ?TESTED:expiry_datetime(<<"localhost">>, access, UTCSeconds),
    %% then
    ?ae(calendar:gregorian_seconds_to_datetime(ExpectedSeconds),
        ActualDT).

is_join_and_split_with_base16_and_zeros_reversible(RawToken) ->
    MAC = base16:encode(crypto:hmac(sha384, <<"unused_key">>, RawToken)),
    Token = <<RawToken/bytes, 0, MAC/bytes>>,
    BodyPartsLen = length(binary:split(RawToken, <<0>>, [global])),
    Parts = binary:split(Token, <<0>>, [global]),
    case BodyPartsLen + 1 == length(Parts) of
        true -> true;
        false ->
            ct:pal("invalid MAC: ~s", [MAC]),
            false
    end.

is_serialization_reversible(Token) ->
    Token =:= ?TESTED:deserialize(?TESTED:serialize(Token)).

revoked_token_is_not_valid(_) ->
    %% given
    ValidSeqNo = 123456,
    RevokedSeqNo = 123455,
    self() ! {valid_seq_no, ValidSeqNo},
    T = #token{type = refresh,
               expiry_datetime = ?TESTED:seconds_to_datetime(utc_now_as_seconds() + 10),
               user_jid = jlib:binary_to_jid(<<"alice@localhost">>),
               sequence_no = RevokedSeqNo},
    Revoked = ?TESTED:serialize(?TESTED:token_with_mac(T)),
    %% when
    ValidationResult = ?TESTED:validate_token(Revoked),
    %% then
    {error, _} = ValidationResult.

%%
%% Helpers
%%

seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds).

utc_now_as_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% Like in:
%% {modules, [
%%            {mod_auth_token, [{{validity_period, access}, {13, minutes}},
%%                              {{validity_period, refresh}, {13, days}}]}
%%           ]}.
validity_period_cfg(Type, Period) ->
    Opts = [ {{validity_period, Type}, Period} ],
    ets:insert(ejabberd_modules, {ejabberd_module, {?TESTED, <<"localhost">>}, Opts}),
    Opts.

%% This is a negative test case helper - that's why we invert the logic below.
%% I.e. we expect the property to fail.
negative_prop(Name, Prop) ->
    Props = proper:conjunction([{Name, Prop}]),
    [[{Name, _}]] = proper:quickcheck(Props, [verbose, long_result, {numtests, 50}]).

mock_mongoose_metrics() ->
    meck:new(mongoose_metrics, []),
    meck:expect(mongoose_metrics, ensure_metric, fun (_, _) -> ok end),
    meck:expect(mongoose_metrics, create_generic_hook_metric, fun (_, _) -> ok end),
    meck:expect(mongoose_metrics, increment_generic_hook_metric, fun (_, _) -> ok end),
    ok.

mock_keystore() ->
    ejabberd_hooks:add(get_key, <<"localhost">>, ?MODULE, mod_keystore_get_key, 50).

mock_gen_iq_handler() ->
    meck:new(gen_iq_handler, []),
    meck:expect(gen_iq_handler, add_iq_handler, fun (_, _, _, _, _, _) -> ok end).

mod_keystore_get_key(_, KeyID) ->
    [{KeyID, <<"unused_key">>}].

mock_tested_backend() ->
    meck:new(mod_auth_token_odbc, []),
    meck:expect(mod_auth_token_odbc, get_valid_sequence_number,
                fun (_) ->
                        receive {valid_seq_no, SeqNo} -> SeqNo end
                end).

mock_ejabberd_commands() ->
    meck:new(ejabberd_commands, []),
    meck:expect(ejabberd_commands, register_commands, fun (_) -> ok end).

%%
%% Generators
%%

token() ->
    ?LET(TokenParts, {token_type(), expiry_datetime(),
                      bare_jid(), seq_no()},
         token_gen(TokenParts)).

token_gen({Type, Expiry, JID, SeqNo}) ->
    T = #token{type = Type,
               expiry_datetime = Expiry,
               user_jid = jlib:binary_to_jid(JID)},
    case Type of
        access ->
            ?TESTED:token_with_mac(T);
        refresh ->
            ?TESTED:token_with_mac(T#token{sequence_no = SeqNo})
    end.

token(Sep) ->
    ?LET({Type, JID, Expiry, SeqNo},
         {oneof([<<"access">>, <<"refresh">>]), bare_jid(), expiry_date_as_seconds(), seq_no()},
         case Type of
             <<"access">> ->
                 <<"access", Sep/bytes, JID/bytes, Sep/bytes, (?i2b(Expiry))/bytes>>;
             <<"refresh">> ->
                 <<"refresh", Sep/bytes, JID/bytes, Sep/bytes, (?i2b(Expiry))/bytes,
                   Sep/bytes, (?i2b(SeqNo))/bytes>>
         end).

token_type() ->
    oneof([access, refresh]).

expiry_datetime() ->
    ?LET(Seconds, pos_integer(), seconds_to_datetime(Seconds)).

expiry_date_as_seconds() -> pos_integer().

seq_no() -> pos_integer().

bare_jid() ->
    ?LET({Username, Domain}, {username(), domain()},
         <<(?l2b(Username))/bytes, "@", (?l2b(Domain))/bytes>>).

%full_jid() ->
%    ?LET({Username, Domain, Res}, {username(), domain(), resource()},
%         <<(?l2b(Username))/bytes, "@", (?l2b(Domain))/bytes, "/", (?l2b(Res))/bytes>>).

username() -> ascii_string().
domain()   -> "localhost".
%resource() -> ascii_string().

ascii_string() ->
    ?LET({Alpha, Alnum}, {ascii_alpha(), list(ascii_alnum())}, [Alpha | Alnum]).

ascii_digit() -> choose($0, $9).
ascii_lower() -> choose($a, $z).
ascii_upper() -> choose($A, $Z).
ascii_alpha() -> union([ascii_lower(), ascii_upper()]).
ascii_alnum() -> union([ascii_alpha(), ascii_digit()]).
