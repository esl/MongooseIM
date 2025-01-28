-module(auth_tokens_SUITE).
-compile([export_all, nowarn_export_all]).

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
       validation_test,
       validation_property,
       validity_period_test,
       choose_key_by_token_type
      ]},
     {revocation, [],
      [
       revoked_token_is_not_valid
      ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    mongoose_config:set_opts(opts()),
    async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                {mongooseim_helper, start_link_loaded_hooks, []}]).

end_per_suite(Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts().

opts() ->
    #{{modules, host_type()} => #{mod_auth_token => config_parser_helper:default_mod_config(mod_auth_token)},
      instrumentation => config_parser_helper:default_config([instrumentation])}.

init_per_testcase(Test, Config)
        when Test =:= serialize_deserialize_property;
             Test =:= validation_test;
             Test =:= validation_property;
             Test =:= choose_key_by_token_type ->
    mock_keystore(),
    mock_rdbms_backend(),
    Config;
init_per_testcase(validity_period_test, Config) ->
    mock_rdbms_backend(),
    mock_gen_iq_handler(),
    Config;
init_per_testcase(revoked_token_is_not_valid, Config) ->
    mock_tested_backend(),
    mock_keystore(),
    Config;
init_per_testcase(_, C) -> C.

end_per_testcase(Test, _C)
        when Test =:= serialize_deserialize_property;
             Test =:= validation_test;
             Test =:= validation_property;
             Test =:= choose_key_by_token_type ->
    meck:unload(mod_auth_token_backend);
end_per_testcase(validity_period_test, _C) ->
    meck:unload(mod_auth_token_backend),
    meck:unload(gen_iq_handler);
end_per_testcase(revoked_token_is_not_valid, _C) ->
    meck:unload(mod_auth_token_backend);
end_per_testcase(_, _) ->
    ok.

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
         ?FORALL(RawToken, serialized_token(<<0>>),
                 is_join_and_split_with_base16_and_zeros_reversible(RawToken))).

serialize_deserialize_property(_) ->
    prop(serialize_deserialize_property,
         ?FORALL(Token, token(), is_serialization_reversible(Token))).

validation_test(Config) ->
    validation_test(Config, provision_token_example()),
    validation_test(Config, refresh_token_example()).

validation_test(_, ExampleToken) ->
    %% given
    Serialized = mod_auth_token:serialize(ExampleToken),
    ct:log("ExampleToken:~n  ~p", [ExampleToken]),

    %% when
    Result = mod_auth_token:authenticate(host_type(), Serialized),
    ct:log("Result: ~p", [Result]),

    %% then
    ?ae(true, is_validation_success(Result)).

validation_property(_) ->
    prop(validation_property,
         ?FORALL(Token, valid_token(), is_valid_token_prop(Token))).

validity_period_test(_) ->
    %% given
    ok = mod_auth_token:start(host_type(), mongoose_config:get_opt([{modules, host_type()}, mod_auth_token])),
    UTCSeconds = utc_now_as_seconds(),
    ExpectedSeconds = UTCSeconds + 3600, %% seconds per hour
    %% when
    ActualDT = mod_auth_token:expiry_datetime(host_type(), access, UTCSeconds),
    %% then
    ?ae(calendar:gregorian_seconds_to_datetime(ExpectedSeconds), ActualDT).

choose_key_by_token_type(_) ->
    %% given mocked keystore (see init_per_testcase)
    %% when mod_auth_token asks for key for given token type
    %% then the correct key is returned
    ?ae(<<"access_or_refresh">>, mod_auth_token:get_key_for_host_type(host_type(), access)),
    ?ae(<<"access_or_refresh">>, mod_auth_token:get_key_for_host_type(host_type(), refresh)),
    ?ae(<<"provision">>, mod_auth_token:get_key_for_host_type(host_type(), provision)).

is_join_and_split_with_base16_and_zeros_reversible(RawToken) ->
    MAC = binary:encode_hex(crypto:mac(hmac, sha384, <<"unused_key">>, RawToken), lowercase),
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
    Token =:= mod_auth_token:deserialize(mod_auth_token:serialize(Token)).

is_valid_token_prop(Token) ->
    Serialized = mod_auth_token:serialize(Token),
    R = mod_auth_token:authenticate(host_type(), Serialized),
    case is_validation_success(R) of
        true -> true;
        _    -> ct:fail(R)
    end.

is_validation_success(Result) ->
    case Result of
        {ok, _, _} -> true;
        {ok, _, _, _} -> true;
        _ -> false
    end.

revoked_token_is_not_valid(_) ->
    %% given
    ValidSeqNo = 123456,
    RevokedSeqNo = 123455,
    self() ! {valid_seq_no, ValidSeqNo},
    T = #token{type = refresh,
               expiry_datetime = mod_auth_token:seconds_to_datetime(utc_now_as_seconds() + 10),
               user_jid = jid:from_binary(<<"alice@localhost">>),
               sequence_no = RevokedSeqNo},
    Revoked = mod_auth_token:serialize(mod_auth_token:token_with_mac(host_type(), T)),
    %% when
    ValidationResult = mod_auth_token:authenticate(host_type(), Revoked),
    %% then
    {error, _} = ValidationResult.

%%
%% Helpers
%%

datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds).

utc_now_as_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% This is a negative test case helper - that's why we invert the logic below.
%% I.e. we expect the property to fail.
negative_prop(Name, Prop) ->
    Props = proper:conjunction([{Name, Prop}]),
    [[{Name, _}]] = proper:quickcheck(Props, [verbose, long_result, {numtests, 50}]).

mock_rdbms_backend() ->
    meck:new(mod_auth_token_backend, []),
    meck:expect(mod_auth_token_backend, start, fun(_, _) -> ok end),
    meck:expect(mod_auth_token_backend, get_valid_sequence_number,
                fun (_, _) -> valid_seq_no_threshold() end),
    ok.

mock_keystore() ->
    gen_hook:add_handler(get_key, host_type(), fun ?MODULE:mod_keystore_get_key/3, #{}, 50).

mock_gen_iq_handler() ->
    meck:new(gen_iq_handler, []),
    meck:expect(gen_iq_handler, add_iq_handler_for_domain, fun (_, _, _, _, _, _) -> ok end).

mod_keystore_get_key(_, #{key_id := {KeyName, _} = KeyID}, _) ->
    Acc = case KeyName of
        token_secret -> [{KeyID, <<"access_or_refresh">>}];
        provision_pre_shared -> [{KeyID, <<"provision">>}]
    end,
    {ok, Acc}.

mock_tested_backend() ->
    meck:new(mod_auth_token_backend, []),
    meck:expect(mod_auth_token_backend, get_valid_sequence_number,
                fun (_, _) ->
                        receive {valid_seq_no, SeqNo} -> SeqNo end
                end).

provision_token_example() ->
    Token =
        #token{
            type = provision,
            expiry_datetime = {{2055,10,27},{10,54,22}},
            user_jid = jid:make(<<"cee2m1s0i">>,domain(),<<>>),
            sequence_no = undefined,
            vcard = #xmlel{
                name = <<"vCard">>,
                attrs = #{<<"sgzldnl">> => <<"inxdutpu">>,
                        <<"scmgsrfi">> => <<"nhgybwu">>,
                        <<"ixrsmzee">> => <<"rysdh">>,
                        <<"oxwothgyei">> => <<"wderkfgexv">>},
                children = [
                    #xmlel{
                        name = <<"nqe">>,
                        attrs = #{<<"i">> => <<"u">>,
                                <<"gagnixjgml">> => <<"odaorofnra">>,
                                <<"ijz">> => <<"zvbrqnybi">>},
                        children = [
                            #xmlcdata{content = <<"uprmzqf">>},
                            #xmlel{name = <<"lnnitxm">>,
                                attrs = #{<<"qytehi">> => <<"axl">>,
                                            <<"xaxforb">> => <<"jrdeydsqhj">>}},
                            #xmlcdata{content = <<"pncgsaxl">>},
                            #xmlel{name = <<"jfofazuau">>,
                                attrs = #{<<"si">> => <<"l">>}}
                        ]},
                    #xmlel{name = <<"moy">>,
                        attrs = #{<<"femjc">> => <<"qqb">>,
                                    <<"tirfmekvpk">> => <<"sa">>}},
                    #xmlcdata{content = <<"bgxlyqdeeuo">>}
                ]},
            mac_signature = undefined,
            token_body = undefined},
    mod_auth_token:token_with_mac(host_type(), Token).

refresh_token_example() ->
    Token =
        #token{
            type = refresh,
            expiry_datetime = {{2055,10,27},{10,54,14}},
            user_jid = jid:make(<<"a">>,domain(),<<>>),
            sequence_no = 4,
            vcard = undefined,
            mac_signature = undefined,
            token_body = undefined},
    mod_auth_token:token_with_mac(host_type(), Token).

%%
%% Generators
%%

valid_token() ->
    ?LET(TokenParts, {token_type(), valid_expiry_datetime(),
                      bare_jid(), valid_seq_no(), vcard()},
         make_token(TokenParts)).

%% Arbitrary date in the future.
validity_threshold() ->
    {{2055,10,27}, {10,54,14}}.

valid_seq_no_threshold() ->
    3.

valid_seq_no() ->
    integer(valid_seq_no_threshold() + 1, inf).

token() ->
    ?LET(TokenParts, {token_type(), expiry_datetime(),
                      bare_jid(), seq_no(), vcard()},
         make_token(TokenParts)).

make_token({Type, Expiry, JID, SeqNo, VCard}) ->
    T = #token{type = Type,
               expiry_datetime = Expiry,
               user_jid = jid:from_binary(JID)},
    case Type of
        access ->
            mod_auth_token:token_with_mac(host_type(), T);
        refresh ->
            mod_auth_token:token_with_mac(host_type(), T#token{sequence_no = SeqNo});
        provision ->
            mod_auth_token:token_with_mac(host_type(), T#token{vcard = VCard})
    end.

serialized_token(Sep) ->
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
    oneof([access, refresh, provision]).

expiry_datetime() ->
    ?LET(Seconds, pos_integer(), seconds_to_datetime(Seconds)).

valid_expiry_datetime() ->
    ?LET(Seconds, integer( datetime_to_seconds(validity_threshold()),
                           datetime_to_seconds({{2100,1,1},{0,0,0}}) ),
         seconds_to_datetime(Seconds)).

expiry_date_as_seconds() -> pos_integer().

seq_no() -> pos_integer().

vcard() ->
    ?LET(Element, xmlel_gen:xmlel(3),
         Element#xmlel{name = <<"vCard">>}).

bare_jid() ->
    ?LET({Username, Domain}, {username(), domain()},
         <<(?l2b(Username))/bytes, "@", (Domain)/bytes>>).

%full_jid() ->
%    ?LET({Username, Domain, Res}, {username(), domain(), resource()},
%         <<(?l2b(Username))/bytes, "@", (?l2b(Domain))/bytes, "/", (?l2b(Res))/bytes>>).

username() -> ascii_string().
domain()   -> <<"localhost">>.
%resource() -> ascii_string().
host_type()   -> <<"localhost">>.

ascii_string() ->
    ?LET({Alpha, Alnum}, {ascii_alpha(), list(ascii_alnum())}, [Alpha | Alnum]).

ascii_digit() -> choose($0, $9).
ascii_lower() -> choose($a, $z).
ascii_upper() -> choose($A, $Z).
ascii_alpha() -> union([ascii_lower(), ascii_upper()]).
ascii_alnum() -> union([ascii_alpha(), ascii_digit()]).
