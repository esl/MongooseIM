-module(auth_tokens_SUITE).


-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("ejabberd/src/mod_auth_token.hrl").


-compile([export_all]).

-define(NS_AUTH_TOKEN, <<"urn:xmpp:tmp:auth-token">>).

-record(iq, {id = <<>>,
             type     ,
             xmlns = <<>>,
             lang = <<>> ,
             sub_el
            }).


all() ->
    [{group, tokencreation}
    ].

groups() ->
    [{tokencreation, [],
      [
       token_mac_concat_test,
       expiry_date_roundtrip_test,
       get_bare_jid_binary_test,
       access_token_body_reassembly_test,
       refresh_token_body_reassembly_test,
       access_token_mac_reassembly_test,
       package_token_token_test
      ]}
    ].

token_mac_concat_test(_) ->
    DummyToken = <<"someuser@somehost&dummytime&seq">>,
    DummyMAC = <<"123">>,
    DummyTokenMac = mod_auth_token:concat_token_mac(DummyToken, DummyMAC),
    ct:pal("~n dummy token_mac to split: ~p ~n ", [DummyTokenMac]),
    %% now, unglue all parts and check if they make together original token and MAC
    Elems = mod_auth_token:token_mac_split(DummyTokenMac),
    ct:pal("~n token_mac_concat_test: ~p ~n ", [Elems]),
    true = DummyToken =:= element(1, Elems),
    true = DummyMAC =:= element(2, Elems).

expiry_date_roundtrip_test(_) ->
    D = {{2015,9,17},{20,28,21}}, %% DateTime
    S =  mod_auth_token:datetime_to_seconds(D),
    ResD = mod_auth_token:seconds_to_datetime(S),
    true = D =:= ResD.

access_token_body_reassembly_test(_) ->

    RequesterUser = <<"alice@localhost">>,

    ExpiryDate = {{2015,9,17},{20,28,21}}, %% DateTime

    TokenBody = mod_auth_token:generate_access_token_body(RequesterUser, ExpiryDate),

    TokenParts  = mod_auth_token:token_body_split(TokenBody),
    ct:pal("~n Token parts after split  ~p ~n ", [TokenParts]),

    TokenType = binary_to_term(lists:nth(1, TokenParts)),
    UserRestored = lists:nth(2, TokenParts),
    El2 = lists:nth(3, TokenParts),
    ExpiryRestored = mod_auth_token:seconds_to_datetime(binary_to_term(El2)),

    ct:pal("~n User from Token ~p ~n Expiry from Token ~p ~n",
           [UserRestored, ExpiryRestored]),

    true = ExpiryDate =:= ExpiryRestored,
    true = RequesterUser =:= UserRestored,
    true = TokenType =:= access.


%% args: binary(), datetime() -> binary()
create_sample_access_token_body(UserBareJid, ExpiryDate, UserKey) ->
    mod_auth_token:generate_access_token(UserBareJid, ExpiryDate, UserKey, sha384).

%% args: binary(), binary(), binary() -> binary()
create_hmac_signature(Token, SecretKey) ->
    mod_auth_token:get_token_mac(Token, SecretKey, sha384).

%% args: binary(), DateTime, binary() -> binary()
create_sample_access_token_with_mac(UserBareJid, ExpiryDate, SecretKey) ->
    {TB, MAC} = create_sample_access_token_body(UserBareJid, ExpiryDate, SecretKey),
    mod_auth_token:concat_token_mac(TB, MAC).

package_token_token_test(_) ->
    RequesterUser = <<"alice@localhost">>,
    ExpiryDate = {{2015,9,17},{20,28,21}}, %% DateTime
    SecretKey = <<"123abc">>,
    {Token, MacNew} = mod_auth_token:generate_access_token(RequesterUser, ExpiryDate, SecretKey, sha384),
    ct:pal(" ~n --- MacNew record : ~p ~n",[MacNew]),

    %% we simulate what server got from transport after decoding:
    TokenDecoded = mod_auth_token:concat_token_mac(Token, MacNew),

    R = mod_auth_token:get_token_as_record(TokenDecoded),
    ct:pal(" ~n --- token record : ~p ~n",[R]),

    #auth_token{type = Type, expiry_datetime = Expiry, user_jid =User, mac_signature = MAC, token_body = TokenBody} = R,

    true = Type =:= access,
    true = User =:= RequesterUser,
    true = ExpiryDate =:= Expiry,
    true = MAC =:= MacNew,
    true = TokenBody =:= Token.


%% args: Token with Mac decoded from transport
%% args: {binary(),binary()} -> #auth_token()
%% get_token_as_record(TokenIn) ->
%%     {Token, MAC} = mod_auth_token:token_mac_split(TokenIn),
%%     TokenParts =  mod_auth_token:token_body_split(Token),
%%     TokenType = binary_to_term(lists:nth(1, TokenParts)),
%%     SeqNo = case TokenType of
%%                 access -> -1;
%%                 refresh -> binary_to_term(lists:nth(4, TokenParts))
%%             end,

%%     #auth_token{type = TokenType,
%%                 expiry_datetime = mod_auth_token:seconds_to_datetime(binary_to_term(lists:nth(3, TokenParts))),
%%                 user_jid = lists:nth(2, TokenParts),
%%                 sequence_no = SeqNo,
%%                 mac_signature = MAC,
%%                 token_body = Token
%%                }.


%% args: binary() -> []
get_token_data(token) ->
    {Token, _Mac} = mod_auth_token:token_mac_split(token),
    mod_auth_token:token_body_split(Token).


%% Check following sequence:
%% Token is assembled from parts
%% MAC is generated
%% Token and MAC glued together and "sent" to user
%% Token is sent by user to server
%% Server recreates MAC knowing user's secret key and compares it with MAC extracted from token
%% Token content is disassembled and checked against original values

access_token_mac_reassembly_test(_) ->

    %% server sends token
    RequesterUser = <<"alice@localhost">>,
    ExpiryDate = {{2015,9,17},{20,28,21}}, %% DateTime
    SecretKey = <<"123abc">>,

    {Token, MAC}  = create_sample_access_token_body(RequesterUser, ExpiryDate, SecretKey),

    %% Assemble token just before base64 encoding for transport
    TokenWithMAC = mod_auth_token:concat_token_mac(Token, MAC),

    ct:pal(" token sent: ~p ~n ", [TokenWithMAC]),

    %% ------------------------------------------------------------------------
    %% token is now received by the server - should be the same, verified by MAC
    %% ------------------------------------------------------------------------

    {TokenReceived, MACReceived}  = mod_auth_token:token_mac_split(TokenWithMAC),
    MACforCheck = create_hmac_signature(TokenReceived, SecretKey),

    ct:pal("~nMAC origi ~p ~nMAC check ~p ~nMAC recvd ~p", [MAC, MACforCheck, MACReceived]),

    true = MACReceived =:= MACforCheck,

    %% if passed let's check the token contents anyway

    TokenParts = mod_auth_token:token_body_split(TokenReceived),
    ct:pal("~n Token parts after split  ~p ~n ", [TokenParts]),

    %%TokenType = lists:nth(1, TokenParts),
    UserFromToken = lists:nth(2, TokenParts),
    ExpiryDateBinary = lists:nth(3, TokenParts),
    ExpiryDateTerm = binary_to_term(ExpiryDateBinary),
    ct:pal("~n Expiry DateTime as term ~p ~n ", [ExpiryDateTerm]),

    ExpiryFromToken = mod_auth_token:seconds_to_datetime(ExpiryDateTerm),

    true = UserFromToken =:= RequesterUser,
    true = ExpiryFromToken =:= ExpiryDate.


refresh_token_body_reassembly_test(_) ->

    RequesterUser = <<"alice@localhost">>,

    ExpiryDate =  {{2015,9,17},{20,28,21}}, %% DateTime

    SequenceNo = {555},

    Token = mod_auth_token:generate_refresh_token_body(RequesterUser, ExpiryDate, SequenceNo),


    TokenParts  = mod_auth_token:token_body_split(Token),
    ct:pal("~n Token parts after split  ~p ~n ", [TokenParts]),

    TokenType  = binary_to_term(lists:nth(1, TokenParts)),
    UserRestored = lists:nth(2, TokenParts),

    El2 = lists:nth(3, TokenParts),
    ExpiryRestored = mod_auth_token:seconds_to_datetime(binary_to_term(El2)),

    {SequenceRestored} = binary_to_term(lists:nth(4, TokenParts)),

    ct:pal("~n User from Token ~p ~n Expiry from Token ~p ~n Sequence nunber ~p ~n",
           [UserRestored, ExpiryRestored, SequenceRestored]),

    true = TokenType =:= refresh,
    true = ExpiryDate =:= ExpiryRestored,
    true = RequesterUser =:= UserRestored,
    ct:pal(" ----- sequence restored : ~p ~n ", [SequenceRestored]),
    true = SequenceNo =:= {SequenceRestored}.


get_bare_jid_binary_test(_) ->

    RawUser = get_alice_jid_serverside(),
    BinaryUser = mod_auth_token:get_bare_jid_binary(RawUser),
    true = <<"alice@localhost">> =:= BinaryUser.

%% simulates what is passed to process_iq handler of modules under test.
get_alice_jid_serverside() ->
   {jid, <<"alicE">>,<<"localhost">>,<<"res1">>,<<"alice">>,<<"localhost">>,<<"res1">>}.

   %% #jid{
   %%    user = <<"alicE">>,
   %%    server = <<"localhost">>,
   %%    resource = <<"res1">>,
   %%    luser = <<"alice">>,
   %%    lserver = <<"localhost">>,
   %%    lresource = <<"res1">>}.

get_token_expiry_date() ->
    DT =  {{2015,9,17},{12,59,24}},
    DTS = calendar:datetime_to_gregorian_seconds(DT),
    <<DTS>>.

generate_new_tokens_request() ->
    SubEl = #xmlel{name = <<"query">>,
                   attrs = [{<<"xmlns">>,<<"urn:xmpp:tmp:auth-token">>}],
                   children = []},
    #iq{type = get, sub_el = SubEl}.

    %% #xmlel{name = <<"iq">>,
    %%        attrs = [{<<"type">>,<<"get">>},
    %%                 {<<"id">>,<<"123">>},
    %%                 {<<"to">>,<<"alicE@localhost">>}],
    %%         children = [
    %%                     #xmlel{name = <<"query">>,
    %%                            attrs = [{<<"xmlns">>,?NS_AUTH_TOKEN}]
    %%                           }]}.



