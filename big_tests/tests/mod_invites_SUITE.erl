-module(mod_invites_SUITE).

% elp:ignore W0054 (no_nowarn_suppressions)
-compile([export_all, nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_COMMANDS, <<"http://jabber.org/protocol/commands">>).
-define(NS_FEATURE_IBR_TOKEN, <<"urn:xmpp:ibr-token:0">>).
-define(NS_INVITE_CREATE_ACCOUNT, <<"urn:xmpp:invite#create-account">>).
-define(NS_INVITE_INVITE, <<"urn:xmpp:invite#invite">>).
-define(NS_INVITE_INVITATION, <<"urn:xmpp:invite#invitation">>).
-define(NS_PARS, <<"urn:xmpp:pars:0">>).

-import(domain_helper, [host_type/0, domain/0]).
-import(config_parser_helper, [default_mod_config/1, mod_config/2]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).

-define(match(Guard, Expr), ?assertMatch(Guard, Expr)).
-define(MAX_INVITES, 5).

-record(invite_token, {token :: binary(),
                       inviter :: {binary(), binary()},
                       %% A non-empty value if `invitee` indicates the invite has been used.
                       invitee = <<>> :: binary(),
                       created_at :: calendar:datetime(),
                       expires :: calendar:datetime(),
                       type = roster_only :: roster_only | account_only | account_subscription | reset_token,
                       %% If type is 'roster_only' then we indicate a token has been used to create
                       %% an account (if allowed) by setting `account_name` to the name of the user
                       %% (which should match `invitee`).
                       account_name = <<>> :: binary()
                      }).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, create_account_allowed},
     {group, create_account_not_allowed}].

groups() ->
    [{create_account_allowed, [parallel],
      [adhoc_items,
       adhoc_info_invite,
       adhoc_info_create_account,
       adhoc_command_invite,
       adhoc_command_create_account,
       max_invites,
       ibr_roster_invite,
       http,
       invites_page,
       reset_token
      ]
     },
     {create_account_not_allowed, [parallel],
      [adhoc_items_create_account_not_allowed,
       adhoc_info_invite,
       adhoc_info_create_account_not_allowed,
       adhoc_command_invite,
       adhoc_command_create_account_not_allowed,
       preauthenticated_roster_subscription,
       stream_feature,
       ibr,
       ibr_conflict,
       invites_page_not_allowed
      ]
     }].

suite() ->
    require_rpc_nodes([mim]) ++
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    Config0 = dynamic_modules:save_modules(HostType, Config),
    CowboyPort = ct:get_config({hosts, mim, cowboy_port}),
    [HttpListener] = mongoose_helper:get_listeners(mim(), #{port => CowboyPort, module => ejabberd_cowboy}),
    #{handlers := Handlers} = HttpListener,
    InvitesHandler = #{module => mod_invites_http, host => '_', path => "/invites"},
    UpdatedHttpListener = HttpListener#{handlers => [InvitesHandler | Handlers]},
    mongoose_helper:restart_listener(mim(), UpdatedHttpListener),
    Config1 = [{http_listener, HttpListener} | Config0],
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    OriginalHttpListener = ?config(http_listener, Config),
    mongoose_helper:restart_listener(mim(), OriginalHttpListener),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(create_account_allowed, Config) ->
    ModInvitesConfig = maps:merge(
                         default_mod_invites_config(),
                         #{access_create_account => register,
                           max_invites => ?MAX_INVITES,
                           landing_page => <<"http://{{host}}:5280/invites/{{invite.token}}">>
                          }),
    ModRegisterConfig = maps:merge(default_mod_config(mod_register),
                                   #{access => admin}),
    dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_invites, ModInvitesConfig},
                                                               {mod_register, ModRegisterConfig}]),
    [{create_account_allowed, true} | Config];
init_per_group(create_account_not_allowed, Config) ->
    ModInvitesConfig = default_mod_invites_config(),
    ModRegisterConfig = maps:merge(default_mod_config(mod_register),
                                   #{access => admin}),
    dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_invites, ModInvitesConfig},
                                                               {mod_register, ModRegisterConfig}]),
    [{create_account_allowed, false} | Config].

end_per_group(_Group, Config) ->
    escalus_fresh:clean(),
    dynamic_modules:stop(domain_helper:host_type(), mod_invites),
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Adhoc commands tests
%%--------------------------------------------------------------------

with_alice(Config, Fun) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun(Alice) ->
                                Server = escalus_client:server(Alice),
                                Fun(Alice, Server)
                        end).
-define(with_alice(Fun), with_alice(Config, Fun)).

adhoc_items(Config) ->
    ?with_alice(
       fun(Alice, Server) ->
               Stanza = escalus:send_and_wait(Alice, escalus_stanza:disco_items(Server, ?NS_COMMANDS)),
               escalus:assert(is_iq_result, Stanza),
               Query = exml_query:subelement(Stanza, <<"query">>),
               ItemInvite = exml_query:subelement_with_attr(Query, <<"node">>, ?NS_INVITE_INVITE),
               ?assertEqual(Server, exml_query:attr(ItemInvite, <<"jid">>)),
               ItemCreateAccount = exml_query:subelement_with_attr(Query, <<"node">>, ?NS_INVITE_CREATE_ACCOUNT),
               ?assertEqual(Server, exml_query:attr(ItemCreateAccount, <<"jid">>)),
               escalus:assert(is_stanza_from, [domain()], Stanza)
       end).

adhoc_info_invite(Config) ->
    ?with_alice(
       fun(Alice, Server) ->
               Stanza = escalus:send_and_wait(Alice, escalus_stanza:disco_info(Server, ?NS_INVITE_INVITE)),
               escalus:assert(is_iq_result, Stanza),
               escalus:assert(has_feature, [?NS_COMMANDS], Stanza),
               escalus:assert(has_identity, [<<"automation">>, <<"command-node">>], Stanza),
               escalus:assert(is_stanza_from, [domain()], Stanza)
       end).

adhoc_info_create_account(Config) ->
    ?with_alice(
       fun(Alice, Server) ->
               Stanza = escalus:send_and_wait(Alice, escalus_stanza:disco_info(Server, ?NS_INVITE_CREATE_ACCOUNT)),
               escalus:assert(is_iq_result, Stanza),
               escalus:assert(has_feature, [?NS_COMMANDS], Stanza),
               escalus:assert(has_identity, [<<"automation">>, <<"command-node">>], Stanza),
               escalus:assert(is_stanza_from, [domain()], Stanza)
       end).

adhoc_items_create_account_not_allowed(Config) ->
    ?with_alice(
       fun(Alice, Server) ->
               Stanza = escalus:send_and_wait(Alice, escalus_stanza:disco_items(Server, ?NS_COMMANDS)),
               escalus:assert(is_iq_result, Stanza),
               Query = exml_query:subelement(Stanza, <<"query">>),
               ?assertEqual(undefined,
                            exml_query:subelement_with_attr(Query, <<"node">>, ?NS_INVITE_CREATE_ACCOUNT)),
               Item = exml_query:subelement_with_attr(Query, <<"node">>, ?NS_INVITE_INVITE),
               ?assertEqual(Server, exml_query:attr(Item, <<"jid">>)),
               escalus:assert(is_stanza_from, [domain()], Stanza)
       end).

adhoc_info_create_account_not_allowed(Config) ->
    ?with_alice(
       fun(Alice, Server) ->
               Stanza = escalus:send_and_wait(Alice, escalus_stanza:disco_info(Server, ?NS_INVITE_CREATE_ACCOUNT)),
               escalus:assert(is_iq_error, Stanza),
               escalus:assert(is_stanza_from, [domain()], Stanza)
       end).

adhoc_command_invite(Config) ->
    ?with_alice(
       fun(Alice, Server) ->
               User = escalus_client:username(Alice),
               URI = send_adhoc_invite(Alice),
               ?match({match, [_, _]},
                      re:run(URI,
                             <<"xmpp:",
                               (re_escape(User))/binary,
                               "@",
                               Server/binary,
                               "\\?roster;preauth=(.+)">>)),
               ?assertEqual(?config(create_account_allowed, Config), has_ibr_suffix(URI)),
               Token = token_from_uri(URI),

               ?match(true, is_token_valid(Server, User, Token))
       end
      ).

adhoc_command_create_account(Config) ->
    ?with_alice(
      fun(Alice, Server) ->
              URI1 = test_create_account(Alice, <<>>, <<"0">>),
              ?assert(re:run(URI1, <<"xmpp:", Server/binary, "\\?register;preauth=(.+)">>) =/= nomatch),
              URI2 = test_create_account(Alice, <<"foobar1">>, <<"0">>),
              ?assert(re:run(URI2, <<"xmpp:foobar1@", Server/binary, "\\?register;preauth=(.+)">>) =/= nomatch),
              URI3 = test_create_account(Alice, <<>>, <<"1">>),
              ?assert(re:run(URI3, <<"xmpp:", Server/binary, "\\?register;preauth=(.+)">>) =/= nomatch),
              URI4 = test_create_account(Alice, <<"foobar2">>, <<"1">>),
              ?assert(re:run(URI4, <<"xmpp:foobar2@", Server/binary, "\\?register;preauth=(.+)">>) =/= nomatch),
              escalus:assert(is_iq_error,
                             send_adhoc_create_account(Alice, <<"foobar1">>, <<"0">>)),
              escalus:assert(is_iq_error,
                             send_adhoc_create_account(Alice, escalus_client:username(Alice), <<"0">>))

      end
      ).

adhoc_command_create_account_not_allowed(Config) ->
    ?with_alice(
      fun(Alice, Server) ->
              Stanza =
                  escalus_client:send_and_wait(Alice, escalus_stanza:to(
                                                        escalus_stanza:adhoc_request(?NS_INVITE_CREATE_ACCOUNT),
                                                        Server)),
              escalus:assert(is_iq_error, Stanza),
              escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Stanza),
              escalus:assert(is_stanza_from, [domain()], Stanza)
      end
      ).

max_invites(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
       fun(Alice) ->
               Server = escalus_client:server(Alice),
               lists:foreach(
                 fun(_) ->
                         escalus:assert(is_iq_result,
                                        send_adhoc_create_account(Alice, <<>>, <<"0">>))
                 end, lists:seq(1, ?MAX_INVITES)),
               escalus:assert(is_iq_error,
                              send_adhoc_create_account(Alice, <<>>, <<"0">>)),
               Token = token_from_uri(send_adhoc_invite(Alice)),
               Bob = fresh_client_with_stream(Config, bob),
               escalus:assert(
                 is_iq_error,
                 send_pars(Bob, Token)
                ),
               ?match(true, is_token_valid(Server, Token))
       end
     ).

%%--------------------------------------------------------------------
%% Preauthenticated Roster Subscription
%%--------------------------------------------------------------------
preauthenticated_roster_subscription(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              BobJID = escalus_client:short_jid(Bob),
              PreAuthToken = token_from_uri(send_adhoc_invite(Bob)),
              escalus_client:send(
                Alice,
                escalus_stanza:iq_set(<<"jabber:iq:roster">>, [#xmlel{name = <<"item">>, attrs = #{<<"jid">> => escalus_client:short_jid(Bob)}}])
               ),
              _ = escalus_client:wait_for_stanzas(Alice, 2),
              escalus_client:send(
                Alice,
                escalus_stanza:to(
                  escalus_stanza:presence(<<"subscribe">>, [preauth(PreAuthToken)]),
                  BobJID
                 )),

              [_, _, _, AliceIq, AlicePres] = escalus_client:wait_for_stanzas(Alice, 5),

              escalus:assert(is_roster_set, AliceIq),
              escalus:assert(roster_contains, [BobJID], AliceIq),
              Query = exml_query:subelement(AliceIq, <<"query">>),
              ?match([#xmlel{attrs = #{<<"subscription">> := <<"to">>}}], Query#xmlel.children),

              escalus:assert(is_presence_with_type, [<<"subscribe">>], AlicePres),

              [BobStanza] = escalus_client:wait_for_stanzas(Bob, 1),
              escalus:assert(is_roster_set, BobStanza),

              ok
      end).

%%--------------------------------------------------------------------
%% Stream feature test
%%--------------------------------------------------------------------
stream_feature(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Alice = escalus_connection:connect(AliceSpec),
    escalus_client:send(Alice, stream_start(Alice)),
    [_StreamStartAnswer, #xmlel{children = StreamFeatures}] = escalus_client:wait_for_stanzas(Alice, 2, 500),
    ?assert([F || #xmlel{name = <<"register">>, attrs = #{<<"xmlns">> := ?NS_FEATURE_IBR_TOKEN}} = F <- StreamFeatures] =/= []).

%%--------------------------------------------------------------------
%% IBR tests
%%--------------------------------------------------------------------

ibr(Config) ->
    Alice = fresh_client_with_stream(Config, alice),
    Username = escalus_client:username(Alice),
    Server = escalus_client:server(Alice),
    escalus:assert(
      is_iq_error,
      send_iq_register(Alice, Username, <<"topSigrid">>)
     ),
    escalus:assert(
      is_iq_error,
      send_pars(Alice, <<"madeuptoken">>)
     ),
    #invite_token{token = Token} = create_account_invite(Server),
    #invite_token{token = Token2} = create_account_invite({<<>>, Server}, <<"reserved">>, false),
    escalus:assert(
      is_iq_result,
      send_pars(Alice, Token)
     ),
    escalus:assert(
      is_iq_error,
      send_iq_register(Alice, <<"reserved">>, <<"topSigrid">>)
     ),
    ?match(true, is_token_valid(Server, Token)),
    escalus:assert(
      is_iq_result,
      send_iq_register(Alice, Username, <<"topSigrid">>)
     ),
    ?match(false, is_token_valid(Server, Token)),
    unregister_user(Username, Server),
    delete_tokens(Server, [Token, Token2]),
    ok.

ibr_roster_invite(Config) ->
    Alice = fresh_client_with_stream(Config, alice),
    Username = escalus_client:username(Alice),
    Server = escalus_client:server(Alice),
    #invite_token{token = Token} = create_roster_invite({<<"inviter">>, Server}),

    escalus:assert(
      is_iq_result,
      send_pars(Alice, Token)
     ),
    ?match(true, is_token_valid(Server, Token)),
    escalus:assert(
      is_iq_result,
      escalus_client:send_and_wait(
        Alice,
        escalus_stanza:register_account(
                     [xmlel(<<"username">>, Username), xmlel(<<"password">>, <<"topSigrid">>)]
         )
       )
     ),
    ?match(true, is_token_valid(Server, Token)),
    escalus:assert(
      is_iq_error,
      send_pars(Alice, Token)
     ),
    escalus:assert(
      is_iq_error,
      escalus_client:send_and_wait(
        Alice,
        escalus_stanza:register_account(
                     [xmlel(<<"username">>, <<"another_account">>), xmlel(<<"password">>, <<"topSigrid">>)]
         )
       )
     ),
    unregister_user(Username, Server),
    delete_tokens(Server, [Token]).

ibr_conflict(Config) ->
    %% To protect against abuse we need to keep track of tokens being used across simultaneous
    %% connections
    Alice = fresh_client_with_stream(Config, alice),
    Bob = fresh_client_with_stream(Config, bob),
    Server = escalus_client:server(Alice),
    #invite_token{token = Token} = create_account_invite(Server),
    escalus:assert(
      is_iq_result,
      send_pars(Alice, Token)
     ),
    escalus:assert(
      is_iq_result,
      send_pars(Bob, Token)
     ),
    escalus:assert(
      is_iq_result,
      escalus_client:send_and_wait(
        Alice,
        escalus_stanza:register_account(
                     [xmlel(<<"username">>, escalus_client:username(Alice)), xmlel(<<"password">>, <<"topSigrid">>)]
         )
       )
     ),
    escalus:assert(
      is_iq_error,
      escalus_client:send_and_wait(
        Bob,
        escalus_stanza:register_account(
                     [xmlel(<<"username">>, escalus_client:username(Bob)), xmlel(<<"password">>, <<"topSigrid">>)]
         )
       )
     ),
    ?match(false, is_token_valid(Server, Token)),
    unregister_user(escalus_client:username(Alice), Server),
    delete_tokens(Server, [Token]).

%%--------------------------------------------------------------------
%% Landing pages tests
%%--------------------------------------------------------------------

http(_Config) ->
    httpc:set_options([{cookies, enabled}]),
    Domain = domain_helper:domain(),
    #{<<"landing_page">> := LandingPage,
     <<"token_uri">> := TokenURI} = format_invite(Domain, create_account_invite(Domain)),
    Token = token_from_uri(TokenURI),
    {ok, {{_, 200, _}, Headers, Body}} = httpc:request(LandingPage),
    {match, [TokenURI]} =
        re:run(
            proplists:get_value("link", Headers), "<(.+)>", [{capture, [1], binary}]),
    {match, RegistrationURLs} =
        re:run(Body,
               <<"href=\"", Token/binary, "([a-zA-Z0-9\/\-]+)\"">>,
               [global, {capture, [1], binary}]),
    Apps =
        rpc(mim(), mod_invites_http, apps_json,
            [<<"en">>, [{static, <<"/static">>}, {uri, <<>>}, {host_type, domain_helper:host_type()}]]),
    ?match(true, length(RegistrationURLs) == length(Apps) + 1),
    BaseURL = LandingPage,
    lists:foreach(fun([URL]) ->
                     FullURL = <<BaseURL/binary, "/", URL/binary>>,
                     ct:pal("Checking url ~p", [FullURL]),
                     ?match({ok, {{_, 200, _}, _, _}}, httpc:request(FullURL))
                  end,
                  RegistrationURLs),

    {ok, {{_, 404, _}, _, _}} = httpc:request(<<BaseURL/binary, "/UnkonwnApp">>),
    {ok, {{_, 404, _}, _, _}} = httpc:request(<<BaseURL/binary, "/UnkonwnApp/registration">>),
    {ok, {{_, 404, _}, _, _}} = httpc:request(<<BaseURL/binary, "/Dino/unknownpath">>),

    [Last] = lists:last(RegistrationURLs),
    RegURL = <<BaseURL/binary, Last/binary>>,
    CSRFToken = get_csrf_token(RegURL),

    {ok, {{_, 400, _}, _, _}} = post(RegURL, <<"badtoken">>, CSRFToken, <<"foo">>, <<"bar">>),
    %% ???
    %%{ok, {{_, 400, _}, _, _}} = post(RegURL, Token, CSRFToken, <<"@invalidUser">>, <<"bar">>),
    {ok, {{_, 400, _}, _, _}} = post(RegURL, Token, <<"foo">>, <<"bar">>),
    {ok, {{_, 400, _}, _, _}} =
        post(RegURL,
             Token,
             <<"guLRkZZFv+CGI7UbCnyija0KwPFmob71RGvGa7dQ5G4=">>,
             <<"foo">>,
             <<"bar">>),
    {ok, {{_, 400, _}, _, _}} = post(RegURL, Token, <<"nohashtoken">>, <<"foo">>, <<"bar">>),
    {ok, {{_, 200, _}, _, _}} = post(RegURL, Token, CSRFToken, <<"foo">>, <<"bar">>),
    {ok, {{_, 404, _}, _, _}} = post(RegURL, Token, CSRFToken, <<"foo">>, <<"bar">>),
    {ok, {{_, 404, _}, _, _}} = httpc:request(LandingPage),
    lists:foreach(fun([URL]) ->
                     FullURL = <<BaseURL/binary, "/", URL/binary>>,
                     ct:pal("Checking url ~p", [FullURL]),
                     ?match({ok, {{_, 404, _}, _, _}}, httpc:request(FullURL))
                  end,
                  RegistrationURLs),

    #{<<"landing_page">> := RosterInviteURL,
     <<"token_uri">> := RosterInviteURI} = format_invite(Domain, create_roster_invite({<<"inviter">>, Domain})),
    RosterInviteToken = token_from_uri(RosterInviteURI),
    {ok, {{_, 200, _}, _, _}} = httpc:request(RosterInviteURL),
    FakeRegURL = <<RosterInviteURL/binary, "/registration">>,
    {ok, {{_, 404, _}, _, _}} =
        post(FakeRegURL, RosterInviteToken, CSRFToken, <<"baz">>, <<"bar">>),

    unregister_user(<<"foo">>, Domain),
    delete_tokens(Domain, [Token, RosterInviteToken]),
    ok.

% elp:ignore W0008 (unreachable_test)
get_csrf_token(URL) ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(URL),
    {match, [[CSRFToken]]} =
        re:run(Body,
               <<"<input.+name=\"csrf_token\" value=\"(.+)\"">>,
               [global, {capture, [1], binary}]),
    ct:pal("extracted csrf token: ~p", [CSRFToken]),
    CSRFToken.

post(URL, Token, User, Password) ->
    Data = to_qs([{token, Token}, {user, User}, {password, Password}]),
    post(URL, [], Data).

post(URL, Token, CSRFToken, User, Password) ->
    Data =
        to_qs([{token, Token}, {user, User}, {password, Password}, {csrf_token, CSRFToken}]),
    post(URL, [], Data).

post(URL, Headers, Data) ->
    httpc:request(post, {URL, Headers, "application/x-www-form-urlencoded", Data}, [], []).

% elp:ignore W0008 (unreachable_test)
to_qs(List) ->
    lists:foldl(fun ({K, V}, <<>>) ->
                        <<(atom_to_binary(K))/binary, "=", (uri_string:quote(V))/binary>>;
                    ({K, V}, QS) ->
                        <<QS/binary,
                          "&",
                          (atom_to_binary(K))/binary,
                          "=",
                          (uri_string:quote(V))/binary>>
                end,
                <<>>,
                List).

%%--------------------------------------------------------------------
%% Create Invites Page
%%--------------------------------------------------------------------

invites_page(Config) ->
    [{_AliceName, AliceSpec}] = escalus_users:get_users([alice]),
    [User, Server, Password] = escalus_users:get_usp(Config, AliceSpec),
    rpc(mim(), ejabberd_auth, try_register, [mongoose_helper:make_jid(User, Server), Password]),

    BaseURL = <<"http://", Server/binary, ":5280/invites">>,

    httpc:set_options([{cookies, enabled}]),

    CSRFToken = get_csrf_token(BaseURL),
    ?match({ok, {{_, 200, _}, _, _}},
           post(BaseURL,
                [],
                to_qs([{user, User}, {password, Password}, {csrf_token, CSRFToken}]))),
    ?match({ok, {{_, 400, _}, _, _}},
           post(BaseURL,
                [],
                to_qs([{user, User}, {password, <<"bad_password">>}, {csrf_token, CSRFToken}]))),
    ?match({ok, {{_, 400, _}, _, _}},
           post(BaseURL,
                [],
                to_qs([{user, User},
                       {password, Password},
                       {csrf_token, CSRFToken},
                       {account_name, User}]))),
    ?match({ok, {{_, 200, _}, _, _}},
           post(BaseURL,
                [],
                to_qs([{user, User},
                       {password, Password},
                       {csrf_token, CSRFToken},
                       {account_name, <<"some_free_account_name">>}]))),
    %% now it's reserved
    ?match({ok, {{_, 400, _}, _, _}},
           post(BaseURL,
                [],
                to_qs([{user, User},
                       {password, Password},
                       {csrf_token, CSRFToken},
                       {account_name, <<"some_free_account_name">>}]))),

    unregister_user(User, Server),
    ok.

invites_page_not_allowed(Config) ->
    [{_AliceName, AliceSpec}] = escalus_users:get_users([alice]),
    [User, Server, Password] = escalus_users:get_usp(Config, AliceSpec),
    rpc(mim(), ejabberd_auth, try_register, [mongoose_helper:make_jid(User, Server), Password]),

    BaseURL = <<"http://", Server/binary, ":5280/invites">>,

    httpc:set_options([{cookies, enabled}]),

    CSRFToken = get_csrf_token(BaseURL),
    ?match({ok, {{_, 400, _}, _, _}},
           post(BaseURL, [], to_qs([{user, User}, {password, Password}, {csrf_token, CSRFToken}]))),
    unregister_user(User, Server),
    ok.

%%--------------------------------------------------------------------
%% PW Reset Token
%%--------------------------------------------------------------------

reset_token(Config) ->
    AliceSpec = escalus_fresh:freshen_spec(Config, alice),
    [User, Server, Password] = escalus_users:get_usp(Config, AliceSpec),
    rpc(mim(), ejabberd_auth, try_register, [mongoose_helper:make_jid(User, Server), Password]),

    #{<<"landing_page">> := BaseURL,
      <<"token_uri">> := TokenURI} = format_invite(Server, create_reset_token(Server, User)),
    Token = token_from_uri(TokenURI),

    httpc:set_options([{cookies, enabled}]),
    CSRFToken = get_csrf_token(BaseURL),

    ?match(true, check_password(User, Server, Password)),

    Alice = escalus_connection:connect(AliceSpec),
    escalus_client:send(Alice, stream_start(Alice)),
    [_StreamStartAnswer, #xmlel{children = _StreamFeatures}] = escalus_client:wait_for_stanzas(Alice, 2, 500),

    escalus:assert(is_iq_error, send_iq_register(Alice, User, <<"newPassword">>)),
    escalus:assert(is_iq_result, send_pars(Alice, Token)),
    escalus:assert(is_iq_error, send_iq_register(Alice, <<"wrong_user">>, <<"newPassword">>)),
    escalus:assert(is_iq_result, send_iq_register(Alice, User, <<"newPassword">>)),

    ?match(true, check_password(User, Server, <<"newPassword">>)),
    ?match(false, check_password(User, Server, Password)),

    ?match(false, is_token_valid(Server, Token)),

    {ok, {{_, 404, _}, _, _}} = post(BaseURL, Token, CSRFToken, User, <<"anotherPassword">>),

    #{<<"landing_page">> := BaseURL2,
      <<"token_uri">> := TokenURI2} = format_invite(Server, create_reset_token(Server, User)),
    Token2 = token_from_uri(TokenURI2),

    CSRFToken2 = get_csrf_token(BaseURL2),

    {ok, {{_, 400, _}, _, _}} =
        post(BaseURL2, Token2, CSRFToken, User, <<"anotherPassword">>),
    {ok, {{_, 400, _}, _, _}} =
        post(BaseURL2, Token2, CSRFToken2, <<"wronguser">>, <<"anotherPassword">>),
    {ok, {{_, 200, _}, _, _}} =
        post(BaseURL2, Token2, CSRFToken2, User, <<"anotherPassword">>),
    ?match(true, check_password(User, Server, <<"anotherPassword">>)),

    unregister_user(User, Server),
    delete_tokens(Server, [Token, Token2]),
    ok.

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------
default_mod_invites_config() ->
    maps:merge(default_mod_config(mod_invites),
              #{backend => mongoose_helper:mnesia_or_rdbms_backend()}).

% elp:ignore W0008 (unreachable_test)
send_adhoc_invite(Client) ->
    Server = escalus_client:server(Client),
    Stanza =
        escalus_client:send_and_wait(Client, escalus_stanza:to(
                                              escalus_stanza:adhoc_request(?NS_INVITE_INVITE),
                                              Server)),
    escalus:assert(is_iq_result, Stanza),
    escalus:assert(is_adhoc_response, [?NS_INVITE_INVITE, <<"completed">>], Stanza),
    #{ns := ?NS_INVITE_INVITATION, kvs := #{<<"uri">> := URI, <<"expire">> := _Expires}} =
        form_helper:find_and_parse_form(
          exml_query:subelement(Stanza, <<"command">>)),
    escalus:assert(is_stanza_from, [domain()], Stanza),
    URI.

send_adhoc_create_account(Client, Username, Subscription) ->
    Server = escalus_client:server(Client),
    FirstStep =
        escalus_client:send_and_wait(Client, escalus_stanza:to(
                                              escalus_stanza:adhoc_request(?NS_INVITE_CREATE_ACCOUNT),
                                              Server)),
    escalus:assert(is_iq_result, FirstStep),
    #xmlel{attrs = FirstCommandAttrs} = FirstCommand = exml_query:subelement(FirstStep, <<"command">>),
    #xmlel{attrs = #{<<"execute">> := <<"complete">>}, children = [#xmlel{name = <<"complete">>}]} = exml_query:subelement(FirstCommand, <<"actions">>),
    escalus:assert(is_adhoc_response, [?NS_INVITE_CREATE_ACCOUNT, <<"executing">>], FirstStep),
    #{type := <<"form">>, kvs := #{<<"username">> := _, <<"roster-subscription">> := _}} =
        form_helper:find_and_parse_form(FirstCommand),
    SID = maps:get(<<"sessionid">>, FirstCommandAttrs),
    FormSpec = #{type => <<"submit">>, fields => [#{var => <<"username">>, values => [Username]},
                                                  #{var => <<"roster-subscription">>, values => [Subscription]}]},
    SubmitForm = set_adhoc_command_sid(
                   SID,
                   escalus_stanza:adhoc_request(?NS_INVITE_CREATE_ACCOUNT, [form_helper:form(FormSpec)])),
    escalus_client:send_and_wait(Client, escalus_stanza:to(SubmitForm, Server)).

set_adhoc_command_sid(SID, IQ = #xmlel{children = Children}) ->
    UpdatedChildren = lists:map(
                        fun(#xmlel{name = <<"command">>, attrs = Attrs} = Command) ->
                                Command#xmlel{attrs = Attrs#{<<"sessionid">> => SID}};
                           (El) ->
                                El
                        end, Children),
    IQ#xmlel{children = UpdatedChildren}.

test_create_account(Client, Username, Subscription) ->
    Result = send_adhoc_create_account(Client, Username, Subscription),
    escalus:assert(is_iq_result, Result),
    escalus:assert(is_adhoc_response, [?NS_INVITE_CREATE_ACCOUNT, <<"completed">>], Result),
    #{ns := ?NS_INVITE_INVITATION, kvs := #{<<"uri">> := URI, <<"expire">> := _Expires}} =
        form_helper:find_and_parse_form(
          exml_query:subelement(Result, <<"command">>)),
    Token = token_from_uri(URI),
    ?match(true, is_token_valid(Client, Token)),
    escalus:assert(is_stanza_from, [domain()], Result),
    URI.

is_token_valid(#client{} = Client, Token) ->
    Username = escalus_client:username(Client),
    Server = escalus_client:server(Client),
    is_token_valid(Server, Username, Token);
is_token_valid(Server, Token) ->
    is_token_valid(Server, <<>>, Token).

is_token_valid(Server, Username, Token) ->
    HostType = domain_helper:host_type(),
    rpc(mim(), mod_invites, is_token_valid, [HostType, Token, {Username, Server}]).

send_iq_register(Client, Username, Password) ->
    escalus_client:send_and_wait(
      Client,
      escalus_stanza:register_account(
        [xmlel(<<"username">>, Username), xmlel(<<"password">>, Password)]
       )
     ).

% elp:ignore W0008 (unreachable_test)
token_from_uri(Uri) ->
    {match, [Token]} =
        re:run(Uri, ".+preauth=([a-zA-z0-9]+)", [{capture, all_but_first, binary}]),
    Token.

% elp:ignore W0008 (unreachable_test)
has_ibr_suffix(URI) ->
    re:run(URI, <<";ibr=y$">>) =/= nomatch.

% elp:ignore W0008 (unreachable_test)
re_escape(Str) ->
    re_escape(Str, <<>>).

re_escape(<<>>, Escaped) ->
    ct:pal("escaped: ~p", [Escaped]),
    Escaped;
re_escape(<<C:1/binary, Tail/binary>>, Acc) ->
    case lists:member(C,
                      [<<".">>,
                       <<"*">>,
                       <<"+">>,
                       <<"?">>,
                       <<"^">>,
                       <<"$">>,
                       <<"(">>,
                       <<")">>,
                       <<"[">>,
                       <<"]">>,
                       <<"{">>,
                       <<"}">>,
                       <<"|">>,
                       <<";">>,
                       <<"!">>,
                       <<"`">>,
                       <<"#">>,
                       <<"~">>,
                       <<"!">>,
                       <<"_">>,
                       <<"-">>,
                       <<"=">>,
                       <<"\\">>])
    of
        true ->
            re_escape(Tail, <<Acc/binary, <<"\\", C/binary>>/binary>>);
        false ->
            re_escape(Tail, <<Acc/binary, C/binary>>)
    end.

% elp:ignore W0008 (unreachable_test)
stream_start(Client) ->
    Server = escalus_utils:get_server(Client),
    From = escalus_utils:get_jid(Client),
    stream_start(Server, From).

stream_start(Server, From) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = #{<<"to">> => Server,
                             <<"from">> => From,
                             <<"version">> => <<"1.0">>,
                             <<"xml:lang">> => <<"en">>,
                             <<"xmlns">> => <<"jabber:client">>,
                             <<"xmlns:stream">> => <<"http://etherx.jabber.org/streams">>}}.

% elp:ignore W0008 (unreachable_test)
create_account_invite(Host) ->
    create_account_invite({<<>>, Host}, <<>>, false).

create_account_invite(Inviter, AccountName, Subscribe) ->
    HostType = domain_helper:host_type(),
    rpc(mim(), mod_invites, create_account_invite, [HostType, Inviter, AccountName, Subscribe]).

% elp:ignore W0008 (unreachable_test)
create_roster_invite(Inviter) ->
    HostType = domain_helper:host_type(),
    rpc(mim(), mod_invites, create_roster_invite, [HostType, Inviter]).

create_reset_token(Server, User) ->
    HostType = domain_helper:host_type(),
    rpc(mim(), mod_invites, create_reset_token, [HostType, User, Server]).

format_invite(Host, Invite) ->
    rpc(mim(), mod_invites, format_invite, [Host, Invite]).

unregister_user(Username, Server) ->
    rpc(mim(), ejabberd_admin, unregister, [Username, Server]).

delete_tokens(Server, Tokens) ->
    lists:foreach(fun(Token) ->
                          rpc(mim(), mod_invites, delete_invite_by_token, [Server, Token])
                  end, Tokens).

fresh_client_with_stream(Config, UsernameOrResourceSpec) ->
    ClientSpec = escalus_fresh:freshen_spec(Config, UsernameOrResourceSpec),
    Client = escalus_connection:connect(ClientSpec),
    escalus_client:send(Client, stream_start(Client)),
    [_StreamStartAnswer, #xmlel{children = _StreamFeatures}] = escalus_client:wait_for_stanzas(Client, 2, 500),
    Client.

xmlel(Name, Body) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Body}]}.

send_pars(Client, Token) ->
    Pars = preauth(Token),
    escalus_client:send_and_wait(
      Client,
      escalus_stanza:iq(escalus_client:server(Client), <<"set">>, [Pars])).

check_password(User, Server, Password) ->
    Jid = jid:make_bare(User, Server),
    rpc(mim(), ejabberd_auth, check_password, [Jid, Password]).

% elp:ignore W0008 (unreachable_test)
preauth(Token) ->
    #xmlel{name = <<"preauth">>, attrs = #{<<"xmlns">> => ?NS_PARS, <<"token">> => Token }}.
