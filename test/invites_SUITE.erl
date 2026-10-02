-module(invites_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("jlib.hrl").
-include("mod_invites.hrl").

-define(match(Guard, Expr), ?assertMatch(Guard, Expr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Eunit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_invites_tree_as_root_t_test_() ->
    {setup,
     fun() ->
             meck:new(db, [non_strict]),
             meck:expect(db,
                         get_invites_t,
                         fun (_, {<<"1">>, _}) ->
                                 [#invite_token{invitee = <<"2@host">>, type = account_only},
                                  #invite_token{invitee = <<"rosterinvite@forcecrash">>}];
                             (_, {<<"2">>, _}) ->
                                 [#invite_token{invitee = <<"3@host">>, type = account_only},
                                  #invite_token{invitee = <<"4@host">>, type = account_only}];
                             (_, {<<"3">>, _}) ->
                                 [#invite_token{invitee = <<"5@host">>, type = account_subscription},
                                  #invite_token{invitee = <<"6@host">>, account_name = <<"6">>},
                                  #invite_token{type = account_only}];
                             (_, {_, <<"host">>}) ->
                                 []
                         end),
             meck:new(mongoose_backend, [passthrough]),
             meck:expect(mongoose_backend, call, fun(_, _, Fun, Args) -> erlang:apply(db, Fun, Args) end),
             [db, mongoose_backend]
     end,
     fun meck:unload/1,
     fun(_) ->
             [?_assertMatch(6,
                            length(mod_invites:get_invites_tree_as_root_t(<<"host">>,
                                                                          {<<"1">>, <<"host">>})))]
     end}.

find_invites_tree_root_t_test_() ->
    {setup,
     fun() ->
             meck:new(db, [non_strict]),
             meck:expect(db,
                         get_invite_by_invitee_t,
                         fun (_, {<<"4">>, <<"host">>}) ->
                                 #invite_token{inviter = {<<"3">>, <<"host">>}};
                             (_, {<<"3">>, <<"host">>}) ->
                                 #invite_token{inviter = {<<"2">>, <<"host">>}};
                             (_, {<<"2">>, <<"host">>}) ->
                                 #invite_token{inviter = {<<"1">>, <<"host">>}};
                             (_, _) ->
                                 {error, not_found}
                         end),
             meck:new(mongoose_backend, [passthrough]),
             meck:expect(mongoose_backend, call, fun(_, _, Fun, Args) -> erlang:apply(db, Fun, Args) end),
             meck:new(calendar, [unstick, passthrough]),
             meck:expect(calendar, now_to_datetime, 1, then),
             meck:expect(calendar, datetime_to_gregorian_seconds, fun(then) -> 1 end),
             [db, mongoose_backend, calendar]
     end,
     fun meck:unload/1,
     fun(_) ->
             [%% lvl not reached
              ?_assertMatch({<<"1">>, <<"host">>},
                            mod_invites:find_invites_tree_root_t(2, host, {<<"3">>, <<"host">>}, 0)),
              %% lvl reached
              ?_assertThrow(speedy_goat,
                            mod_invites:find_invites_tree_root_t(2, host, {<<"4">>, <<"host">>}, 0)),
              %% lvl reached but later
              ?_assertMatch({<<"1">>, <<"host">>},
                            mod_invites:find_invites_tree_root_t(?SPEEDY_GOAT_SECONDS + 1,
                                                                 host,
                                                                 {<<"4">>, <<"host">>},
                                                                 0)),
              ?_assert(meck:validate(db))]
     end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [ gen_invite
    , expire_and_delete
    , cleanup_expired
    , token_valid
    , remove_user
    , expire_tokens
    , is_reserved
    ].

init_per_suite(Config0) ->
    application:ensure_all_started(jid),
    mnesia:create_schema([node()]),
    mnesia:start(),

    Server = <<"localhost">>,
    User = <<"test">>,

    mongoose_config:set_opts(
      #{
        hosts => [Server],
        host_types => [],
        internal_databases => #{mnesia => #{}},
        instrumentation => config_parser_helper:default_config([instrumentation]),
        {modules, Server} =>
            config_parser_helper:config(
              [modules],
              #{mod_invites => #{ token_expire_seconds => 3600
                                , max_invites => infinity
                                }})
    }),

    Config = [{server, Server},
              {user, User}
             | Config0],
    Config1 =  async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                           {gen_hook, start_link, []},
                                           {mongoose_domain_core, start_link, [[], []]}]),
    mongoose_modules:start(),
    Config1.

end_per_suite(Config) ->
    mongoose_config:erase_opts(),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% CT Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_invite(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),

    meck:new(ejabberd_auth, [passthrough]),
    meck:expect(ejabberd_auth, does_user_exist,
                fun(#jid{luser = LUser, lserver = LServer}) ->
                        LUser == User andalso LServer == Server
                end),

    {TokenURI, _LandingPage} = create_invite(<<"foo">>, Server),
    ?match(<<"xmpp:foo@", Server:(size(Server))/binary, "?register;preauth=", _/binary>>,
           TokenURI),
    Token = token_from_uri(TokenURI),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = <<"foo">>} =
        mod_invites:get_invite(Server, Token),
    {TokenURI2, _LP2} = create_invite(<<>>, Server),
    ?match(<<"xmpp:", _/binary>>, TokenURI2),
    Token2 = token_from_uri(TokenURI2),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = <<>>} =
        mod_invites:get_invite(Server, Token2),
    ?match({error, user_exists}, create_invite(User, Server)),
    ?match({error, account_name_invalid},
           create_invite(<<"@bad_acccount_name">>, Server)),
    ?match({error, host_unknown}, create_invite(<<"bar">>, <<"non.existant.host">>)),

    ?match(2, length(unlift(mod_invites:list_invites(Server)))),
    %% not working because unknown hostname takes precedence
    %% TooLongHostname = list_to_binary([$a || _ <- lists:seq(1, 1024)]),
    %% ?match({error, hostname_invalid}, create_invite(<<"foo">>, TooLongHostname)),
    mod_invites:expire_invites(Server, <<>>),
    ?match(2, mod_invites:cleanup_expired()),
    ok.

expire_and_delete(Config) ->
    Server = ?config(server, Config),
    #invite_token{token = Token} = create_account_invite(Server, {<<"foo">>, Server}),
    ?match(ok, mod_invites:expire_invite_by_token(Server, Token)),
    ?match(true,
           mod_invites:is_expired(
               mod_invites:get_invite(Server, Token))),
    ?match(ok, mod_invites:delete_invite_by_token(Server, Token)),
    ?match({error, "Token not found"}, mod_invites:delete_invite_by_token(Server, Token)).

cleanup_expired(Config) ->
    Server = ?config(server, Config),
    create_account_invite(Server, {<<"foo">>, Server}),
    mod_invites:expire_invites(Server, <<"foo">>),
    Token = token_from_uri(element(1, create_invite(<<"foobar">>, Server))),
    ?match(1, mod_invites:cleanup_expired()),
    ?match(#invite_token{}, mod_invites:get_invite(Server, Token)),
    ?match(0, mod_invites:cleanup_expired()),
    mod_invites:expire_invites(Server, <<>>),
    ?match(1, mod_invites:cleanup_expired()).

token_valid(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    {TokenURI, _LandingPage} = create_invite(<<"foobar">>, Server),
    Token = token_from_uri(TokenURI),
    ?match(true, mod_invites:is_token_valid(Server, Token)),
    Inviter = {<<"foo">>, Server},
    #invite_token{token = AccountToken} = create_account_invite(Server, Inviter),
    ?match(true, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    try mod_invites:is_token_valid(Server, <<"madeUptoken">>) of
        break ->
            broken
    catch
        _:E ->
            ?match(not_found, E)
    end,
    ?match(false,
           mod_invites:is_token_valid(Server, AccountToken, {<<"someoneElse">>, Server})),
    mod_invites:expire_invites(Server, <<"foo">>),
    ?match(false, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    mod_invites:cleanup_expired(),
    remove_user(User, Server),
    mod_invites:expire_invites(Server, <<>>),
    ?match(1, mod_invites:cleanup_expired()).

remove_user(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    Inviter = {User, Server},
    #invite_token{} = create_account_invite(Server, Inviter),
    ?match(1, length(get_invites(Server, Inviter))),
    remove_user(User, Server),
    ?match(0, length(get_invites(Server, Inviter))).

expire_tokens(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    Inviter = {User, Server},
    #invite_token{token = RosterToken} = create_roster_invite(Server, Inviter),
    #invite_token{token = AccountToken} = create_account_invite(Server, Inviter),
    ?match(true, mod_invites:is_token_valid(Server, RosterToken, Inviter)),
    ?match(1, mod_invites:expire_invites(Server, User)),
    ?match(true, mod_invites:is_token_valid(Server, RosterToken, Inviter)),
    ?match(false, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    ?match(0, mod_invites:expire_invites(Server, User)),
    mod_invites:cleanup_expired(),
    remove_user(User, Server).

is_reserved(Config) ->
    Server = ?config(server, Config),
    Inviter = {<<"inviter">>, Server},
    mod_invites:expire_invites(Server, <<"inviter">>),
    mod_invites:cleanup_expired(),
    #invite_token{token = Token} =
        mod_invites:create_account_invite(Server, Inviter, <<"reserved_user">>, false),
    ?match({error, reserved},
           mod_invites:create_account_invite(Server, Inviter, <<"reserved_user">>, false)),
    ?match(false, mod_invites:is_reserved(Server, Token, <<"some_other_username">>)),
    ?match(false, mod_invites:is_reserved(Server, Token, <<"reserved_user">>)),
    ?match(true,
           mod_invites:is_reserved(Server, <<"some_other_token">>, <<"reserved_user">>)),
    %% "use" token to create account under different name, then it should not be reserved anymore
    mod_invites:set_invitee(Server, Token, jid:make_bare(<<"some_other_username">>, Server)),
    ?match(false,
           mod_invites:is_reserved(Server, <<"some_other_token">>, <<"reserved_user">>)),
    #invite_token{token = OtherToken} =
        mod_invites:create_account_invite(Server, Inviter, <<"reserved_user">>, false),
    ?match(true, OtherToken /= Token),
    remove_user(<<"inviter">>, Server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% elp:ignore W0008 (unreachable_test)
unlift({ok, Res}) -> Res;
unlift({error, _Reason} = Error) -> Error;
unlift(Res) -> Res.

% elp:ignore W0008 (unreachable_test)
token_from_uri(Uri) ->
    {match, [Token]} =
        re:run(Uri, ".+preauth=([a-zA-z0-9]+)", [{capture, all_but_first, binary}]),
    Token.

landing_page(_, _) ->
    <<>>.

create_invite(AccountName, Host0) ->
    Host = jid:nameprep(Host0),
    case mod_invites:create_account_invite(Host, {<<>>, Host}, AccountName, false) of
        {error, _Reason} = Error ->
            Error;
        #invite_token{} = Invite ->
            {mod_invites:token_uri(Invite), landing_page(Host, Invite)}
    end.

create_account_invite(Server, Inviter) ->
    mod_invites:create_account_invite(Server, Inviter, <<>>, false).

create_roster_invite(Server, Inviter) ->
    mod_invites:create_roster_invite(Server, Inviter).

get_invites(Host, Inviter) ->
    mod_invites:transaction(Host, fun() -> mod_invites:get_invites_t(Host, Inviter) end).

remove_user(User, Server) ->
    {ok, foo} = mod_invites:remove_user(foo, #{jid => jid:make_bare(User, Server)}, #{host_type => Server}),
    ok.
