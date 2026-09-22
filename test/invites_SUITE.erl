-module(invites_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("jlib.hrl").
-include("mod_invites.hrl").

-define(match(Guard, Expr), ?assertMatch(Guard, Expr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
        {group, basic}
    ].

groups() ->
    [
        {basic, [parallel], [
                             gen_invite
        ]}
    ].

init_per_suite(C) ->
    application:ensure_all_started(jid),
    [{server, <<"localhost">>},
     {user, <<"test">>}
    | C].

end_per_suite(C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_invite(Config) ->
    %_ = mod_invites:cleanup_expired(), %% clean from old runs
    ct:pal("config: ~p", [Config]),
    Server = ?config(server, Config),
    User = ?config(user, Config),
    {TokenURI, _LandingPage} = Res = gen_invite(<<"foo">>, Server),
    ct:pal("result: ~p", [Res]),
    ?match(<<"xmpp:foo@", Server:(size(Server))/binary, "?register;preauth=", _/binary>>,
           TokenURI),
    Token = token_from_uri(TokenURI),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = <<"foo">>} =
        mod_invites:get_invite(Server, Token),
    {TokenURI2, _LP2} = gen_invite(Server),
    ?match(<<"xmpp:", _/binary>>, TokenURI2),
    Token2 = token_from_uri(TokenURI2),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = <<>>} =
        mod_invites:get_invite(Server, Token2),
    ?match({error, user_exists}, gen_invite(User, Server)),
    ?match({error, account_name_invalid},
           gen_invite(<<"@bad_acccount_name">>, Server)),
    ?match({error, host_unknown}, gen_invite(<<"bar">>, <<"non.existant.host">>)),

    ?match(2, length(mod_invites:list_invites(Server))),
    %% TooLongHostname = list_to_binary([$a || _ <- lists:seq(1, 1024)]),
    %% ?match({error, hostname_invalid}, mod_invites:gen_invite(<<"foo">>, TooLongHostname)),
    mod_invites:expire_invites(<<>>, Server),
    %%?match(2, mod_invites:cleanup_expired()),
    ok.

token_from_uri(Uri) ->
    {match, [Token]} =
        re:run(Uri, ".+preauth=([a-zA-z0-9]+)", [{capture, all_but_first, binary}]),
    Token.

landing_page(_, _) ->
    <<>>.

gen_invite(AccountName, Host0) ->
    Host = jid:nameprep(Host0),
    case mod_invites:create_account_invite(Host, {<<>>, Host}, AccountName, false) of
        {error, _Reason} = Error ->
            Error;
        Invite ->
            {token_from_uri(Invite), landing_page(Host, Invite)}
    end.
