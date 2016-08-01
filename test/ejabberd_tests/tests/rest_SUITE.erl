%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(rest_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-import(rest_helper,
        [
            assert_inlist/2,
            assert_notinlist/2,
            decode_maplist/1,
            gett/1,
            post/2,
            putt/2,
            delete/1,
            gett/2,
            post/3,
            putt/3,
            delete/2
        ]
    ).

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).
-define(OK, {<<"200">>, <<"OK">>}).
-define(CREATED, {<<"201">>, <<"Created">>}).
-define(ERROR, {<<"500">>, _}).
-define(NOT_FOUND, {<<"404">>, _}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds
-define(ATOMS, [name, desc, category, action, security_policy, args, result, sender]).

all() ->
    [
        {group, adminside}
        %{group, userside}
    ].

groups() ->
    [{adminside, [], test_cases()},
     {userside, [], user_test_cases()}
    ].

test_cases() ->
    [assertions,
     comands_are_listed,
     non_existent_command_returns_404,
     user_can_be_registered_and_removed,
     sessions_are_listed,
     session_can_be_kicked
     %messages,
     %changepassword
     ].

user_test_cases() ->
    [user_messages].


suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

host() ->
    ct:get_config({hosts, mim, domain}).

init_per_suite(Config) ->
    Config1 = maybe_enable_mam(mam_backend(), Config),
    escalus:init_per_suite(Config1).

mam_backend() ->
    Funs = [fun maybe_odbc/1, fun maybe_riak/1],
    determine_backend(host(), Funs).

determine_backend(_, []) ->
    disabled;
determine_backend(Host, [F | Rest]) ->
    case F(Host) of
        false ->
            determine_backend(Host, Rest);
        Result ->
            Result
    end.

maybe_odbc(Host) ->
    case mam_helper:is_odbc_enabled(Host) of
        true ->
            odbc;
        _ ->
            false
    end.

maybe_riak(Host) ->
    case mam_helper:is_riak_enabled(Host) of
        true ->
            riak;
        _ ->
            false
    end.

maybe_enable_mam(odbc, Config) ->
    init_module(host(), mod_mam_odbc_arch, [muc, pm, simple]),
    init_module(host(), mod_mam_odbc_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    [{mam_backend, odbc} | Config];
maybe_enable_mam(riak, Config) ->
    init_module(host(), mod_mam_riak_timed_arch_yz, [pm, muc]),
    init_module(host(), mod_mam_mnesia_prefs, [pm, muc]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    [{mam_backend, riak}, {yz_wait, 2500} | Config];
maybe_enable_mam(_, C) ->
    [{mam_backend, disabled} | C].


init_module(Host, Mod, Opts) ->
    dynamic_modules:start(Host, Mod, Opts).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    maybe_disable_mam(proplists:get_value(mam_enabled, Config)),
    escalus:end_per_suite(Config).

maybe_disable_mam(odbc) ->
    stop_module(host(), mod_mam_odbc_arch),
    stop_module(host(), mod_mam_odbc_prefs),
    stop_module(host(), mod_mam_odbc_user),
    stop_module(host(), mod_mam),
    stop_module(host(), mod_mam_muc);
maybe_disable_mam(riak) ->
    stop_module(host(), mod_mam_riak_timed_arch_yz),
    stop_module(host(), mod_mam_mnesia_prefs),
    stop_module(host(), mod_mam),
    stop_module(host(), mod_mam_muc);
maybe_disable_mam(_) ->
    ok.


stop_module(Host, Mod) ->
    dynamic_modules:stop(Host, Mod).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).


end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, mike])).

init_per_testcase(CaseName, Config) ->
    MAMTestCases = [messages, user_messages],
    maybe_skip_mam_test_cases(lists:member(CaseName, MAMTestCases), CaseName, Config).

maybe_skip_mam_test_cases(false, CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config);
maybe_skip_mam_test_cases(true, CaseName, Config) ->
    case proplists:get_value(mam_backend, Config) of
        disabled ->
            {skip, mam_not_available};
        _ ->
            escalus:init_per_testcase(CaseName, Config)
    end.

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

assertions(_Config) ->
    Lst = [<<"a">>, <<"b">>, <<"c">>],
    assert_inlist(<<"a">>, Lst),
    assert_inlist(<<"b">>, Lst),
    assert_inlist(<<"c">>, Lst),
    assert_notinlist(<<"d">>, Lst),
    Maplst = [#{a => 1, b => 2}, #{a => 1, b => 3}, #{a => 4, b => 5}],
    assert_inlist(#{a => 1}, Maplst),
    assert_inlist(#{a => 1, b => 2}, Maplst),
    assert_inlist(#{a => 4}, Maplst),
    assert_notinlist(#{a => 5}, Maplst),
    assert_notinlist(#{a => 1, b => 1}, Maplst).

commands_are_listed(_C) ->
    {?OK, Lcmds} = gett(<<"/commands">>),
    DecCmds = decode_maplist(Lcmds),
    assert_inlist(#{name => <<"listmethods">>}, DecCmds).

non_existent_command_returns_404(_C) ->
    {?NOT_FOUND, _} = gett(<<"/isitthereornot">>).

user_can_be_registered_and_removed(_Config) ->
    % list users
    {?OK, Lusers} = gett(<<"/users/localhost">>),
    assert_inlist(<<"alice@localhost">>, Lusers),
    % create user
    CrUser = #{user => <<"mike">>, password => <<"nicniema">>},
    {?CREATED, _} = post(<<"/users/localhost">>, CrUser),
    {?OK, Lusers1} = gett(<<"/users/localhost">>),
    assert_inlist(<<"mike@localhost">>, Lusers1),
    % try to create the same user
    {?ERROR, _} = post(<<"/users/localhost">>, CrUser),
    % delete user
    {?OK, _} = delete(<<"/users/localhost/mike">>),
    {?OK, Lusers2} = gett(<<"/users/localhost">>),
    assert_notinlist(<<"mike@localhost">>, Lusers2),
    ok.


sessions_are_listed(_) ->
    % no session
    {?OK, Sessions} = gett("/sessions/localhost"),
    [] = Sessions.

session_can_be_kicked(Config) ->
    escalus:story(Config, [{alice, 1}], fun(_Alice) ->
            % Alice is connected
            {?OK, Sessions1} = gett("/sessions/localhost"),
            assert_inlist(<<"alice@localhost/res1">>, Sessions1),
            % kick alice
            {?OK, _} = delete("/sessions/localhost/alice/res1"),
            {?OK, Sessions2} = gett("/sessions/localhost"),
            [] = Sessions2
        end),
    ok.


messages(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        M = #{caller => BobJID, to => AliceJID, msg => <<"hello from Bob">>},
        {?OK, _} = post(<<"/message">>, M),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, [<<"hello from Bob">>], Res),
        M1 = #{caller => AliceJID, to => BobJID, msg => <<"hello from Alice">>},
        {?OK, _} = post(<<"/message">>, M1),
        Res1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [<<"hello from Alice">>], Res1),
        GetPath = lists:flatten(["/message/caller/",binary_to_list(AliceJID),
                                 "/other/",binary_to_list(BobJID),"/limit/10"]),
        mam_helper:maybe_wait_for_yz(Config),
        {?OK, Msgs} = gett(GetPath),
        ?PRT("Msgs", Msgs),
        [Last, Previous|_] = lists:reverse(decode_maplist(Msgs)),
        ?PRT("Last", Last),
        ?PRT("Previous", Previous),
        <<"hello from Alice">> = maps:get(body, Last),
        AliceJID = maps:get(sender, Last),
        <<"hello from Bob">> = maps:get(body, Previous),
        BobJID = maps:get(sender, Previous)
    end).


changepassword(Config) ->
    % bob logs in with his regular password
    escalus:story(Config, [{bob, 1}], fun(_Bob) ->
            skip
        end),
    % we change password
    {?OK, _} = putt("/user/caller/bob@localhost", #{newpass => <<"niemakrolika">>}),
    % he logs with his alternative password
    escalus:story(Config, [{bob_altpass, 1}], fun(_Bob) ->
            ignore
        end),
    % we can't log with regular passwd anymore
    try escalus:story(Config, [{bob, 1}], fun(Bob) -> ?PRT("Bob", Bob) end) of
        _ -> ct:fail("this shouldn't have worked")
    catch error:{badmatch, _} ->
        ok
    end,
    % we change it back
    {?OK, _} = putt("/user/caller/bob@localhost", #{newpass => <<"makrolika">>}),
    % now he logs again with the regular one
    escalus:story(Config, [{bob, 1}], fun(_Bob) ->
            just_dont_do_anything
        end),
    ok.

to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_list(V) ->
    V.
