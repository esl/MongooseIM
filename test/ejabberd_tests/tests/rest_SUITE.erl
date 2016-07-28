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
        {group, adminside},
        {group, userside}
    ].

groups() ->
    [{adminside, [], test_cases()},
     {userside, [], user_test_cases()}
    ].

test_cases() ->
    [assertions,
     basic,
     sessions,
     messages,
     changepassword].

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
    init_module(host(), mod_mam_odbc_arch, [muc, pm, simple]),
    init_module(host(), mod_mam_odbc_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    escalus:init_per_suite(Config).

init_module(Host, Mod, Opts) ->
    dynamic_modules:start(Host, Mod, Opts).

end_per_suite(Config) ->
    stop_module(host(), mod_mam_odbc_arch),
    stop_module(host(), mod_mam_odbc_prefs),
    stop_module(host(), mod_mam_odbc_user),
    stop_module(host(), mod_mam),
    stop_module(host(), mod_mam_muc),
    escalus:end_per_suite(Config).

stop_module(Host, Mod) ->
    dynamic_modules:stop(Host, Mod).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])),
    escalus:delete_users(Config, escalus:get_users([mike])).


end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([bob, mike])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

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


basic(_Config) ->
    % list commands
    {?OK, Lcmds} = gett(<<"/list">>),
    DecCmds = decode_maplist(Lcmds),
    assert_inlist(#{name => <<"listmethods">>}, DecCmds),
    % nonexistent command
    {?NOT_FOUND, _} = gett(<<"/isitthereornot">>),
    % list users
    {?OK, Lusers} = gett(<<"/user/host/localhost">>),
    assert_inlist(<<"alice@localhost">>, Lusers),
    % create user
    CrUser = #{user => <<"mike">>, password => <<"nicniema">>},
    {?CREATED, _} = post(<<"/user/host/localhost">>, CrUser),
    {?OK, Lusers1} = gett(<<"/user/host/localhost">>),
    assert_inlist(<<"mike@localhost">>, Lusers1),
    % try to create the same user
    {?ERROR, _} = post(<<"/user/host/localhost">>, CrUser),
    % delete user
    {?OK, _} = delete(<<"/user/jid/mike@localhost">>),
    {?OK, Lusers2} = gett(<<"/user/host/localhost">>),
    assert_notinlist(<<"mike@localhost">>, Lusers2),
    ok.


sessions(Config) ->
    % no session
    {?OK, Sessions} = gett("/session/host/localhost"),
    [] = Sessions,
    escalus:story(Config, [{alice, 1}], fun(_Alice) ->
            % Alice is connected
            {?OK, Sessions1} = gett("/session/host/localhost"),
            assert_inlist(<<"alice@localhost/res1">>, Sessions1),
            % kick alice
            {?OK, _} = delete("/session/jid/alice@localhost%2Fres1"),
            {?OK, Sessions2} = gett("/session/host/localhost"),
            [] = Sessions2
        end),
    ok.


messages(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        M = #{caller => <<"bob@localhost">>, to => <<"alice@localhost">>, msg => <<"hello from Bob">>},
        {?OK, _} = post(<<"/message">>, M),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, [<<"hello from Bob">>], Res),
        M1 = #{caller => <<"alice@localhost">>, to => <<"bob@localhost">>, msg => <<"hello from Alice">>},
        {?OK, _} = post(<<"/message">>, M1),
        Res1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [<<"hello from Alice">>], Res1),
        {?OK, Msgs} = gett("/message/caller/alice@localhost/other/bob@localhost/limit/10"),
        ?PRT("Msgs", Msgs),
        [Last, Previous|_] = lists:reverse(decode_maplist(Msgs)),
        ?PRT("Last", Last),
        ?PRT("Previous", Previous),
        <<"hello from Alice">> = maps:get(body, Last),
        <<"alice@localhost">> = maps:get(sender, Last),
        <<"hello from Bob">> = maps:get(body, Previous),
        <<"bob@localhost">> = maps:get(sender, Previous)
                                        end),
    ok.


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


user_messages(_Config) ->
    % send message using http authorisation
    Cred = {<<"alice@localhost">>, <<"matygrysa">>},
    M1 = #{to => <<"bob@localhost">>, msg => <<"hello from Alice">>},
    {?OK, _} = post(<<"/message">>, M1, Cred),
    % list messages the same way - this one should be at the end
    {?OK, Msgs} = gett("/message/other/bob@localhost/limit/10", Cred),
    ?PRT("Msgs", Msgs),
    [Last|_] = lists:reverse(decode_maplist(Msgs)),
    <<"hello from Alice">> = maps:get(body, Last),
    <<"alice@localhost">> = maps:get(sender, Last),
    ok.


to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_list(V) ->
    V.
