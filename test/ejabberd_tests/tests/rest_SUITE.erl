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
            delete/1
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
-define(ATOMS, [name, desc, category, action, security_policy, args, result]).

all() ->
    [{group, restapi}].

groups() ->
    [{restapi, [sequence], test_cases()}
    ].

test_cases() ->
    [assertions,
     basic,
     sessions].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice])).


end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice])).

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
    CrUser = #{user => <<"bbb">>, password => <<"bbb">>, host => <<"localhost">>},
    {?CREATED, _} = post(<<"/user/host/localhost">>, CrUser),
    {?OK, Lusers1} = gett(<<"/user/host/localhost">>),
    assert_inlist(<<"bbb@localhost">>, Lusers1),
    % try to create the same user
    {?ERROR, _} = post(<<"/user/host/localhost">>, CrUser),
    % delete user
    {?OK, _} = delete(<<"/user/jid/bbb@localhost">>),
    {?OK, Lusers2} = gett(<<"/user/host/localhost">>),
    assert_notinlist(<<"bbb@localhost">>, Lusers2),
    ok.


sessions(Config) ->
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





