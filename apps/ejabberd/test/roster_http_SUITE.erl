%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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

-module(roster_http_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(HOST_NAME, <<"localhost">>).
-define(PORT_NUMBER, <<"12000">>).
-define(ROSTER_PATH, <<"/roster/">>).

-define(DOMAIN, "localhost").
-define(ALICE, <<"alice">>).
-define(DOMAIN2, <<"localhost2">>).
-define(ROSTER_HOST, "http://localhost:12000").
-define(BASIC_AUTH, "softkitty:purrpurrpurr").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, roster}].

groups() ->
    [{roster, [sequence], all_tests()}].

all_tests() ->
    [get_roster].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    application:start(p1_stringprep),
    meck_config(),
    mim_ct_rest:start(?BASIC_AUTH, Config),
    % Separate process needs to do this, because this one will terminate
    % so will supervisor and children and ETS tables
    mim_ct_rest:do(fun() ->
                           mim_ct_sup:start_link(ejabberd_sup),
                           mod_roster_http:init(?ROSTER_HOST, [])
                   end),
    meck_cleanup(),
    Config.

end_per_suite(Config) ->
    exit(whereis(ejabberd_sup), kill),
    Config.

init_per_group(_GroupName, Config) ->
    %% Config2 = lists:keystore(scram_group, 1, Config,
    %%                          {scram_group, GroupName == auth_requests_scram}),
    %% meck_config(Config2),
    %% mim_ct_rest:register(<<"alice">>, ?DOMAIN1, do_scram(<<"makota">>, Config2)),
    %% mim_ct_rest:register(<<"bob">>, ?DOMAIN1, do_scram(<<"niema5klepki">>, Config2)),
    %% meck_cleanup(),
    %% Config2.
    Config.

end_per_group(_GroupName, Config) ->
    %% mim_ct_rest:remove_user(<<"alice">>, ?DOMAIN1),
    %% mim_ct_rest:remove_user(<<"bob">>, ?DOMAIN1),
    Config.

%% init_per_testcase(remove_user, Config) ->
%%     meck_config(Config),
%%     mim_ct_rest:register(<<"toremove1">>, ?DOMAIN1, do_scram(<<"pass">>, Config)),
%%     mim_ct_rest:register(<<"toremove2">>, ?DOMAIN1, do_scram(<<"pass">>, Config)),
%%     Config;
init_per_testcase(_CaseName, Config) ->
    meck_config(),
    Config.

%% end_per_testcase(try_register, Config) ->
%%     %% mim_ct_rest:remove_user(<<"nonexistent">>, ?DOMAIN1),
%%     %% meck_cleanup(),
%%     Config;
%% end_per_testcase(remove_user, Config) ->
%%     %% mim_ct_rest:remove_user(<<"toremove1">>, ?DOMAIN1),
%%     %% mim_ct_rest:remove_user(<<"toremove2">>, ?DOMAIN1),
%%     %% meck_cleanup(),
%%     Config;
end_per_testcase(_CaseName, Config) ->
    meck_cleanup(),
    Config.

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

get_roster(_Config) ->
    [] = mod_roster_http:get_roster(?ALICE, ?DOMAIN).

%%--------------------------------------------------------------------
%% Helpers (mocking)
%%--------------------------------------------------------------------

meck_config() ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
                fun(roster_opts, _Host) ->
                        [{host, ?HOST_NAME},
			 {port, ?PORT_NUMBER},
                         {path_prefix, ?ROSTER_PATH}]
                end).

meck_cleanup() ->
    meck:validate(ejabberd_config),
    meck:unload(ejabberd_config).
