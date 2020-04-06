%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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

-module(sasl_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(TEST_MECHANISM, <<"TEST-MECHANISM">>).
-define(TEXT_CONTENT, <<"Call 555-123-1234 for assistance">>).

-behaviour(cyrsasl).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, domain_config},
     {group, node_config}].

groups() ->
    G = [{domain_config, [sequence], all_tests()},
         {node_config, [sequence], all_tests()}],
    ct_helper:repeat_all_until_all_ok(G).

all_tests() ->
    [text_response].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    Config1 = set_sasl_mechanisms(mech_option_key(GroupName), Config),
    escalus:create_users(Config1, escalus:get_users([alice])).

end_per_group(GroupName, Config) ->
    reset_sasl_mechanisms(mech_option_key(GroupName), Config),
    escalus:delete_users(Config, escalus:get_users([alice])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% cyrsasl tests
%%--------------------------------------------------------------------

text_response(Config) ->
    mongoose_helper:inject_module(?MODULE),
    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Client, _} = escalus_connection:start(AliceSpec, [start_stream, stream_features]),
    Stanza = escalus_stanza:auth(?TEST_MECHANISM,
                                 [#xmlcdata{content = base64:encode(<<"alice">>)}]),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_failure_with_text(Result).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

mech_option_key(domain_config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    {sasl_mechanisms, Domain};
mech_option_key(node_config) ->
    sasl_mechanisms.

set_sasl_mechanisms(Key, Config) ->
    %% pretend that an auth module is set for this mechanism
    rpc(mim(), meck, new, [ejabberd_auth, [no_link, passthrough]]),
    rpc(mim(), meck, expect, [ejabberd_auth, supports_sasl_module,
                              fun(_, M) -> M =:= ?MODULE end]),

    %% configure the mechanism
    Mechs = rpc(mim(), ejabberd_config, get_local_option, [Key]),
    rpc(mim(), ejabberd_config, add_local_option, [Key, [?MODULE]]),
    [{mechs, Mechs} | Config].

reset_sasl_mechanisms(Key, Config) ->
    case ?config(mechs, Config) of
        undefined -> rpc(mim(), ejabberd_config, del_local_option, [Key]);
        Mechs -> rpc(mim(), ejabberd_config, add_local_option, [Key, Mechs])
    end,
    rpc(mim(), meck, unload, [ejabberd_auth]).

assert_is_failure_with_text(#xmlel{name = <<"failure">>,
                                   children = Children}) ->
    assert_has_text(Children);
assert_is_failure_with_text(Result) ->
    ct:fail("Result is not a failure stanza: ~p", [Result]).

assert_has_text([]) ->
    ct:fail("Result has no or incorrect text field");
assert_has_text([#xmlel{name = <<"text">>,
                        children = [#xmlcdata{content = ?TEXT_CONTENT}]}
                 | _]) ->
    ok;
assert_has_text([_ | Tail]) ->
    assert_has_text(Tail).

%%--------------------------------------------------------------------
%% cyrsasl test callback functions
%%--------------------------------------------------------------------

mechanism() ->
    ?TEST_MECHANISM.

mech_new(_Host, _Creds) ->
    {ok, state}.

mech_step(_State, _ClientIn) ->
    {error, {<<"not-authorized">>, ?TEXT_CONTENT}, <<>>}.
