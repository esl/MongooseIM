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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, text_response}].

groups() ->
    [{text_response, [sequence], all_tests()}].

all_tests() ->
    [text_response].

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
%% cyrsasl tests
%%--------------------------------------------------------------------

text_response(Config) ->
    {Mod, Bin, File} = code:get_object_code(?MODULE),
    escalus_ejabberd:rpc(code, load_binary, [Mod, File, Bin]),
    escalus_ejabberd:rpc(
      cyrsasl, register_mechanism, [?TEST_MECHANISM, ?MODULE, plain]),

    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Client, _, _} = escalus_connection:start(AliceSpec,
                                                  [start_stream,
                                                   stream_features]),
    Stanza = escalus_stanza:auth(?TEST_MECHANISM,
                                 [#xmlcdata{content =
                                            base64:encode(<<"alice">>)}]),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_failure_with_text(Result).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

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

mech_new(_Host, _Creds) ->
    {ok, state}.

mech_step(_State, _ClientIn) ->
    {error, {<<"not-authorized">>, ?TEXT_CONTENT}, <<>>}.
