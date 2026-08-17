%% End-to-end tests for the HTTP auth backend, mocked by `mim_ct_rest' (copied
%% into big_tests/src by the Makefile `prepare' target). Unlike auth_http_SUITE,
%% this checks the client gets a SASL failure rather than a dropped connection.

-module(auth_http_big_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [host_type/0]).
-import(config_parser_helper, [config/2]).

-define(BASIC_AUTH, "softkitty:purrpurrpurr"). % enforced by mim_ct_rest_handler
-define(AUTH_HOST, "http://localhost:12000"). % port hardcoded in mim_ct_rest:init/1

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, plain},
     {group, scram}].

groups() ->
    [{plain, [], tests()},
     {scram, [], tests()}].

tests() ->
    [login_succeeds_when_auth_service_is_up,
     unexpected_codes_are_rejected_cleanly].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    mim_ct_rest:start(?BASIC_AUTH, Config1),
    start_auth_pool(),
    Config1.

end_per_suite(Config) ->
    rpc(mim(), mongoose_wpool, stop, [http, global, auth]),
    mim_ct_rest:stop(),
    escalus:end_per_suite(Config).

start_auth_pool() ->
    %% ejabberd_auth_http requests relative paths and looks up the `auth' tag
    Pool = config([outgoing_pools, http, auth],
                  #{conn_opts => #{host => ?AUTH_HOST, path_prefix => <<"/auth/">>}}),
    [{ok, _Pid}] = rpc(mim(), mongoose_wpool, start_configured_pools, [[Pool]]).

init_per_group(plain, Config) ->
    init_group(Config, plain, []);
init_per_group(scram, Config) ->
    init_group(Config, {scram, [sha]}, [{escalus_auth_method, <<"SCRAM-SHA-1">>}]).

end_per_group(_Group, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice])),
    mongoose_helper:restore_config(Config).

init_group(Config0, PasswordFormat, EscalusOpts) ->
    AuthOpts = mongoose_helper:auth_opts_with_password_format(PasswordFormat),
    Config1 = mongoose_helper:backup_and_set_config_option(
                Config0, {auth, host_type()},
                AuthOpts#{methods => [http], http => #{basic_auth => ?BASIC_AUTH}}),
    EscalusOpts ++ escalus:create_users(Config1, escalus:get_users([alice])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

%% Without this the case below would pass even with a broken pool or mock
login_succeeds_when_auth_service_is_up(Config) ->
    {ok, Client} = escalus_client:start(Config, alice, <<"res">>),
    escalus_client:stop(Config, Client).

unexpected_codes_are_rejected_cleanly(Config) ->
    lists:foreach(fun(Code) -> unexpected_code_is_rejected_cleanly(Config, Code) end,
                  [500, 502, 503, 504, 429]).

unexpected_code_is_rejected_cleanly(Config, Code) ->
    mim_ct_rest:fail(Code), % one-shot, consumed by the next request
    {error, {connection_step_failed, _, Reason}} =
        escalus_client:start(Config, alice, <<"res">>),
    assert_sasl_failure(Code, Reason).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% PLAIN reports it where it expects the result, SCRAM where it expects the
%% challenge; anything else means no failure was sent at all
assert_sasl_failure(_Code, {Step, _, #xmlel{name = <<"failure">>} = El})
  when Step =:= auth_failed; Step =:= expected_challenge ->
    ?assertMatch(#xmlel{}, exml_query:subelement(El, <<"not-authorized">>));
assert_sasl_failure(Code, Other) ->
    ct:fail({no_sasl_failure_from_server, [{http_code, Code}, {got, Other}]}).
