-module(mod_version_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_version}].

groups() ->
    [{mod_version, [], [version_service_discovery, ask_for_version]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    dynamic_modules:start(<<"localhost">>, mod_version, []),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    dynamic_modules:stop(<<"localhost">>, mod_version),
    escalus:end_per_suite(Config).

init_per_group(mod_version, Config) ->
    escalus:create_users(Config, escalus:get_users([bob])).

end_per_group(mod_version, Config) ->
    escalus:delete_users(Config, escalus:get_users([bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

version_service_discovery(Config) ->
    escalus:story(Config, [{bob, 1}],
        fun(Bob) ->
            ServJID = escalus_client:server(Bob),
            Result = escalus:send_and_wait(Bob,
                                           escalus_stanza:disco_info(ServJID)),
            escalus:assert(is_iq_result, Result),
            escalus:assert(has_feature, [?NS_SOFT_VERSION], Result)
        end).

ask_for_version(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        Server = escalus_users:get_server(Config, alice),
        ID = escalus_stanza:id(),
        SoftStanza = soft_version_stanza(Server, ID),
        escalus_client:send(Bob, SoftStanza),
        Reply = escalus:wait_for_stanza(Bob, 5000),
        escalus:assert(is_iq_result, Reply),
        escalus:assert(fun check_namespace/1, Reply),
        escalus:assert(fun check_name_and_version_presence/1, Reply)
    end).

soft_version_stanza(Server, ID) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"get">>},
                    {<<"to">>, Server},
                    {<<"id">>, ID}],
           children = [#xmlel{name = <<"query">>,
                              attrs = [{<<"xmlns">>, ?NS_SOFT_VERSION}]}]}.

check_namespace(#xmlel{name = <<"iq">>, attrs = _, children = [Child]}) ->
    case Child of
        #xmlel{name = <<"query">>,
               attrs = [{<<"xmlns">>, ?NS_SOFT_VERSION}],
               children = _}
            -> true;
        _   -> false
    end;

check_namespace(_) -> false.

check_name_and_version_presence(#xmlel{name = <<"iq">>, attrs = _, children = Children}) ->
    case Children of
        [#xmlel{name = <<"name">>, attrs = [], children = [#xmlcdata{content = _}]},
         #xmlel{name = <<"version">>, attrs = [], children = [#xmlcdata{content = _}]}]
            -> true;
        _   -> false
    end;

check_name_and_version_presence(_) -> false.
