-module(mod_version_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

all() ->
    [{group, mod_version}].

groups() ->
    [{mod_version, [], [ask_for_version, version_service_discovery]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    dynamic_modules:start(<<"localhost">>, mod_version, []),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    dynamic_modules:stop(<<"localhost">>, mod_version),
    escalus:end_per_suite(Config).

init_per_group(mod_version, Config) ->
    escalus:create_users(Config, escalus:get_users([alice])).

end_per_group(mod_version, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice])).


init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

ask_for_version(Config) ->
    escalus:story(Config, [{alice, 1}],
        fun(Alice) ->
            Server = escalus_users:get_server(Config, alice),
            ID = escalus_stanza:id(),
            VersionStanza = version_request_stanza(Server, ID),
            escalus_client:send(Alice, VersionStanza),
            Reply = escalus:wait_for_stanza(Alice, 5000),
            escalus:assert(is_iq_result, Reply),
            escalus:assert(fun check_ns/1, Reply),
            {Name, Version, Os} = data_from_stanza(Reply),
            ?assertEqual(true, name_regex(Name)),
            ?assertEqual(true, version_regex(Version)),
            ?assertEqual(true, os_regex(Os))
        end).

version_service_discovery(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Client) ->
            ServerJID = escalus_client:server(Client),
            Res = escalus:send_and_wait(Client,
                escalus_stanza:disco_info(ServerJID)),
            escalus:assert(is_iq_result, Res),
            escalus:assert(has_feature, [?NS_SOFT_VERSION], Res)
        end).

%%--------------------
%% Helpers
%%--------------------
version_request_stanza(Server, ID) ->
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"get">>},
            {<<"id">>, ID}, {<<"to">>, Server}],
        children = #xmlel{name = <<"query">>,
            attrs = [{<<"xmlns">>, ?NS_SOFT_VERSION}]}}.

check_ns(#xmlel{name = <<"iq">>, attrs = _, children = [Child]}) ->
    case Child of
        #xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_SOFT_VERSION}], children = _} -> true;
        _ -> false
    end;
check_ns(_) ->
    false.

data_from_stanza(#xmlel{name = <<"iq">>, attrs = _, children = [Child]}) ->
    case Child of
        #xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_SOFT_VERSION}], children = Data} ->
            case Data of
                [#xmlel{name = <<"name">>, attrs = _, children = [#xmlcdata{content = Name}]},
                    #xmlel{name = <<"version">>, attrs = _, children = [#xmlcdata{content = Version}]},
                    #xmlel{name = <<"os">>, attrs = _, children = [#xmlcdata{content = Os}]}] ->
                    {Name, Version, Os};
                _ -> no_data
            end;
        _ -> wrong_stanza
    end.

name_regex(Name) ->
    String = binary_to_list(Name),
    case String of
        "mongooseim" -> true;
        _ -> false
    end .

version_regex(Version) ->
    String = binary_to_list(Version),
    case re:run(String, "([0-9]+\.)+[0-9]+") of
        {match, _} -> true;
        {_, _} -> false
    end.

os_regex(Os) ->
    String = binary_to_list(Os),
    case String of
        "" -> false;
        _ -> true
    end.
