-module(gdpr_SUITE).

%% Tests for features related to GDPR compliance.

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
         retrieve_vcard/1,
         retrieve_roster/1,
         retrieve_mam/1,
         retrieve_offline/1,
         retrieve_pubsub/1,
         retrieve_pep/1,
         retrieve_private_xml/1,
         retrieve_inbox/1,
         retrieve_logs/1
        ]).
-export([
         data_is_not_retrieved_for_missing_user/1
        ]).

-import(ejabberdctl_helper, [ejabberdctl/3]).

%% -------------------------------------------------------------
%% Common Test stuff
%% -------------------------------------------------------------

suite() ->
    escalus:suite().

all() ->
    [
     {group, retrieve_personal_data}
    ].

groups() ->
    [
     {retrieve_personal_data, [parallel], [
                                   % per type
                                   retrieve_vcard,
                                   retrieve_roster,
                                   retrieve_mam,
                                   retrieve_offline,
                                   retrieve_pubsub,
                                   retrieve_pep,
                                   retrieve_private_xml,
                                   retrieve_inbox,
                                   retrieve_logs,
                                   % negative
                                   data_is_not_retrieved_for_missing_user
                                  ]}
    ].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GN, Config) ->
    Config.

end_per_group(_GN, Config) ->
    Config.

init_per_testcase(CN, Config) ->
    escalus:init_per_testcase(CN, Config).

end_per_testcase(CN, Config) ->
    escalus:end_per_testcase(CN, Config).

%% -------------------------------------------------------------
%% Test cases
%% -------------------------------------------------------------

%% ------------------------- Data retrieval - per type verification -------------------------

retrieve_vcard(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            case vcard_update:is_vcard_ldap() of
                true ->
                    {skip, skipped_for_simplicity_for_now}; % TODO: Fix the case for LDAP as well
                _ ->
                    AliceFields = [{<<"FN">>, <<"Alice">>},
                                   {<<"LN">>, <<"Ecila">>}],
                    AliceSetResultStanza
                    = escalus:send_and_wait(Alice,
                                            escalus_stanza:vcard_update(AliceFields)),
                    escalus:assert(is_iq_result, AliceSetResultStanza),
                    ExpectedHeader = ["vcard"], % TODO? Expand vCard into separate CSV columns?
                    ExpectedItems = [
                                     #{ "vcard" => [{contains, "Alice"},
                                                    {contains, "Ecila"}] }
                                    ],
                    retrieve_and_validate_personal_data(
                      Alice, Config, "vcard", ExpectedHeader, ExpectedItems)
            end
        end).

retrieve_roster(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            escalus_story:make_all_clients_friends([Alice, Bob]),
            ExpectedHeader = ["jid", "name", "groups"], % TODO
            ExpectedItems = [
                             #{ "jid" => [binary_to_list(escalus_client:short_jid(Bob))] }
                            ],
            retrieve_and_validate_personal_data(
              Alice, Config, "roster", ExpectedHeader, ExpectedItems)
        end).

retrieve_mam(Config) ->
    ok.

retrieve_offline(Config) ->
    ok.

retrieve_pubsub(Config) ->
    ok.

retrieve_pep(Config) ->
    ok.

retrieve_private_xml(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            NS = <<"alice:gdpr:ns">>,
            Content = <<"dGhlcmUgYmUgZHJhZ29ucw==">>,
            XML = #xmlel{ name = <<"fingerprint">>,
                          attrs = [{<<"xmlns">>, NS}],
                          children = [#xmlcdata{ content = Content }]},
            PrivateStanza = escalus_stanza:private_set(XML),
            escalus_client:send(Alice, PrivateStanza),
            escalus:assert(is_iq_result, [PrivateStanza], escalus_client:wait_for_stanza(Alice)),
            ExpectedHeader = ["ns", "xml"], % TODO?
            ExpectedItems = [
                             #{ "xml" => [{contains, "alice:gdpr:ns"},
                                          {contains, binary_to_list(Content)}] }
                            ],
            retrieve_and_validate_personal_data(
              Alice, Config, "private", ExpectedHeader, ExpectedItems)
        end).

retrieve_inbox(Config) ->
    ok.

retrieve_logs(Config) ->
    mongoose_helper:successful_rpc(error_logger, error_msg,
                                   ["event=disturbance_in_the_force, jid=sith@localhost", []]),
    Dir = request_and_unzip_personal_data(<<"sith">>, <<"localhost">>, Config),
    Filename = filename:join(Dir, "logs.txt"),
    {ok, Content} = file:read_file(Filename),
    {match, _} = re:run(Content, "disturbance_in_the_force").

%% ------------------------- Data retrieval - Negative case -------------------------

data_is_not_retrieved_for_missing_user(Config) ->
    {Filename, 1} = retrieve_personal_data("non-person", "oblivion", Config),
    {error, _} = file:read_file_info(Filename).

%% -------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------

retrieve_and_validate_personal_data(Alice, Config, FilePrefix, ExpectedHeader, ExpectedItems) ->
    PersonalCSV = retrieve_and_decode_personal_data(Alice, Config, FilePrefix),
    PersonalMaps = csv_to_maps(ExpectedHeader, PersonalCSV),
    try validate_personal_maps(PersonalMaps, ExpectedItems) of
        _ -> ok
    catch
        C:R ->
            ct:fail(#{
              class => C,
              reason => R,
              stacktrace => erlang:get_stacktrace(),
              personal_maps => PersonalMaps,
              expected_items => ExpectedItems
             })
    end.

csv_to_maps(ExpectedHeader, [ExpectedHeader | Rows]) ->
    lists:foldl(fun(Row, Maps) -> [ csv_row_to_map(ExpectedHeader, Row) | Maps ] end, [], Rows).

csv_row_to_map(Header, Row) ->
    maps:from_list(lists:zip(Header, Row)).

validate_personal_maps([Map | RMaps], [Checks | RChecks]) ->
    maps:fold(fun(K, Conditions, _) ->
                      validate_personal_item(maps:get(K, Map), Conditions)
              end, ok, Checks),
    validate_personal_maps(RMaps, RChecks).

validate_personal_item(_Value, []) ->
    ok;
validate_personal_item(Value, [{contains, String} | RConditions]) ->
    {match, _} = re:run(Value, String),
    validate_personal_item(Value, RConditions);
validate_personal_item(ExactValue, [ExactValue | _]) ->
    ok.

retrieve_and_decode_personal_data(Client, Config, FilePrefix) ->
    User = escalus_client:username(Client),
    Domain = escalus_client:server(Client),
    Dir = request_and_unzip_personal_data(User, Domain, Config),
    CSVPath = filename:join(Dir, FilePrefix ++ ".csv"),
    {ok, Content} = file:read_file(CSVPath),
    % We expect non-empty list because it must contain at least header with columns names
    [_ | _] = csv:decode_binary(Content).

request_and_unzip_personal_data(User, Domain, Config) ->
    {Filename, 0} = retrieve_personal_data(User, Domain, Config),
    Dir = Filename ++ ".unzipped",
    {ok, _} = zip:extract(Filename, [{cwd, Dir}]),
    Dir.

retrieve_personal_data(User, Domain, Config) ->
    Filename = random_filename(Config),
    {_, Code} = ejabberdctl("retrieve_personal_data", [User, Domain, Filename], Config),
    {Filename, Code}.

random_filename(Config) ->
    TCName = atom_to_list(?config(tc_name, Config)),
    "/tmp/" ++ TCName ++ "." ++ integer_to_list(erlang:system_time()) ++ ".zip".

