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
         data_is_retrieved_for_existing_user/1,
         data_is_not_retrieved_for_missing_user/1,
         data_is_in_csv_and_zipped/1
        ]).
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
                                   % general
                                   data_is_retrieved_for_existing_user,
                                   data_is_not_retrieved_for_missing_user,
                                   data_is_in_csv_and_zipped,
                                   % per type
                                   retrieve_vcard,
                                   retrieve_roster,
                                   retrieve_mam,
                                   retrieve_offline,
                                   retrieve_pubsub,
                                   retrieve_pep,
                                   retrieve_private_xml,
                                   retrieve_inbox,
                                   retrieve_logs
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

%% ------------------------- Data retrieval - Generic verification -------------------------

data_is_retrieved_for_existing_user(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            {Filename, 0} = retrieve_personal_data(Alice, Config),
            {ok, _} = file:read_file_info(Filename)
        end).

data_is_not_retrieved_for_missing_user(Config) ->
    {Filename, 1} = retrieve_personal_data("non-person", "oblivion", Config),
    {error, _} = file:read_file_info(Filename).

data_is_in_csv_and_zipped(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            {Filename, 0} = retrieve_personal_data(Alice, Config),
            Dir = Filename ++ ".unzipped",
            {ok, FileList} = zip:extract(Filename, [{cwd, Dir}]),
            lists:foreach(
              fun(FileName) ->
                      {ok, Content} = file:read_file(FileName),
                      case csv:decode_binary(Content) of
                          Parsed when is_list(Parsed) -> ok
                      end
              end, FileList)
        end).

%% ------------------------- Data retrieval - per type verification -------------------------

retrieve_vcard(Config) ->
   ok.

retrieve_roster(Config) ->
    ok.

retrieve_mam(Config) ->
    ok.

retrieve_offline(Config) ->
    ok.

retrieve_pubsub(Config) ->
    ok.

retrieve_pep(Config) ->
    ok.

retrieve_private_xml(Config) ->
    ok.

retrieve_inbox(Config) ->
    ok.

retrieve_logs(Config) ->
    ok.

%% -------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------

retrieve_personal_data(Client, Config) ->
    User = escalus_client:username(Client),
    Domain = escalus_client:server(Client),
    retrieve_personal_data(User, Domain, Config).

retrieve_personal_data(User, Domain, Config) ->
    Filename = random_filename(Config),
    {_, Code} = ejabberdctl("retrieve_personal_data", [User, Domain, Filename], Config),
    {Filename, Code}.

random_filename(Config) ->
    TCName = atom_to_list(?config(tc_name, Config)),
    "/tmp/" ++ TCName ++ "." ++ integer_to_list(erlang:system_time()) ++ ".zip".

