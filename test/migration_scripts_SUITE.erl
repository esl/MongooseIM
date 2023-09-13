-module(migration_scripts_SUITE).

% CT callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
% sender-jid-from-mam-message.escript test cases
-export([
         sender_jid_from_mam_muc_eterm_stream/1,
         sender_jid_from_mam_muc_xml_stream/1,
         sender_jid_from_mam_muc_doesnt_crash_on_unsupported_eterm_input/1,
         sender_jid_from_mam_muc_doesnt_crash_on_unsupported_xml_input/1,
         sender_jid_from_mam_muc_doesnt_crash_on_malformed_eterm_input/1,
         sender_jid_from_mam_muc_doesnt_crash_on_malformed_xml_input/1
        ]).

%% ----------------------------------------------------------
%% CT callbacks
%% ----------------------------------------------------------

all() ->
    [
     {group, sender_jid_from_mam_message}
    ].

groups() ->
    [
     {sender_jid_from_mam_message, [parallel], [
                                                sender_jid_from_mam_muc_eterm_stream,
                                                sender_jid_from_mam_muc_xml_stream,
                                                sender_jid_from_mam_muc_doesnt_crash_on_unsupported_eterm_input,
                                                sender_jid_from_mam_muc_doesnt_crash_on_unsupported_xml_input,
                                                sender_jid_from_mam_muc_doesnt_crash_on_malformed_eterm_input,
                                                sender_jid_from_mam_muc_doesnt_crash_on_malformed_xml_input
                                               ]}
    ].

init_per_suite(Config) ->
    file:delete("/tmp/script-debug"),
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(Config) ->
    {ok, DebugData} = file:read_file("/tmp/script-debug"),
    ct:pal("~p", [DebugData]),
    Config.

%% ----------------------------------------------------------
%% Test cases
%% ----------------------------------------------------------

%% ----------------- sender-jid-from-mam-message.escript ----------------------

sender_jid_from_mam_muc_eterm_stream(Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript",
            ["eterm", filename:join(small_path_helper:repo_dir(Config),
                                    "tools/migration/sender-jid-from-mam-message.example.eterm")]),
    sender_jid_from_mam_muc_data_stream(Port).

sender_jid_from_mam_muc_xml_stream(Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript",
            ["xml", filename:join(small_path_helper:repo_dir(Config),
                                  "tools/migration/sender-jid-from-mam-message.example.xml")]),
    sender_jid_from_mam_muc_data_stream(Port).

sender_jid_from_mam_muc_data_stream(Port) ->
    BareJID = <<"gżegżółka@brzęczyszczykiewicz.pl"/utf8>>,
    BareJID = script_helper:read(Port).

sender_jid_from_mam_muc_doesnt_crash_on_unsupported_eterm_input(Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript",
            ["eterm", filename:join(small_path_helper:repo_dir(Config),
                                    "tools/migration/unsupported_input.example.eterm")]),
    sender_jid_from_mam_muc_doesnt_crash_on_unsupported_input(Port).

sender_jid_from_mam_muc_doesnt_crash_on_unsupported_xml_input(Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript",
            ["xml", filename:join(small_path_helper:repo_dir(Config),
                                  "tools/migration/unsupported_input.example.xml")]),
    sender_jid_from_mam_muc_doesnt_crash_on_unsupported_input(Port).


sender_jid_from_mam_muc_doesnt_crash_on_malformed_eterm_input(Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript",
            ["eterm", filename:join(small_path_helper:repo_dir(Config),
                                    "tools/migration/malformed_input.example.eterm")]),
    sender_jid_from_mam_muc_doesnt_crash_on_malformed_input(Port).

sender_jid_from_mam_muc_doesnt_crash_on_malformed_xml_input(Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript",
            ["xml", filename:join(small_path_helper:repo_dir(Config),
                                  "tools/migration/malformed_input.example.xml")]),
    sender_jid_from_mam_muc_doesnt_crash_on_malformed_input(Port).

sender_jid_from_mam_muc_doesnt_crash_on_unsupported_input(Port) ->
    {error, -2} = script_helper:read(Port).

sender_jid_from_mam_muc_doesnt_crash_on_malformed_input(Port) ->
    {error, -1} = script_helper:read(Port).
