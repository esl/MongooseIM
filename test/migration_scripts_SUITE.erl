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
    ok = stringprep:start(),
    Config.

end_per_suite(Config) ->
    {ok, DebugData} = file:read_file("/tmp/script-debug"),
    ct:pal("~p", [DebugData]),
    Config.

%% ----------------------------------------------------------
%% Test cases
%% ----------------------------------------------------------

%% ----------------- sender-jid-from-mam-message.escript ----------------------

sender_jid_from_mam_muc_eterm_stream(_Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript", ["eterm"]),
    sender_jid_from_mam_muc_data_stream(Port, fun binary_string_to_eterm/1).

sender_jid_from_mam_muc_xml_stream(_Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript", ["xml"]),
    sender_jid_from_mam_muc_data_stream(Port, fun(B) -> B end).

sender_jid_from_mam_muc_data_stream(Port, PayloadConverterFun) ->
    lists:foreach(fun(JID) ->
                          MsgBin = sample_archived_muc_message(JID),
                          script_helper:write(Port, PayloadConverterFun(MsgBin)),
                          BareJID = mod_mam_utils:bare_jid(jid:from_binary(JID)),
                          BareJID = script_helper:read(Port)
                  end, [<<"alice@localhost">>, <<"zAżółćgęśLąjaźń@localhost2/res3"/utf8>>,
                        <<"kate@kędZierzyn.koźle.pl"/utf8>>]).

sender_jid_from_mam_muc_doesnt_crash_on_unsupported_eterm_input(_Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript", ["eterm"]),
    sender_jid_from_mam_muc_doesnt_crash_on_unsupported_input(Port, fun binary_string_to_eterm/1).

sender_jid_from_mam_muc_doesnt_crash_on_unsupported_xml_input(_Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript", ["xml"]),
    sender_jid_from_mam_muc_doesnt_crash_on_unsupported_input(Port, fun(B) -> B end).


sender_jid_from_mam_muc_doesnt_crash_on_malformed_eterm_input(_Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript", ["eterm"]),
    sender_jid_from_mam_muc_doesnt_crash_on_malformed_input(Port, fun binary_string_to_eterm/1).

sender_jid_from_mam_muc_doesnt_crash_on_malformed_xml_input(_Config) ->
    Port = script_helper:start("tools/migration/sender-jid-from-mam-message.escript", ["xml"]),
    sender_jid_from_mam_muc_doesnt_crash_on_malformed_input(Port, fun(B) -> B end).

sender_jid_from_mam_muc_doesnt_crash_on_unsupported_input(Port, PayloadConverterFun) ->
    %% First we expect that the script replies with -2 length (non MUC message)....
    InvalidPayload = PayloadConverterFun(sample_archived_1_to_1_message()),
    script_helper:write(Port, InvalidPayload),
    {error, -2} = script_helper:read(Port),

    %% Then we confirm with valid payload that the script actually still works
    sender_jid_from_mam_muc_data_stream(Port, PayloadConverterFun).

sender_jid_from_mam_muc_doesnt_crash_on_malformed_input(Port, PayloadConverterFun) ->
    %% First we expect that the script replies with -1 length (malformed message)....
    InvalidPayload = PayloadConverterFun(sample_malformed_muc_message()),
    script_helper:write(Port, InvalidPayload),
    {error, -1} = script_helper:read(Port),

    %% Then we confirm with valid payload that the script actually still works
    sender_jid_from_mam_muc_data_stream(Port, PayloadConverterFun).

%% ----------------------------------------------------------
%% Helpers
%% ----------------------------------------------------------

sample_archived_muc_message(JID) ->
  <<"<message xmlns='jabber:client'
        from='coven@chat.shakespeare.lit/firstwitch'
        id='162BEBB1-F6DB-4D9A-9BD8-CFDCC801A0B2'
        type='groupchat'>
        <body>Zażółć gęślą jaźń</body>
        <x xmlns='http://jabber.org/protocol/muc#user'>
          <item affiliation='none'
                jid='"/utf8, JID/binary, "'
                role='participant' />
        </x>
      </message>"/utf8>>.

sample_archived_1_to_1_message() ->
    <<"<message from='a@localhost' to='b@localhost' type='chat'><body>"
      "Zażółć gęślą jaźń</body></message>"/utf8>>.

sample_malformed_muc_message() ->
    <<"<message xmlns='jabber:client'
        from='coven@chat.shakespeare.lit/firstwitch'
        id='162BEBB1-F6DB-4D9A-9BD8-CFDCC801A0B2'
        type='groupchat'>
        <body>Zażółć gęślą jaźń</body>
        <x xmlns='http://jabber.org/protocol/muc#user'>
          <item_malformed affiliation='none'
                jid='a@localhost'
                role='participant' />
        </x>
      </message>"/utf8>>.


binary_string_to_eterm(Bin) ->
    {ok, XmlEl} = exml:parse(Bin),
    term_to_binary(XmlEl).

