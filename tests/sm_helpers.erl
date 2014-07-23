-module(sm_helpers).
-export([buffer_unacked_messages_and_die/3,
         connect_and_die/1,
         discard_offline_messages/2,
         get_session_pid/2,
         mk_resume_stream/2
        ]).

-import(escalus_stanza, [setattr/3]).
-import(vcard_update, [discard_vcard_update/1,
                       server_string/1]).


discard_offline_messages(Config, UserName) ->
    discard_offline_messages(Config, UserName, 1).

discard_offline_messages(Config, UserName, H) when is_atom(UserName) ->
    Spec = escalus_users:get_options(Config, UserName),
    {ok, User, _, _} = escalus_connection:start(Spec),
    escalus_connection:send(User, escalus_stanza:presence(<<"available">>)),
    discard_offline_messages(Config, User, H);
discard_offline_messages(Config, User, H) ->
    Stanza = escalus_connection:get_stanza(User, maybe_offline_msg),
    escalus_connection:send(User, escalus_stanza:sm_ack(H)),
    case escalus_pred:is_presence(Stanza) of
        true ->
            ok;
        false ->
            discard_offline_messages(Config, User, H+1)
    end.

mk_resume_stream(SMID, PrevH) ->
    fun (Conn, Props, Features) ->
            escalus_connection:send(Conn, escalus_stanza:resume(SMID, PrevH)),
            Resumed = escalus_connection:get_stanza(Conn, get_resumed),
            true = escalus_pred:is_sm_resumed(SMID, Resumed),
            {ok, PrevH} = escalus_connection:set_sm_h(Conn, PrevH),
            {Conn, [{smid, SMID} | Props], Features}
    end.


connect_and_die(AliceSpec) ->

    {ok, Alice, Props, _} = escalus_connection:start(AliceSpec, steps()),
    InitialPresence = setattr(escalus_stanza:presence(
                                <<"available">>), <<"id">>, <<"presence1">>),
    escalus_connection:send(Alice, InitialPresence),
    Presence = escalus_connection:get_stanza(Alice, presence1),
    escalus:assert(is_presence, Presence),
    Res = server_string(proplists:get_value(resource, AliceSpec)),

    {ok, C2SPid} = get_session_pid(AliceSpec, Res),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    _Presence = escalus_connection:get_stanza(Alice, presence2),
    discard_vcard_update(Alice),
    escalus_connection:send(Alice, escalus_stanza:carbons_enable()),
    _Presence3 = escalus_connection:get_stanza(Alice, presence3),
    _IqResult = escalus_connection:get_stanza(Alice, carbon_result),
    LastH = escalus_connection:get_sm_h(Alice),
    %% Alice's connection is violently terminated.
    escalus_connection:kill(Alice),
    {C2SPid, proplists:get_value(smid, Props), LastH}.

buffer_unacked_messages_and_die(AliceSpec, Bob, Messages) ->
    {ok, Alice, Props, _} = escalus_connection:start(AliceSpec, steps()),
    InitialPresence = setattr(escalus_stanza:presence(<<"available">>),
                              <<"id">>, <<"presence1">>),
    escalus_connection:send(Alice, InitialPresence),
    Presence = escalus_connection:get_stanza(Alice, presence1),
    escalus:assert(is_presence, Presence),
    Res = server_string("escalus-default-resource"),
    {ok, C2SPid} = get_session_pid(AliceSpec, Res),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    _Presence = escalus_connection:get_stanza(Alice, presence2),
    discard_vcard_update(Alice),
    %% Bobs sends some messages to Alice.
    [escalus:send(Bob, escalus_stanza:chat_to(alice, Msg))
     || Msg <- Messages],
    %% Alice receives them, but doesn't ack.
    Stanzas = [escalus_connection:get_stanza(Alice, {msg, I})
               || I <- lists:seq(1, 3)],
    [escalus:assert(is_chat_message, [Msg], Stanza)
     || {Msg, Stanza} <- lists:zip(Messages, Stanzas)],
    %% Alice's connection is violently terminated.
    escalus_connection:kill(Alice),
    timer:sleep(250),
    {C2SPid, proplists:get_value(smid, Props)}.


get_session_pid(UserSpec, Resource) ->
    ConfigUS = [proplists:get_value(username, UserSpec),
                proplists:get_value(server, UserSpec)],
    [U, S] = [server_string(V) || V <- ConfigUS],
    case escalus_ejabberd:rpc(ejabberd_sm, get_session_pid, [U, S, server_string(Resource)]) of
        none ->
            {error, no_found};
        C2SPid ->
            {ok, C2SPid}
    end.

steps() ->
    [start_stream, maybe_use_ssl, authenticate,
     bind, session, stream_resumption].



%% Copy'n'paste from github.com/lavrin/ejabberd-trace

match_session_pid({_User, _Domain, _Resource} = UDR) ->
    [{%% match pattern
      set(session(), [{2, {'_', '$1'}},
                      {3, UDR}]),
      %% guards
      [],
      %% return
      ['$1']}];

match_session_pid({User, Domain}) ->
    [{%% match pattern
      set(session(), [{2, {'_', '$1'}},
                      {3, '$2'},
                      {4, {User, Domain}}]),
      %% guards
      [],
      %% return
      [{{'$2', '$1'}}]}].

set(Record, FieldValues) ->
    F = fun({Field, Value}, Rec) ->
                setelement(Field, Rec, Value)
        end,
    lists:foldl(F, Record, FieldValues).

session() ->
    set(erlang:make_tuple(6, '_'), [{1, session}]).

%% End of copy'n'paste from github.com/lavrin/ejabberd-trace
