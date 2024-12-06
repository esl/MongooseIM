%% Stream Management helpers
-module(sm_helper).

-export([client_to_spec0/1,
         client_to_spec/1,
         client_to_smid/1,
         get_sid_by_stream_id/2]).

%% Connection helpers
-export([connect_fresh/3,
         connect_fresh/4,
         connect_spec/2,
         connect_spec/3,
         connect_same/2,
         connect_same/3,
         connect_resume/2]).

%% Waiting and introspection helpers
-export([try_to_resume_stream/3,
         kill_and_connect_with_resume_session_without_waiting_for_result/1,
         stop_client_and_wait_for_termination/1,
         assert_alive_resources/2,
         get_user_present_resources/1,
         wait_for_queue_length/2,
         wait_for_resource_count/2,
         wait_for_c2s_unacked_count/2,
         wait_for_process_termination/1,
         wait_until_resume_session/1,
         process_initial_stanza/1,
         kill_and_connect_resume/1,
         kill_client_and_wait_for_termination/1,
         monitor_session/1]).

%% Stanza helpers
-export([send_initial_presence/1,
         send_messages/3,
         wait_for_messages/2,
         wait_for_delayed_messages/2,
         assert_delayed/1,
         assert_messages/2,
         send_and_receive/3,
         get_ack/1,
         ack_initial_presence/1]).

-include_lib("escalus/include/escalus.hrl").

-import(distributed_helper, [mim/0,
                             rpc/4]).

-define(MOD_SM, mod_stream_management).
-define(EXT_C2S_STATE(S), {external_state, S}).

client_to_smid(#client{props = Props}) ->
    proplists:get_value(smid, Props).

client_to_spec(#client{props = Props}) ->
    Props.

%% Returns the original spec, without the dynamically added fields
client_to_spec0(#client{props = Props}) ->
    lists:foldl(fun proplists:delete/2, Props, [stream_id, resource]).

get_sid_by_stream_id(HostType, SMID) ->
    rpc(mim(), ?MOD_SM, get_sid, [HostType, SMID]).


%% Connection helpers

final_step_to_steps(before_auth) ->
    connection_steps_before_auth();
final_step_to_steps(auth) ->
    connection_steps_to_authenticate();
final_step_to_steps(session) ->
    connection_steps_to_session();
final_step_to_steps(presence) ->
    connection_steps_to_presence();
final_step_to_steps(sr_session) ->
    connection_steps_to_enable_stream_resumption();
final_step_to_steps(sr_presence) ->
    connection_steps_to_enable_stream_resumption_and_presence();
final_step_to_steps(sm_presence) ->
    connection_steps_to_enable_stream_mgmt_and_presence();
final_step_to_steps(sm_after_session) ->
    connection_steps_to_enable_stream_mgmt(after_session);
final_step_to_steps(sm_after_bind) ->
    connection_steps_to_enable_stream_mgmt(after_bind);
final_step_to_steps(sm_before_session) ->
    connection_steps_to_enable_stream_mgmt(after_bind) ++ [session];
final_step_to_steps({resume, SMID, H}) ->
    connection_steps_to_stream_resumption(SMID, H).

%% Connection steps
connection_steps_before_auth() ->
    [start_stream,
     stream_features,
     maybe_use_ssl].
connection_steps_to_authenticate() ->
    connection_steps_before_auth() ++ [authenticate].

connection_steps_to_bind() ->
    connection_steps_to_authenticate() ++ [bind].

connection_steps_to_session() ->
    connection_steps_to_bind() ++ [session].

connection_steps_to_enable_stream_mgmt(after_session) ->
    connection_steps_to_session() ++ [stream_management];
connection_steps_to_enable_stream_mgmt(after_bind) ->
    connection_steps_to_bind() ++ [stream_management].

connection_steps_to_enable_stream_resumption() ->
    connection_steps_to_session() ++ [stream_resumption].

connection_steps_to_enable_stream_resumption_and_presence() ->
    connection_steps_to_enable_stream_resumption() ++ [fun initial_presence_step/2].

connection_steps_to_presence() ->
    connection_steps_to_session() ++ [fun initial_presence_step/2].

connection_steps_to_enable_stream_mgmt_and_presence() ->
    connection_steps_to_enable_stream_mgmt(after_session) ++ [fun initial_presence_step/2].

connection_steps_to_stream_resumption(SMID, H) ->
    connection_steps_to_authenticate() ++ [mk_resume_stream(SMID, H)].

mk_resume_stream(SMID, PrevH) ->
    fun (Conn = #client{props = Props}, Features) ->
            Resumed = try_to_resume_stream(Conn, SMID, PrevH),
            true = escalus_pred:is_sm_resumed(SMID, Resumed),
            {Conn#client{props = [{smid, SMID} | Props]}, Features}
    end.

try_to_resume_stream(Conn, SMID, PrevH) ->
    escalus_connection:send(Conn, escalus_stanza:resume(SMID, PrevH)),
    escalus_connection:get_stanza(Conn, get_resumed).

initial_presence_step(Conn, Features) ->
    process_initial_stanza(Conn),
    {Conn, Features}.

%% Making connections with different set of steps is a common procedure in sm_SUITE
%% This group of functions helps to keep code short and clean
connect_fresh(Config, Name, FinalStep) ->
    connect_fresh(Config, Name, FinalStep, auto).

connect_fresh(Config, Name, FinalStep, Ack) ->
    Spec = escalus_fresh:create_fresh_user(Config, Name),
    connect_spec(Spec, FinalStep, Ack).

connect_spec(Spec, FinalStep) ->
    connect_spec(Spec, FinalStep, auto).

connect_spec(Spec, FinalStep, Ack) ->
    Steps = final_step_to_steps(FinalStep),
    ExtraOpts = ack_to_extra_opts(Ack),
    {ok, Client, _} = escalus_connection:start(ExtraOpts ++ Spec, Steps),
    Client.

connect_same(Client, FinalStep) ->
    connect_same(Client, FinalStep, auto).

connect_same(Client, FinalStep, Ack) ->
    connect_spec(client_to_spec0(Client), FinalStep, Ack).

ack_to_extra_opts(auto) -> [];
ack_to_extra_opts(manual) -> [{manual_ack, true}].

kill_and_connect_resume(Client) ->
    %% Get SMH before killing the old connection
    SMH = escalus_connection:get_sm_h(Client),
    escalus_connection:kill(Client),
    connect_resume(Client, SMH).

connect_resume(Client, SMH) ->
    SMID = client_to_smid(Client),
    Spec = client_to_spec(Client),
    C2SPid = mongoose_helper:get_session_pid(Client),
    Steps = connection_steps_to_stream_resumption(SMID, SMH),
    try
        {ok, Client2, _} = escalus_connection:start(Spec, Steps),
        Client2
    catch Class:Error:Stacktrace ->
        ct:pal("C2S info ~p", [rpc:pinfo(C2SPid, [messages, current_stacktrace])]),
        erlang:raise(Class, Error, Stacktrace)
    end.

kill_and_connect_with_resume_session_without_waiting_for_result(Alice) ->
    SMH = escalus_connection:get_sm_h(Alice),
    %% SMID could be anything,
    %% we would fail anyway because C2S process would be dead when resuming
    SMID = client_to_smid(Alice),
    %% kill alice connection
    kill_client_and_wait_for_termination(Alice),
    NewAlice = connect_same(Alice, auth),
    escalus_connection:send(NewAlice, escalus_stanza:resume(SMID, SMH)),
    NewAlice.

kill_client_and_wait_for_termination(Alice) ->
    C2SRef = monitor_session(Alice),
    escalus_connection:kill(Alice),
    ok = wait_for_process_termination(C2SRef).

stop_client_and_wait_for_termination(Alice) ->
    C2SRef = monitor_session(Alice),
    escalus_connection:stop(Alice),
    ok = wait_for_process_termination(C2SRef).


%% Waiting and introspection helpers

monitor_session(Client) ->
    C2SPid = mongoose_helper:get_session_pid(Client),
    erlang:monitor(process, C2SPid).

-spec wait_for_process_termination(MRef :: reference()) -> ok.
wait_for_process_termination(MRef) ->
    receive
        {'DOWN', MRef, _Type, _C2SPid, _Info} ->
            ok
    after 5000 ->
              ct:fail(wait_for_process_termination_timeout)
    end.

wait_for_queue_length(Pid, Length) ->
    F = fun() ->
                case rpc(mim(), erlang, process_info, [Pid, messages]) of
                    {messages, List} when length(List) =:= Length ->
                        ok;
                    {messages, List} ->
                        {messages, length(List), List};
                    Other ->
                        Other
                end
        end,
    wait_helper:wait_until(F, ok, #{name => {wait_for_queue_length, Length}}).

wait_for_c2s_unacked_count(C2SPid, Count) ->
    wait_helper:wait_until(fun() -> get_c2s_unacked_count(C2SPid) end, Count,
                           #{name => get_c2s_unacked_count}).

get_c2s_unacked_count(C2SPid) ->
    StateData = mongoose_helper:get_c2s_state_data(C2SPid),
    {ok, SmStateData} = rpc(mim(), mongoose_c2s, get_mod_state, [StateData, mod_stream_management]),
    element(3, SmStateData).

wait_for_resource_count(Client, N) ->
    wait_helper:wait_until(fun() -> length(get_user_alive_resources(Client)) end,
                           N, #{name => get_user_alive_resources}).

assert_alive_resources(Alice, N) ->
    N = length(get_user_alive_resources(Alice)).

get_user_alive_resources(Client) ->
    UserSpec = client_to_spec(Client),
    {U, S} = get_us_from_spec(UserSpec),
    JID = mongoose_helper:make_jid(U, S, <<>>),
    rpc(mim(), ejabberd_sm, get_user_resources, [JID]).

get_user_present_resources(Client) ->
    UserSpec = client_to_spec(Client),
    {U, S} = get_us_from_spec(UserSpec),
    JID = mongoose_helper:make_jid(U, S, <<>>),
    rpc(mim(), ejabberd_sm, get_user_present_resources, [JID]).

get_us_from_spec(UserSpec) ->
    U = proplists:get_value(username, UserSpec),
    S = proplists:get_value(server, UserSpec),
    {U, S}.

wait_until_resume_session(C2SPid) ->
    mongoose_helper:wait_for_c2s_state_name(C2SPid, ?EXT_C2S_STATE(resume_session)).


%% Stanza helpers

send_initial_presence(User) ->
    InitialPresence = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(User, InitialPresence).

process_initial_stanza(User) ->
    send_initial_presence(User),
    Stanza = escalus:wait_for_stanza(User),
    case escalus_pred:is_iq(Stanza) of
        true ->
            Stanza2 = escalus:wait_for_stanza(User),
            escalus:assert(is_presence, Stanza2);
        false -> escalus:assert(is_presence, Stanza)
    end.

send_messages(Bob, Alice, Texts) ->
    [escalus:send(Bob, escalus_stanza:chat_to(Alice, Text))
     || Text <- Texts].

wait_for_messages(Alice, Texts) ->
    Stanzas = escalus:wait_for_stanzas(Alice, length(Texts)),
    assert_messages(Stanzas, Texts).

wait_for_delayed_messages(Alice, Texts) ->
    Stanzas = escalus:wait_for_stanzas(Alice, length(Texts)),
    assert_messages(Stanzas, Texts),
    [assert_delayed(S) || S <- Stanzas].

assert_delayed(Stanza) ->
    escalus:assert(has_ns, [<<"urn:xmpp:delay">>], exml_query:subelement(Stanza, <<"delay">>)).

assert_messages(Stanzas, Texts) ->
    Bodies = lists:map(fun get_body/1, Stanzas),
    case Bodies of
        Texts ->
            ok;
        _ ->
            ct:fail({assert_messages_failed, Stanzas,
                     {expected, Texts},
                     {received, Bodies}})
    end.

%% Useful to ensure no other stanzas buffered
send_and_receive(From, To, Text) ->
    escalus:send(From, escalus_stanza:chat_to(To, Text)),
    escalus:assert(is_chat_message, [Text], escalus:wait_for_stanza(To)).

get_body(Stanza) ->
      exml_query:path(Stanza, [{element, <<"body">>}, cdata]).

get_ack(Client) ->
    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Client, ack)).

ack_initial_presence(Client) ->
    escalus_connection:send(Client, escalus_stanza:sm_ack(1)).
