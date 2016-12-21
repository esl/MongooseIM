-module(bosh_interleave_reqs).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
% -include_lib("eqc/include/eqc_statem.hrl").
% -include_lib("eqc/include/eqc.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

-export([test/1, sample/0, prop/1]).

-export([initial_state/1, command/1, precondition/2, postcondition/3,
         next_state/3]).

-export([read_config/1,
         connect_carol/1,
         connect_geralt/1,
         send_from_carol/2,
         send_from_geralt/2,
         wait_for_msgs_carol/2,
         wait_for_msgs_geralt/2]).

-export([ct_config_giver/1]).

-record(state, {carol,
                geralt,
                msgs_to_carol,
                msgs_to_geralt,
                config_pid}).

test(Config) ->
    proper:quickcheck(?MODULE:prop(Config)).

sample() ->
    proper_gen:pick(commands(?MODULE)).

prop(Config) ->
    Pid = spawn_link(?MODULE, ct_config_giver, [Config]),
    ?FORALL(Cmds, commands(?MODULE, initial_state(Pid)),
            ?TRAPEXIT(
               begin
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   maybe_stop_client(State#state.carol),
                   maybe_stop_client(State#state.geralt),
                   ?WHENFAIL(ct:log(error, "History: ~p~nState: ~p\nResult: ~p~n",
                                    [History, State, Result]),
                             aggregate(command_names(Cmds), Result =:= ok))
               end)).

ct_config_giver(Config) ->
    receive
        {give_me_config, Pid} ->
            Pid ! {ok, Config},
            ct_config_giver(Config)
    end.

maybe_stop_client(undefined) -> ok;
maybe_stop_client(Client) ->
    escalus_client:stop(Client).

initial_state(Pid) ->
    #state{carol = undefined,
           geralt = undefined,
           msgs_to_carol = [],
           msgs_to_geralt = [],
           config_pid = Pid}.

command(S) ->
    Cmds = possible_commands(S),
    oneof(Cmds).

possible_commands(S) ->
    Carol = (S#state.carol /= undefined),
    Geralt = (S#state.geralt /= undefined),
    Users = Carol andalso Geralt,
    MsgsToCarol = (S#state.msgs_to_carol /= []),
    MsgsToGeralt = (S#state.msgs_to_geralt /= []),
    Pid = S#state.config_pid,
    [{call, ?MODULE, connect_carol, [Pid]} || not Carol] ++
    [{call, ?MODULE, connect_geralt, [Pid]} || not Geralt] ++
    [{call, ?MODULE, send_from_carol, [S#state.carol, S#state.geralt]} || Users] ++
    [{call, ?MODULE, send_from_geralt, [S#state.geralt, S#state.carol]} || Users] ++
    [{call, ?MODULE, wait_for_msgs_carol, [S#state.carol, S#state.msgs_to_carol]}
     || MsgsToCarol] ++
    [{call, ?MODULE, wait_for_msgs_geralt, [S#state.geralt, S#state.msgs_to_geralt]}
     || MsgsToGeralt].


precondition(_, _) -> true.
postcondition(_, _, _) -> true.

next_state(S, V, {call, _, connect_carol, [_]}) ->
    S#state{carol = V};
next_state(S, V, {call, _, connect_geralt, [_]}) ->
    S#state{geralt = V};
next_state(#state{msgs_to_geralt = Msgs} = S, V, {call, _, send_from_carol, _}) ->
    S#state{msgs_to_geralt = [V | Msgs]};
next_state(#state{msgs_to_carol = Msgs} = S, V, {call, _, send_from_geralt, _}) ->
    S#state{msgs_to_carol = [V | Msgs]};
next_state(S, _, {call, _, wait_for_msgs_carol, _}) ->
    S#state{msgs_to_carol = []};
next_state(S, _, {call, _, wait_for_msgs_geralt, _}) ->
    S#state{msgs_to_geralt = []};
next_state(S, _, _) ->
    S.

read_config(Pid) ->
    Pid ! {give_me_config, self()},
    receive
        {ok, Config} ->
            Config
    after 100 ->
              error
    end.

connect_carol(Pid) ->
    Spec = given_fresh_spec(read_config(Pid), carol),
    connect_user([{keepalive, true} | Spec]).

connect_geralt(Pid) ->
    Spec = given_fresh_spec(read_config(Pid), alice),
    connect_user(Spec).

given_fresh_spec(Config, User) ->
    NewConfig = escalus_fresh:create_users(Config, [{User, 1}]),
    escalus_users:get_userspec(NewConfig, User).

connect_user(Spec) ->
    Res = base64:encode(crypto:rand_bytes(4)),
    {ok, Conn, Props, _} = escalus_connection:start([{resource, Res} | Spec]),
    JID = make_jid(Props),
    escalus:send(Conn, escalus_stanza:presence(<<"available">>)),
    escalus:wait_for_stanza(Conn, timer:seconds(5)),
    Conn#client{jid = JID}.

make_jid(Proplist) ->
    {username, U} = lists:keyfind(username, 1, Proplist),
    {server, S} = lists:keyfind(server, 1, Proplist),
    {resource, R} = lists:keyfind(resource, 1, Proplist),
    <<U/binary, "@", S/binary, "/", R/binary>>.

send_from_carol(Carol, Geralt) ->
    Msg = gen_msg(),
    escalus:send(Carol, escalus_stanza:chat_to(Geralt, Msg)),
    {escalus_client:short_jid(Carol),
     Msg
    }.

send_from_geralt(Geralt, Carol) ->
    Msg = gen_msg(),
    escalus:send(Geralt, escalus_stanza:chat_to(Carol, Msg)),
    {escalus_client:short_jid(Geralt),
     Msg
    }.

gen_msg() ->
    Msg = base64:encode(crypto:rand_bytes(15)),
    Msg.

wait_for_msgs_carol(Carol, Msgs) ->
    L = length(Msgs),
    Stanzas = escalus:wait_for_stanzas(Carol, L, timer:seconds(5)),
    RMsgs = [exml_query:path(E, [{element, <<"body">>}, cdata])
             || E <- Stanzas],
    SortedMsgs = lists:sort([Msg || {_, Msg} <- Msgs]),
    SortedMsgs = lists:sort(RMsgs),
    ok.

wait_for_msgs_geralt(Geralt, Msgs) ->
    wait_for_msgs(Geralt, lists:reverse(Msgs)).

wait_for_msgs(_Client, []) ->
    ok;
wait_for_msgs(Client, [{_, Msg} | Rest]) ->
    escalus:assert(is_chat_message, [Msg],
                   escalus:wait_for_stanza(Client, timer:seconds(5))),
    wait_for_msgs(Client, Rest).

