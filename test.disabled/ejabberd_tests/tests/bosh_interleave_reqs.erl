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
         connect_alice/1,
         send_from_carol/2,
         send_from_alice/2,
         wait_for_msgs_carol/2,
         wait_for_msgs_alice/2]).

-export([ct_config_giver/1]).

-record(state, {carol,
                alice,
                msgs_to_carol,
                msgs_to_alice,
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
                   maybe_stop_client(State#state.alice),
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
    escalus_connection:stop(Client).

initial_state(Pid) ->
    #state{carol = undefined,
           alice = undefined,
           msgs_to_carol = [],
           msgs_to_alice = [],
           config_pid = Pid}.

command(S) ->
    Cmds = possible_commands(S),
    oneof(Cmds).

possible_commands(S) ->
    Carol = (S#state.carol /= undefined),
    Alice = (S#state.alice /= undefined),
    Users = Carol andalso Alice,
    MsgsToCarol = (S#state.msgs_to_carol /= []),
    MsgsToAlice = (S#state.msgs_to_alice /= []),
    Pid = S#state.config_pid,
    [{call, ?MODULE, connect_carol, [Pid]} || not Carol] ++
    [{call, ?MODULE, connect_alice, [Pid]} || not Alice] ++
    [{call, ?MODULE, send_from_carol, [S#state.carol, S#state.alice]} || Users] ++
    [{call, ?MODULE, send_from_alice, [S#state.alice, S#state.carol]} || Users] ++
    [{call, ?MODULE, wait_for_msgs_carol, [S#state.carol, S#state.msgs_to_carol]}
     || MsgsToCarol] ++
    [{call, ?MODULE, wait_for_msgs_alice, [S#state.alice, S#state.msgs_to_alice]}
     || MsgsToAlice].


precondition(_, _) -> true.
postcondition(_, _, _) -> true.

next_state(S, V, {call, _, connect_carol, [_]}) ->
    S#state{carol = V};
next_state(S, V, {call, _, connect_alice, [_]}) ->
    S#state{alice = V};
next_state(#state{msgs_to_alice = Msgs} = S, V, {call, _, send_from_carol, _}) ->
    S#state{msgs_to_alice = [V | Msgs]};
next_state(#state{msgs_to_carol = Msgs} = S, V, {call, _, send_from_alice, _}) ->
    S#state{msgs_to_carol = [V | Msgs]};
next_state(S, _, {call, _, wait_for_msgs_carol, _}) ->
    S#state{msgs_to_carol = []};
next_state(S, _, {call, _, wait_for_msgs_alice, _}) ->
    S#state{msgs_to_alice = []};
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

connect_alice(Pid) ->
    Spec = given_fresh_spec(read_config(Pid), alice),
    connect_user(Spec).

given_fresh_spec(Config, User) ->
    NewConfig = escalus_fresh:create_users(Config, [{User, 1}]),
    escalus_users:get_userspec(NewConfig, User).

connect_user(Spec) ->
    Res = base64:encode(crypto:strong_rand_bytes(4)),
    {ok, Conn, _} = escalus_connection:start([{resource, Res} | Spec]),
    escalus:send(Conn, escalus_stanza:presence(<<"available">>)),
    escalus:wait_for_stanza(Conn, timer:seconds(5)),
    Conn.

send_from_carol(Carol, Alice) ->
    Msg = gen_msg(),
    escalus:send(Carol, escalus_stanza:chat_to(Alice, Msg)),
    {escalus_client:short_jid(Carol),
     Msg
    }.

send_from_alice(Alice, Carol) ->
    Msg = gen_msg(),
    escalus:send(Alice, escalus_stanza:chat_to(Carol, Msg)),
    {escalus_client:short_jid(Alice),
     Msg
    }.

gen_msg() ->
    Msg = base64:encode(crypto:strong_rand_bytes(15)),
    Msg.

wait_for_msgs_carol(Carol, Msgs) ->
    wait_for_msgs(Carol, lists:reverse(Msgs)).

wait_for_msgs_alice(Alice, Msgs) ->
    wait_for_msgs(Alice, lists:reverse(Msgs)).

wait_for_msgs(Client, []) ->
    ok;
wait_for_msgs(Client, All) ->
    StanzasFromServer = escalus:wait_for_stanzas(Client, length(All), timer:seconds(5)),
    ValidBodies = [ Body || {_, Body} <- All ],
    equal_bodies(ValidBodies, [ exml_query:path(El, [{element, <<"body">>}, cdata]) || El <- StanzasFromServer ]).

equal_bodies(Bodies, Bodies) -> ok.

