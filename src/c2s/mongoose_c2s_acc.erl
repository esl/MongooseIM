%% @doc Interface for actions that handler modules might request `mongoose_c2s' to act upon.
%% Note that some of the keys take a list of elements, which, in the case of a hook with many
%% handlers, means that at the end of the queue `mongoose_c2s' will get a list of requests
%% by different modules. These will be acted upon in the same order they were inserted in the
%% hook list, according to their priority.
%% The following keys are defined:
%% - `state_mod': key-value pairs of gen_mod module names and their desired state.
%% - `actions': a list of valid `gen_statem:action()' to request the `mongoose_c2s' engine.
%% - `c2s_state': a new state is requested for the state machine.
%% - `c2s_data': a new state data is requested for the state machine.
%% - `stop': an action of type `{cast, {stop, Reason}}' is to be triggered.
%% - `hard_stop': no other request is allowed, the state machine is immediatly triggered to stop.
%% - `route': mongoose_acc elements to trigger the whole `handle_route' pipeline.
%% - `flush': mongoose_acc elements to trigger the `handle_flush` pipeline.
%% - `socket_send': xml elements to send on the socket to the user.
%% - `socket_send_first': xml elements to send on the socket to the user, but appended first.
-module(mongoose_c2s_acc).

-export([new/0, new/1,
         get_statem_result/1,
         from_mongoose_acc/2,
         to_acc/3, to_acc_many/2
        ]).

-type key() :: state_mod
             | actions
             | c2s_state
             | c2s_data
             | stop
             | hard_stop
             | route
             | flush
             | socket_send
             | socket_send_first.
-type pairs() :: [pair()].
-type pair() :: {state_mod, {module(), term()}}
              | {actions, gen_statem:action()}
              | {c2s_state, mongoose_c2s:state()}
              | {c2s_data, mongoose_c2s:data()}
              | {stop, term() | {shutdown, atom()}}
              | {hard_stop, term() | {shutdown, atom()}}
              | {route, mongoose_acc:t()}
              | {flush, mongoose_acc:t()}
              | {socket_send, exml:element() | [exml:element()]}
              | {socket_send_first, exml:element() | [exml:element()]}.

-type t() :: #{
        state_mod := #{module() => term()},
        actions := [gen_statem:action()],
        c2s_state := undefined | mongoose_c2s:state(),
        c2s_data := undefined | mongoose_c2s:data(),
        hard_stop := undefined | Reason :: term(),
        socket_send := [exml:element()]
       }.

-type params() :: #{
        state_mod => #{module() => term()},
        actions => [gen_statem:action()],
        c2s_state => mongoose_c2s:state(),
        c2s_data => mongoose_c2s:data(),
        stop => Reason :: term(),
        hard_stop => Reason :: term(),
        route => [mongoose_acc:t()],
        flush => [mongoose_acc:t()],
        socket_send => [exml:element()]
       }.

-export_type([t/0, pairs/0]).

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------

-spec new() -> t().
new() ->
    #{
      state_mod => #{},
      actions => [],
      c2s_state => undefined,
      c2s_data => undefined,
      hard_stop => undefined,
      socket_send => []
     }.

-spec new(params()) -> t().
new(Params) ->
    Params1 = extract_stop(Params),
    Params2 = extract_routes(Params1),
    Params3 = extract_flushes(Params2),
    maps:merge(new(), Params3).

extract_stop(Params = #{stop := Reason}) ->
    WithoutStop = maps:remove(stop, Params),
    NewAction = [{next_event, cast, {stop, Reason}}],
    Fun = fun(Actions) -> [NewAction | Actions] end,
    maps:update_with(actions, Fun, NewAction, WithoutStop);
extract_stop(Params) ->
    Params.

extract_flushes(Params = #{flush := Accs}) ->
    WithoutFlush = maps:remove(flush, Params),
    NewAction = [{next_event, internal, {flush, Acc}} || Acc <- Accs ],
    Fun = fun(Actions) -> NewAction ++ Actions end,
    maps:update_with(actions, Fun, NewAction, WithoutFlush);
extract_flushes(Params) ->
    Params.

extract_routes(Params = #{route := Accs}) ->
    WithoutRoute = maps:remove(route, Params),
    NewAction = [{next_event, info, {route, Acc}} || Acc <- Accs ],
    Fun = fun(Actions) -> NewAction ++ Actions end,
    maps:update_with(actions, Fun, NewAction, WithoutRoute);
extract_routes(Params) ->
    Params.

-spec get_statem_result(mongoose_acc:t()) -> mongoose_c2s_acc:t().
get_statem_result(Acc) ->
    C2SAcc = #{actions := Actions,
               socket_send := SocketSend} = mongoose_acc:get_statem_acc(Acc),
    C2SAcc#{actions := lists:reverse(Actions),
            socket_send := lists:reverse(SocketSend)}.

-spec from_mongoose_acc(mongoose_acc:t(), key()) -> term().
from_mongoose_acc(Acc, Key) ->
    #{Key := Value} = mongoose_acc:get_statem_acc(Acc),
    Value.

-spec to_acc(mongoose_acc:t(), state_mod, {module(), term()}) -> mongoose_acc:t();
            (mongoose_acc:t(), actions, gen_statem:action() | [gen_statem:action()]) -> mongoose_acc:t();
            (mongoose_acc:t(), c2s_state, mongoose_c2s:state()) -> mongoose_acc:t();
            (mongoose_acc:t(), c2s_data, mongoose_c2s:data()) -> mongoose_acc:t();
            (mongoose_acc:t(), hard_stop, atom()) -> mongoose_acc:t();
            (mongoose_acc:t(), stop, atom() | {shutdown, atom()}) -> mongoose_acc:t();
            (mongoose_acc:t(), route, mongoose_acc:t()) -> mongoose_acc:t();
            (mongoose_acc:t(), flush, mongoose_acc:t()) -> mongoose_acc:t();
            (mongoose_acc:t(), socket_send, exml:element() | [exml:element()]) -> mongoose_acc:t();
            (mongoose_acc:t(), socket_send_first, exml:element() | [exml:element()]) -> mongoose_acc:t().
to_acc(Acc, Key, NewValue) ->
    C2SAcc = mongoose_acc:get_statem_acc(Acc),
    C2SAcc1 = to_c2s_acc(C2SAcc, {Key, NewValue}),
    mongoose_acc:set_statem_acc(C2SAcc1, Acc).

-spec to_acc_many(mongoose_acc:t(), pairs()) -> mongoose_acc:t().
to_acc_many(Acc, Pairs) ->
    C2SAcc = mongoose_acc:get_statem_acc(Acc),
    to_acc_many(Acc, C2SAcc, Pairs).

-spec to_acc_many(mongoose_acc:t(), mongoose_c2s_acc:t(), pairs()) -> mongoose_acc:t().
to_acc_many(Acc, C2SAcc, []) ->
    mongoose_acc:set_statem_acc(C2SAcc, Acc);
to_acc_many(Acc, C2SAcc, [Pair | Pairs]) ->
    NewCAcc = to_c2s_acc(C2SAcc, Pair),
    to_acc_many(Acc, NewCAcc, Pairs).

-spec to_c2s_acc(mongoose_c2s_acc:t(), pair()) -> mongoose_c2s_acc:t().
to_c2s_acc(C2SAcc = #{state_mod := ModStates}, {state_mod, {ModName, ModState}}) ->
    C2SAcc#{state_mod := ModStates#{ModName => ModState}};
to_c2s_acc(C2SAcc = #{actions := Actions}, {actions, NewActions}) when is_list(NewActions) ->
    C2SAcc#{actions := lists:reverse(NewActions) ++ Actions};
to_c2s_acc(C2SAcc = #{actions := Actions}, {actions, Action}) ->
    C2SAcc#{actions := [Action | Actions]};
to_c2s_acc(C2SAcc = #{actions := Actions}, {route, Accs}) when is_list(Accs) ->
    Routes = [{next_event, info, {route, Acc}} || Acc <- Accs ],
    C2SAcc#{actions := lists:reverse(Routes) ++ Actions};
to_c2s_acc(C2SAcc = #{actions := Actions}, {route, Acc}) ->
    C2SAcc#{actions := [{next_event, info, {route, Acc}} | Actions]};
to_c2s_acc(C2SAcc = #{actions := Actions}, {flush, Accs}) when is_list(Accs) ->
    Routes = [{next_event, internal, {flush, Acc}} || Acc <- Accs ],
    C2SAcc#{actions := lists:reverse(Routes) ++ Actions};
to_c2s_acc(C2SAcc = #{actions := Actions}, {flush, Acc}) ->
    C2SAcc#{actions := [{next_event, internal, {flush, Acc}} | Actions]};
to_c2s_acc(C2SAcc = #{socket_send := Stanzas}, {socket_send, NewStanzas}) when is_list(NewStanzas) ->
    C2SAcc#{socket_send := lists:reverse(NewStanzas) ++ Stanzas};
to_c2s_acc(C2SAcc = #{socket_send := Stanzas}, {socket_send, Stanza}) ->
    C2SAcc#{socket_send := [Stanza | Stanzas]};
to_c2s_acc(C2SAcc = #{socket_send := Stanzas}, {socket_send_first, NewStanzas}) when is_list(NewStanzas) ->
    C2SAcc#{socket_send := Stanzas ++ lists:reverse(NewStanzas)};
to_c2s_acc(C2SAcc = #{socket_send := Stanzas}, {socket_send_first, Stanza}) ->
    C2SAcc#{socket_send := Stanzas ++ [Stanza]};
to_c2s_acc(C2SAcc = #{actions := Actions}, {stop, Reason}) ->
    C2SAcc#{actions := [{next_event, cast, {stop, Reason}} | Actions]};
to_c2s_acc(C2SAcc, {Key, NewValue}) ->
    #{Key := _OldValue} = C2SAcc,
    C2SAcc#{Key := NewValue}.
