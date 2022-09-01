%% @doc Interface for actions that handler modules might request `mongoose_c2s' to act upon.
%% Note that some of the keys take a list of elements, which, in the case of a hook with many
%% handlers, means that at the end of the queue `mongoose_c2s' will get a list of requests
%% by different modules. These will be acted upon in the same order they were inserted in the
%% hook list, according to their priority.
%% The following keys are defined:
%% - `handlers': key-value pairs of gen_mod module names and their desired state.
%% - `actions': a list of valid `gen_statem:action()' to request the `mongoose_c2s' engine.
%% - `c2s_state': a new state is requested for the state machine.
%% - `c2s_data': a new state data is requested for the state machine.
%% - `stop': an action of type `{info, {stop, Reason}}' is to be triggered.
%% - `hard_stop': no other request is allowed, the state machine is immediatly triggered to stop.
%% - `socket_send': xml elements to send on the socket to the user.
-module(mongoose_c2s_acc).

-export([new/0, new/1,
         get_statem_result/1,
         from_mongoose_acc/2,
         to_acc/3, to_acc_many/2
        ]).

-type key() :: handlers | actions | c2s_state | c2s_data | stop | hard_stop | socket_send.
-type pairs() :: {handlers, {module(), term()}}
               | {actions, gen_statem:action()}
               | {c2s_state, mongoose_c2s:c2s_state()}
               | {c2s_data, mongoose_c2s:c2s_data()}
               | {stop, term() | {shutdown, atom()}}
               | {hard_stop, term() | {shutdown, atom()}}
               | {socket_send, exml:element()}.

-type t() :: #{
        handlers := #{module() => term()},
        actions := [gen_statem:action()],
        c2s_state := undefined | mongoose_c2s:c2s_state(),
        c2s_data := undefined | mongoose_c2s:c2s_data(),
        hard_stop := undefined | Reason :: term(),
        socket_send := exml:element() | [exml:element()]
       }.

-type params() :: #{
        handlers => #{module() => term()},
        actions => [gen_statem:action()],
        c2s_state => mongoose_c2s:c2s_state(),
        c2s_data => mongoose_c2s:c2s_data(),
        stop => Reason :: term(),
        hard_stop => Reason :: term(),
        socket_send => exml:element() | [exml:element()]
       }.

-export_type([t/0]).

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------

-spec new() -> t().
new() ->
    #{
      handlers => #{},
      actions => [],
      c2s_state => undefined,
      c2s_data => undefined,
      hard_stop => undefined,
      socket_send => []
     }.

-spec new(params()) -> t().
new(Params = #{stop := Reason}) ->
    WithoutStop = maps:remove(stop, Params),
    NewAction = [{next_event, info, {stop, Reason}}],
    Fun = fun(Actions) -> [NewAction | Actions] end,
    NewParams = maps:update_with(actions, Fun, NewAction, WithoutStop),
    new(NewParams);
new(T) ->
    maps:merge(new(), T).

-spec get_statem_result(mongoose_acc:t()) -> mongoose_c2s_acc:t().
get_statem_result(Acc) ->
    CAcc = #{actions := Actions,
             socket_send := SocketSend} = mongoose_acc:get_statem_acc(Acc),
    CAcc#{actions := lists:reverse(Actions),
          socket_send := lists:reverse(SocketSend)}.

-spec from_mongoose_acc(mongoose_acc:t(), key()) -> term().
from_mongoose_acc(Acc, Key) ->
    #{Key := Value} = mongoose_acc:get_statem_acc(Acc),
    Value.

-spec to_acc(mongoose_acc:t(), handlers, {atom(), term()}) -> mongoose_acc:t();
            (mongoose_acc:t(), actions, [gen_statem:action()]) -> mongoose_acc:t();
            (mongoose_acc:t(), actions, gen_statem:action()) -> mongoose_acc:t();
            (mongoose_acc:t(), c2s_state, term()) -> mongoose_acc:t();
            (mongoose_acc:t(), c2s_data, mongoose_c2s:c2s_data()) -> mongoose_acc:t();
            (mongoose_acc:t(), hard_stop, atom()) -> mongoose_acc:t();
            (mongoose_acc:t(), stop, atom() | {shutdown, atom()}) -> mongoose_acc:t();
            (mongoose_acc:t(), socket_send, exml:element()) -> mongoose_acc:t().
to_acc(Acc, handlers, {Name, Handler}) ->
    C2SAcc = mongoose_acc:get_statem_acc(Acc),
    C2SAcc1 = to_cacc(C2SAcc, handlers, {Name, Handler}),
    mongoose_acc:set_statem_acc(C2SAcc1, Acc);
to_acc(Acc, actions, Actions) when is_list(Actions) ->
    C2SAcc = mongoose_acc:get_statem_acc(Acc),
    C2SAcc1 = to_cacc(C2SAcc, actions, Actions),
    mongoose_acc:set_statem_acc(C2SAcc1, Acc);
to_acc(Acc, socket_send, Stanza) ->
    C2SAcc = mongoose_acc:get_statem_acc(Acc),
    C2SAcc1 = to_cacc(C2SAcc, socket_send, Stanza),
    mongoose_acc:set_statem_acc(C2SAcc1, Acc);
to_acc(Acc, stop, Reason) ->
    C2SAcc = mongoose_acc:get_statem_acc(Acc),
    C2SAcc1 = to_cacc(C2SAcc, stop, Reason),
    mongoose_acc:set_statem_acc(C2SAcc1, Acc);
to_acc(Acc, Key, NewValue) ->
    C2SAcc = mongoose_acc:get_statem_acc(Acc),
    C2SAcc1 = to_cacc(C2SAcc, Key, NewValue),
    mongoose_acc:set_statem_acc(C2SAcc1, Acc).

-spec to_acc_many(mongoose_acc:t(), [pairs()]) -> mongoose_acc:t().
to_acc_many(Acc, Pairs) ->
    CAcc = mongoose_acc:get_statem_acc(Acc),
    to_acc_many(Acc, CAcc, Pairs).

-spec to_acc_many(mongoose_acc:t(), mongoose_c2s_acc:t(), [pairs()]) -> mongoose_acc:t().
to_acc_many(Acc, CAcc, []) ->
    mongoose_acc:set_statem_acc(CAcc, Acc);
to_acc_many(Acc, CAcc, [{Key, Value} | Rest]) ->
    NewCAcc = to_cacc(CAcc, Key, Value),
    to_acc_many(Acc, NewCAcc, Rest).

-spec to_cacc(mongoose_c2s_acc:t(), handlers, {atom(), term()}) -> mongoose_c2s_acc:t();
             (mongoose_c2s_acc:t(), actions, [gen_statem:action()]) -> mongoose_c2s_acc:t();
             (mongoose_c2s_acc:t(), actions, gen_statem:action()) -> mongoose_c2s_acc:t();
             (mongoose_c2s_acc:t(), c2s_state, term()) -> mongoose_c2s_acc:t();
             (mongoose_c2s_acc:t(), c2s_data, mongoose_c2s:c2s_data()) -> mongoose_c2s_acc:t();
             (mongoose_c2s_acc:t(), hard_stop, atom()) -> mongoose_c2s_acc:t();
             (mongoose_c2s_acc:t(), stop, atom() | {shutdown, atom()}) -> mongoose_c2s_acc:t();
             (mongoose_c2s_acc:t(), socket_send, exml:element()) -> mongoose_c2s_acc:t().
to_cacc(CAcc = #{handlers := Handlers}, handlers, {Name, Handler}) ->
    CAcc#{handlers := Handlers#{Name => Handler}};
to_cacc(CAcc = #{actions := Actions}, actions, NewActions) when is_list(NewActions) ->
    CAcc#{actions := NewActions ++ Actions};
to_cacc(CAcc = #{actions := Actions}, actions, Action) ->
    CAcc#{actions := [Action | Actions]};
to_cacc(CAcc = #{socket_send := Stanzas}, socket_send, []) ->
    CAcc#{socket_send := Stanzas};
to_cacc(CAcc = #{socket_send := Stanzas}, socket_send, Stanza) ->
    CAcc#{socket_send := [Stanza | Stanzas]};
to_cacc(CAcc = #{actions := Actions}, stop, Reason) ->
    CAcc#{actions := [{next_event, info, {stop, Reason}} | Actions]};
to_cacc(CAcc, Key, NewValue) ->
    #{Key := _OldValue} = CAcc,
    CAcc#{Key := NewValue}.
