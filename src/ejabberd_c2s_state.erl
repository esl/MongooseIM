-module(ejabberd_c2s_state).

-include("ejabberd_c2s.hrl").

-export([server/1, jid/1]).
-export([get_handler_state/2, set_handler_state/3]).
-export([privacy_list/1]).

server(#state{ server = Server }) ->
    Server.

jid(#state{ jid = JID }) ->
    JID.

privacy_list(#state{ privacy_list = PrivacyList}) ->
    PrivacyList.

get_handler_state(HandlerName, StateData) ->
    maps:get(HandlerName, StateData#state.handlers, empty_state).


set_handler_state(HandlerName, NewHandlerState, StateData) ->
    NewStates = maps:put(HandlerName, NewHandlerState, StateData#state.handlers),
    StateData#state{handlers = NewStates}.
