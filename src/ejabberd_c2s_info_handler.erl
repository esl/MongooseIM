-module(ejabberd_c2s_info_handler).

-include("ejabberd_c2s.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

% API for other modules
-export([add/3, remove/2, call/3]).

-callback handle_c2s_info(Args :: term(), HandlerState :: term(), C2SParams :: map()) ->
      any().

-type handler_tag() :: atom().
-type handler_state() :: term().

% API for ejabberd_c2s
-export([get_for/2,
         add_to_state/3,
         remove_from_state/2,
         safe_call/4]).

-spec add(pid(), atom(), term()) -> ok.
add(C2S, Tag, HandlerState) ->
    p1_fsm_old:send_all_state_event(C2S, {add_info_handler, Tag, HandlerState}).

-spec remove(pid(), atom()) -> ok.
remove(C2S, Tag) ->
    p1_fsm_old:send_all_state_event(C2S, {remove_info_handler, Tag}).

-spec call(jid:jid() | pid(), atom(), term()) -> ok | session_not_found.
call(#jid{} = JID, Tag, Data) ->
    case ejabberd_sm:get_session_pid(JID) of
        none ->
            session_not_found;
        Pid ->
            call(Pid, Tag, Data)
    end;
call(FsmRef, Tag, Data) ->
    FsmRef ! {call_info_handler, Tag, Data},
    ok.

-spec get_for(handler_tag(), state()) -> {handler_tag(), handler_state()} | no_handler.
get_for(Tag, #state{ info_handlers = InfoHandlers }) ->
    maps:get(Tag, InfoHandlers, no_handler).

-spec add_to_state(handler_tag(), handler_state(), state()) -> state().
add_to_state(Tag, HandlerState, #state{ info_handlers = InfoHandlers0 } = C2SState) ->
    C2SState#state{ info_handlers = InfoHandlers0#{ Tag => {Tag, HandlerState} } }.

-spec remove_from_state(handler_tag(), state()) -> state().
remove_from_state(Tag, #state{ info_handlers = InfoHandlers0 } = C2SState) ->
    C2SState#state{ info_handlers = maps:remove(Tag, InfoHandlers0) }.

-spec safe_call(handler_tag(), term(), handler_state(), state()) -> state() | {error, term()}.
safe_call(Tag, Data, HandlerState, C2SState) ->
    Jid = ejabberd_c2s_state:jid(C2SState),
    Server = ejabberd_c2s_state:server(C2SState),
    try Tag:handle_c2s_info(Data, HandlerState, #{jid => Jid, server => Server}) of
        HandlerState ->
            C2SState;
        NewHandlerState ->
            add_to_state(Tag, NewHandlerState, C2SState)
    catch
        C:R:S ->
            ?ERROR_MSG("event=custom_c2s_info_handler_crash,tag=~p,data=~p,"
                       "extra=~p,class=~p,reason=~p,stacktrace=~1000p",
                       [Tag, Data, HandlerState, C, R, S]),
            {error, {C, R, S}}
    end.

