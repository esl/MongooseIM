-module(ejabberd_c2s_info_handler).

-include("ejabberd_c2s.hrl").
-include("mongoose.hrl").

% API for other modules
-export([add/5, remove/2]).

% API for ejabberd_c2s
-export([get_for/2,
         add_to_state/5,
         remove_from_state/2,
         safe_call/3,
         safe_call/6]).

add(C2S, Tag, M, F, HandlerState) ->
    p1_fsm_old:send_all_state_event(C2S, {add_info_handler, Tag, M, F, HandlerState}).

remove(C2S, Tag) ->
    p1_fsm_old:send_all_state_event(C2S, {remove_info_handler, Tag}).

get_for(Tag, #state{ info_handlers = InfoHandlers }) ->
    maps:get(Tag, InfoHandlers, undefined).

add_to_state(Tag, M, F, HandlerState, #state{ info_handlers = InfoHandlers0 } = C2SState) ->
    C2SState#state{ info_handlers = InfoHandlers0#{ Tag => {M, F, HandlerState} } }.

remove_from_state(Tag, #state{ info_handlers = InfoHandlers0 } = C2SState) ->
    C2SState#state{ info_handlers = maps:remove(Tag, InfoHandlers0) }.

safe_call(Tag, Data, C2SState) ->
    case get_for(Tag, C2SState) of
        {M, F, HandlerState} ->
            case safe_call(Tag, Data, M, F, HandlerState, C2SState) of
                {error, _} ->
                    C2SState;
                NC2SState0 ->
                    NC2SState0
            end;
        _ ->
            undefined
    end.

safe_call(Tag, Data, M, F, HandlerState, C2SState) ->
    try M:F(Data, HandlerState, C2SState) of
        {NewHandlerState, NewC2SState0} ->
            % Perhaps may be optimized?
            add_to_state(Tag, M, F, NewHandlerState, NewC2SState0)
    catch
        C:R:S ->
            ?ERROR_MSG("event=custom_c2s_info_handler_crash,tag=~p,data=~p,m=~p,f=~p,"
                       "extra=~p,class=~p,reason=~p,stacktrace=~1000p",
                       [Tag, Data, M, F, HandlerState, C, R, S]),
            {error, {C, R, S}}
    end.

