-module(mongoose_c2s).

-behaviour(gen_statem).
-include("mongoose_logger.hrl").

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4]).

-record(state, {
          ref :: reference(),
          transport,
          socket
         }).
-type state() :: #state{}.
-type fsm_res() :: gen_statem:event_handler_result(state()).

-type retries() :: pos_integer().
-type stream_state() :: stream_start | authenticated.
-type feature_state() :: before_auth | after_auth.
-type fsm_state() :: connecting
                   | {wait_for_stream, stream_state()}
                   | {wait_for_feature, feature_state(), retries()}
                   | {wait_for_sasl_response, retries()}
                   | session_established
                   | resume_session.

-export_type([state/0]).

%%%----------------------------------------------------------------------
%%% gen_statem
%%%----------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init({ranch:ref(), module(), map()}) -> gen_statem:init_result(state()).
init({Ref, Transport, Opts}) ->
    StateData = #state{ref = Ref, transport = Transport},
    gen_statem:cast(self(), connect),
    {ok, connecting, StateData}.

-spec handle_event(gen_statem:event_type(), term(), fsm_state(), state()) -> fsm_res().
handle_event(cast, connect, connecting, StateData = #state{ref = Ref, transport = Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    Transport:setopts(Socket, [{active, once}]),
    NewState = StateData#state{socket = Socket},
    {next_state, {wait_for_stream, stream_start}, NewState};

handle_event(EventType, EventContent, FsmState, StateData) ->
    ?LOG_DEBUG(#{what => unknown_info_event, stuff => [EventType, EventContent, FsmState, StateData]}),
    keep_state_and_data.

terminate(Reason, FsmState, StateData) ->
    ?LOG_DEBUG(#{what => c2s_statem_terminate, stuff => [Reason, FsmState, StateData]}),
	ok.
