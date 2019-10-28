%%%===================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc HTTP routing layer for MongooseIM's Cowboy listener
%%%
%%% Allows to set more than one handler for an endpoint:
%%% - one handler for http
%%% - one handler for each WS protocol
%%%
%%% It's a proxy between cowboy and actual handlers.
%%%
%%% So, this module handles calls from cowboy, and forwards the calls to the
%%% handler modules.
%%%
%%% I.e. it both implements and uses the cowboy_handler and cowboy_websocket
%%% behaviours.
%%% @end
%%%===================================================================
-module(mod_cowboy).

-behaviour(cowboy_handler).
-behaviour(cowboy_websocket).

%% common callbacks
-export([init/2,
     terminate/3]).

%% cowboy_websocket callbacks
-export([websocket_init/1,
     websocket_handle/2,
     websocket_info/2]).

-record(state, {handler,
                handler_state,
                handler_opts,
                protocol :: ws | http,
                ws_protocol}).

-type option() :: {http, module()} | {ws, atom(), module()}.
-type state() :: #state{}.

-include("mongoose_logger.hrl").

%%--------------------------------------------------------------------
%% common callbacks
%%--------------------------------------------------------------------

-spec init(cowboy_req:req(), [option()])
-> {ok, cowboy_req:req(), state() | no_state} |
   % upgrade protocol:
   {cowboy_websocket, cowboy_req:req(), state()}.
init(Req, Opts) ->
    try
        init_unsafe(Req, Opts)
    catch
        Class:Reason:StackTrace ->
              %% Because cowboy ignores stacktraces
              %% and we can get a cryptic error like {crash,error,undef}
              ?ERROR_MSG("issue=init_failed "
                          "reason=~p:~p stacktrace=~1000p",
                         [Class, Reason, StackTrace]),
              erlang:raise(Class, Reason, StackTrace)
    end.

init_unsafe(Req, Opts) ->
    case protocol(Req) of
        ws ->
            handle_ws_init(Req, Opts);
        http ->
            handle_http_init(Req, Opts);
        _ ->
            Req = cowboy_req:reply(404, Req),
            {ok, Req, no_state}
    end.

-spec terminate(any(), cowboy_req:req(), state() | no_state) -> ok.
terminate(_Reason, _Req, #state{handler=undefined}) ->
    ok;
terminate(_Reason, _Req, no_state) -> %% failed to init
    ok;
terminate(Reason, Req, #state{handler = Handler, handler_state = HandlerState, protocol = ws}) ->
    Handler:websocket_terminate(Reason, Req, HandlerState);
terminate(Reason, Req, #state{handler = Handler, handler_state = HandlerState, protocol = http}) ->
    Handler:terminate(Reason, Req, HandlerState).

%%--------------------------------------------------------------------
%% cowboy_websocket_handler callbacks
%%--------------------------------------------------------------------
websocket_init(State) ->
    handle_websocket_init(State).

websocket_handle(InFrame, State) ->
    try
        websocket_handle_unsafe(InFrame, State)
    catch
        Class:Reason:StackTrace ->
              %% Because cowboy ignores stacktraces
              %% and we can get a cryptic error like {crash,error,undef}
              ?ERROR_MSG("issue=websocket_handle_failed "
                          "reason=~p:~p stacktrace=~1000p",
                         [Class, Reason, StackTrace]),
              erlang:raise(Class, Reason, StackTrace)
    end.

websocket_handle_unsafe(InFrame,
                 #state{handler=Handler, handler_state=HandlerState}=State) ->
    case Handler:websocket_handle(InFrame, HandlerState) of
        {ok, HandlerState1} ->
            {ok, update_handler_state(State, HandlerState1)};
        {ok, HandlerState1, hibernate} ->
            {ok, update_handler_state(State, HandlerState1), hibernate};
        {reply, OutFrame, HandlerState1} ->
            {reply, OutFrame, update_handler_state(State, HandlerState1)};
        {reply, OutFrame, HandlerState1, hibernate} ->
            {reply, OutFrame, update_handler_state(State, HandlerState1),
             hibernate};
        {stop, HandlerState1} ->
            {stop, update_handler_state(State, HandlerState1)}
    end.

websocket_info(Info, State) ->
    try
        websocket_info_unsafe(Info, State)
    catch
        Class:Reason:StackTrace ->
              %% Because cowboy ignores stacktraces
              %% and we can get a cryptic error like {crash,error,undef}
              ?ERROR_MSG("issue=websocket_info_failed "
                          "reason=~p:~p stacktrace=~1000p",
                         [Class, Reason, StackTrace]),
              erlang:raise(Class, Reason, StackTrace)
    end.

websocket_info_unsafe(Info,
                      #state{handler=Handler, handler_state=HandlerState}=State) ->
    case Handler:websocket_info(Info, HandlerState) of
        {ok, HandlerState1} ->
            {ok, update_handler_state(State, HandlerState1)};
        {ok, HandlerState1, hibernate} ->
            {ok, update_handler_state(State, HandlerState1), hibernate};
        {reply, OutFrame, HandlerState1} ->
            {reply, OutFrame, update_handler_state(State, HandlerState1)};
        {reply, OutFrame, HandlerState1, hibernate} ->
            {reply, OutFrame, update_handler_state(State, HandlerState1),
             hibernate};
        {stop, HandlerState1} ->
            {stop, update_handler_state(State, HandlerState1)}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
handle_http_init(Req, Opts) ->
    case http_handler(Opts) of
        {Handler, HandlerOpts} ->
            init_http_handler(Handler, Req, HandlerOpts);
        _ ->
             Req1 = cowboy_req:reply(404, Req),
            {stop, Req1, no_state}
    end.

handle_ws_init(Req, Opts) ->
    %% Make the same check as in websocket_init
    %% We should return 404, if ws subprotocol is not supported
    Protocol = ws_protocol(Req),
    case ws_handler(Protocol, Opts) of
        {Handler, HandlerOpts} ->
            init_ws_handler(Handler, Req, HandlerOpts, Protocol);
        _ ->
            Req2 = cowboy_req:reply(404, Req),
            {stop, Req2, no_state}
    end.

init_http_handler(Handler, Req, Opts) ->
    case Handler:init(Req, Opts) of
        {ok, Req1, HandlerState} ->
            {ok, Req1, init_state(Handler, Opts, HandlerState, http)};
        {stop, Req1, HandlerState} ->
            {stop, Req1, init_state(Handler, Opts, HandlerState, http)}
    end.

init_ws_handler(Handler, Req, Opts, Protocol) ->
    case Handler:init(Req, Opts) of
        {cowboy_websocket, Req1, HandlerState} ->
            State = init_state(Handler, Opts, HandlerState, ws, Protocol),
            %% Ask cowboy to call websocket_init/1 next
            {cowboy_websocket, Req1, State}
    end.

handle_websocket_init(State=#state{handler=Handler, handler_state=HandlerState}) ->
    case Handler:websocket_init(HandlerState) of
        {ok, HandlerState1} ->
            {ok, update_handler_state(State, HandlerState1)};
        {ok, HandlerState1, hibernate} ->
            {ok, update_handler_state(State, HandlerState1), hibernate};
        {stop, HandlerState1} ->
            {stop, HandlerState1}
    end.

http_handler(Handlers) ->
    case lists:keyfind(http, 1, Handlers) of
        {http, Handler, Opts} ->
            {Handler, Opts};
        {http, Handler} ->
            {Handler, []};
        _ ->
            undefined
    end.

ws_handler(undefined, _) ->
    undefined;
ws_handler(_Protocol, []) ->
    undefined;
ws_handler(Protocol, [{ws, ProtocolAtom, Handler}|Tail]) ->
    case atom_to_binary(ProtocolAtom, utf8) of
        Protocol -> {Handler, []};
        _        -> ws_handler(Protocol, Tail)
    end;
ws_handler(Protocol, [{ws, ProtocolAtom, Handler, Opts}|Tail]) ->
    case atom_to_binary(ProtocolAtom, utf8) of
        Protocol -> {Handler, Opts};
        _        -> ws_handler(Protocol, Tail)
    end;
ws_handler(Protocol, [_|Tail]) ->
    ws_handler(Protocol, Tail).

protocol(Req) ->
    case cowboy_req:header(<<"upgrade">>, Req) of
        <<"websocket">> ->
            ws;
        undefined ->
            http;
        _ ->
            undefined
    end.

ws_protocol(Req) ->
    cowboy_req:header(<<"sec-websocket-protocol">>, Req).

init_state(Handler, Opts, State, Protocol) ->
    init_state(Handler, Opts, State, Protocol, undefined).

init_state(Handler, Opts, State, Protocol, WsProto) ->
    #state{handler = Handler,
           handler_opts = Opts,
           handler_state = State,
           protocol = Protocol,
           ws_protocol = WsProto}.

update_handler_state(State, HandlerState) ->
    State#state{handler_state = HandlerState}.
