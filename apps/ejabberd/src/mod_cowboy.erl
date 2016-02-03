%%%===================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc HTTP routing layer for MongooseIM's Cowboy listener
%%% @end
%%%===================================================================
-module(mod_cowboy).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

%% common callbacks
-export([init/3]).

%% cowboy_http_handler callbacks
-export([handle/2,
         terminate/3]).

%% cowboy_websocket_handler callbacks
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, {handler, handler_state, handler_opts}).

-type option() :: {atom(), any()}.
-type state() :: #state{}.

%%--------------------------------------------------------------------
%% common callback
%%--------------------------------------------------------------------
-spec init({atom(), http}, cowboy_req:req(), [option()])
    -> {ok, cowboy_req:req(), state()} |
       {shutdown, cowboy_req:req(), state()} |
       {upgrade, protocol, cowboy_websocket, cowboy_req:req(), state()}.
init(Transport, Req, Opts) ->
    case protocol(Req) of
        {ws, Req1} ->
            {upgrade, protocol, cowboy_websocket, Req1, Opts};
        {http, Req1} ->
            handle_http_init(Transport, Req1, Opts);
        _ ->
            {ok, Req1} = cowboy_req:reply(404, Req),
            {shutdown, Req1, #state{}}
    end.

%%--------------------------------------------------------------------
%% cowboy_http_handler callbacks
%%--------------------------------------------------------------------
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, #state{handler=Handler, handler_state=HandlerState}=State) ->
    {ok, Req1, HandlerState1} = Handler:handle(Req, HandlerState),
    {ok, Req1, update_handler_state(State, HandlerState1)}.

-spec terminate(any(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, #state{handler=undefined}) ->
    ok;
terminate(Reason, Req, #state{handler=Handler, handler_state=HandlerState}) ->
    Handler:terminate(Reason, Req, HandlerState).

%%--------------------------------------------------------------------
%% cowboy_websocket_handler callbacks
%%--------------------------------------------------------------------
websocket_init(Transport, Req, Opts) ->
    handle_ws_init(Transport, Req, Opts).

websocket_handle(InFrame, Req,
                 #state{handler=Handler, handler_state=HandlerState}=State) ->
    case Handler:websocket_handle(InFrame, Req, HandlerState) of
        {ok, Req1, HandlerState1} ->
            {ok, Req1, update_handler_state(State, HandlerState1)};
        {ok, Req1, HandlerState1, hibernate} ->
            {ok, Req1, update_handler_state(State, HandlerState1), hibernate};
        {reply, OutFrame, Req1, HandlerState1} ->
            {reply, OutFrame, Req1, update_handler_state(State, HandlerState1)};
        {reply, OutFrame, Req1, HandlerState1, hibernate} ->
            {reply, OutFrame, Req1, update_handler_state(State, HandlerState1),
             hibernate};
        {shutdown, Req1, HandlerState1} ->
            {shutdown, Req1, update_handler_state(State, HandlerState1)}
    end.

websocket_info(Info, Req,
               #state{handler=Handler, handler_state=HandlerState}=State) ->
    case Handler:websocket_info(Info, Req, HandlerState) of
        {ok, Req1, HandlerState1} ->
            {ok, Req1, update_handler_state(State, HandlerState1)};
        {ok, Req1, HandlerState1, hibernate} ->
            {ok, Req1, update_handler_state(State, HandlerState1), hibernate};
        {reply, OutFrame, Req1, HandlerState1} ->
            {reply, OutFrame, Req1, update_handler_state(State, HandlerState1)};
        {reply, OutFrame, Req1, HandlerState1, hibernate} ->
            {reply, OutFrame, Req1, update_handler_state(State, HandlerState1),
             hibernate};
        {shutdown, Req1, HandlerState1} ->
            {shutdown, Req1, update_handler_state(State, HandlerState1)}
    end.

websocket_terminate(_Reason, _Req, #state{handler=undefined}) ->
    ok;
websocket_terminate(Reason, Req,
                    #state{handler=Handler, handler_state=HandlerState}) ->
    Handler:websocket_terminate(Reason, Req, HandlerState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
handle_http_init(Transport, Req, Opts) ->
    case http_handler(Opts) of
        {Handler, HandlerOpts} ->
            init_http_handler(Handler, Transport, Req, HandlerOpts);
        _ ->
            {ok, Req1} = cowboy_req:reply(404, Req),
            {shutdown, Req1, #state{}}
    end.

handle_ws_init(Transport, Req, Opts) ->
    {Protocol, Req1} = ws_protocol(Req),
    case ws_handler(Protocol, Opts) of
        {Handler, HandlerOpts} ->
            init_ws_handler(Handler, Transport, Req1, HandlerOpts);
        _ ->
            {ok, Req2} = cowboy_req:reply(404, Req1),
            {shutdown, Req2}
    end. 

init_http_handler(Handler, Transport, Req, Opts) ->
    case Handler:init(Transport, Req, Opts) of
        {ok, Req1, HandlerState} ->
            {ok, Req1, init_state(Handler, Opts, HandlerState)};
        {shutdown, Req1, HandlerState} ->
            {shutdown, Req1, init_state(Handler, Opts, HandlerState)}
    end.

init_ws_handler(Handler, Transport, Req, Opts) ->
    case Handler:websocket_init(Transport, Req, Opts) of
        {ok, Req1, HandlerState} ->
            {ok, Req1, init_state(Handler, Opts, HandlerState)};
        {ok, Req1, HandlerState, hibernate} ->
            {ok, Req1, init_state(Handler, Opts, HandlerState), hibernate};
        {ok, Req1, HandlerState, Timeout} ->
            {ok, Req1, init_state(Handler, Opts, HandlerState), Timeout};
        {ok, Req1, HandlerState, Timeout, hibernate} ->
            {ok, Req1, init_state(Handler, Opts, HandlerState),
             Timeout, hibernate};
        {shutdown, Req1} ->
            {shutdown, Req1}
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
        {<<"websocket">>, Req1} ->
            {ws, Req1};
        {undefined, Req1} ->
            {http, Req1};
        {_, Req1} ->
            {undefined, Req1}
    end.

ws_protocol(Req) ->
    cowboy_req:header(<<"sec-websocket-protocol">>, Req).

init_state(Handler, Opts, State) ->
    #state{handler = Handler,
           handler_opts = Opts,
           handler_state = State}.

update_handler_state(State, HandlerState) ->
    State#state{handler_state = HandlerState}.
