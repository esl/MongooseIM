%%%-------------------------------------------------------------------
%%% @doc
%%% This module a wpool worker capable of establishing HTTP/2
%%% connections using the `Gun' library.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_gun_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {host :: inet:hostname() | inet:ip_address(),
                port :: inet:port_number(),
                opts :: gun:opts(),
                pid :: pid() | undefined,
                monitor :: reference() | undefined,
                protocol :: http | http2 | undefined,
                requests = #{} :: #{reference() := {request(), response_data()}}}).

-record(response_data, {from :: {pid(), term()},
                        timestamp :: integer(),
                        timeout_timer :: reference(),
                        status :: binary() | undefined,
                        headers :: gun:headers(),
                        acc = <<>> :: binary()}).

-type response_data() :: #response_data{}.

-type request() :: {request,
                    Path :: iodata(),
                    Method :: iodata(),
                    Headers :: gun:headers(),
                    Query :: iodata(),
                    Retries :: non_neg_integer(),
                    Timeout :: non_neg_integer()}.

-include("mongoose_logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(Destination, Options) ->
    gen_server:start_link(?MODULE, {Destination, Options}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init({{inet:hostname() | inet:ip_address(), inet:port_number()}, gun:opts()}) ->
    {ok, State :: #state{}, {continue, init}}.
init({{Host, Port}, Opts}) ->
    {H, _P, O} = parse_uri(Host),
    {ok, #state{host = H, port = Port, opts = maps:merge(O, Opts), requests = #{}},
     {continue, init}};
init({Host, Opts}) ->
    {H, P, O} = parse_uri(Host),
    {ok, #state{host = H, port = P, opts = maps:merge(O, Opts), requests = #{}},
     {continue, init}}.

handle_continue(init, State) ->
    NewState = open_connection(State),
    {noreply, NewState}.

-spec handle_call(Request :: request(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_call({request, FullPath, Method, Headers, Query, _Retries, Timeout} = Request,
            From,
            #state{requests = Requests} = State) ->

    Now = erlang:monotonic_time(millisecond),
    LHeaders = lowercase_headers(Headers),

    {StreamRef, TRef} = queue_request(FullPath, Method, LHeaders, Query, Timeout, State#state.pid),
    NewRequests = Requests#{StreamRef => {Request, #response_data{from = From,
                                                                  timestamp = Now,
                                                                  timeout_timer = TRef}}},
    {noreply, State#state{requests = NewRequests}}.

-spec handle_cast(any(), State :: #state{}) -> {noreply, State :: #state{}}.
handle_cast(_R, State) ->
    {noreply, State}.

%% @doc Handles gun messages.
%% `gun_response' is a message that informs about the response to the request
%% and provides the response headers and status. If there is no body, the `fin'
%% flag is set.
handle_info({gun_response, ConnPid, StreamRef, fin, Status, Headers},
            #state{pid = ConnPid, requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State};
        {_Req, ResData} ->
            erlang:cancel_timer(ResData#response_data.timeout_timer),
            Now = erlang:monotonic_time(millisecond),
            gen_server:reply(ResData#response_data.from,
                {ok, {{integer_to_binary(Status), reason},
                       Headers,
                       no_data,
                       0,
                       ResData#response_data.timestamp - Now}}),
            {noreply, State#state{requests = maps:remove(StreamRef, Requests)}}
    end;
handle_info({gun_response, ConnPid, StreamRef, nofin, Status, Headers},
            #state{pid = ConnPid, requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State};
        {Req, ResData} ->
            NewData = ResData#response_data{status = integer_to_binary(Status), headers = Headers},
            {noreply, State#state{requests = Requests#{StreamRef := {Req, NewData}}}}
    end;
%% `gun_data' is a message that carries the response body. With the last part of
%% the body, the `fin' flag is set.
handle_info({gun_data, ConnPid, StreamRef, nofin, Data},
            #state{pid = ConnPid, requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State};
        {Req, ResData} ->
            Acc = ResData#response_data.acc,
            NewData = ResData#response_data{acc = <<Acc/binary, Data/binary>>},
            {noreply, State#state{requests = Requests#{StreamRef := {Req, NewData}}}}
    end;
handle_info({gun_data, ConnPid, StreamRef, fin, Data},
            #state{pid = ConnPid, requests = Requests} = State) ->
    Now = erlang:monotonic_time(millisecond),
    case maps:get(StreamRef, Requests, undefined) of
    undefined ->
        {noreply, State};
    {_Req, ResData} ->
        erlang:cancel_timer(ResData#response_data.timeout_timer),
        Acc = ResData#response_data.acc,
        NewData = ResData#response_data{acc = <<Acc/binary, Data/binary>>},
        gen_server:reply(NewData#response_data.from,
                        {ok, {{NewData#response_data.status, reason},
                            NewData#response_data.headers,
                            NewData#response_data.acc,
                            byte_size(NewData#response_data.acc),
                            NewData#response_data.timestamp - Now}}),
        {noreply, State#state{requests = maps:remove(StreamRef, Requests)}}
    end;
%% `timeout' is a message responsible for terminating and restarting requests
%% that take too long to complete. There is no such functionality in Gun, so it
%% is sent from this module.
handle_info({timeout, StreamRef}, #state{requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State}; % The request was already removed, but the timer sent a message while
                              % it was being processed. This should be a very rare case.
        {Req, Res} ->
            New = retry_request(State#state.pid, {Req, Res}, #{}),
            Requests1 = maps:remove(StreamRef, Requests),
            Requests2 = maps:merge(New, Requests1),

            {noreply, State#state{requests = Requests2}}
    end;
%% `gun_down' is a message informing that Gun has lost the connection.
%% It may try to reconnect, according to the `retry' and `retry_timeout' options,
%% passed in the init options. They default to 5 retries, each with 5 second
%% timeout.
handle_info({gun_down, PID, Protocol, Reason, KilledStreams, UnprocessedStreams}, State) ->
    ?WARNING_MSG("gun_down in mongoose_gun_worker. "
                 "Gun has lost the ~p connection ~p because of \"~p\", killing ~p streams.",
                 [Protocol, PID, Reason, length(KilledStreams) + length(UnprocessedStreams)]),
    ?DEBUG("gun_down in mongoose_gun_worker. Killed streams: ~p. Unprocessed streams: ~p",
           [KilledStreams, UnprocessedStreams]),
    {noreply, State};
%% `gun_up' is a message informing that Gun has reconnected after a `gun_down'.
handle_info({gun_up, ConnPid, _Protocol},
            State = #state{pid = ConnPid}) ->
    ?DEBUG("gun_up in mongoose_gun_worker. Connection is back up with PID: ~p", [ConnPid]),
    {noreply, State};
%% `gun_error' is a message informing about any errors concerning connections or
%% streams handled by Gun.
handle_info({gun_error, ConnPid, Reason},
            State = #state{pid = ConnPid}) ->
    ?WARNING_MSG("gun_error in mongoose_gun_worker. Reason: ~p.", [Reason]),
    {noreply, State};
handle_info({gun_error, ConnPid, StreamRef, Reason},
            State = #state{pid = ConnPid}) ->
    ?WARNING_MSG("gun_error in mongoose_gun_worker. Reason: ~p. Stream reference: ~p",
                 [Reason, StreamRef]),
    {noreply, State};
%% After `retry' number of `gun_down' messages, the connection process dies.
%% Because it is monitored, we receive a `DOWN' message and may restart the
%% connection.
handle_info({'DOWN', MRef, process, ConnPid, Reason},
            #state{pid = ConnPid, monitor = MRef} = State) ->
    ?WARNING_MSG("Mongoose_gun_worker has lost the connection with PID ~p. Reason ~p.",
                 [ConnPid, Reason]),
    ConnectedState = open_connection(State),
    NewState = retry_all(ConnectedState),
    {noreply, NewState};
handle_info(M, S) ->
    ?ERROR_MSG("Unexpected message in gun_worker ~p", [M]),
    {noreply, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_uri(Host) ->
    M = uri_string:parse(Host),
    H = maps:get(host, M, "") ++ maps:get(path, M, ""),
    Tls = case maps:get(scheme, M, undefined) of
        "https" -> #{transport => tls};
              _ -> #{}
    end,
    {H, maps:get(port, M, undefined), maps:merge(default_opts(), Tls)}.

open_connection(State) ->
    {ok, PID} = gun:open(State#state.host, State#state.port, State#state.opts),
    MonitorRef = monitor(process, PID),
    {ok, Protocol} = gun:await_up(PID, 10000, MonitorRef),
    State#state{pid = PID, protocol = Protocol, monitor = MonitorRef}.

queue_request(FullPath, Method, LHeaders, Query, Timeout, PID) ->
    StreamRef = gun:request(PID, Method, FullPath, LHeaders, Query),
    TRef = erlang:send_after(Timeout, self(), {timeout, StreamRef}),
    {StreamRef, TRef}.

retry_all(State) ->
    Requests = State#state.requests,
    NewRequests = maps:fold(fun(_K, V, Acc) -> retry_request(State#state.pid, V, Acc) end,
                            #{},
                            Requests),
    State#state{requests = NewRequests}.

retry_request(PID, {Req, Res}, ReqAcc) ->
    {request, FullPath, Method, Headers, Query, Retries, Timeout} = Req,
    LHeaders = lowercase_headers(Headers),
    erlang:cancel_timer(Res#response_data.timeout_timer),
    case Retries of
        0 ->
            ?DEBUG("Mongoose_gun_worker dropping request ~w", [Req]),
            gen_server:reply(Res#response_data.from, {error, request_timeout}),
            ReqAcc;
        _N ->
            ?DEBUG("Mongoose_gun_worker retrying request ~w", [Req]),
            {NewStreamRef, TRef} = queue_request(FullPath, Method, LHeaders, Query, Timeout, PID),

            ReqAcc#{NewStreamRef => {
                {request, FullPath, Method, LHeaders, Query, Retries - 1, Timeout},
                #response_data{from = Res#response_data.from,
                               timestamp = erlang:monotonic_time(millisecond),
                               timeout_timer = TRef}}}
    end.

default_opts() ->
    #{}.

lowercase_headers(Headers) ->
    [{string:lowercase(K), V} || {K, V} <- Headers].
