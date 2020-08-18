%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements a wpool worker capable of establishing HTTP/2
%%% connections using the `Gun' library.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_gun_worker).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-record(gun_worker_state, {
          host :: inet:hostname() | inet:ip_address(),
          port :: inet:port_number(),
          opts :: gun:opts(),
          pid  :: pid() | undefined,
          monitor :: reference() | undefined,
          requests = #{} :: #{reference() := {request(), response_data()}}
         }).
-type gun_worker_state() :: #gun_worker_state{}.

-record(response_data, {
          from :: {pid(), term()},
          timestamp :: integer(),
          timeout_timer :: reference(),
          status :: binary() | undefined,
          headers :: gun:headers(),
          acc = <<>> :: binary()
         }).
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
%%% gen_server callbacks
%%%===================================================================
-spec init({{inet:hostname() | inet:ip_address(), inet:port_number()}, gun:opts()}) ->
    {ok, State :: gun_worker_state(), {continue, init}}.
init({{Host, Port}, Opts}) ->
    {H, _P, O} = parse_uri_and_opts(Host, Opts),
    {ok, #gun_worker_state{host = H, port = Port, opts = O},
     {continue, init}};
init({Host, Opts}) ->
    {H, P, O} = parse_uri_and_opts(Host, Opts),
    {ok, #gun_worker_state{host = H, port = P, opts = O},
     {continue, init}}.

-spec handle_continue(term(), gun_worker_state()) ->
    {noreply, gun_worker_state()}.
handle_continue(init, State) ->
    NewState = launch_connection(State),
    {noreply, NewState}.

-spec handle_call(Request, From, State) -> Result when
      Request :: request(),
      From :: {pid(), term()},
      State :: gun_worker_state(),
      Result :: {noreply, gun_worker_state()}.
handle_call(Request, From, #gun_worker_state{pid = GunPid, requests = Requests} = State)
  when is_pid(GunPid) ->
    Now = erlang:monotonic_time(millisecond),
    LRequest = lowercase_request(Request),
    {StreamRef, TRef} = queue_request(LRequest, GunPid),
    ResponseData = #response_data{from = From, timestamp = Now, timeout_timer = TRef},
    NewRequests = Requests#{StreamRef => {LRequest, ResponseData}},
    {noreply, State#gun_worker_state{requests = NewRequests}}.

-spec handle_cast(any(), gun_worker_state()) -> {noreply, gun_worker_state()}.
handle_cast(_R, State) ->
    {noreply, State}.

%% @doc Handles gun messages.
%% `gun_response' is a message that informs about the response to the request
%% and provides the response headers and status. If there is no body, the `fin'
%% flag is set.
handle_info({gun_response, ConnPid, StreamRef, fin, Status, Headers},
            #gun_worker_state{pid = ConnPid, requests = Requests} = State) ->
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
            {noreply, State#gun_worker_state{requests = maps:remove(StreamRef, Requests)}}
    end;
handle_info({gun_response, ConnPid, StreamRef, nofin, Status, Headers},
            #gun_worker_state{pid = ConnPid, requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State};
        {Req, ResData} ->
            NewData = ResData#response_data{status = integer_to_binary(Status), headers = Headers},
            {noreply, State#gun_worker_state{requests = Requests#{StreamRef := {Req, NewData}}}}
    end;

%% `gun_data' is a message that carries the response body. With the last part of
%% the body, the `fin' flag is set.
handle_info({gun_data, ConnPid, StreamRef, nofin, Data},
            #gun_worker_state{pid = ConnPid, requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State};
        {Req, ResData} ->
            Acc = ResData#response_data.acc,
            NewData = ResData#response_data{acc = <<Acc/binary, Data/binary>>},
            {noreply, State#gun_worker_state{requests = Requests#{StreamRef := {Req, NewData}}}}
    end;
handle_info({gun_data, ConnPid, StreamRef, fin, Data},
            #gun_worker_state{pid = ConnPid, requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State};
        {_Req, ResData} ->
            Now = erlang:monotonic_time(millisecond),
            erlang:cancel_timer(ResData#response_data.timeout_timer),
            Acc = ResData#response_data.acc,
            NewData = ResData#response_data{acc = <<Acc/binary, Data/binary>>},
            gen_server:reply(NewData#response_data.from,
                             {ok, {{NewData#response_data.status, reason},
                                   NewData#response_data.headers,
                                   NewData#response_data.acc,
                                   byte_size(NewData#response_data.acc),
                                   NewData#response_data.timestamp - Now}}),
            {noreply, State#gun_worker_state{requests = maps:remove(StreamRef, Requests)}}
    end;

%% `timeout' is a message responsible for terminating and restarting requests
%% that take too long to complete. There is no such functionality in Gun, so it
%% is sent from this module.
handle_info({timeout, StreamRef}, #gun_worker_state{requests = Requests} = State) ->
    case maps:get(StreamRef, Requests, undefined) of
        undefined ->
            {noreply, State}; % The request was already removed, but the timer sent a message while
                              % it was being processed. This should be a very rare case.
        {_Req, ResData} ->
            gen_server:reply(ResData#response_data.from, {error, timeout}),
            Requests1 = maps:remove(StreamRef, Requests),
            {noreply, State#gun_worker_state{requests = Requests1}}
    end;

%% `gun_up' informs the owner process that the connection or reconnection completed.
handle_info({gun_up, ConnPid, _Protocol},
            State = #gun_worker_state{pid = ConnPid}) ->
    {noreply, State};

%% `gun_down' is a message informing that Gun has lost the connection.
%% It may try to reconnect, according to the `retry' and `retry_timeout' options,
%% passed in the init options.
handle_info({gun_down, PID, Protocol, Reason, KilledStreams, UnprocessedStreams}, State) ->
    ?LOG_WARNING(#{what => http_connection_down, reason => Reason, protocol => Protocol,
                   gun_pid => PID, streams_killed => length(KilledStreams) + length(UnprocessedStreams)}),
    ?LOG_DEBUG(#{what => http_connection_down, reason => Reason, killed_streams => KilledStreams,
                 unprocessed_streams => UnprocessedStreams}),
    {noreply, State};

%% `gun_error' is a message informing about any errors concerning connections or
%% streams handled by Gun.
handle_info({gun_error, ConnPid, Reason},
            State = #gun_worker_state{pid = ConnPid}) ->
    ?LOG_WARNING(#{what => http_connection_error, reason => Reason}),
    {noreply, State};
handle_info({gun_error, ConnPid, StreamRef, Reason},
            State = #gun_worker_state{pid = ConnPid}) ->
    ?LOG_WARNING(#{what => http_stream_error, reason => Reason, stream_reference => StreamRef}),
    {noreply, State};

%% After `retry' number of `gun_down' messages, the connection process dies.
%% Because it is monitored, we receive a `DOWN' message and may restart the
%% connection.
handle_info({'DOWN', MRef, process, ConnPid, Reason},
            #gun_worker_state{pid = ConnPid, monitor = MRef} = State) ->
    ?LOG_WARNING(#{what => http_connection_lost,
                   worker => ConnPid, reason => Reason}),
    ConnectedState = launch_connection(State),
    NewState = retry_all(ConnectedState),
    {noreply, NewState};

handle_info(M, S) ->
    ?LOG_ERROR(#{what => unexpected_gun_message, message => M}),
    {noreply, S}.

terminate(_Reason, #gun_worker_state{pid = undefined}) ->
    ok;
terminate(_Reason, #gun_worker_state{pid = GunPid, requests = Requests}) ->
    maps:fold(
     fun(StreamRef, {_Request, #response_data{from = From, timeout_timer = TRef}}, _) ->
             erlang:cancel_timer(TRef),
             gun:cancel(GunPid, StreamRef),
             gun:flush(StreamRef),
             gen_server:reply(From, {error, request_timeout})
     end, ok, Requests),
    gun:flush(GunPid),
    gun:shutdown(GunPid).

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_uri_and_opts(Host, Opts) ->
    M = uri_string:parse(Host),
    H = maps:get(host, M, "") ++ maps:get(path, M, ""),
    Tls = case maps:get(scheme, M, undefined) of
        "https" -> #{transport => tls};
              _ -> #{}
    end,
    O = maps:merge(maps:merge(default_opts(), Tls), Opts),
    {H, maps:get(port, M, undefined), O}.

launch_connection(#gun_worker_state{host = H, port = P, opts = Opts} = State) ->
    {ok, PID} = gun:open(H, P, Opts),
    MRef = monitor(process, PID),
    State#gun_worker_state{pid = PID, monitor = MRef}.

queue_request(LRequest, PID) ->
    {request, FullPath, Method, LHeaders, Query, _Retries, Timeout} = LRequest,
    StreamRef = gun:request(PID, Method, FullPath, LHeaders, Query),
    ?LOG_DEBUG(#{what => http_request, status => submit, request => LRequest}),
    TRef = erlang:send_after(Timeout, self(), {timeout, StreamRef}),
    {StreamRef, TRef}.

retry_all(#gun_worker_state{requests = Requests} = State) ->
    NewRequests = maps:fold(fun(_K, V, Acc) ->
                                    retry_request(State#gun_worker_state.pid, V, Acc)
                            end, #{}, Requests),
    State#gun_worker_state{requests = NewRequests}.

retry_request(PID, {Req, Res}, ReqAcc) ->
    {request, FullPath, Method, LHeaders, Query, Retries, Timeout} = Req,
    erlang:cancel_timer(Res#response_data.timeout_timer),
    case Retries of
        0 ->
            ?LOG_DEBUG(#{what => http_request, status => drop, request => Req}),
            gen_server:reply(Res#response_data.from, {error, request_timeout}),
            ReqAcc;
        _N ->
            ?LOG_DEBUG(#{what => http_request, status => retry, request => Req}),
            {NewStreamRef, TRef} = queue_request(Req, PID),

            ReqAcc#{NewStreamRef => {
                {request, FullPath, Method, LHeaders, Query, Retries - 1, Timeout},
                #response_data{from = Res#response_data.from,
                               timestamp = erlang:monotonic_time(millisecond),
                               timeout_timer = TRef}}}
    end.

default_opts() ->
    #{retry => 1, retry_timeout => 1000}.

lowercase_request({request, FullPath, Method, Headers, Query, Retries, Timeout}) ->
    {request, FullPath, Method, lowercase_headers(Headers), Query, Retries, Timeout}.

lowercase_headers(Headers) ->
    [{string:lowercase(K), V} || {K, V} <- Headers].
