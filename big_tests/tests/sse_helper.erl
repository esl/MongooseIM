-module(sse_helper).

-compile([export_all, nowarn_export_all]).

connect_to_sse(Port, Path, Creds, ExtraOpts) ->
    ct:log("Connect to SSE, port: ~p, path: ~s, creds: ~p, extra opts: ~p",
           [Port, Path, Creds, ExtraOpts]),
    Headers = auth_headers(Creds) ++ [{<<"host">>, <<"localhost">>},
                                      {<<"accept">>, <<"text/event-stream">>}],
    Opts = maps:merge(basic_opts(), ExtraOpts),
    {ok, ConnPid} = gun:open("localhost", Port, Opts),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, Path, Headers),
    {response, nofin, Status, RespHeaders} = gun:await(ConnPid, StreamRef),
    Result = case {Status, maps:from_list(RespHeaders)} of
                 {200, #{<<"content-type">> := <<"text/event-stream">>}} ->
                     #{pid => ConnPid, stream_ref => StreamRef};
                 _ ->
                     {ok, Body} = gun:await_body(ConnPid, StreamRef),
                     jiffy:decode(Body, [return_maps])
             end,
    ct:log("SSE response code: ~p, headers: ~p, result: ~p", [Status, RespHeaders, Result]),
    {Status, Result}.

wait_for_event(Stream) ->
    wait_for_event(Stream, 5000).

wait_for_event(#{pid := Pid, stream_ref := StreamRef}, Timeout) ->
    {sse, #{data := [Response]} = Event} = gun:await(Pid, StreamRef, Timeout),
    ct:log("Received SSE event: ~p", [Event]),
    jiffy:decode(Response, [return_maps]).

wait_for_events(Stream, Timeout) ->
    wait_for_events(Stream, all, Timeout, []).

wait_for_events(Stream, Count, Timeout) ->
    wait_for_events(Stream, Count, Timeout, []).

wait_for_events(_, 0, _, Received) ->
    return_event_list(Received);
wait_for_events(Stream, Count, Timeout, Received) ->
    #{pid := Pid, stream_ref := StreamRef} = Stream,
    case gun:await(Pid, StreamRef, Timeout) of
        {sse, #{data := _} = Event} ->
            ct:log("Received SSE event: ~p", [Event]),
            wait_for_events(Stream, decrease_count(Count), Timeout, [Event | Received]);
        {error, timeout} ->
            return_or_fail(decrease_count(Count), Received)
    end.

decrease_count(all) -> all;
decrease_count(I) -> I - 1.

return_or_fail(all, Received) ->
    return_event_list(Received);
return_or_fail(0, Received) ->
    return_event_list(Received);
return_or_fail(Remaining, _) ->
    {error, {still_waiting_for_events, Remaining}}.

return_event_list(Received) ->
    lists:map(fun(#{data := R}) -> jiffy:decode(R, [return_maps]) end, lists:reverse(Received)).

stop_sse(#{pid := Pid}) ->
    gun:close(Pid).

auth_headers({Username, Password}) ->
    Base64 = base64:encode(binary_to_list(Username) ++ [$: | binary_to_list(Password)]),
    [{<<"authorization">>, <<"basic ", Base64/binary>>}];
auth_headers(undefined) ->
    [].

basic_opts() ->
    #{protocols => [http],
      http_opts => #{content_handlers => [gun_sse_h, gun_data_h]}}.
