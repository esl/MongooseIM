-module(external_filter_mock).

-export([start/1]).
-export([stop/0]).
-export([port/0]).
-export([init/2]).
-export([subscribe/2]).
-export([wait_for_request/2]).

start(Config) ->
    application:ensure_all_started(cowboy),
    CertDir =
        filename:join(
            path_helper:test_dir(Config), "priv/ssl"),
    CertPath =
        path_helper:canonicalize_path(
            filename:join(CertDir, "cert.pem")),
    KeyPath =
        path_helper:canonicalize_path(
            filename:join(CertDir, "key.pem")),

    Dispatch = cowboy_router:compile([{'_', [{<<"/api">>, ?MODULE, #{}}]}]),
    {ok, Pid} =
        cowboy:start_tls(external_filter_https_mock,
                         [{certfile, CertPath}, {keyfile, KeyPath}],
                         #{env => #{dispatch => Dispatch}}),
    ets:new(external_filter_mock_subscribers, [public, named_table, {heir, Pid, take_care}]).

port() ->
    ranch:get_port(external_filter_https_mock).

stop() ->
    cowboy:stop_listener(external_filter_https_mock).

subscribe(Message, Response) ->
    ets:insert(external_filter_mock_subscribers, {Message, self(), Response}).

wait_for_request(Message, Timeout) ->
    receive
        {request, Message, Body, Resp} ->
            {jiffy:decode(Body, [return_maps]), Resp}
    after Timeout ->
        ct:fail("timeout_waiting_for_request")
    end.

init(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    #{<<"variables">> := #{<<"messageBody">> := Message}} = jiffy:decode(Body, [return_maps]),
    [{Message, Subscriber, Response}] = ets:lookup(external_filter_mock_subscribers, Message),
    Subscriber ! {request, Message, Body, Response},
    Req3 =
        case Response of
            server_error ->
                cowboy_req:reply(500, #{}, <<"Internal server error">>, Req2);
            _ when is_map(Response) ->
                RespBody = jiffy:encode(Response),
                cowboy_req:reply(200, #{}, RespBody, Req2)
        end,
    {ok, Req3, State}.
