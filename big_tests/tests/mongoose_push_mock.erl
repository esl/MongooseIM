-module(mongoose_push_mock).

-export([start/1]).
-export([stop/0]).
-export([port/0]).
-export([init/2]).
-export([subscribe/2]).
-export([wait_for_push_request/2]).

start(Config) ->
    application:ensure_all_started(cowboy),
    CertDir = filename:join(path_helper:test_dir(Config), "priv/ssl"),
    CertPath = path_helper:canonicalize_path(filename:join(CertDir, "cert.pem")),
    KeyPath = path_helper:canonicalize_path(filename:join(CertDir, "key.pem")),

    Dispatch = cowboy_router:compile([{'_', [{<<"/v2/notification/:token">>, ?MODULE, #{}},
                                             {<<"/v3/notification/:token">>, ?MODULE, #{}}
                                            ]}
                                     ]),
    {ok, Pid} = cowboy:start_tls(mongoose_push_https_mock,
                                 [{certfile, CertPath}, {keyfile, KeyPath}],
                                 #{env => #{dispatch => Dispatch}}),
    ets:new(mongoose_push_mock_subscribers,
            [public, named_table, {heir, Pid, take_care}]).

port() ->
    ranch:get_port(mongoose_push_https_mock).

stop() ->
    cowboy:stop_listener(mongoose_push_https_mock).

subscribe(Token, Response) ->
    ets:insert(mongoose_push_mock_subscribers, {Token, self(), Response}).

wait_for_push_request(Token, Timeout) ->
    receive
        {push_request, Token, Body, Resp} ->
            {jiffy:decode(Body, [return_maps]), Resp}
    after Timeout ->
              ct:fail("timeout_waiting_for_push_request")
    end.

init(Req, State) ->
    Token = cowboy_req:binding(token, Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    [{_, Subscriber, {RespStatus, RespBody} = Resp}] = ets:lookup(mongoose_push_mock_subscribers, Token),
    Subscriber ! {push_request, Token, Body, Resp},
    Req3 = cowboy_req:reply(RespStatus, #{}, RespBody, Req2),
    {ok, Req3, State}.

