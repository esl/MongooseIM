-module(simple_rest_api).

%% In this scenario half of users send messages through
%% MongooseIM REST API, and another half receives them over XMPP.
%%
%% Using system environment "AMOC_xmpp_servers" XMPP nodes configuration is
%% retrvied. It contains:
%%  - XMPP server address
%%  - XMPP server port
%%  - MongooseIM REST API port (it assumes that host addres is same as in XMPP)
%%
%% The value retrived from env var has a form of list of proplists:
%% [ [{host, <<"mim-1">>}, {port, 5222}, {rest_api_port, 8089}] ]

-behavior(amoc_scenario).
-compile([{parse_transform, lager_transform}]).

-export([start/1]).
-export([init/0]).

-include_lib("exml/include/exml.hrl").

-define(HOST, <<"localhost">>).
-define(SLEEP_TIME_AFTER_MESSAGE, 5000).
-define(DEFAULT_REST_API_PORT, 8089).
-define(REST_API_MESSAGES_URI, <<"/api/messages">>).
-define(REST_API_RESP_TIMEOUT, infinity).
%% Total count of messages sent
-define(MESSAGES_CT, [amoc, counters, messages_sent, api]).
%% Time to deliver from API client to XMPP client
-define(MESSAGE_TTD_CT, [amoc, times, message_ttd]).
%% Response time of server after sending message through API
-define(API_RESP_TIME_CT, [amoc, times, api_resp]).
%% Total count of timed out connections
-define(CONNECTION_TIMEDOUT_CT, [amoc, counters, api_timed_out]).

%%%
% Behaviour functions
%%%

-spec init() -> ok.
init() ->
    lager:info("Init metrics"),
    exometer:new(?MESSAGES_CT, spiral),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGES_CT, [one, count], 10000),
    exometer:new(?CONNECTION_TIMEDOUT_CT, spiral),
    exometer_report:subscribe(exometer_report_graphite, ?CONNECTION_TIMEDOUT_CT, [one, count], 10000),
    exometer:new(?MESSAGE_TTD_CT, histogram),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGE_TTD_CT, [mean, min, max, median, 95, 99, 999], 10000),
    exometer:new(?API_RESP_TIME_CT, histogram),
    exometer_report:subscribe(exometer_report_graphite, ?API_RESP_TIME_CT, [mean, min, max, median, 95, 99, 999], 10000),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    XmppOrRest = xmpp_or_rest(MyId),
    Cfg1 = make_user(MyId, <<"xmpp-res">>),
    Cfg2 = [{socket_opts, socket_opts()} | Cfg1],
    do_start(XmppOrRest, MyId, Cfg2).

%%%
% Startup functions
%%%

-spec do_start(rest|xmpp, amoc_scenario:user_id(), escalus_users:user_spec()) ->
    no_return().
do_start(rest, MyId, Cfg) ->
    AuthHeader = auth_header(Cfg),
    Header = [AuthHeader
              ,{<<"content-type">>, <<"application/json">>}
              ,{<<"Connection">>, <<"keep-alive">>}
%              ,{<<"connection">>, <<"close">>}
             ],
    Endpoint = format_rest_endpoint(Cfg),
    {ok, Client} = fusco:start(Endpoint, []),
    FullJid = full_jid(MyId - 1),
    send_forever(Client, Header, FullJid),
    ok;
do_start(xmpp, _MyId, Cfg) ->
    {ok, Client, _, _} = escalus_connection:start(Cfg),
    exometer:update([amoc, counters, connections], 1),
    send_presence_available(Client),
    receive_forever(Client),
    escalus_connection:stop(Client).

%%%
% Loop functions
%%%

-spec send_forever(pid(), fusco:headers(), binary()) -> no_return().
send_forever(Client, Header, FullJid) ->
    TimeStamp = integer_to_binary(usec:from_now(os:timestamp())),
    Msg = jiffy:encode(
            #{to => FullJid,
              body => <<"Hello, sir! timestamp: ", TimeStamp/binary>>}),
    {RespTime, Result} = send_message_via_rest_with_timer(Client, Header, Msg),
    exometer:update(?API_RESP_TIME_CT, RespTime),
    case Result of
        {ok, _} ->
            exometer:update(?MESSAGES_CT, 1);
        {error, connect_timeout} ->
            exometer:update(?CONNECTION_TIMEDOUT_CT, 1);
        {error, Error} ->
            lager:info("Error occured: ~p", [Error]),
            ok
    end,
    timer:sleep(?SLEEP_TIME_AFTER_MESSAGE),
    send_forever(Client, Header, FullJid).

receive_forever(Client) ->
    Stanza = escalus_connection:get_stanza(Client, message, infinity),
    Now = usec:from_now(os:timestamp()),
    Data = get_message_body(Stanza),
    case re:run(Data, <<"timestamp: (\\d+)">>, [{capture, all, binary}]) of
        {match, [_, TimeStamp]} ->
            TTD = (Now - binary_to_integer(TimeStamp)),
            exometer:update(?MESSAGE_TTD_CT, TTD);
        _ ->
            ok
    end,
    receive_forever(Client).

%%%
%  Helper functions
%%%

send_message_via_rest_with_timer(Client, Header, Msg) ->
    timer:tc(
      fusco,
      request,
      [Client, ?REST_API_MESSAGES_URI, <<"POST">>, Header, Msg,
       ?REST_API_RESP_TIMEOUT]).

send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).


-spec get_message_body(#xmlel{}) -> binary().
get_message_body(#xmlel{name = <<"message">>} = Stanza) ->
    Body = exml_query:subelement(Stanza, <<"body">>),
    case Body of
        undefined ->
            <<>>;
        #xmlel{} ->
            exml_query:cdata(Body)
    end;
get_message_body(_) ->
    <<>>.

-spec auth_header(escalus_users:user_spec()) -> {binary(), binary()}.
auth_header(Cfg) ->
    Username = proplists:get_value(username, Cfg),
    Server = proplists:get_value(server, Cfg),
    Password = proplists:get_value(password, Cfg),
    User = <<Username/binary, "@", Server/binary>>,
    UserAndPass = <<User/binary, ":", Password/binary>>,
    Base64  = base64:encode(UserAndPass),
    Basic = <<"basic ",Base64/binary>>,
    {<<"authorization">>, Basic}.

-spec socket_opts() -> [gen_tcp:option()].
socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].

-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = username(Id),
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

username(BinId) when is_binary(BinId) ->
    <<"user_", BinId/binary>>;
username(Id) when is_integer(Id) ->
    username(integer_to_binary(Id)).

full_jid(Id) ->
    U = username(Id),
    H = host(),
    <<U/binary, "@", H/binary>>.

host() ->
    ?HOST.

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    [ {username, ProfileId},
      {server, host()},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ] ++ pick_server().

-spec format_rest_endpoint(escalus_users:user_spec()) -> binary().
format_rest_endpoint(Server) ->
    Host = proplists:get_value(host, Server),
    Port = proplists:get_value(rest_api_port, Server, ?DEFAULT_REST_API_PORT),
    PortBinary = list_to_binary(integer_to_list(Port)),
    erlang:binary_to_list(<<"https://", Host/binary, ":", PortBinary/binary>>).

xmpp_or_rest(MyId) ->
    Id = MyId rem 2 + 1,
    element(Id, {rest, xmpp}).

-spec pick_server() -> [proplists:property()].
pick_server() ->
    Servers = amoc_config:get(xmpp_servers),
    verify(Servers),
    S = length(Servers),
    N = erlang:phash2(self(), S) + 1,
    lists:nth(N, Servers).

verify(Servers) ->
    lists:foreach(
      fun(Proplist) ->
          true = proplists:is_defined(host, Proplist)
      end,
      Servers
     ).
