%%%===================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Cowboy based BOSH support for MongooseIM
%%%
%%% @end
%%%===================================================================
-module(mod_bosh).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
%% cowboy_loop is a long polling handler
-behaviour(cowboy_loop).
-xep([{xep, 206}, {version, "1.4"}]).
-xep([{xep, 124}, {version, "1.11"}]).
%% API
-export([get_inactivity/0,
         set_inactivity/1,
         get_max_wait/0,
         set_max_wait/1,
         get_server_acks/0,
         set_server_acks/1]).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_loop_handler callbacks
-export([init/2,
         info/3,
         terminate/3]).

%% Hooks callbacks
-export([node_cleanup/2]).

%% For testing and debugging
-export([get_session_socket/1, store_session/2]).

-export([config_metrics/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").

-define(DEFAULT_MAX_AGE, 1728000).  %% 20 days in seconds
-define(DEFAULT_INACTIVITY, 30).  %% seconds
-define(DEFAULT_MAX_WAIT, infinity).  %% seconds
-define(DEFAULT_SERVER_ACKS, false).
-define(DEFAULT_ALLOW_ORIGIN, <<"*">>).

-export_type([session/0,
              sid/0,
              event_type/0,
              socket/0
             ]).

-type socket() :: #bosh_socket{}.
-type session() :: #bosh_session{
                      sid :: mod_bosh:sid(),
                      socket :: pid()
                     }.
-type sid() :: binary().
-type event_type() :: streamstart
                    | restart
                    | normal
                    | pause
                    | streamend.

-type headers_list() :: [{binary(), binary()}].

%% Request State
-record(rstate, {req_sid}).
-type rstate() :: #rstate{}.
-type req() :: cowboy_req:req().

-type info() :: accept_options
              | accept_get
              | item_not_found
              | no_body
              | policy_violation
              | {bosh_reply, exml:element()}
              | {close, _}
              | {wrong_method, _}.

%% Behaviour callbacks

-callback start(list()) -> any().
-callback create_session(mod_bosh:session()) -> any().
-callback delete_session(mod_bosh:sid()) -> any().
-callback get_session(mod_bosh:sid()) -> [mod_bosh:session()].
-callback get_sessions() -> [mod_bosh:session()].
-callback node_cleanup(Node :: atom()) -> any().

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec get_inactivity() -> pos_integer() | infinity.
get_inactivity() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, inactivity, ?DEFAULT_INACTIVITY).


%% @doc Return true if succeeded, false otherwise.
-spec set_inactivity(Seconds :: pos_integer() | infinity) -> boolean().
set_inactivity(infinity) ->
    gen_mod:set_module_opt(?MYNAME, ?MODULE, inactivity, infinity);
set_inactivity(Seconds) when is_integer(Seconds), Seconds > 0 ->
    gen_mod:set_module_opt(?MYNAME, ?MODULE, inactivity, Seconds).


-spec get_max_wait() -> pos_integer() | infinity.
get_max_wait() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, max_wait, ?DEFAULT_MAX_WAIT).


%% @doc Return true if succeeded, false otherwise.
-spec set_max_wait(Seconds :: pos_integer() | infinity) -> boolean().
set_max_wait(infinity) ->
    gen_mod:set_module_opt(?MYNAME, ?MODULE, max_wait, infinity);
set_max_wait(Seconds) when is_integer(Seconds), Seconds > 0 ->
    gen_mod:set_module_opt(?MYNAME, ?MODULE, max_wait, Seconds).


-spec get_server_acks() -> boolean().
get_server_acks() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, server_acks, ?DEFAULT_SERVER_ACKS).


-spec set_server_acks(EnableServerAcks :: boolean()) -> boolean().
set_server_acks(EnableServerAcks) ->
    gen_mod:set_module_opt(?MYNAME, ?MODULE, server_acks, EnableServerAcks).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

-spec start(jid:server(), [option()]) -> any().
start(_Host, Opts) ->
    try
        start_backend(Opts),
        {ok, _Pid} = mod_bosh_socket:start_supervisor(),
        ejabberd_hooks:add(node_cleanup, global, ?MODULE, node_cleanup, 50)
    catch
        error:{badmatch, ErrorReason} ->
            ErrorReason
    end.

stop(_Host) ->
    ok.
%%--------------------------------------------------------------------
%% Hooks handlers
%%--------------------------------------------------------------------

node_cleanup(Acc, Node) ->
    Res = mod_bosh_backend:node_cleanup(Node),
    maps:put(?MODULE, Res, Acc).

%%--------------------------------------------------------------------
%% cowboy_loop_handler callbacks
%%--------------------------------------------------------------------

-type option() :: {atom(), any()}.
-spec init(req(), _Opts :: [option()]) -> {cowboy_loop, req(), rstate()}.
init(Req, _Opts) ->
    ?DEBUG("issue=bosh_init", []),
    Msg = init_msg(Req),
    self() ! Msg,
    %% Upgrade to cowboy_loop behaviour to enable long polling
    {cowboy_loop, Req, #rstate{}}.


%% ok return keep the handler looping.
%% stop handler is used to reply to the client.
-spec info(info(), req(), rstate()) -> {ok, req(), _} | {stop, req(), _}.
info(accept_options, Req, State) ->
    Origin = cowboy_req:header(<<"origin">>, Req),
    Headers = ac_all(Origin),
    ?DEBUG("OPTIONS response: ~p~n", [Headers]),
    Req1 = cowboy_reply(200, Headers, <<>>, Req),
    {stop, Req1, State};
info(accept_get, Req, State) ->
    Headers = [content_type(),
               ac_allow_methods(),
               ac_allow_headers(),
               ac_max_age()],
    Body = <<"MongooseIM bosh endpoint">>,
    Req1 = cowboy_reply(200, Headers, Body, Req),
    {stop, Req1, State};
info(forward_body, Req, S) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    %% TODO: the parser should be stored per session,
    %%       but the session is identified inside the to-be-parsed element
    {ok, BodyElem} = exml:parse(Body),
    Sid = exml_query:attr(BodyElem, <<"sid">>, <<"missing">>),
    ?DEBUG("issue=bosh_receive sid=~ts request_body=~p", [Sid, Body]),
    %% Remember req_sid, so it can be used to print a debug message in bosh_reply
    forward_body(Req1, BodyElem, S#rstate{req_sid = Sid});
info({bosh_reply, El}, Req, S) ->
    BEl = exml:to_binary(El),
    ?DEBUG("issue=bosh_send sid=~ts req_sid=~ts reply_body=~p",
           [exml_query:attr(El, <<"sid">>, <<"missing">>), S#rstate.req_sid, BEl]),
    Headers = bosh_reply_headers(),
    Req1 = cowboy_reply(200, Headers, BEl, Req),
    {stop, Req1, S};

info({close, Sid}, Req, S) ->
    ?DEBUG("issue=bosh_close sid=~ts", [Sid]),
    Req1 = cowboy_reply(200, [], <<>>, Req),
    {stop, Req1, S};
info(no_body, Req, State) ->
    ?DEBUG("issue=bosh_stop reason=missing_request_body req=~p", [Req]),
    Req1 = no_body_error(Req),
    {stop, Req1, State};
info({wrong_method, Method}, Req, State) ->
    ?DEBUG("issue=bosh_stop reason=wrong_request_method method=~p req=~p",
           [Method, Req]),
    Req1 = method_not_allowed_error(Req),
    {stop, Req1, State};
info(item_not_found, Req, S) ->
    Req1 = terminal_condition(<<"item-not-found">>, Req),
    {stop, Req1, S};
info(policy_violation, Req, S) ->
    Req1 = terminal_condition(<<"policy-violation">>, Req),
    {stop, Req1, S}.


terminate(_Reason, _Req, _State) ->
    ?DEBUG("issue=bosh_terminate", []),
    ok.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

init_msg(Req) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"OPTIONS">> ->
            accept_options;
        <<"POST">> ->
            case cowboy_req:has_body(Req) of
                true ->
                    forward_body;
                false ->
                    no_body
            end;
        <<"GET">> ->
            accept_get;
        _ ->
            {wrong_method, Method}
    end.

-spec start_backend([option()]) -> any().
start_backend(Opts) ->
    gen_mod:start_backend_module(mod_bosh, Opts, []),
    mod_bosh_backend:start(Opts).

-spec to_event_type(exml:element()) -> event_type().
to_event_type(Body) ->
    %% Order of checks is important:
    %% stream restart has got sid attribute,
    %% so check for it at the end.
    check_event_type_streamend(Body).

check_event_type_streamend(Body) ->
    case exml_query:attr(Body, <<"type">>) of
        <<"terminate">> ->
            streamend;
        _ ->
            check_event_type_restart(Body)
    end.

check_event_type_restart(Body) ->
    case exml_query:attr(Body, <<"xmpp:restart">>) of
        <<"true">> ->
            restart;
        _ ->
            check_event_type_pause(Body)
    end.

check_event_type_pause(Body) ->
    case exml_query:attr(Body, <<"pause">>) of
        undefined ->
            check_event_type_streamstrart(Body);
        _ ->
            pause
    end.

check_event_type_streamstrart(Body) ->
    case exml_query:attr(Body, <<"sid">>) of
        undefined ->
            streamstart;
        _ ->
            normal
    end.

-spec forward_body(req(), exml:element(), rstate())
            -> {ok, req(), rstate()} | {stop, req(), rstate()}.
forward_body(Req, #xmlel{} = Body, S) ->
    Type = to_event_type(Body),
    case Type of
        streamstart ->
            {SessionStarted, Req1} = maybe_start_session(Req, Body),
            case SessionStarted of
                true ->
                    {ok, Req1, S};
                false ->
                    {stop, Req1, S}
            end;
        _ ->
            Sid = exml_query:attr(Body, <<"sid">>),
            case get_session_socket(Sid) of
                {ok, Socket} ->
                    %% Forward request from a client to c2s process
                    handle_request(Socket, Type, Body),
                    {ok, Req, S};
                {error, item_not_found} ->
                    ?WARNING_MSG("issue=bosh_stop "
                                 "reason=session_not_found sid=~ts", [Sid]),
                    Req1 = terminal_condition(<<"item-not-found">>, Req),
                    {stop, Req1, S}
            end
    end.


-spec handle_request(pid(), event_type(), exml:element()) -> ok.
handle_request(Socket, EventType, Body) ->
    mod_bosh_socket:handle_request(Socket, {EventType, self(), Body}).


-spec get_session_socket(mod_bosh:sid()) -> {ok, pid()} | {error, item_not_found}.
get_session_socket(Sid) ->
    case mod_bosh_backend:get_session(Sid) of
        [BS] ->
            {ok, BS#bosh_session.socket};
        [] ->
            {error, item_not_found}
    end.


-spec maybe_start_session(req(), exml:element()) ->
    {SessionStarted :: boolean(), req()}.
maybe_start_session(Req, Body) ->
    Host = exml_query:attr(Body, <<"to">>),
    case is_known_host(Host) of
        true ->
            maybe_start_session_on_known_host(Req, Body);
        false ->
            Req1 = terminal_condition(<<"host-unknown">>, Req),
            {false, Req1}
    end.

maybe_start_session_on_known_host(Req, Body) ->
    try
        maybe_start_session_on_known_host_unsafe(Req, Body)
    catch
        error:Reason ->
            %% It's here because something catch-y was here before
            ?ERROR_MSG("issue=bosh_stop issue=undefined_condition reason=~p",
                       [Reason]),
            Req1 = terminal_condition(<<"undefined-condition">>, [], Req),
            {false, Req1}
    end.

maybe_start_session_on_known_host_unsafe(Req, Body) ->
    %% Version isn't checked as it would be meaningless when supporting
    %% only a subset of the specification.
    {ok, NewBody} = set_max_hold(Body),
    Peer = cowboy_req:peer(Req),
    PeerCert = cowboy_req:cert(Req),
    start_session(Peer, PeerCert, NewBody),
    {true, Req}.

%% @doc Is the argument locally served host?
is_known_host(Host) ->
    Hosts = ejabberd_config:get_global_option(hosts),
    lists:member(Host, Hosts).

-spec start_session(mongoose_transport:peer(), binary() | undefined, exml:element()) -> any().
start_session(Peer, PeerCert, Body) ->
    Sid = make_sid(),
    {ok, Socket} = mod_bosh_socket:start(Sid, Peer, PeerCert),
    store_session(Sid, Socket),
    handle_request(Socket, streamstart, Body),
    ?DEBUG("issue=bosh_start_seassion sid=~ts", [Sid]).

-spec store_session(Sid :: sid(), Socket :: pid()) -> any().
store_session(Sid, Socket) ->
    mod_bosh_backend:create_session(#bosh_session{sid = Sid, socket = Socket}).

-spec make_sid() -> binary().
make_sid() ->
    sha:sha1_hex(term_to_binary(make_ref())).

%%--------------------------------------------------------------------
%% HTTP errors
%%--------------------------------------------------------------------

-spec no_body_error(cowboy_req:req()) -> cowboy_req:req().
no_body_error(Req) ->
    cowboy_reply(400, ac_all(?DEFAULT_ALLOW_ORIGIN),
                 <<"Missing request body">>, Req).


-spec method_not_allowed_error(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed_error(Req) ->
    cowboy_reply(405, ac_all(?DEFAULT_ALLOW_ORIGIN),
                 <<"Use POST request method">>, Req).

%%--------------------------------------------------------------------
%% BOSH Terminal Binding Error Conditions
%%--------------------------------------------------------------------

-spec terminal_condition(binary(), cowboy_req:req()) -> cowboy_req:req().
terminal_condition(Condition, Req) ->
    terminal_condition(Condition, [], Req).


-spec terminal_condition(binary(), [exml:element()], cowboy_req:req())
            -> cowboy_req:req().
terminal_condition(Condition, Details, Req) ->
    Body = terminal_condition_body(Condition, Details),
    Headers = [content_type()] ++ ac_all(?DEFAULT_ALLOW_ORIGIN),
    cowboy_reply(200, Headers, Body, Req).


-spec terminal_condition_body(binary(), [exml:element()]) -> binary().
terminal_condition_body(Condition, Children) ->
    exml:to_binary(#xmlel{name = <<"body">>,
                          attrs = [{<<"type">>, <<"terminate">>},
                                   {<<"condition">>, Condition},
                                   {<<"xmlns">>, ?NS_HTTPBIND}],
                          children = Children}).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

content_type() ->
    {<<"content-type">>, <<"text/xml; charset=utf8">>}.

ac_allow_origin(Origin) ->
    {<<"access-control-allow-origin">>, Origin}.

ac_allow_methods() ->
    {<<"access-control-allow-methods">>, <<"POST, OPTIONS, GET">>}.

ac_allow_headers() ->
    {<<"access-control-allow-headers">>, <<"content-type">>}.

ac_max_age() ->
    {<<"access-control-max-age">>, integer_to_binary(?DEFAULT_MAX_AGE)}.


-spec ac_all('undefined' | binary()) -> headers_list().
ac_all(Origin) ->
    [ac_allow_origin(Origin),
     ac_allow_methods(),
     ac_allow_headers(),
     ac_max_age()].

-spec bosh_reply_headers() -> headers_list().
bosh_reply_headers() ->
    [content_type(),
     ac_allow_origin(?DEFAULT_ALLOW_ORIGIN),
     ac_allow_methods(),
     ac_allow_headers(),
     ac_max_age()].

set_max_hold(Body) ->
    HoldBin = exml_query:attr(Body, <<"hold">>),
    ClientHold = binary_to_integer(HoldBin),
    maybe_set_max_hold(ClientHold, Body).


maybe_set_max_hold(1, Body) ->
    {ok, Body};
maybe_set_max_hold(ClientHold, #xmlel{attrs = Attrs} = Body) when ClientHold > 1 ->
    NewAttrs = lists:keyreplace(<<"hold">>, 1, Attrs, {<<"hold">>, <<"1">>}),
    {ok, Body#xmlel{attrs = NewAttrs}};
maybe_set_max_hold(_, _) ->
    {error, invalid_hold}.

-spec cowboy_reply(non_neg_integer(), headers_list(), binary(), req()) -> req().
cowboy_reply(Code, Headers, Body, Req) when is_list(Headers) ->
    cowboy_req:reply(Code, maps:from_list(Headers), Body, Req).

config_metrics(Host) ->
    OptsToReport = [{backend, mnesia}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
