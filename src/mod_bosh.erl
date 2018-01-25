%%%===================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Cowboy based BOSH support for MongooseIM
%%% @end
%%%===================================================================
-module(mod_bosh).
-behaviour(gen_mod).
-behaviour(cowboy_loop_handler).
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
-export([init/3,
         info/3,
         terminate/3]).

%% Hooks callbacks
-export([node_cleanup/2]).

%% For testing and debugging
-export([get_session_socket/1, store_session/2]).

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

%% Request State
-record(rstate, {}).
-type rstate() :: #rstate{}.
-type req() :: cowboy_req:req().

-type info() :: 'accept_options'
              | 'accept_get'
              | 'item_not_found'
              | 'no_body'
              | 'policy_violation'
              | {'bosh_reply', exml:element()}
              | {'close', _}
              | {'wrong_method', _}.

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
    maps:put(cleanup_result, Res, Acc).

%%--------------------------------------------------------------------
%% cowboy_loop_handler callbacks
%%--------------------------------------------------------------------

-type option() :: {atom(), any()}.
-spec init(_Transport, req(), _Opts :: [option()]) -> {loop, req(), rstate()}.
init(_Transport, Req, _Opts) ->
    ?DEBUG("New request~n", []),
    {Msg, NewReq} = try
        {Method, Req2} = cowboy_req:method(Req),
        case Method of
            <<"OPTIONS">> ->
                {accept_options, Req2};
            <<"POST">> ->
                {has_body, true} = {has_body, cowboy_req:has_body(Req2)},
                {forward_body, Req2};
            <<"GET">> ->
                {accept_get, Req2};
            _ ->
                error({badmatch, {Method, Req2}})
        end
    catch
        %% In order to issue a reply, init() must accept the request for processing.
        %% Hence, handling of these errors is forwarded to info().
        error:{badmatch, {has_body, false}} ->
            {no_body, Req};
        error:{badmatch, {WrongMethod, NReq}} when is_binary(WrongMethod) ->
            {{wrong_method, WrongMethod}, NReq}
    end,
    self() ! Msg,
    {loop, NewReq, #rstate{}}.


-spec info(info(), req(), rstate()) -> {'ok', req(), _}.
info(accept_options, Req, State) ->
    {Origin, Req2} = cowboy_req:header(<<"origin">>, Req),
    Headers = ac_all(Origin),
    ?DEBUG("OPTIONS response: ~p~n", [Headers]),
    {ok, strip_ok(cowboy_req:reply(200, Headers, <<>>, Req2)), State};
info(accept_get, Req, State) ->
    Headers = [content_type(),
               ac_allow_methods(),
               ac_allow_headers(),
               ac_max_age()],
    {ok,
     strip_ok(cowboy_req:reply(200, Headers, <<"MongooseIM bosh endpoint">>, Req)),
     State};
info(no_body, Req, State) ->
    ?DEBUG("Missing request body: ~p~n", [Req]),
    {ok, no_body_error(Req), State};
info({wrong_method, Method}, Req, State) ->
    ?DEBUG("Wrong request method: ~p~n", [Method]),
    {ok, method_not_allowed_error(Req), State};
info(forward_body, Req, S) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    %% TODO: the parser should be stored per session,
    %%       but the session is identified inside the to-be-parsed element
    {ok, BodyElem} = exml:parse(Body),
    ?DEBUG("Parsed body: ~p~n", [BodyElem]),
    forward_body(Req1, BodyElem, S);
info({bosh_reply, El}, Req, S) ->
    BEl = exml:to_binary(El),
    ?DEBUG("Sending (binary) to ~p: ~p~n", [exml_query:attr(El, <<"sid">>), BEl]),
    {ok, Req1} = cowboy_req:reply(200, [content_type(),
                                        ac_allow_origin(?DEFAULT_ALLOW_ORIGIN),
                                        ac_allow_methods(),
                                        ac_allow_headers(),
                                        ac_max_age()], BEl, Req),
    {ok, Req1, S};
info({close, Sid}, Req, S) ->
    ?DEBUG("Closing handler for ~p~n", [Sid]),
    {ok, Req1} = cowboy_req:reply(200, [], [], Req),
    {ok, Req1, S};
info(item_not_found, Req, S) ->
    {ok, terminal_condition(<<"item-not-found">>, Req), S};
info(policy_violation, Req, S) ->
    {ok, terminal_condition(<<"policy-violation">>, Req), S}.


terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

-spec start_backend([option()]) -> any().
start_backend(Opts) ->
    gen_mod:start_backend_module(mod_bosh, Opts, []),
    mod_bosh_backend:start(Opts).

-spec event_type(exml:element()) -> event_type().
event_type(Body) ->
    %% Order of checks is important:
    %% stream restart has got sid attribute,
    %% so check for it at the end.
    catch begin
        case exml_query:attr(Body, <<"type">>) of
            <<"terminate">> ->
                throw(streamend);
            _ ->
                check_next
        end,
        case exml_query:attr(Body, <<"xmpp:restart">>) of
            <<"true">> ->
                throw(restart);
            _ ->
                check_next
        end,
        case exml_query:attr(Body, <<"pause">>) of
            undefined ->
                check_next;
            _ ->
                throw(pause)
        end,
        case exml_query:attr(Body, <<"sid">>) of
            undefined ->
                throw(streamstart);
            _ ->
                normal
        end
    end.


-spec forward_body(req(), exml:element(), rstate())
            -> {'loop', _, rstate()} | {'ok', req(), rstate()}.
forward_body(Req, #xmlel{} = Body, S) ->
    try
        case Type = event_type(Body) of
            streamstart ->
                case maybe_start_session(Req, Body) of
                    {true, Req1} ->
                        {loop, Req1, S};
                    {false, Req1} ->
                        {ok, Req1, S}
                end;
            _ ->
                Socket = get_session_socket(exml_query:attr(Body, <<"sid">>)),
                handle_request(Socket, {Type, Body}),
                {loop, Req, S}
        end
    catch
        error:item_not_found ->
            ?WARNING_MSG("session not found!~n~p~n", [Body]),
            {ok, terminal_condition(<<"item-not-found">>, Req), S}
    end.


-spec handle_request(pid(), {event_type(), exml:element()}) -> 'ok'.
handle_request(Socket, {EventType, Body}) ->
    mod_bosh_socket:handle_request(Socket, {EventType, self(), Body}).


-spec get_session_socket(mod_bosh:sid()) -> 'undefined' | pid().
get_session_socket(Sid) ->
    case mod_bosh_backend:get_session(Sid) of
        [BS] ->
            BS#bosh_session.socket;
        [] ->
            error(item_not_found)
    end.


-spec maybe_start_session(req(), exml:element()) -> {boolean(), req()}.
maybe_start_session(Req, Body) ->
    try
        Hosts = ejabberd_config:get_global_option(hosts),
        {<<"to">>, true} = {<<"to">>,
                            lists:member(exml_query:attr(Body, <<"to">>),
                                         Hosts)},
        %% Version isn't checked as it would be meaningless when supporting
        %% only a subset of the specification.
        {ok, NewBody} = set_max_hold(Body),
        {Peer, Req1} = cowboy_req:peer(Req),
        start_session(Peer, NewBody),
        {true, Req1}
    catch
        error:{badmatch, {<<"to">>, _}} ->
            {false, terminal_condition(<<"host-unknown">>, Req)};
        error:{badmatch, {_Attr, _Value}} ->
            %% TODO: return some sensible condition details
            {false, terminal_condition(<<"undefined-condition">>, [], Req)}
    end.


-spec start_session(_, exml:element()) -> any().
start_session(Peer, Body) ->
    Sid = make_sid(),
    {ok, Socket} = mod_bosh_socket:start(Sid, Peer),
    store_session(Sid, Socket),
    handle_request(Socket, {streamstart, Body}),
    ?DEBUG("Created new session ~p~n", [Sid]).

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
    strip_ok(cowboy_req:reply(400, ac_all(?DEFAULT_ALLOW_ORIGIN),
                              <<"Missing request body">>, Req)).


-spec method_not_allowed_error(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed_error(Req) ->
    strip_ok(cowboy_req:reply(405, ac_all(?DEFAULT_ALLOW_ORIGIN),
                              <<"Use POST request method">>, Req)).

-spec strip_ok({'ok', cowboy_req:req()}) -> cowboy_req:req().
strip_ok({ok, Req}) ->
    Req.

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
    strip_ok(cowboy_req:reply(200, Headers, Body, Req)).


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
    {<<"Access-Control-Allow-Origin">>, Origin}.

ac_allow_methods() ->
    {<<"Access-Control-Allow-Methods">>, <<"POST, OPTIONS, GET">>}.

ac_allow_headers() ->
    {<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}.

ac_max_age() ->
    {<<"Access-Control-Max-Age">>, integer_to_binary(?DEFAULT_MAX_AGE)}.


-spec ac_all('undefined' | binary()) -> [{binary(), _}, ...].
ac_all(Origin) ->
    [ac_allow_origin(Origin),
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

