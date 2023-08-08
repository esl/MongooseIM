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
-xep([{xep, 124}, {version, "1.11.2"}]).

%% gen_mod callbacks
-export([start/2,
         stop/1,
         config_spec/0,
         supported_features/0]).

%% cowboy_loop_handler callbacks
-export([init/2,
         info/3,
         terminate/3]).

%% Hooks callbacks
-export([node_cleanup/3]).

%% For testing and debugging
-export([get_session_socket/1, store_session/2]).

-export([config_metrics/1]).

-ignore_xref([get_session_socket/1, store_session/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").
-include("mongoose_config_spec.hrl").

-define(DEFAULT_MAX_AGE, 1728000).  %% 20 days in seconds
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
-record(rstate, {req_sid, opts}).
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

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, Opts) ->
    case mod_bosh_socket:is_supervisor_started() of
        true ->
            ok; % There is only one backend implementation (mnesia), so it is started globally
        false ->
            mod_bosh_backend:start(Opts),
            {ok, _Pid} = mod_bosh_socket:start_supervisor(),
            gen_hook:add_handlers(hooks())
    end.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    gen_hook:delete_handlers(hooks()),
    ok.

-spec hooks() -> gen_hook:hook_list().
hooks() ->
    [{node_cleanup, global, fun ?MODULE:node_cleanup/3, #{}, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"backend">> => #option{type = atom,
                                                validate = {module, mod_bosh}},
                       <<"inactivity">> => #option{type = int_or_infinity,
                                                   validate = positive},
                       <<"max_wait">> => #option{type = int_or_infinity,
                                                 validate = positive},
                       <<"server_acks">> => #option{type = boolean},
                       <<"max_pause">> => #option{type = integer,
                                                  validate = positive}
                      },
             defaults = #{<<"backend">> => mnesia,
                          <<"inactivity">> => 30, % seconds
                          <<"max_wait">> => infinity, % seconds
                          <<"server_acks">> => false,
                          <<"max_pause">> => 120} % seconds
            }.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%--------------------------------------------------------------------
%% Hooks handlers
%%--------------------------------------------------------------------

-spec node_cleanup(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: map(),
    Params :: #{node := node()},
    Extra :: gen_hook:extra().
node_cleanup(Acc, #{node := Node}, _) ->
    Res = mod_bosh_backend:node_cleanup(Node),
    {ok, maps:put(?MODULE, Res, Acc)}.

%%--------------------------------------------------------------------
%% cowboy_loop_handler callbacks
%%--------------------------------------------------------------------

-spec init(req(), mongoose_http_handler:options()) -> {cowboy_loop, req(), rstate()}.
init(Req, Opts) ->
    ?LOG_DEBUG(#{what => bosh_init, req => Req}),
    Msg = init_msg(Req),
    self() ! Msg,
    %% Upgrade to cowboy_loop behaviour to enable long polling
    {cowboy_loop, Req, #rstate{opts = Opts}}.


%% ok return keep the handler looping.
%% stop handler is used to reply to the client.
-spec info(info(), req(), rstate()) -> {ok, req(), _} | {stop, req(), _}.
info(accept_options, Req, State) ->
    Origin = cowboy_req:header(<<"origin">>, Req),
    Headers = ac_all(Origin),
    ?LOG_DEBUG(#{what => bosh_accept_options, headers => Headers,
                 text => <<"Handle OPTIONS request in Bosh">>}),
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
    ?LOG_DEBUG(#{what => bosh_receive, sid => Sid, request_body => Body}),
    %% Remember req_sid, so it can be used to print a debug message in bosh_reply
    forward_body(Req1, BodyElem, S#rstate{req_sid = Sid});
info({bosh_reply, El}, Req, S) ->
    BEl = exml:to_binary(El),
    %% for BOSH 'data.xmpp.sent.raw' metric includes 'body' wrapping elements
    %% and resending attempts
    mongoose_metrics:update(global, [data, xmpp, sent, c2s, bosh], byte_size(BEl)),
    ?LOG_DEBUG(#{what => bosh_send, req_sid => S#rstate.req_sid, reply_body => BEl,
                 sid => exml_query:attr(El, <<"sid">>, <<"missing">>)}),
    Headers = bosh_reply_headers(),
    Req1 = cowboy_reply(200, Headers, BEl, Req),
    {stop, Req1, S};

info({close, Sid}, Req, S) ->
    ?LOG_DEBUG(#{what => bosh_close, sid => Sid}),
    Req1 = cowboy_reply(200, [], <<>>, Req),
    {stop, Req1, S};
info(no_body, Req, State) ->
    ?LOG_DEBUG(#{what => bosh_stop, reason => missing_request_body, req => Req}),
    Req1 = no_body_error(Req),
    {stop, Req1, State};
info({wrong_method, Method}, Req, State) ->
    ?LOG_DEBUG(#{what => bosh_stop, reason => wrong_request_method,
                 method => Method, req => Req}),
    Req1 = method_not_allowed_error(Req),
    {stop, Req1, State};
info(item_not_found, Req, S) ->
    Req1 = terminal_condition(<<"item-not-found">>, Req),
    {stop, Req1, S};
info(policy_violation, Req, S) ->
    Req1 = terminal_condition(<<"policy-violation">>, Req),
    {stop, Req1, S}.


terminate(_Reason, _Req, _State) ->
    ?LOG_DEBUG(#{what => bosh_terminate}),
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
forward_body(Req, #xmlel{} = Body, #rstate{opts = Opts} = S) ->
    Type = to_event_type(Body),
    case Type of
        streamstart ->
            {SessionStarted, Req1} = maybe_start_session(Req, Body, Opts),
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
                    ?LOG_WARNING(#{what => bosh_stop, reason => session_not_found,
                                   sid => Sid}),
                    Req1 = terminal_condition(<<"item-not-found">>, Req),
                    {stop, Req1, S}
            end
    end.


-spec handle_request(pid(), event_type(), exml:element()) -> ok.
handle_request(Socket, EventType, Body) ->
    %% for BOSH 'data.xmpp.received.raw' metric includes 'body' wrapping elements
    mongoose_metrics:update(global, [data, xmpp, received, c2s, bosh], exml:xml_size(Body)),
    mod_bosh_socket:handle_request(Socket, {EventType, self(), Body}).


-spec get_session_socket(mod_bosh:sid()) -> {ok, pid()} | {error, item_not_found}.
get_session_socket(Sid) ->
    case mod_bosh_backend:get_session(Sid) of
        [BS] ->
            {ok, BS#bosh_session.socket};
        [] ->
            {error, item_not_found}
    end.


-spec maybe_start_session(req(), exml:element(), map()) ->
    {SessionStarted :: boolean(), req()}.
maybe_start_session(Req, Body, Opts) ->
    Domain = exml_query:attr(Body, <<"to">>),
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            case gen_mod:is_loaded(HostType, ?MODULE) of
                true ->
                    maybe_start_session_on_known_host(HostType, Req, Body, Opts);
                false ->
                    {false, terminal_condition(<<"host-unknown">>, Req)}
            end;
        {error, not_found} ->
            {false, terminal_condition(<<"host-unknown">>, Req)}
    end.

-spec maybe_start_session_on_known_host(mongooseim:host_type(), req(), exml:element(), map()) ->
          {SessionStarted :: boolean(), req()}.
maybe_start_session_on_known_host(HostType, Req, Body, Opts) ->
    try
        maybe_start_session_on_known_host_unsafe(HostType, Req, Body, Opts)
    catch
        error:Reason:Stacktrace ->
            %% It's here because something catch-y was here before
            ?LOG_ERROR(#{what => bosh_stop, issue => undefined_condition,
                         reason => Reason, stacktrace => Stacktrace}),
            Req1 = terminal_condition(<<"undefined-condition">>, [], Req),
            {false, Req1}
    end.

-spec maybe_start_session_on_known_host_unsafe(mongooseim:host_type(), req(), exml:element(), map()) ->
          {SessionStarted :: boolean(), req()}.
maybe_start_session_on_known_host_unsafe(HostType, Req, Body, Opts) ->
    %% Version isn't checked as it would be meaningless when supporting
    %% only a subset of the specification.
    {ok, NewBody} = set_max_hold(Body),
    Peer = cowboy_req:peer(Req),
    PeerCert = cowboy_req:cert(Req),
    start_session(HostType, Peer, PeerCert, NewBody, Opts),
    {true, Req}.

-spec start_session(mongooseim:host_type(), mongoose_transport:peer(),
                    binary() | undefined, exml:element(), map()) -> any().
start_session(HostType, Peer, PeerCert, Body, Opts) ->
    Sid = make_sid(),
    {ok, Socket} = mod_bosh_socket:start(HostType, Sid, Peer, PeerCert, Opts),
    store_session(Sid, Socket),
    handle_request(Socket, streamstart, Body),
    ?LOG_DEBUG(#{what => bosh_start_session, sid => Sid}).

-spec store_session(Sid :: sid(), Socket :: pid()) -> any().
store_session(Sid, Socket) ->
    mod_bosh_backend:create_session(#bosh_session{sid = Sid, socket = Socket}).

%% MUST be unique and unpredictable
%% https://xmpp.org/extensions/xep-0124.html#security-sidrid
%% Also, CETS requires to use node as a part of the key
%% (but if the key is always random CETS is happy with that too)
-spec make_sid() -> binary().
make_sid() ->
    base16:encode(crypto:strong_rand_bytes(20)).

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

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).
