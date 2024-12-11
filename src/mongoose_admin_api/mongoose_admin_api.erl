-module(mongoose_admin_api).

-behaviour(mongoose_http_handler).

%% mongoose_http_handler callbacks
-export([config_spec/0, routes/1]).

%% config processing callbacks
-export([process_config/1]).

%% Utilities for the handler modules
-export([init/2,
         is_authorized/2,
         parse_body/1,
         parse_qs/1,
         try_handle_request/3,
         throw_error/2,
         resource_created/4,
         respond/3]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-type handler_options() :: #{path := string(), username => binary(), password => binary(),
                             atom() => any()}.
-type req() :: cowboy_req:req().
-type state() :: #{atom() => any()}.
-type error_type() :: bad_request | denied | not_found | duplicate | internal.

-export_type([state/0]).

-callback routes(state()) -> mongoose_http_handler:routes().

%% mongoose_http_handler callbacks

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Handlers = all_handlers(),
    #section{items = #{<<"username">> => #option{type = binary},
                       <<"password">> => #option{type = binary},
                       <<"handlers">> => #list{items = #option{type = atom,
                                                               validate = {enum, Handlers}},
                                               validate = unique}},
             defaults = #{<<"handlers">> => Handlers},
             process = fun ?MODULE:process_config/1}.

-spec process_config(handler_options()) -> handler_options().
process_config(Opts) ->
    case maps:is_key(username, Opts) =:= maps:is_key(password, Opts) of
        true ->
            Opts;
        false ->
            error(#{what => both_username_and_password_required, opts => Opts})
    end.

-spec routes(handler_options()) -> mongoose_http_handler:routes().
routes(Opts = #{path := BasePath}) ->
    [{[BasePath, Path], Module, ModuleOpts} || {Path, Module, ModuleOpts} <- api_paths(Opts)].

all_handlers() ->
    [contacts, users, sessions, messages, stanzas, muc_light, muc, inbox, domain, metrics].

-spec api_paths(handler_options()) -> mongoose_http_handler:routes().
api_paths(#{handlers := Handlers} = Opts) ->
    State = maps:with([username, password], Opts),
    lists:flatmap(fun(Handler) -> api_paths_for_handler(Handler, State) end, Handlers).

api_paths_for_handler(Handler, State) ->
    HandlerModule = list_to_existing_atom("mongoose_admin_api_" ++ atom_to_list(Handler)),
    HandlerModule:routes(State).

%% Utilities for the handler modules

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    {cowboy_rest, Req, State}.

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    AuthDetails = mongoose_api_common:get_auth_details(Req),
    case authorize(State, AuthDetails) of
        true ->
            {true, Req, State};
        false ->
            mongoose_api_common:make_unauthorized_response(Req, State)
    end.

authorize(#{username := Username, password := Password}, AuthDetails) ->
    case AuthDetails of
        {AuthMethod, Username, Password} ->
            mongoose_api_common:is_known_auth_method(AuthMethod);
        _ ->
            false
    end;
authorize(#{}, AuthDetails) ->
    AuthDetails =:= undefined. % Do not accept basic auth when not configured

-spec parse_body(req()) -> #{atom() => jiffy:json_value()}.
parse_body(Req) ->
    try
        {ok, Body, _Req2} = cowboy_req:read_body(Req),
        {DecodedBody} = jiffy:decode(Body),
        maps:from_list([{binary_to_existing_atom(K), V} || {K, V} <- DecodedBody])
    catch Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => parse_body_failed,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            throw_error(bad_request, <<"Invalid request body">>)
    end.

-spec parse_qs(req()) -> #{atom() => binary() | true}.
parse_qs(Req) ->
    try
        maps:from_list([{binary_to_existing_atom(K), V} || {K, V} <- cowboy_req:parse_qs(Req)])
    catch Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => parse_qs_failed,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            throw_error(bad_request, <<"Invalid query string">>)
    end.

-spec try_handle_request(req(), state(), fun((req(), state()) -> Result)) -> Result.
try_handle_request(Req, State, F) ->
    try
        F(Req, State)
    catch throw:#{error_type := ErrorType, message := Msg} ->
            error_response(ErrorType, Msg, Req, State)
    end.

-spec throw_error(error_type(), iodata()) -> no_return().
throw_error(ErrorType, Msg) ->
    throw(#{error_type => ErrorType, message => Msg}).

-spec resource_created(req(), state(), iodata(), iodata()) -> {stop, req(), state()}.
resource_created(Req, State, Path, Body) ->
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Headers = #{<<"location">> => Path},
    Req3 = cowboy_req:reply(201, Headers, Req2),
    {stop, Req3, State}.

%% @doc Send response when it can't be returned in a tuple from the handler (e.g. for DELETE)
-spec respond(req(), state(), jiffy:json_value()) -> {stop, req(), state()}.
respond(Req, State, Response) ->
    Req2 = cowboy_req:set_resp_body(jiffy:encode(Response), Req),
    Req3 = cowboy_req:reply(200, Req2),
    {stop, Req3, State}.

-spec error_response(error_type(), iodata(), req(), state()) -> {stop, req(), state()}.
error_response(ErrorType, Message, Req, State) ->
    BinMessage = iolist_to_binary(Message),
    ?LOG(log_level(ErrorType), #{what => mongoose_admin_api_error_response,
                                 error_type => ErrorType,
                                 message => BinMessage,
                                 req => Req}),
    Req1 = cowboy_req:reply(error_code(ErrorType), #{}, jiffy:encode(BinMessage), Req),
    {stop, Req1, State}.

-spec error_code(error_type()) -> non_neg_integer().
error_code(bad_request) -> 400;
error_code(denied) -> 403;
error_code(not_found) -> 404;
error_code(duplicate) -> 409;
error_code(internal) -> 500.

-spec log_level(error_type()) -> logger:level().
log_level(bad_request) -> warning;
log_level(denied) -> warning;
log_level(not_found) -> warning;
log_level(duplicate) -> warning;
log_level(internal) -> error.
