-module(mongoose_client_api).

-behaviour(mongoose_http_handler).

%% mongoose_http_handler callbacks
-export([config_spec/0, routes/1]).

%% Utilities for the handler modules
-export([init/2,
         is_authorized/2,
         parse_body/1,
         parse_qs/1,
         try_handle_request/3,
         throw_error/2]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-type handler_options() :: #{path := string(), handlers := [module()], docs := boolean(),
                             atom() => any()}.
-type req() :: cowboy_req:req().
-type state() :: #{atom() => any()}.
-type error_type() :: bad_request | denied | not_found.

-callback routes() -> mongoose_http_handler:routes().

%% mongoose_http_handler callbacks

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Handlers = all_handlers(),
    #section{items = #{<<"handlers">> => #list{items = #option{type = atom,
                                                               validate = {enum, Handlers}},
                                               validate = unique},
                       <<"docs">> => #option{type = boolean}},
             defaults = #{<<"handlers">> => Handlers,
                          <<"docs">> => true}}.

-spec routes(handler_options()) -> mongoose_http_handler:routes().
routes(Opts = #{path := BasePath}) ->
    [{[BasePath, Path], Module, ModuleOpts}
     || {Path, Module, ModuleOpts} <- api_paths(Opts)] ++ api_doc_paths(Opts).

all_handlers() ->
    [sse, messages, contacts, rooms, rooms_config, rooms_users, rooms_messages].

-spec api_paths(handler_options()) -> mongoose_http_handler:routes().
api_paths(#{handlers := Handlers}) ->
    lists:flatmap(fun api_paths_for_handler/1, Handlers).

api_paths_for_handler(Handler) ->
    HandlerModule = list_to_existing_atom("mongoose_client_api_" ++ atom_to_list(Handler)),
    HandlerModule:routes().

api_doc_paths(#{docs := true}) ->
    [{"/api-docs", cowboy_swagger_redirect_handler, #{}},
     {"/api-docs/swagger.json", cowboy_swagger_json_handler, #{}},
     {"/api-docs/[...]", cowboy_static, {priv_dir, cowboy_swagger, "swagger",
                                         [{mimetypes, cow_mimetypes, all}]}
     }];
api_doc_paths(#{docs := false}) ->
    [].

init(Req, _Opts) ->
    State = #{},
    case cowboy_req:header(<<"origin">>, Req) of
        undefined ->
            {cowboy_rest, Req, State};
        Origin ->
            Req1 = set_cors_headers(Origin, Req),
            {cowboy_rest, Req1, State}
    end.

set_cors_headers(Origin, Req) ->
    %% set CORS headers
    Headers = [{<<"access-control-allow-origin">>, Origin},
               {<<"access-control-allow-methods">>, <<"GET, OPTIONS">>},
               {<<"access-control-allow-credentials">>, <<"true">>},
               {<<"access-control-allow-headers">>, <<"authorization, content-type">>}
              ],

    lists:foldl(fun set_cors_header/2, Req, Headers).

set_cors_header({Header, Value}, Req) ->
    cowboy_req:set_resp_header(Header, Value, Req).

%%--------------------------------------------------------------------
%% Authorization
%%--------------------------------------------------------------------

% @doc cowboy callback
is_authorized(Req, State) ->
    HTTPMethod = cowboy_req:method(Req),
    AuthDetails = mongoose_api_common:get_auth_details(Req),
    case AuthDetails of
        undefined ->
            mongoose_api_common:make_unauthorized_response(Req, State);
        {AuthMethod, User, Password} ->
            authorize(AuthMethod, User, Password, HTTPMethod, Req, State)
    end.

authorize(AuthMethod, User, Password, HTTPMethod, Req, State) ->
    MaybeJID = jid:from_binary(User),
    case do_authorize(AuthMethod, MaybeJID, Password, HTTPMethod) of
        noauth ->
            {true, Req, State};
        {true, Creds} ->
            {true, Req, State#{user => User, jid => MaybeJID, creds => Creds}};
        false ->
            mongoose_api_common:make_unauthorized_response(Req, State)
    end.

do_authorize(AuthMethod, MaybeJID, Password, HTTPMethod) ->
    case is_noauth_http_method(HTTPMethod) of
        true ->
            noauth;
        false ->
            mongoose_api_common:is_known_auth_method(AuthMethod) andalso
                mongoose_api_common:check_password(MaybeJID, Password)
    end.

% Constraints
is_noauth_http_method(<<"OPTIONS">>) -> true;
is_noauth_http_method(_) -> false.

-spec parse_body(req()) -> #{atom() => jiffy:json_value()}.
parse_body(Req) ->
    try
        {ok, Body, _Req2} = cowboy_req:read_body(Req),
        decoded_json_to_map(jiffy:decode(Body))
    catch Class:Reason:Stacktrace ->
            ?LOG_INFO(#{what => parse_body_failed,
                        class => Class, reason => Reason, stacktrace => Stacktrace}),
            throw_error(bad_request, <<"Invalid request body">>)
    end.

decoded_json_to_map({L}) when is_list(L) ->
    maps:from_list([{binary_to_existing_atom(K), decoded_json_to_map(V)} || {K, V} <- L]);
decoded_json_to_map(V) ->
    V.

-spec parse_qs(req()) -> #{atom() => binary() | true}.
parse_qs(Req) ->
    try
        maps:from_list([{binary_to_existing_atom(K), V} || {K, V} <- cowboy_req:parse_qs(Req)])
    catch Class:Reason:Stacktrace ->
            ?LOG_INFO(#{what => parse_qs_failed,
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

-spec error_response(error_type(), iodata(), req(), state()) -> {stop, req(), state()}.
error_response(ErrorType, Message, Req, State) ->
    BinMessage = iolist_to_binary(Message),
    ?LOG(log_level(ErrorType), #{what => mongoose_client_api_error_response,
                                 error_type => ErrorType,
                                 message => BinMessage,
                                 req => Req}),
    Req1 = cowboy_req:reply(error_code(ErrorType), #{}, jiffy:encode(BinMessage), Req),
    {stop, Req1, State}.

-spec error_code(error_type()) -> non_neg_integer().
error_code(bad_request) -> 400;
error_code(denied) -> 403;
error_code(not_found) -> 404.

-spec log_level(error_type()) -> logger:level().
log_level(bad_request) -> info;
log_level(denied) -> info;
log_level(not_found) -> info.
