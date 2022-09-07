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

-include("mongoose_config_spec.hrl").

-type handler_options() :: #{path := string(), username => binary(), password => binary(),
                             atom() => any()}.
-type req() :: cowboy_req:req().
-type state() :: map().

%% mongoose_http_handler callbacks

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"username">> => #option{type = binary},
                       <<"password">> => #option{type = binary}},
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
    [{[BasePath, Path], Module, ModuleOpts}
     || {Path, Module, ModuleOpts} <- api_paths(Opts)].

api_paths(Opts) ->
    [{"/contacts/:user/[:contact]", mongoose_admin_api_contacts, Opts},
     {"/contacts/:user/:contact/manage", mongoose_admin_api_contacts, Opts#{suffix => manage}},
     {"/users/:domain/[:username]", mongoose_admin_api_users, Opts},
     {"/sessions/:domain/[:username]/[:resource]", mongoose_admin_api_sessions, Opts},
     {"/messages/:owner/:with", mongoose_admin_api_messages, Opts},
     {"/messages/[:owner]", mongoose_admin_api_messages, Opts},
     {"/stanzas", mongoose_admin_api_stanzas, Opts},
     {"/muc-lights/:domain", mongoose_admin_api_muc_light, Opts},
     {"/muc-lights/:domain/:id/participants", mongoose_admin_api_muc_light,
      Opts#{suffix => participants}},
     {"/muc-lights/:domain/:id/messages", mongoose_admin_api_muc_light,
      Opts#{suffix => messages}},
     {"/muc-lights/:domain/:id/management", mongoose_admin_api_muc_light,
      Opts#{suffix => management}},
     {"/mucs/:domain", mongoose_admin_api_muc, Opts},
     {"/mucs/:domain/:name/:arg", mongoose_admin_api_muc, Opts},
     {"/inbox/:host_type/:days/bin", mongoose_admin_api_inbox, Opts},
     {"/inbox/:domain/:user/:days/bin", mongoose_admin_api_inbox, Opts}
    ].

%% Utilities for the handler modules

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    {cowboy_rest, set_cors_headers(Req), State}.

set_cors_headers(Req) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>,
                                      <<"GET, OPTIONS, PUT, POST, DELETE">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,
                                      <<"*">>, Req1),
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>,
                               <<"Content-Type">>, Req2).

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
authorize(#{}, _) ->
    true. % no credentials required

-spec parse_body(req()) -> jiffy:json_value().
parse_body(Req) ->
    try
        {Params, _} = mongoose_api_common:parse_request_body(Req),
        maps:from_list(Params)
    catch _:_ ->
            throw_error(bad_request, <<"Invalid request body">>)
    end.

-spec parse_qs(req()) -> #{atom() => binary()}.
parse_qs(Req) ->
    maps:from_list([{binary_to_existing_atom(K), V} || {K, V} <- cowboy_req:parse_qs(Req)]).

-spec try_handle_request(req(), state(), fun((req(), state()) -> Result)) -> Result.
try_handle_request(Req, State, F) ->
    try
        F(Req, State)
    catch throw:#{error_type := ErrorType, message := Msg} ->
            mongoose_api_common:error_response(ErrorType, Msg, Req, State)
    end.

-spec throw_error(atom(), iodata()) -> no_return().
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