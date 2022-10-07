-module(mongoose_admin_api_domain).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         to_json/2,
         from_json/2,
         delete_resource/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [parse_body/1, try_handle_request/3, throw_error/2, resource_created/4]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/domains/:domain", ?MODULE, State}].

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_admin_api:init(Req, State).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_admin_api:is_authorized(Req, State).

-spec content_types_provided(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

-spec content_types_accepted(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PATCH">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% @doc Called for a method of type "PUT" or "PATCH"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    F = case cowboy_req:method(Req) of
            <<"PUT">> -> fun handle_put/2;
            <<"PATCH">> -> fun handle_patch/2
        end,
    try_handle_request(Req, State, F).

%% @doc Called for a method of type "DELETE"
-spec delete_resource(req(), state()) -> {true | stop, req(), state()}.
delete_resource(Req, State) ->
    try_handle_request(Req, State, fun handle_delete/2).

%% Internal functions

handle_get(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    Domain = get_domain(Bindings),
    case mongoose_domain_sql:select_domain(Domain) of
        {ok, Props} ->
            {jiffy:encode(Props), Req, State};
        {error, not_found} ->
            throw_error(not_found, <<"Domain not found">>)
    end.

handle_put(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    Domain = get_domain(Bindings),
    Args = parse_body(Req),
    HostType = get_host_type(Args),
    case mongoose_domain_api:insert_domain(Domain, HostType) of
        ok ->
            {true, Req, State};
        {error, duplicate} ->
            throw_error(duplicate, <<"Duplicate domain">>);
        {error, static} ->
            throw_error(denied, <<"Domain is static">>);
        {error, {db_error, _}} ->
            throw_error(internal, <<"Database error">>);
        {error, service_disabled} ->
            throw_error(denied, <<"Service disabled">>);
        {error, unknown_host_type} ->
            throw_error(denied, <<"Unknown host type">>)
    end.

handle_patch(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    Domain = get_domain(Bindings),
    Args = parse_body(Req),
    Result = case get_enabled(Args) of
                 true ->
                     mongoose_domain_api:enable_domain(Domain);
                 false ->
                     mongoose_domain_api:disable_domain(Domain)
             end,
    case Result of
        ok ->
            {true, Req, State};
        {error, not_found} ->
            throw_error(not_found, <<"Domain not found">>);
        {error, static} ->
            throw_error(denied, <<"Domain is static">>);
        {error, service_disabled} ->
            throw_error(denied, <<"Service disabled">>);
        {error, {db_error, _}} ->
            throw_error(internal, <<"Database error">>)
    end.

handle_delete(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    Domain = get_domain(Bindings),
    Args = parse_body(Req),
    HostType = get_host_type(Args),
    case mongoose_domain_api:delete_domain(Domain, HostType) of
        ok ->
            {true, Req, State};
        {error, {db_error, _}} ->
            throw_error(internal, <<"Database error">>);
        {error, static} ->
            throw_error(denied, <<"Domain is static">>);
        {error, service_disabled} ->
            throw_error(denied, <<"Service disabled">>);
        {error, wrong_host_type} ->
            throw_error(denied, <<"Wrong host type">>);
        {error, unknown_host_type} ->
            throw_error(denied, <<"Unknown host type">>)
    end.

get_domain(#{domain := Domain}) ->
    case jid:nameprep(Domain) of
        error -> throw_error(bad_request, <<"Invalid domain name">>);
        PrepDomain -> PrepDomain
    end.

get_host_type(#{host_type := HostType}) -> HostType;
get_host_type(#{}) -> throw_error(bad_request, <<"'host_type' field is missing">>).

get_enabled(#{enabled := Enabled}) -> Enabled;
get_enabled(#{}) -> throw_error(bad_request, <<"'enabled' field is missing">>).
