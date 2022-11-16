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
         delete_resource/2,
         delete_completed/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [parse_body/1, throw_error/2, resource_created/4]).

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

-spec delete_completed(req(), state()) -> {boolean(), req(), state()}.
delete_completed(Req, #{deletion := in_process} = State) ->
    {false, Req, State};
delete_completed(Req, State) ->
    {true, Req, State}.

%% Internal functions

try_handle_request(Req, State, Handler) ->
    F = fun(ReqIn, StateIn) ->
                case service_domain_db:enabled() of
                    true -> Handler(ReqIn, StateIn);
                    false -> throw_error(denied, <<"Dynamic domains service is disabled">>)
                end
        end,
    mongoose_admin_api:try_handle_request(Req, State, F).

handle_get(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    Domain = get_domain(Bindings),
    case mongoose_domain_api:get_domain_details(Domain) of
        {ok, Props} ->
            {jiffy:encode(maps:with([host_type, status], Props)), Req, State};
        {not_found, Msg} ->
            throw_error(not_found, Msg);
        {_, Msg} ->
            throw_error(denied, Msg)
    end.

handle_put(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    Domain = get_domain(Bindings),
    Args = parse_body(Req),
    HostType = get_host_type(Args),
    case mongoose_domain_api:insert_domain(Domain, HostType) of
        {ok, _} ->
            {true, Req, State};
        {duplicate, Msg} ->
            throw_error(duplicate, Msg);
        {_, Msg} ->
            throw_error(denied, Msg)
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
        {ok, _} ->
            {true, Req, State};
        {not_found, Msg} ->
            throw_error(not_found, Msg);
        {_, Msg} ->
            throw_error(denied, Msg)
    end.

handle_delete(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    Domain = get_domain(Bindings),
    Args = parse_body(Req),
    handle_delete(Req, State, Domain, Args).

handle_delete(Req, State, Domain, #{host_type := HostType, request := true}) ->
    async_delete(Req, State, Domain, HostType);
handle_delete(Req, State, Domain, #{host_type := HostType}) ->
    sync_delete(Req, State, Domain, HostType);
handle_delete(_Req, _State, _Domain, #{}) ->
    throw_error(bad_request, <<"'host_type' field is missing">>).

async_delete(Req, State, Domain, HostType) ->
    case mongoose_domain_api:request_delete_domain(Domain, HostType) of
        {ok, _} ->
            {true, Req, State#{deletion => in_process}};
        {not_found, Msg} ->
            throw_error(not_found, Msg);
        {_Reason, Msg} ->
            throw_error(denied, Msg)
    end.

sync_delete(Req, State, Domain, HostType) ->
    case mongoose_domain_api:delete_domain(Domain, HostType) of
        {ok, _} ->
            {true, Req, State};
        {not_found, Msg} ->
            throw_error(not_found, Msg);
        {_Reason, Msg} ->
            throw_error(denied, Msg)
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
