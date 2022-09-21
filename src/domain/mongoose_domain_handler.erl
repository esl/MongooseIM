%% REST API for domain actions.
-module(mongoose_domain_handler).

-behaviour(mongoose_http_handler).
-behaviour(cowboy_rest).

%% mongoose_http_handler callbacks
-export([config_spec/0, routes/1]).

%% config processing callbacks
-export([process_config/1]).

%% Standard cowboy_rest callbacks.
-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         delete_resource/2,
         delete_completed/2]).

%% Custom cowboy_rest callbacks.
-export([handle_domain/2,
         to_json/2]).

-ignore_xref([cowboy_router_paths/2, handle_domain/2, to_json/2]).

-include("mongoose_logger.hrl").
-include("mongoose_config_spec.hrl").
-type state() :: map().

-type handler_options() :: #{path := string(), username => binary(), password => binary(),
                             atom() => any()}.

%% mongoose_http_handler callbacks

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"username">> => #option{type = binary},
                       <<"password">> => #option{type = binary}},
             process = fun ?MODULE:process_config/1}.

process_config(Opts) ->
    case maps:is_key(username, Opts) =:= maps:is_key(password, Opts) of
        true ->
            Opts;
        false ->
            error(#{what => both_username_and_password_required, opts => Opts})
    end.

-spec routes(handler_options()) -> mongoose_http_handler:routes().
routes(Opts = #{path := BasePath}) ->
    [{[BasePath, "/domains/:domain"], ?MODULE, Opts}].

%% cowboy_rest callbacks

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_domain}],
        Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

is_authorized(Req, State) ->
    HeaderDetails = cowboy_req:parse_header(<<"authorization">>, Req),
    ConfigDetails = state_to_details(State),
    case check_auth(HeaderDetails, ConfigDetails) of
        ok ->
            {true, Req, State};
        {error, auth_header_passed_but_not_expected} ->
            {false, reply_error(403, <<"basic auth provided, but not configured">>, Req), State};
        {error, auth_password_invalid} ->
            {false, reply_error(403, <<"basic auth provided, invalid password">>, Req), State};
        {error, no_basic_auth_provided} ->
            {false, reply_error(403, <<"basic auth is required">>, Req), State}
    end.

state_to_details(#{username := User, password := Pass}) ->
    {basic, User, Pass};
state_to_details(_) ->
    not_configured.

check_auth({basic, _User, _Pass}, _ConfigDetails = not_configured) ->
    {error, auth_header_passed_but_not_expected};
check_auth(_HeaderDetails, _ConfigDetails = not_configured) ->
    ok;
check_auth({basic, User, Pass}, {basic, User, Pass}) ->
    ok;
check_auth({basic, _, _}, {basic, _, _}) ->
    {error, auth_password_invalid};
check_auth(_, {basic, _, _}) ->
    {error, no_basic_auth_provided}.

%% Custom cowboy_rest callbacks:
-spec to_json(Req, State) -> {Body, Req, State} | {stop, Req, State}
    when Req :: cowboy_req:req(), State :: state(), Body :: binary().
to_json(Req, State) ->
    ExtDomain = cowboy_req:binding(domain, Req),
    Domain = jid:nameprep(ExtDomain),
    case mongoose_domain_sql:select_domain(Domain) of
        {ok, Props} ->
            {jiffy:encode(Props), Req, State};
        {error, not_found} ->
            {stop, reply_error(404, <<"domain not found">>, Req), State}
    end.

-spec handle_domain(Req, State) -> {boolean(), Req, State}
    when Req :: cowboy_req:req(), State :: state().
handle_domain(Req, State) ->
    Method = cowboy_req:method(Req),
    ExtDomain = cowboy_req:binding(domain, Req),
    Domain = jid:nameprep(ExtDomain),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    MaybeParams = json_decode(Body),
    case Method of
        <<"PUT">> ->
            insert_domain(Domain, MaybeParams, Req2, State);
        <<"PATCH">> ->
            patch_domain(Domain, MaybeParams, Req2, State)
    end.

%% Private helper functions:
insert_domain(Domain, {ok, #{<<"host_type">> := HostType}}, Req, State) ->
    case mongoose_domain_api:insert_domain(Domain, HostType) of
        ok ->
            {true, Req, State};
        {error, duplicate} ->
            {false, reply_error(409, <<"duplicate">>, Req), State};
        {error, static} ->
            {false, reply_error(403, <<"domain is static">>, Req), State};
        {error, {db_error, _}} ->
            {false, reply_error(500, <<"database error">>, Req), State};
        {error, service_disabled} ->
            {false, reply_error(403, <<"service disabled">>, Req), State};
        {error, unknown_host_type} ->
            {false, reply_error(403, <<"unknown host type">>, Req), State}
    end;
insert_domain(_Domain, {ok, #{}}, Req, State) ->
    {false, reply_error(400, <<"'host_type' field is missing">>, Req), State};
insert_domain(_Domain, {error, empty}, Req, State) ->
    {false, reply_error(400, <<"body is empty">>, Req), State};
insert_domain(_Domain, {error, _}, Req, State) ->
    {false, reply_error(400, <<"failed to parse JSON">>, Req), State}.

patch_domain(Domain, {ok, #{<<"enabled">> := true}}, Req, State) ->
    Res = mongoose_domain_api:enable_domain(Domain),
    handle_enabled_result(Res, Req, State);
patch_domain(Domain, {ok, #{<<"enabled">> := false}}, Req, State) ->
    Res = mongoose_domain_api:disable_domain(Domain),
    handle_enabled_result(Res, Req, State);
patch_domain(_Domain, {ok, #{}}, Req, State) ->
    {false, reply_error(400, <<"'enabled' field is missing">>, Req), State};
patch_domain(_Domain, {error, empty}, Req, State) ->
    {false, reply_error(400, <<"body is empty">>, Req), State};
patch_domain(_Domain, {error, _}, Req, State) ->
    {false, reply_error(400, <<"failed to parse JSON">>, Req), State}.

handle_enabled_result(Res, Req, State) ->
    case Res of
        ok ->
            {true, Req, State};
        {error, not_found} ->
            {false, reply_error(404, <<"domain not found">>, Req), State};
        {error, static} ->
            {false, reply_error(403, <<"domain is static">>, Req), State};
        {error, service_disabled} ->
            {false, reply_error(403, <<"service disabled">>, Req), State};
        {error, {db_error, _}} ->
            {false, reply_error(500, <<"database error">>, Req), State}
    end.

delete_resource(Req, State) ->
    ExtDomain = cowboy_req:binding(domain, Req),
    Domain = jid:nameprep(ExtDomain),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    MaybeParams = json_decode(Body),
    delete_domain(Domain, MaybeParams, Req2, State).

delete_completed(Req, #{deletion := in_process} = State) ->
    {false, Req, State};
delete_completed(Req, State) ->
    {true, Req, State}.

async_delete(Domain, HostType, Req, State) ->
    mongoose_domain_api:request_delete_domain(Domain, HostType),
    {true, Req, State#{deletion => in_process}}.

sync_delete(Domain, HostType, Req, State) ->
    case mongoose_domain_api:delete_domain(Domain, HostType) of
        ok ->
            {true, Req, State};
        {error, {db_error, _}} ->
            {false, reply_error(500, <<"database error">>, Req), State};
        {error, static} ->
            {false, reply_error(403, <<"domain is static">>, Req), State};
        {error, service_disabled} ->
            {false, reply_error(403, <<"service disabled">>, Req), State};
        {error, wrong_host_type} ->
            {false, reply_error(403, <<"wrong host type">>, Req), State};
        {error, unknown_host_type} ->
            {false, reply_error(403, <<"unknown host type">>, Req), State}
    end.

delete_domain(Domain, {ok, #{<<"host_type">> := HostType,
                             <<"request">> := <<"true">>}}, Req, State) ->
    async_delete(Domain, HostType, Req, State);
delete_domain(Domain, {ok, #{<<"host_type">> := HostType}}, Req, State) ->
    sync_delete(Domain, HostType, Req, State);
delete_domain(_Domain, {ok, #{}}, Req, State) ->
    {false, reply_error(400, <<"'host_type' field is missing">>, Req), State};
delete_domain(_Domain, {error, empty}, Req, State) ->
    {false, reply_error(400, <<"body is empty">>, Req), State};
delete_domain(_Domain, {error, _}, Req, State) ->
    {false, reply_error(400, <<"failed to parse JSON">>, Req), State}.

reply_error(Code, What, Req) ->
    ?LOG_ERROR(#{what => rest_domain_failed, reason => What,
                 code => Code, req => Req}),
    Body = jiffy:encode(#{what => What}),
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, Body, Req).

json_decode(<<>>) ->
    {error, empty};
json_decode(Bin) ->
    try
        {ok, jiffy:decode(Bin, [return_maps])}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.
