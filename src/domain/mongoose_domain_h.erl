%% REST API for domain actions.
-module(mongoose_domain_h).

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% Standard cowboy_rest callbacks.
-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2]).

%% Custom cowboy_rest callbacks.
-export([handle_domain/2,
         to_json/2]).

cowboy_router_paths(Base, Opts) ->
    [{[Base, "/domains/:domain"], ?MODULE, []}].

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"PATCH">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_domain}],
        Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    ExtDomain = cowboy_req:binding(domain, Req),
    Domain = jid:nameprep(ExtDomain),
    case mongoose_domain_sql:select_domain(Domain) of
        {ok, Props} ->
            {jiffy:encode(Props), Req, State};
        {error, not_found} ->
            {stop, reply_error(404, <<"domain not found">>, Req), State};
        {error, _} ->
            {stop, reply_error(500, <<"unknown error">>, Req), State}
    end.

handle_domain(Req, State) ->
    Method = cowboy_req:method(Req),
    ExtDomain = cowboy_req:binding(domain, Req),
    Domain = jid:nameprep(ExtDomain),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Params = jiffy:decode(Body, [return_maps]),
    case Method of
        <<"PUT">> ->
            insert_domain(Domain, Params, Req2, State);
        <<"PATCH">> ->
            patch_domain(Domain, Params, Req2, State)
    end.

insert_domain(Domain, #{<<"host_type">> := HostType}, Req, State) ->
    case mongoose_domain_api:insert_domain(Domain, HostType) of
        ok ->
            {true, Req, State};
        {error, duplicate} ->
            {false, reply_error(409, <<"duplicate">>, Req), State};
        {error, {db_error, _}} ->
            {false, reply_error(500, <<"database error">>, Req), State};
        {error, service_disabled} ->
            {false, reply_error(403, <<"service disabled">>, Req), State};
        {error, unknown_host_type} ->
            {false, reply_error(403, <<"unknown host type">>, Req), State};
        {error, _} ->
            {false, reply_error(500, <<"unknown error">>, Req), State}
    end.

patch_domain(Domain, #{<<"enabled">> := true}, Req, State) ->
    Res = mongoose_domain_api:enable_domain(Domain),
    handle_enabled_result(Res, Req, State);
patch_domain(Domain, #{<<"enabled">> := false}, Req, State) ->
    Res = mongoose_domain_api:disable_domain(Domain),
    handle_enabled_result(Res, Req, State).

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
        {error, _} ->
            {false, reply_error(500, <<"unknown error">>, Req), State}
    end.

reply_error(Code, What, Req) ->
    Body = jiffy:encode(#{what => What}),
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, Body, Req).
