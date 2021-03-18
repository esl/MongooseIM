%% REST API for domain actions.
-module(mongoose_domain_h).

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% Standard cowboy_rest callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% Custom cowboy_rest callbacks.
-export([insert_domain/2]).

cowboy_router_paths(Base, Opts) ->
    [{[Base, "/domains/:domain"], ?MODULE, []}].

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, insert_domain}],
        Req, State}.

insert_domain(Req, State) ->
    Domain = cowboy_req:binding(domain, Req),
    SDomain = jid:nameprep(Domain),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    #{<<"host_type">> := HostType} = jiffy:decode(Body, [return_maps]),
    case mongoose_domain_api:insert_domain(SDomain, HostType) of
        ok ->
            {true, Req2, State};
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

reply_error(Code, What, Req) ->
    Body = jiffy:encode(#{what => What}),
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, Body, Req).
