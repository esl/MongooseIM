%% REST API for domain actions.
-module(mongoose_domain_handler).
-behaviour(cowboy_rest).

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% Standard cowboy_rest callbacks.
-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2]).

%% Custom cowboy_rest callbacks.
-export([handle_domain/2,
         to_json/2]).

-include("mongoose_logger.hrl").
-type state() :: term().

-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
        ejabberd_cowboy:implemented_result().
cowboy_router_paths(Base, _Opts) ->
    [{[Base, "/domains/:domain"], ?MODULE, []}].

%% cowboy_rest callbacks:
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_domain}],
        Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

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

delete_domain(Domain, {ok, #{<<"host_type">> := HostType}}, Req, State) ->
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
    end;
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
