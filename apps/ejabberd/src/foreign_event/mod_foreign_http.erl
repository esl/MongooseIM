%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_foreign_http).
-author('szymon.mentel@erlang-solutions.com').
%% Shall that be mod?
-behaviour(mod_foreign).

-include("jlib.hrl").
-include("ejabberd.hrl").

%% API
-export([start/2, stop/1, make_request/3]).
%% Internal exports
-export([make_request_and_respond/4]).

-type pool_size() :: non_neg_integer().
%% passed from the backend opts from the config file
-type opt() :: {pool_size, pool_size()}.

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

-define(HTTP_REQUEST_TIME_METRIC, [?MODULE, http_request_time]).
-define(SUCCESSFUL_HTTP_REQS_METRIC, [?MODULE, successful_http_requests]).
-define(FAILED_HTTP_REQS_METRIC, [?MODULE, failed_http_requests]).

-define(DEFAULT_POOL_SIZE, 100).
-define(HTTP_TIMEOUT, 5000).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(ejabberd:server(), [opt()]) -> ok | {error, term()}.
start(Host, Opts) ->
    ensure_metrics(Host),
    application:ensure_all_started(worker_pool),
    wpool:start_sup_pool(pool_name(Host), [{workers, pool_size(Opts)}]).

stop(Host) ->
    wpool:stop_pool(pool_name(Host)).

-spec make_request(ejabberd:server(),
                   mod_foreign:foreign_request(),
                   mod_foreign:on_response())
                  -> ok | error.
make_request(Host, Request, OnResponse) ->
    case parse(Request) of
        {error, _Error} ->
            error;
        {ok, {_Host, _Path, _Method, _Headers, _Body} = Req} ->
            Task = {?MODULE, make_request_and_respond,
                    [Host, Req, ?HTTP_TIMEOUT, OnResponse]},
            wpool:cast(pool_name(Host), Task, next_available_worker)
    end.

%%--------------------------------------------------------------------
%% Internals: parsing
%%--------------------------------------------------------------------

-spec parse(xmlel()) ->  {ok, {Host :: string(),
                               Path :: string(),
                               Method :: binary(),
                               Headers :: [{Key :: binary(), Value :: binary()}],
                               Payload :: binary()}}
                             | {error, [Reason :: term()]}.
parse(Request) ->
    {Host, Path} = HP =  parse_url(Request),
    Method = parse_method(Request),
    Headers = parse_headers(Request),
    Body = parse_body(Request),
    case lists:foldl(fun({error, E}, Errors) -> [E | Errors];
                        (_, Errors) -> Errors
                     end, [],
                     [HP, Method, Headers, Body])
    of
        [_] = Errors ->
            {error, Errors};
        [] ->
            {ok, {Host, Path, Method, Headers, Body}}
    end.


parse_url(#xmlel{} = Request) ->
    Url = exml_query:path(Request, [{attr, <<"url">>}]),
    do_parse_url(Url).

do_parse_url(undefined) ->
    {error, no_url};
do_parse_url(<<"http://", Rest/binary>>) ->
    do_parse_url(<<"http://">>, Rest);
do_parse_url(<<"https://", Rest/binary>>) ->
    do_parse_url(<<"https://">>, Rest);
do_parse_url(_) ->
    {error, malformed_url}.

do_parse_url(Scheme, Rest) ->
    [Host, Path] = case binary:split(Rest, [<<"/">>]) of
                       [_H, _P] = R ->
                           R;
                       [H] ->
                           [H, <<"/">>]
                   end,
    {binary_to_list(<<Scheme/binary, Host/binary>>), Path}.

parse_method(#xmlel{} = Request) ->
    Method = exml_query:path(Request, [{attr, <<"method">>}]),
    do_parse_method(Method).

do_parse_method(undefined) ->
    {error, no_http_method};
do_parse_method(<<"get">>) ->
    <<"GET">>;
do_parse_method(<<"post">>) ->
    <<"POST">>;
do_parse_method(_UnknownMethod) ->
    {error, unknown_http_method}.

parse_headers(#xmlel{} = Request) ->
    Headers = exml_query:paths(Request, [{element, <<"header">>}]),
    lists:foldl(fun(#xmlel{attrs = [{<<"name">>, Name}],
                           children = [#xmlcdata{content = Value}]},
                    Acc) ->
                        [{Name, Value} | Acc]
                end, [], Headers).

parse_body(#xmlel{} = Request) ->
    exml_query:path(Request, [{element, <<"payload">>}, cdata]).

%%--------------------------------------------------------------------
%% Internals: encoding
%%--------------------------------------------------------------------

encode({ok, {{StatusCode, _ReasonPhare}, Headers, Body, _Size, _Time}}) ->
    #xmlel{name = <<"response">>,
           attrs = [
                    {<<"xmlns">>, ?NS_FOREIGN_EVENT_HTTP},
                    {<<"type">>, <<"http">>},
                    {<<"status">>, exml:escape_attr(StatusCode)}
                   ],
           children = [
                       #xmlel{name = <<"payload">>, children = [exml:escape_cdata(Body)]}
                       | lists:foldl(fun({Name, Value}, Acc) ->
                                             [#xmlel{name = <<"header">>,
                                                     attrs = [{<<"name">>, exml:escape_attr(Name)}],
                                                     children = [exml:escape_cdata(Value)]}
                                              | Acc ]
                                     end, [], Headers)
                      ]};
encode({error, Reason}) ->
    #xmlel{name = <<"failure">>,
           attrs = [{<<"reason">>,
                     exml:escape_attr(atom_to_binary(Reason, latin1))}]}.

%%--------------------------------------------------------------------
%% Internals: request and response
%%--------------------------------------------------------------------

%% TODO: Internal server error on do_make_request exception (try catch)
make_request_and_respond(Host, Request, Timeout, OnResponse) ->
    Response = do_make_request(Request, Timeout),
    respond(encode(Response), OnResponse),
    update_metrics(Host, Response).

%% The `Timeout' is the overall request timeout including the time spent on
%% initiating the connection.
do_make_request({Host, Path, Method, Headers, Body}, Timeout) ->
    {ok, Client} = fusco:start(Host, []),
    Response = fusco:request(Client, Path, Method, Headers, Body, Timeout),
    ok = fusco:disconnect(Client),
    Response.

respond(_Response, noreply) ->
    do_nothing;
respond(Response, OnResponse) ->
    OnResponse(Response).

%%--------------------------------------------------------------------
%% Internals: pool
%%--------------------------------------------------------------------

-spec pool_name(Host :: ejabberd:lserver()) -> atom().
pool_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

-spec pool_size([opt()]) -> pool_size().
pool_size(Opts) ->
    proplists:get_value(pool_size, Opts, ?DEFAULT_POOL_SIZE).

%%--------------------------------------------------------------------
%% Internals: pool
%%--------------------------------------------------------------------

ensure_metrics(Host) ->
    mongoose_metrics:ensure_metric(Host, ?HTTP_REQUEST_TIME_METRIC, histogram),
    mongoose_metrics:ensure_metric(Host, ?SUCCESSFUL_HTTP_REQS_METRIC, spiral),
    mongoose_metrics:ensure_metric(Host, ?FAILED_HTTP_REQS_METRIC, spiral),
    ok.

update_metrics(Host, {ok, {_SatusAndReason, _Headers, _Body, _Size, Time}}) ->
    mongoose_metrics:update(Host, ?HTTP_REQUEST_TIME_METRIC, Time),
    mongoose_metrics:update(Host, ?SUCCESSFUL_HTTP_REQS_METRIC, 1),
    ok;
update_metrics(Host, {error, _}) ->
    mongoose_metrics:update(Host, ?FAILED_HTTP_REQS_METRIC, 1).
