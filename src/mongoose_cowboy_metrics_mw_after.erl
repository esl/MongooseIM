%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
%%
%% @doc Cowboy middleware updating metrics related to request handling
%%
%% This middleware needs to run after `mongoose_cowboy_metrics_mw_before' and  `cowboy_router'
%% middleware. However, it does not need to run after `cowboy_hander' middleware because metrics are
%% updated in `onresponse' callback which is called whenever the request is sent.
%%
%% It is executed only if listener's env variable `record_metrics' is set to `true'.
%%
%% This middleware does not create any metrics, they need to be created earlier using
%% `mongoose_metrics' module. Metric names used by the middleware are contructed as follows:
%%  - take some `Prefix', a list
%%  - take a request method, `Method', one of:
%%    - `<<"GET">>'
%%    - `<<"HEAD">>'
%%    - `<<"POST">>'
%%    - `<<"PUT">>'
%%    - `<<"DELETE">>'
%%    - `<<"OPTIONS">>'
%%    - `<<"PATCH">>'
%% - take a request status class, `Class', and create a string like e.g. `<<"2XX">>' for success
%%   status codes
%% - for each `Method' and `Class' define following metric names
%%   - `Prefix ++ [Method, request, count]' - updated by `1' whenever a request with method `Method'
%%     is about to be handled
%%   - `Prefix ++ [Method, response, Class, count]' - updated by `1' whenever a response of status
%%      class `Class' to a request with method `Method' is sent
%%   - `Prefix ++ [Method, response, Class, latency]' -  updated by number of microseconds which
%%     passed since request timestamp was recorded by `mongoose_cowboy_metrics_mw_before' whenever
%%     a response of status class `Class' to a request with method `Method' is sent
%%
%% As you might have already guessed it makes sense to define `count' metrics as spirals, and
%% `latency' metrics as histograms. The middleware will always try to update the metric regardless
%% of whether it was created. Note that it's run after `cowboy_router' middleware, which means that
%% error responses returned by the router (such as 404 for no matching handler) won't be recorded.
%%
%% And what about `Prefix'? By default prefix is the name of the handler handling the
%% request wrapped in a list. However, you might provide `handler_to_metric_prefix' map as Cowboy
%% listener environment  value, where keys are handler names and values are corresponding prefixes.
%%
%% You can use functions from `mongoose_cowboy_metrics' module to generate names of metrics recorded
%% by this module.
%%
%%==============================================================================

-module(mongoose_cowboy_metrics_mw_after).

-behaviour(cowboy_middleware).

%% cowboy_middleware callbacks
-export([execute/2]).

%%-------------------------------------------------------------------
%% cowboy_middleware callbacks
%%-------------------------------------------------------------------

execute(Req, Env) ->
    case proplists:get_value(record_metrics, Env, false) of
        true ->
            {req_timestamp, StartTs} = proplists:lookup(req_timestamp, Env),
            {handler, Handler} = proplists:lookup(handler, Env),
            Method = get_req_method(Req),
            HandlerToPrefixMappings = proplists:get_value(handler_to_metric_prefix, Env, #{}),
            Prefix = maps:get(Handler, HandlerToPrefixMappings, [Handler]),
            mongoose_metrics:update(global, mongoose_cowboy_metrics:request_count_metric(Prefix, Method), 1),
            OnResponse = on_response_fun(StartTs, Method, Prefix),
            {ok, cowboy_req:set([{onresponse, OnResponse}], Req), Env};
        false ->
            {ok, Req, Env}
    end.

%%-------------------------------------------------------------------
%% Internals
%%-------------------------------------------------------------------

-spec on_response_fun(erlang:timestamp(), mongoose_cowboy_metrics:method(),
                      mongoose_cowboy_metrics:prefix()) -> cowboy:onresponse_fun().
on_response_fun(StartTs, Method, Prefix) ->
    fun(Status, _Headers, _Body, RespReq) ->
        EndTs = erlang:timestamp(),
        Latency = calculate_latency(StartTs, EndTs),
        Class = calculate_status_class(Status),
        mongoose_metrics:update(global, mongoose_cowboy_metrics:response_count_metric(Prefix, Method, Class), 1),
        mongoose_metrics:update(global, mongoose_cowboy_metrics:response_latency_metric(Prefix, Method, Class), Latency),
        RespReq
    end.

-spec calculate_latency(erlang:timestamp(), erlang:timestamp()) -> Microsecs :: non_neg_integer().
calculate_latency(StartTs, EndTs) ->
    timestamp_to_microsecs(EndTs) - timestamp_to_microsecs(StartTs).

-spec timestamp_to_microsecs(erlang:timestamp()) -> Microsecs :: non_neg_integer().
timestamp_to_microsecs({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

-spec get_req_method(cowboy_req:req()) -> mongoose_cowboy_metrics:method().
get_req_method(Req) ->
    {Method, _} = cowboy_req:method(Req),
    Method.

-spec calculate_status_class(100..599) -> mongoose_cowboy_metrics:status_class().
calculate_status_class(S) when S >= 100, S < 200 -> <<"1XX">>;
calculate_status_class(S) when S >= 200, S < 300 -> <<"2XX">>;
calculate_status_class(S) when S >= 300, S < 400 -> <<"3XX">>;
calculate_status_class(S) when S >= 400, S < 500 -> <<"4XX">>;
calculate_status_class(S) when S >= 500, S < 600 -> <<"5XX">>.

