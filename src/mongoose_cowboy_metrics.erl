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
%% @doc Functions for generating names of metric's updated by `mongoose_cowboy_metrics_mw_after'
%%
%% To generate metric names use `request_count_metric/2', `response_latency_metric/2' and
%% `response_count_metric/3'. See `mongoose_cowboy_metric_mw_after' module to check what values
%% `Prefix', `Method' and `Class' may take.
%%
%%==============================================================================

-module(mongoose_cowboy_metrics).

%% API
-export([request_count_metric/2]).
-export([response_count_metric/3]).
-export([response_latency_metric/3]).

-type prefix() :: list().
-type method() :: binary(). %% <<"GET">>, <<"POST">>, etc.
-type status_class() :: binary(). %% <<"2XX">>, <<"4XX">>, etc.
-type metric_name() :: list().

-export_type([prefix/0]).
-export_type([method/0]).
-export_type([status_class/0]).
-export_type([metric_name/0]).

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

-spec request_count_metric(prefix(), method()) -> metric_name().
request_count_metric(Prefix, Method) ->
    Prefix ++ [Method, request, count].

-spec response_count_metric(prefix(), method(), status_class()) -> metric_name().
response_count_metric(Prefix, Method, Class) ->
    Prefix ++ [Method, response, Class, count].

-spec response_latency_metric(prefix(), method(), status_class()) -> metric_name().
response_latency_metric(Prefix, Method, Class) ->
    Prefix ++ [Method, response, Class, latency].
