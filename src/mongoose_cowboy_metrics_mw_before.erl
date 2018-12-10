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
%% @doc Cowboy middleware, one of the two responsible for recoding HTTP API metrics
%%
%% The job of this middleware is to record the timestamp of when the request comes in and to set.
%%
%% It's executed only if listener's env variable `record_metrics' is set to `true'.
%%
%% This middleware should be placed as soon as possible in the middleware chain, so that request
%% timestamp of the request will be captured as quickly as possible.
%%
%%==============================================================================

-module(mongoose_cowboy_metrics_mw_before).

-behaviour(cowboy_middleware).

%% cowboy_middleware callbacks
-export([execute/2]).

%%-------------------------------------------------------------------
%% cowboy_middleware callbacks
%%-------------------------------------------------------------------

execute(Req, Env) ->
    case proplists:get_value(record_metrics, Env, false) of
        true ->
            Ts = erlang:timestamp(),
            {ok, Req, [{req_timestamp, Ts} | Env]};
        false ->
            {ok, Req, Env}
    end.

