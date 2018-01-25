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

-module(mod_global_distrib_worker_sup).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(supervisor).

-export([start_link/0, get_worker/1, init/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec get_worker(From :: jid:lserver()) -> atom().
get_worker(From) when is_binary(From) ->
    Name = mod_global_distrib_utils:any_binary_to_atom(From),
    case whereis(Name) of
        undefined -> supervisor:start_child(?MODULE, [Name]);
        _ -> ok
    end,
    Name.

%%--------------------------------------------------------------------
%% supervisor API
%%--------------------------------------------------------------------

init(_) ->
    Module = mod_global_distrib_worker,
    Child = {Module, {Module, start_link, []}, transient, 5000, worker, [Module]},
    {ok, {{simple_one_for_one, 100, 1}, [Child]}}.
