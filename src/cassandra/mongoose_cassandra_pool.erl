%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
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
-module(mongoose_cassandra_pool).
-author('rafal.slota@erlang-solutions.com').

-include("mongoose_logger.hrl").


%% ====================================================================
%% Exports
%% ====================================================================

%% Module callbacks

%% API
-export([call_query/3, cast_query/3]).

%% Types

%% ====================================================================
%% Module API
%% ====================================================================

call_query(PoolName, undefined, Call) ->
    mongoose_wpool:call(cassandra, global, PoolName, Call);
call_query(PoolName, ContextId, Call) ->
    mongoose_wpool:call(cassandra, global, PoolName, ContextId, Call).

cast_query(PoolName, undefined, Call) ->
    mongoose_wpool:cast(cassandra, global, PoolName, Call);
cast_query(PoolName, ContextId, Call) ->
    mongoose_wpool:cast(cassandra, global, PoolName, ContextId, Call).

