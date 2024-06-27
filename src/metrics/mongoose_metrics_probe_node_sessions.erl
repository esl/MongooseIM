%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
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
-module(mongoose_metrics_probe_node_sessions).
-behaviour(mongoose_instrument_probe).

-export([probe/2]).

-spec probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
    mongoose_instrument:measurements().
probe(sm_node_sessions, #{}) ->
    #{count => ejabberd_sm:get_node_sessions_number()}.
