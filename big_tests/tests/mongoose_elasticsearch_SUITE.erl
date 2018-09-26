%% Copyright © 2018 Erlang Solutions Ltd
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

-module(mongoose_elasticsearch_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0, rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

suite() ->
    [{require, ejabberd_node},
     {require, ejabberd_cookie} |
     distributed_helper:require_rpc_nodes([mim])].

all() ->
    [{group, all}].

groups() ->
    [{all, [], all_test_cases()}].

all_test_cases() ->
    [start_and_stop_sequence].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    case is_elasticsearch_enabled() of
        true ->
            ok = rpc(mim(), mongoose_wpool, stop, [elastic, global, default]),
            Config;
        false ->
            {skip, elasticsearch_unavailable}
    end.

end_per_suite(_Config) ->
    case is_elasticsearch_enabled() of
        true ->
            ok;
        false ->
            ok = rpc(mim(), mongoose_elasticsearch, start, [])
    end.


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

start_and_stop_sequence(_Config) ->
    rpc(mim(), mongoose_wpool, start, [elastic, global, default, [], []]),
    ?assertMatch({ok, _}, rpc(mim(), mongoose_elasticsearch, health, [])),

    rpc(mim(), mongoose_wpool, stop, [elastic, global, default]),
    ?assertMatch({error, _}, rpc(mim(), mongoose_elasticsearch, health, [])),

    rpc(mim(), mongoose_wpool, start, [elastic, global, default, [], []]),
    ?assertMatch({ok, _}, rpc(mim(), mongoose_elasticsearch, health, [])).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec is_elasticsearch_enabled() -> boolean().
is_elasticsearch_enabled() ->
    rpc(mim(), mongoose_wpool, is_configured, [elastic]).

