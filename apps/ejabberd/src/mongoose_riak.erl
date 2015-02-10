%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
-module(mongoose_riak).

-include("ejabberd.hrl").

%% API
-export([start/0]).
-export([stop/0]).

-export([start_worker/2, start_worker/3]).

-spec start() -> {ok, pid()} | ignore.
start() ->
    case ejabberd_config:get_local_option({riak_config, ?MYNAME}) of
        undefine ->
            ignore;
        RiakOpts ->
            {_, Workers} = lists:keyfind(workers, 1, RiakOpts),
            {_, PoolSpec} = lists:keyfind(connection_details, 1, RiakOpts),
            mongoose_riak_sup:start_link(Workers, PoolSpec)
    end.

stop() ->
    mongoose_riak_sup:stop().


start_worker(Address, Port) ->
    start_worker(Address, Port, []).

start_worker(Address, Port, Opts) ->
    riakc_pb_socket:start_link(Address, Port, Opts).