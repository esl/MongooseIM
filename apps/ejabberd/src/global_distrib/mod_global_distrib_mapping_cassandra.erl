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

-module(mod_global_distrib_mapping_cassandra).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(mod_global_distrib_mapping).

-export([prepared_queries/0, put_session/3, get_session/2, delete_session/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

prepared_queries() ->
    [
     {put, "INSERT INTO sessions (jid, stamp, host) VALUES (?, ?, ?)"},
     {get, "SELECT * FROM sessions WHERE jid = ? LIMIT 1"},
     {delete, "DELETE FROM sessions WHERE jid = ? AND host = ?"}
    ].

put_session(Pool, Jid, Host) ->
    Stamp = p1_time_compat:system_time(milli_seconds),
    case mongoose_cassandra:cql_write(Pool, undefined, ?MODULE, put,
                                      [#{jid => Jid, stamp => Stamp, host => Host}]) of
        ok -> ok;
        _ -> error
    end.

get_session(Pool, Jid) ->
    case mongoose_cassandra:cql_read(Pool, undefined, ?MODULE, get, #{jid => Jid}) of
        {ok, [#{host := Host}]} -> {ok, Host};
        _ -> error
    end.

delete_session(Pool, Jid, Host) ->
    case mongoose_cassandra:cql_write(Pool, undefined, ?MODULE, delete,
                                      [#{jid => Jid, host => Host}]) of
        ok -> ok;
        _ -> error
    end.
