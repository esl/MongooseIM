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

-export([prepared_queries/0, put_session/3, get_session/2, delete_session/3, put_domain/3,
         get_domain/2, delete_domain/3, get_domains/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

prepared_queries() ->
    [
     {put_session, "INSERT INTO sessions (jid, stamp, host) VALUES (?, ?, ?)"},
     {get_session, "SELECT * FROM sessions WHERE jid = ? LIMIT 1"},
     {delete_session, "DELETE FROM sessions WHERE jid = ? AND host = ?"},
     {put_domain, "INSERT INTO domains (domain, stamp, host) VALUES (?, ?, ?)"},
     {get_domain, "SELECT * FROM domains WHERE domain = ? LIMIT 1"},
     {delete_domain, "DELETE FROM domains WHERE domain = ? AND host = ?"},
     {get_domains, "SELECT DISTINCT domain FROM domains"}
    ].

put_session(Pool, Jid, Host) ->
    Stamp = p1_time_compat:system_time(milli_seconds),
    case mongoose_cassandra:cql_write(Pool, undefined, ?MODULE, put_session,
                                      [#{jid => Jid, stamp => Stamp, host => Host}]) of
        ok -> ok;
        _ -> error
    end.

get_session(Pool, Jid) ->
    case mongoose_cassandra:cql_read(Pool, undefined, ?MODULE, get_session, #{jid => Jid}) of
        {ok, [#{host := Host}]} -> {ok, Host};
        _ -> error
    end.

delete_session(Pool, Jid, Host) ->
    case mongoose_cassandra:cql_write(Pool, undefined, ?MODULE, delete_session,
                                      [#{jid => Jid, host => Host}]) of
        ok -> ok;
        _ -> error
    end.

put_domain(Pool, Domain, Host) ->
    Stamp = p1_time_compat:system_time(milli_seconds),
    case mongoose_cassandra:cql_write(Pool, undefined, ?MODULE, put_domain,
                                      [#{domain => Domain, stamp => Stamp, host => Host}]) of
        ok -> ok;
        _ -> error
    end.

get_domain(Pool, Domain) ->
    case mongoose_cassandra:cql_read(Pool, undefined, ?MODULE, get_domain, #{domain => Domain}) of
        {ok, [#{host := Host}]} -> {ok, Host};
        _ -> error
    end.

delete_domain(Pool, Domain, Host) ->
    case mongoose_cassandra:cql_write(Pool, undefined, ?MODULE, delete_domain,
                                      [#{domain => Domain, host => Host}]) of
        ok -> ok;
        _ -> error
    end.

get_domains(Pool) ->
    case mongoose_cassandra:cql_read(Pool, undefined, ?MODULE, get_domains, #{}) of
        {ok, Results} -> {ok, [Domain || #{domain := Domain} <- Results]};
        _ -> error
    end.
