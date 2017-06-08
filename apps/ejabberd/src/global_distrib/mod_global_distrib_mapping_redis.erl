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

-module(mod_global_distrib_mapping_redis).
-author('konrad.zemek@erlang-solutions.com').

-include("ejabberd.hrl").

-behaviour(mod_global_distrib_mapping).

-export([start/1, stop/1,
         put_session/3, get_session/2, delete_session/3,
         put_domain/3, get_domain/2, delete_domain/3,
         get_domains/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Opts) ->
    Server = proplists:get_value(server, Opts, "127.0.0.1"),
    Port = proplists:get_value(port, Opts, 8102),
    Password = proplists:get_value(password, Opts, ""),
    PoolSize = proplists:get_value(pool_size, Opts, 1),
    RefreshAfter = proplists:get_value(refresh_after, Opts, 60),

    ets:new(?MODULE, [protected, named_table, {read_concurrency, true}]),
    ExpireAfter = proplists:get_value(expire_after, Opts, 120),
    ets:insert(?MODULE, {expire_after, ExpireAfter}),

    EredisArgs = [Server, Port, 0, Password, 100, 5000],
    WorkerPool = {?MODULE, {wpool, start_pool, [?MODULE, [{workers, PoolSize},
                                                          {worker, {eredis_client, EredisArgs}}]]},
                  permanent, 10000, supervisor, dynamic},
    Refresher = {mod_global_distrib_refresher,
                 {mod_global_distrib_refresher, start_link, [RefreshAfter, fun refresh/1]},
                 permanent, 10000, worker, mod_global_distrib_refresher},
    supervisor:start_child(ejabberd_sup, WorkerPool),
    supervisor:start_child(ejabberd_sup, Refresher),
    {ok, ?MODULE}.

stop(_) ->
    supervisor:terminate_child(ejabberd_sup, mod_global_distrib_refresher),
    supervisor:delete_child(ejabberd_sup, mod_global_distrib_refresher).
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

put_session(Pool, Jid, Host) ->
    {ok, _} = eredis:q(worker(Pool), [<<"SET">>, make_key(Host, Jid), stamp(), <<"EX">>, opt(expire_after)]),
    mod_global_distrib_refresher:add_key({jid, Jid}, )
    ok.

get_session(Pool, Jid) ->
    Keys = [make_key(Host, Jid) || Host <- opt(hosts)],
    {ok, Stamps} = eredis:q(worker(Pool), [<<"MGET">> | Keys]),
    case lists:max(lists:zip(Stamps, opt(hosts))) of
        {undefined, _Host} -> error;
        {_Stamp, Host} -> {ok, Host}
    end.

delete_session(Pool, Jid, Host) ->
    {ok, _} = eredis:q(worker(Pool), [<<"DEL">>, make_key(Host, Jid)]),
    ok.

%% refresh_session(Pool, Jid) ->
%%     case eredis:q(worker(Pool), [<<"EXPIRE">>, Jid, integer_to_binary(120)]) of %% TODO: 120
%%         {ok, _} -> ok;
%%         {error, Reason} ->
%%             ?ERROR_MSG("Failed to refresh session ~s: ~p", [Jid, Reason]),
%%             error
%%     end.

put_domain(Pool, Domain, Host) ->
    put_session(Pool, Domain, Host),
    {ok, _} = eredis:q(worker(Pool), [<<"SADD">>, domains_key(Host), Domain]),
    ok.

get_domain(Pool, Domain) ->
    get_session(Pool, Domain).

delete_domain(Pool, Domain, Host) ->
    delete_session(Pool, Domain, Host),
    {ok, _} = eredis:q(worker(Pool), [<<"SREM">>, domains_key(Host), Domain]),
    ok.

%% refresh_domain(Pool, Domain) ->
%%     case refresh_session(Pool, Domain) of
%%         ok -> put_domain_in_set(Pool, Domain);
%%         error -> error
%%     end.

get_domains(Pool) ->
    Keys = [domains_key(Host) || Host <- opt(hosts)],
    {ok, Domains} = eredis:q(worker(Pool), [<<"SUNION">> | Keys]),
    {ok, Domains}.

%% put_domain_in_set(Pool, Domain, Host) ->
%%     Key = domains_key(opt(local_host)),

worker(Pool) ->
    wpool_pool:best_worker(Pool).

stamp() ->
    integer_to_binary(p1_time_compat:system_time()).

domains_key(Host) ->
    make_key(Host, <<"{domains}">>).

make_key(Host, Value) ->
    <<Host/binary, "#", Value/binary>>.

opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib_mapping, Key).
