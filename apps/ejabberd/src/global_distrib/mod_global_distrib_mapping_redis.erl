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

-export([start/1, stop/0,
         put_session/2, get_session/1, delete_session/2,
         put_domain/2, get_domain/1, delete_domain/2,
         get_domains/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Opts) ->
    Server = proplists:get_value(server, Opts, "127.0.0.1"),
    Port = proplists:get_value(port, Opts, 8102),
    Password = proplists:get_value(password, Opts, ""),
    PoolSize = proplists:get_value(pool_size, Opts, 1),
    RefreshAfter = proplists:get_value(refresh_after, Opts, 60),

    mod_global_distrib_utils:create_opts_ets(?MODULE),
    ExpireAfter = proplists:get_value(expire_after, Opts, 120),
    ets:insert(?MODULE, {expire_after, ExpireAfter}),

    EredisArgs = [Server, Port, 0, Password, 100, 5000],
    WorkerPool = {?MODULE, {wpool, start_pool, [?MODULE, [{workers, PoolSize},
                                                          {worker, {eredis_client, EredisArgs}}]]},
                  permanent, 10000, supervisor, dynamic},
    Refresher = {mod_global_distrib_refresher,
                 {mod_global_distrib_refresher, start_link, [RefreshAfter, fun refresh/1]},
                 permanent, 10000, worker, [mod_global_distrib_refresher]},
    {ok, _} = supervisor:start_child(ejabberd_sup, WorkerPool),
    {ok, _} = supervisor:start_child(ejabberd_sup, Refresher),
    mod_global_distrib_refresher:add_key(domains),
    ok.

stop() ->
    supervisor:terminate_child(ejabberd_sup, mod_global_distrib_refresher),
    supervisor:delete_child(ejabberd_sup, mod_global_distrib_refresher),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ets:delete(?MODULE).

put_session(Jid, Host) ->
    {ok, _} = eredis:q(worker(), [<<"SET">>, Jid, Host, <<"EX">>, expire_after()]),
    ok = mod_global_distrib_refresher:add_key({jid, Jid}),
    ok.

get_session(Jid) ->
    case eredis:q(worker(), [<<"GET">>, Jid]) of
        {ok, undefined} -> error;
        {ok, Host} -> {ok, Host}
    end.

delete_session(Jid, _Host) ->
    LocalHost = opt(local_host),
    case eredis:q(worker(), [<<"GET">>, Jid]) of
        {ok, LocalHost} ->
            {ok, _} = eredis:q(worker(), [<<"DEL">>, Jid]);
        _ ->
            ok
    end,
    ok = mod_global_distrib_refresher:del_key({jid, Jid}),
    ok.

refresh(Keys) ->
    LocalHost = opt(local_host),
    lists:foreach(
      fun
          ({jid, Jid}) ->
              case get_session(Jid) of
                  {ok, LocalHost} -> eredis:q(worker(), [<<"EXPIRE">>, Jid, expire_after()]);
                  _ -> eredis:q(worker(), [<<"SET">>, Jid, LocalHost, expire_after()])
              end;
          (domains) ->
              eredis:q(worker(), [<<"EXPIRE">>, domains_key(), expire_after()])
      end,
      Keys).

put_domain(Domain, Host) ->
    {ok, _} = eredis:q(worker(), [<<"SET">>, Domain, Host, <<"EX">>, expire_after()]),
    {ok, _} = eredis:q(worker(), [<<"SADD">>, domains_key(), Domain]),
    ok.

get_domain(Domain) ->
    get_session(Domain).

delete_domain(Domain, Host) ->
    delete_session(Domain, Host),
    {ok, _} = eredis:q(worker(), [<<"SREM">>, domains_key(), Domain]),
    ok.

get_domains() ->
    Keys = [domains_key(Host) || Host <- opt(hosts)],
    {ok, Domains} = eredis:q(worker(), [<<"SUNION">> | Keys]),
    {ok, Domains}.

worker() ->
    wpool_pool:best_worker(?MODULE).

domains_key() ->
    LocalHost = opt(local_host),
    domains_key(LocalHost).

domains_key(Host) ->
    <<Host/binary, "#{domains}">>.

opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib_mapping, Key).

expire_after() ->
    [{_, Val}] = ets:lookup(?MODULE, expire_after),
    Val.
