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

-module(mod_global_distrib_mapping).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-define(DOMAIN_TAB, mod_global_distrib_domain_cache_tab).
-define(JID_TAB, mod_global_distrib_jid_cache_tab).

-export([start/2, stop/1, deps/2]).
-export([for_domain/1, insert_for_domain/1, insert_for_domain/2, insert_for_domain/3,
         cache_domain/2, delete_for_domain/1, all_domains/0, public_domains/0]).
-export([for_jid/1, insert_for_jid/1, cache_jid/2, delete_for_jid/1, clear_cache/1]).
-export([register_subhost/3, unregister_subhost/2, packet_to_component/3,
         session_opened/4, session_closed/5]).
-export([endpoints/1, hosts/0]).

-type endpoint() :: mod_global_distrib_utils:endpoint().

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback start(Opts :: proplists:proplist()) -> any().
-callback stop() -> any().
-callback put_session(JID :: binary()) -> ok | error.
-callback get_session(JID :: binary()) -> {ok, Host :: binary()} | error.
-callback delete_session(JID :: binary()) -> ok | error.
-callback put_domain(Domain :: binary(), IsHidden :: boolean()) -> ok | error.
-callback get_domain(Domain :: binary()) -> {ok, Host :: binary()} | error.
-callback delete_domain(Domain :: binary()) -> ok | error.
-callback get_domains() -> {ok, [Domain :: binary()]} | error.
-callback get_public_domains() -> {ok, [Domain :: binary()]} | error.
-callback get_endpoints(Host :: binary()) -> {ok, [endpoint()]}.
-callback get_hosts() -> [Host :: jid:lserver()].

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec for_domain(Domain :: binary()) -> {ok, Host :: jid:lserver()} | error.
for_domain(Domain) when is_binary(Domain) ->
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MAPPING_FETCHES, 1),
    {Time, R} = timer:tc(ets_cache, lookup, [?DOMAIN_TAB, Domain, fun() -> get_domain(Domain) end]),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MAPPING_FETCH_TIME, Time),
    R.

-spec insert_for_domain(Domain :: binary()) -> ok.
insert_for_domain(Domain) when is_binary(Domain) ->
    insert_for_domain(Domain, false).

-spec insert_for_domain(Domain :: binary(), IsHidden :: boolean()) -> ok.
insert_for_domain(Domain, IsHidden) when is_binary(Domain) ->
    LocalHost = opt(local_host),
    insert_for_domain(Domain, LocalHost, IsHidden).

-spec insert_for_domain(Domain :: binary(), Host :: binary(), IsHidden :: boolean()) -> ok.
insert_for_domain(Domain, Host, IsHidden) when is_binary(Domain), is_binary(Host) ->
    do_insert_for_domain(Domain, Host, fun(ToStore) -> put_domain(ToStore, IsHidden) end).

-spec cache_domain(Domain :: binary(), Host :: binary()) -> ok.
cache_domain(Domain, Host) when is_binary(Domain), is_binary(Host) ->
    do_insert_for_domain(Domain, Host, fun(_) -> ok end).

-spec delete_for_domain(Domain :: binary()) -> ok.
delete_for_domain(Domain) when is_binary(Domain) ->
    delete_domain(Domain),
    ets_cache:delete(?DOMAIN_TAB, Domain).

-spec for_jid(jid:jid() | jid:ljid()) -> {ok, Host :: jid:lserver()} | error.
for_jid(#jid{} = Jid) -> for_jid(jid:to_lower(Jid));
for_jid({_, _, _} = Jid) ->
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MAPPING_FETCHES, 1),
    {Time, R} = timer:tc(fun do_lookup_jid/1, [Jid]),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MAPPING_FETCH_TIME, Time),
    R.

-spec insert_for_jid(jid:jid() | jid:ljid()) -> ok.
insert_for_jid(Jid) ->
    LocalHost = opt(local_host),
    do_insert_for_jid(Jid, LocalHost, fun put_session/1).

-spec cache_jid(jid:jid() | jid:ljid(), Host :: jid:lserver()) -> ok.
cache_jid(Jid, Host) when is_binary(Host) ->
    do_insert_for_jid(Jid, Host, fun(_) -> ok end).

-spec clear_cache(jid:jid()) -> ok.
clear_cache(#jid{} = Jid) ->
    GlobalHost = opt(global_host),
    case jid:to_lower(Jid) of
        {_, GlobalHost, _} = LJid ->
            [ets_cache:delete(?JID_TAB, J) || J <- normalize_jid(LJid)];
        {_, SubHost, _} ->
            ets_cache:delete(?DOMAIN_TAB, SubHost)
    end.

-spec delete_for_jid(jid:jid() | jid:ljid()) -> ok.
delete_for_jid(#jid{} = Jid) -> delete_for_jid(jid:to_lower(Jid));
delete_for_jid({_, _, _} = Jid) ->
    lists:foreach(
      fun(BinJid) ->
              delete_session(BinJid),
              ets_cache:delete(?JID_TAB, BinJid)
      end,
      normalize_jid(Jid)).

-spec all_domains() -> {ok, [jid:lserver()]}.
all_domains() ->
    mod_global_distrib_mapping_backend:get_domains().

-spec public_domains() -> {ok, [jid:lserver()]}.
public_domains() ->
    mod_global_distrib_mapping_backend:get_public_domains().

-spec endpoints(Host :: jid:lserver()) -> {ok, [endpoint()]}.
endpoints(Host) ->
    mod_global_distrib_mapping_backend:get_endpoints(Host).

-spec hosts() -> [Host :: jid:lserver()].
hosts() ->
    mod_global_distrib_mapping_backend:get_hosts().

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: jid:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    AdvEndpoints = get_advertised_endpoints(Opts0),
    Opts = [{advertised_endpoints, AdvEndpoints}, {backend, redis}, {redis, [no_opts]}, {cache_missed, true},
            {domain_lifetime_seconds, 600}, {jid_lifetime_seconds, 5}, {max_jids, 10000} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: jid:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

-spec deps(Host :: jid:server(), Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Host, Opts) ->
    mod_global_distrib_utils:deps(?MODULE, Host, Opts, fun deps/1).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec session_opened(mongoose_acc:t(), ejabberd_sm:sid(), UserJID :: jid:jid(), Info :: list()) ->
    mongoose_acc:t().
session_opened(Acc, _SID, UserJid, _Info) ->
    insert_for_jid(UserJid),
    Acc.

-spec session_closed(mongoose_acc:t(),
                     ejabberd_sm:sid(),
                     UserJID :: jid:jid(),
                     Info :: list(),
                     _Status :: any()) ->
    mongoose_acc:t().
session_closed(Acc, _SID, UserJid, _Info, _Reason) ->
    delete_for_jid(UserJid),
    Acc.

-spec packet_to_component(Acc :: mongoose_acc:t(),
                          From :: jid:jid(),
                          To :: jid:jid()) -> mongoose_acc:t().
packet_to_component(Acc, From, _To) ->
    mod_global_distrib_utils:maybe_update_mapping(From, Acc),
    Acc.

-spec register_subhost(any(), SubHost :: binary(), IsHidden :: boolean()) -> ok.
register_subhost(_, SubHost, IsHidden) ->
    IsSubhostOf =
        fun(Host) ->
                case binary:match(SubHost, Host) of
                    {Start, Length} -> Start + Length == byte_size(SubHost);
                    _ -> false
                end
        end,

    GlobalHost = opt(global_host),
    case lists:filter(IsSubhostOf, ?MYHOSTS) of
        [GlobalHost] -> insert_for_domain(SubHost, IsHidden);
        _ -> ok
    end.

-spec unregister_subhost(any(), SubHost :: binary()) -> ok.
unregister_subhost(_, SubHost) ->
    delete_for_domain(SubHost).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec deps(proplists:proplist()) -> gen_mod:deps_list().
deps(_Opts) ->
    [{mod_global_distrib_receiver, hard}].

-spec start() -> any().
start() ->
    Host = opt(global_host),
    Backend = opt(backend),
    gen_mod:start_backend_module(?MODULE, [{backend, Backend}]),
    mod_global_distrib_mapping_backend:start(opt(Backend)),

    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MAPPING_FETCH_TIME, histogram),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MAPPING_FETCHES, spiral),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MAPPING_CACHE_MISSES, spiral),

    CacheMissed = opt(cache_missed),
    DomainLifetime = opt(domain_lifetime_seconds) * 1000,
    JidLifetime = opt(jid_lifetime_seconds) * 1000,
    MaxJids = opt(max_jids),

    ejabberd_hooks:add(register_subhost, global, ?MODULE, register_subhost, 90),
    ejabberd_hooks:add(unregister_subhost, global, ?MODULE, unregister_subhost, 90),
    ejabberd_hooks:add(packet_to_component, global, ?MODULE, packet_to_component, 90),
    ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, session_opened, 90),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, session_closed, 90),

    ets_cache:new(?DOMAIN_TAB, [{cache_missed, CacheMissed}, {life_time, DomainLifetime}]),
    ets_cache:new(?JID_TAB, [{cache_missed, CacheMissed}, {life_time, JidLifetime},
                             {max_size, MaxJids}]).

-spec stop() -> any().
stop() ->
    Host = opt(global_host),

    ets_cache:delete(?JID_TAB),
    ets_cache:delete(?DOMAIN_TAB),

    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, session_closed, 90),
    ejabberd_hooks:delete(sm_register_connection_hook, Host, ?MODULE, session_opened, 90),
    ejabberd_hooks:delete(packet_to_component, global, ?MODULE, packet_to_component, 90),
    ejabberd_hooks:delete(unregister_subhost, global, ?MODULE, unregister_subhost, 90),
    ejabberd_hooks:delete(register_subhost, global, ?MODULE, register_subhost, 90),

    mod_global_distrib_mapping_backend:stop().

-spec normalize_jid(jid:ljid()) -> [binary()].
normalize_jid({_, _, _} = FullJid) ->
    case jid:to_bare(FullJid) of
        FullJid -> [jid:to_binary(FullJid)];
        BareJid -> [jid:to_binary(FullJid), jid:to_binary(BareJid)]
    end.

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

-spec get_session(Key :: binary()) -> {ok, term()} | error.
get_session(Key) ->
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MAPPING_CACHE_MISSES, 1),
    mod_global_distrib_mapping_backend:get_session(Key).

-spec put_session(Key :: binary()) -> ok.
put_session(Key) ->
    ?DEBUG("event=put_session key=~ts", [Key]),
    mod_global_distrib_mapping_backend:put_session(Key).

-spec delete_session(Key :: binary()) -> ok.
delete_session(Key) ->
    ?DEBUG("event=delete_session key=~ts", [Key]),
    mod_global_distrib_mapping_backend:delete_session(Key).

-spec get_domain(Key :: binary()) -> {ok, term()} | error.
get_domain(Key) ->
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MAPPING_CACHE_MISSES, 1),
    mod_global_distrib_mapping_backend:get_domain(Key).

-spec put_domain(Key :: binary(), IsHidden :: boolean()) -> ok.
put_domain(Key, IsHidden) ->
    mod_global_distrib_mapping_backend:put_domain(Key, IsHidden).

-spec delete_domain(Key :: binary()) -> ok.
delete_domain(Key) ->
    mod_global_distrib_mapping_backend:delete_domain(Key).

-spec do_insert_for_jid(jid:jid() | jid:ljid(), Host :: jid:lserver(),
                        PutSession :: fun((binary()) -> ok | error)) -> ok.
do_insert_for_jid(#jid{} = Jid, Host, PutSession) ->
    do_insert_for_jid(jid:to_lower(Jid), Host, PutSession);
do_insert_for_jid({_, _, _} = Jid, Host, PutSession) ->
    lists:foreach(
      fun(BinJid) ->
              ets_cache:update(?JID_TAB, BinJid, {ok, Host}, fun() -> PutSession(BinJid) end)
      end,
      normalize_jid(Jid)).

-spec do_insert_for_domain(Domain :: binary(), Host :: jid:lserver(),
                           PutDomain :: fun((binary()) -> ok | error)) -> ok.
do_insert_for_domain(Domain, Host, PutDomain) ->
    ets_cache:update(?DOMAIN_TAB, Domain, {ok, Host}, fun() -> PutDomain(Domain) end).

-spec do_lookup_jid(jid:ljid()) -> {ok, Host :: jid:lserver()} | error.
do_lookup_jid({_, _, _} = Jid) ->
    BinJid = jid:to_binary(Jid),
    LookupInDB = fun(BJid) -> fun() -> get_session(BJid) end end,
    case ets_cache:lookup(?JID_TAB, BinJid, LookupInDB(BinJid)) of
        {ok, _} = Result -> Result;
        Other ->
            case jid:to_bare(Jid) of
                Jid -> Other;
                BareJid -> ets_cache:lookup(?JID_TAB, BinJid, LookupInDB(jid:to_binary(BareJid)))
            end
    end.

-spec get_advertised_endpoints(Opts :: list()) -> [endpoint()].
get_advertised_endpoints(Opts) ->
    Conns = proplists:get_value(connections, Opts, []),
    proplists:get_value(advertised_endpoints, Conns, false).
