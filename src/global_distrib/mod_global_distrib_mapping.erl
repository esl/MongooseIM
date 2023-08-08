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

-export([start/2, stop/1, hooks/1, deps/2]).
-export([for_domain/1, insert_for_domain/1, insert_for_domain/2, insert_for_domain/3,
         cache_domain/2, delete_for_domain/1, all_domains/0, public_domains/0]).
-export([for_jid/1, insert_for_jid/1, cache_jid/2, delete_for_jid/1, clear_cache/1]).
-export([register_subhost/3, unregister_subhost/3, packet_to_component/3,
         session_opened/3, session_closed/3]).
-export([endpoints/1, hosts/0]).

-ignore_xref([
    delete_for_domain/1, delete_for_jid/1, insert_for_domain/1,
    insert_for_domain/2, insert_for_domain/3, insert_for_jid/1
]).

-type endpoint() :: mod_global_distrib_utils:endpoint().

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

-spec all_domains() -> [jid:lserver()].
all_domains() ->
    mod_global_distrib_mapping_backend:get_domains().

-spec public_domains() -> [jid:lserver()].
public_domains() ->
    mod_global_distrib_mapping_backend:get_public_domains().

-spec endpoints(Host :: jid:lserver()) -> [endpoint()].
endpoints(Host) ->
    mod_global_distrib_mapping_backend:get_endpoints(Host).

-spec hosts() -> [Host :: jid:lserver()].
hosts() ->
    mod_global_distrib_mapping_backend:get_hosts().

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(_HostType, Opts = #{cache := CacheOpts}) ->
    mod_global_distrib_mapping_backend:start(Opts#{backend => redis}),

    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MAPPING_FETCH_TIME, histogram),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MAPPING_FETCHES, spiral),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MAPPING_CACHE_MISSES, spiral),

    #{cache_missed := CacheMissed,
      domain_lifetime_seconds := DomainLifetimeSec,
      jid_lifetime_seconds := JidLifeTimeSec,
      max_jids := MaxJids} = CacheOpts,
    DomainLifetime = timer:seconds(DomainLifetimeSec),
    JidLifetime = timer:seconds(JidLifeTimeSec),


    ets_cache:new(?DOMAIN_TAB, [{cache_missed, CacheMissed}, {life_time, DomainLifetime}]),
    ets_cache:new(?JID_TAB, [{cache_missed, CacheMissed}, {life_time, JidLifetime},
                             {max_size, MaxJids}]).

-spec stop(mongooseim:host_type()) -> any().
stop(_HostType) ->
    ets_cache:delete(?JID_TAB),
    ets_cache:delete(?DOMAIN_TAB),
    mod_global_distrib_mapping_backend:stop().

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    [{mod_global_distrib_utils, Opts, hard},
     {mod_global_distrib_receiver, Opts, hard}].

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec session_opened(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: any(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
session_opened(Acc, #{jid := UserJid}, _) ->
    insert_for_jid(UserJid),
    {ok, Acc}.

-spec session_closed(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
session_closed(Acc, #{jid := UserJid}, _) ->
    delete_for_jid(UserJid),
    {ok, Acc}.

-spec packet_to_component(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{from := jid:jid()},
      Extra :: gen_hook:extra().
packet_to_component(Acc, #{from := From}, _) ->
    mod_global_distrib_utils:maybe_update_mapping(From, Acc),
    {ok, Acc}.

-spec register_subhost(Acc, Params, Extra) -> {ok, ok} when
      Acc :: any(),
      Params :: #{ldomain := binary(), is_hidden := boolean()},
      Extra :: gen_hook:extra().
register_subhost(_, #{ldomain := SubHost, is_hidden := IsHidden}, _) ->
    IsSubhostOf =
        fun(Host) ->
                case binary:match(SubHost, Host) of
                    {Start, Length} -> Start + Length == byte_size(SubHost);
                    _ -> false
                end
        end,

    GlobalHost = opt(global_host),
    NewAcc = case lists:filter(IsSubhostOf, ?MYHOSTS) of
        [GlobalHost] -> insert_for_domain(SubHost, IsHidden);
        _ -> ok
    end,
    {ok, NewAcc}.

-spec unregister_subhost(Acc, Params, Extra) -> {ok, ok} when
      Acc :: any(),
      Params :: #{ldomain := binary()},
      Extra :: gen_hook:extra().
unregister_subhost(_, #{ldomain := SubHost}, _) ->
    {ok, delete_for_domain(SubHost)}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

hooks(HostType) ->
    [{register_subhost, global, fun ?MODULE:register_subhost/3, #{}, 90},
     {unregister_subhost, global, fun ?MODULE:unregister_subhost/3, #{}, 90},
     {packet_to_component, global, fun ?MODULE:packet_to_component/3, #{}, 90},
     {sm_register_connection_hook, HostType, fun ?MODULE:session_opened/3, #{}, 90},
     {sm_remove_connection_hook, HostType, fun ?MODULE:session_closed/3, #{}, 90}].

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
    ?LOG_DEBUG(#{what => gd_mapper_put_session, key => Key}),
    mod_global_distrib_mapping_backend:put_session(Key).

-spec delete_session(Key :: binary()) -> ok.
delete_session(Key) ->
    ?LOG_DEBUG(#{what => gd_mapper_delete_session, key => Key}),
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
