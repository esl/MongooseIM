-module(mongoose_user_cache).

-include("mongoose_config_spec.hrl").

-export([start_new_cache/3, stop_cache/2, handle_telemetry_event/4,
         config_spec/0, process_cache_config/1]).

-export([is_member/3, get_entry/3, merge_entry/4, delete_user/3, delete_domain/3]).

-spec is_member(mongooseim:host_type(), module(), jid:jid()) -> boolean().
is_member(HostType, Module, Jid) ->
    CacheName = cache_name(HostType, Module),
    segmented_cache:is_member(CacheName, key(Jid)).

-spec get_entry(mongooseim:host_type(), module(), jid:jid()) -> term() | not_found.
get_entry(HostType, Module, Jid) ->
    CacheName = cache_name(HostType, Module),
    segmented_cache:get_entry(CacheName, key(Jid)).

-spec merge_entry(mongooseim:host_type(), module(), jid:jid(), map()) -> boolean().
merge_entry(HostType, Module, Jid, Entry) ->
    CacheName = cache_name(HostType, Module),
    segmented_cache:merge_entry(CacheName, key(Jid), Entry).

-spec delete_user(mongooseim:host_type(), module(), jid:jid()) -> true.
delete_user(HostType, Module, Jid) ->
    CacheName = cache_name(HostType, Module),
    segmented_cache:delete_entry(CacheName, key(Jid)).

-spec delete_domain(mongooseim:host_type(), module(), jid:lserver()) -> true.
delete_domain(HostType, Module, Domain) ->
    CacheName = cache_name(HostType, Module),
    segmented_cache:delete_pattern(CacheName, {{'_', Domain}, '_'}).


%% path: (host_config[].)modules.*.cache
config_spec() ->
    #section{
       items = #{<<"module">> => #option{type = atom, validate = {enum, [internal, mod_cache_users]}},
                 <<"strategy">> => #option{type = atom, validate = {enum, [fifo, lru]}},
                 <<"time_to_live">> => #option{type = int_or_infinity, validate = positive},
                 <<"number_of_segments">> => #option{type = integer, validate = positive}
                },
       process = fun ?MODULE:process_cache_config/1
      }.

%% If an external module is provided, disallow any other configuration key
process_cache_config(KVs) ->
    case lists:keyfind(module, 1, KVs) of
        {module, Module} when Module =/= internal -> [{module, _}] = KVs;
        _ -> ok
    end,
    KVs.

-spec start_new_cache(mongooseim:host_type(), module(), gen_mod:module_opts()) -> any().
start_new_cache(HostType, Module, Opts) ->
    CacheName = gen_mod:get_module_proc(HostType, Module),
    CacheOpts = #{merger_fun => gen_mod:get_opt(merger_fun, Opts, fun maps:merge/2),
                  segment_num => gen_mod:get_opt(number_of_segments, Opts, 3),
                  strategy => gen_mod:get_opt(strategy, Opts, fifo),
                  ttl => gen_mod:get_opt(time_to_live, Opts, {hours, 8})},
    Spec = #{id => CacheName, start => {segmented_cache, start_link, [CacheName, CacheOpts]},
             restart => permanent, shutdown => 5000,
             type => worker, modules => [segmented_cache]},
    {ok, _} = ejabberd_sup:start_child(Spec),
    create_metrics(HostType, Module, CacheName),
    ok.

create_metrics(HostType, Module, CacheName) ->
    telemetry:attach(CacheName,
                     [segmented_cache, request],
                     fun ?MODULE:handle_telemetry_event/4,
                     #{host_type => HostType, module => Module}),
    mongoose_metrics:ensure_metric(HostType, [Module, hit], counter),
    mongoose_metrics:ensure_metric(HostType, [Module, miss], counter),
    mongoose_metrics:ensure_metric(HostType, [Module, latency], histogram).

handle_telemetry_event([segmented_cache, request], #{hit := Hit, time := Latency},
                       _, #{host_type := HostType, module := Module}) ->
    case Hit of
        true -> mongoose_metrics:update(HostType, [Module, hit], 1);
        false -> mongoose_metrics:update(HostType, [Module, miss], 1)
    end,
    mongoose_metrics:update(HostType, [Module, latency], Latency),
    ok.

-spec stop_cache(mongooseim:host_type(), module()) -> ok.
stop_cache(HostType, Module) ->
    ok = ejabberd_sup:stop_child(cache_name(HostType, Module)).

-spec cache_name(mongooseim:host_type(), module()) -> atom().
cache_name(HostType, Module) ->
    case gen_mod:get_module_opt(HostType, Module, cache_name, internal) of
        internal -> gen_mod:get_module_proc(HostType, Module);
        ConfiguredModule -> gen_mod:get_module_proc(HostType, ConfiguredModule)
    end.

-compile({inline, [key/1]}).
-spec key(jid:jid()) -> jid:simple_bare_jid().
key(Jid) ->
    jid:to_lus(Jid).
