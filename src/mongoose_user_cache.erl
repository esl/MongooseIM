-module(mongoose_user_cache).

-include("mongoose_config_spec.hrl").

-export([start_new_cache/3, stop_cache/2,
         config_spec/0, process_cache_config/1]).

-export([is_member/3, get_entry/3, merge_entry/4, delete_user/3, delete_domain/3]).

%% Used by small tests
-export([cache_name/2, key/1]).

-ignore_xref([cache_name/2, key/1]).

-spec is_member(mongooseim:host_type(), module(), jid:jid()) -> boolean().
is_member(HostType, Module, Jid) ->
    CacheName = cache_name(HostType, Module),
    mongoose_instrument:span(user_cache_lookup, #{host_type => HostType, cache_name => CacheName},
                             fun segmented_cache:is_member/2, [CacheName, key(Jid)],
                             fun(Time, Result) -> handle_is_member_result(Time, Result, Jid) end).

handle_is_member_result(Time, false, Jid) ->
    #{misses => 1, latency => Time, jid => Jid};
handle_is_member_result(Time, true, Jid) ->
    #{hits => 1, latency => Time, jid => Jid}.

-spec get_entry(mongooseim:host_type(), module(), jid:jid()) -> term() | not_found.
get_entry(HostType, Module, Jid) ->
    CacheName = cache_name(HostType, Module),
    mongoose_instrument:span(user_cache_lookup, #{host_type => HostType, cache_name => CacheName},
                             fun segmented_cache:get_entry/2, [CacheName, key(Jid)],
                             fun(Time, Result) -> handle_get_entry_result(Time, Result, Jid) end).

handle_get_entry_result(Time, not_found, Jid) ->
    #{misses => 1, latency => Time, jid => Jid};
handle_get_entry_result(Time, _Entry, Jid) ->
    #{hits => 1, latency => Time, jid => Jid}.

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
       process = fun ?MODULE:process_cache_config/1,
       defaults = #{<<"module">> => internal,
                    <<"strategy">> => fifo,
                    <<"time_to_live">> => 480,
                    <<"number_of_segments">> => 3}
      }.

%% If an external module is provided, disallow any other configuration key
process_cache_config(#{module := internal} = Config) ->
    Config;
process_cache_config(#{module := Module}) ->
    #{module => Module};
process_cache_config(Config) ->
    Config.

-spec start_new_cache(mongooseim:host_type(), module(), gen_mod:module_opts()) -> any().
start_new_cache(HostType, Module, Opts) ->
    case gen_mod:get_opt(module, Opts, internal) of
        internal -> do_start_new_cache(HostType, Module, Opts);
        _ -> ok
    end.

do_start_new_cache(HostType, Module, Opts) ->
    CacheName = gen_mod:get_module_proc(HostType, Module),
    CacheOpts = #{scope => mim_scope,
                  merger_fun => fun maps:merge/2,
                  segment_num => gen_mod:get_opt(number_of_segments, Opts),
                  strategy => gen_mod:get_opt(strategy, Opts),
                  ttl => gen_mod:get_opt(time_to_live, Opts)},
    Spec = #{id => CacheName, start => {segmented_cache, start_link, [CacheName, CacheOpts]},
             restart => permanent, shutdown => 5000,
             type => worker, modules => [segmented_cache]},
    {ok, _} = ejabberd_sup:start_child(Spec),
    mongoose_instrument:set_up(instrumentation(HostType, CacheName)),
    ok.

instrumentation(HostType, CacheName) ->
    [{user_cache_lookup, #{cache_name => CacheName, host_type => HostType},
      #{metrics => #{hits => counter, misses => counter, latency => histogram}}}].

-spec stop_cache(mongooseim:host_type(), module()) -> ok.
stop_cache(HostType, Module) ->
    CacheName = gen_mod:get_module_proc(HostType, Module),
    mongoose_instrument:tear_down(instrumentation(HostType, CacheName)),
    case gen_mod:get_module_opt(HostType, Module, module, internal) of
        internal -> ok = ejabberd_sup:stop_child(cache_name(HostType, Module));
        _ConfiguredModule -> ok
    end.

-spec cache_name(mongooseim:host_type(), module()) -> atom().
cache_name(HostType, Module) ->
    case gen_mod:get_module_opt(HostType, Module, module, internal) of
        internal -> gen_mod:get_module_proc(HostType, Module);
        ConfiguredModule -> gen_mod:get_module_proc(HostType, ConfiguredModule)
    end.

-compile({inline, [key/1]}).
-spec key(jid:jid()) -> jid:simple_bare_jid().
key(Jid) ->
    jid:to_lus(Jid).
