-module(mongoose_shaper).

-export([child_spec/0]).
-export([new/1, update/2, wait/5, reset_all_shapers/1]).
-ignore_xref([reset_all_shapers/1]).

-type name() :: atom(). % special value 'none' means no shaper
-type shaper() :: opuntia:shaper().
-export_type([name/0, shaper/0]).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    WPoolOpts = [{workers, 10}, {worker, {opuntia_srv, {?MODULE, #{}}}}],
    #{id => ?MODULE,
      start => {wpool, start_pool, [?MODULE, WPoolOpts]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor}.

-spec new(name()) -> opuntia:shaper().
new(Name) ->
    opuntia:new(get_shaper_config(Name)).

-spec update(opuntia:shaper(), opuntia:tokens()) -> {opuntia:shaper(), opuntia:delay()}.
update(Shaper, Tokens) ->
    opuntia:update(Shaper, Tokens).

%% @doc Shapes the caller from executing the action.
-spec wait(HostType :: mongooseim:host_type_or_global(),
           Domain :: jid:lserver(),
           Action :: atom(),
           FromJID :: jid:jid(),
           Size :: integer()) -> continue | {error, max_delay_reached}.
wait(HostType, Domain, Action, FromJID, Size) ->
    Worker = wpool_pool:hash_worker(?MODULE, FromJID),
    Config = get_shaper_config(get_shaper_name(HostType, Domain, Action, FromJID)),
    Key = new_key(Domain, Action, FromJID),
    opuntia_srv:wait(Worker, Key, Size, Config).

-type key() :: {global | jid:server(), atom(), jid:jid()}.
-spec new_key(jid:server() | global, atom(), jid:jid()) -> key().
new_key(Domain, Action, FromJID) ->
    {Domain, Action, FromJID}.

%% @doc Ask all shaper servers to forget current shapers and read settings again
reset_all_shapers(_HostType) ->
    [ opuntia_srv:reset_shapers(ProcName) || ProcName <- wpool:get_workers(?MODULE) ],
    ok.

-spec get_shaper_name(HostType :: mongooseim:host_type_or_global(),
                      Domain :: global | jid:server(),
                      Action :: atom(),
                      FromJID :: jid:jid()) -> name().
get_shaper_name(HostType, Domain, Action, FromJID) ->
    case acl:match_rule(HostType, Domain, Action, FromJID) of
        deny -> none;
        ShaperName -> ShaperName
    end.

-spec get_shaper_config(name()) -> opuntia:config().
get_shaper_config(none) ->
    0;
get_shaper_config(Name) ->
    MaxRatePerSecond = mongoose_config:get_opt([shaper, Name, max_rate]),
    #{bucket_size => MaxRatePerSecond, rate => MaxRatePerSecond,
      time_unit => second, start_full => true}.
