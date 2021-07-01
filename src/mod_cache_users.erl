%%% @doc Caches info about non-anonymous users using a queue of ets tables
-module(mod_cache_users).

-behaviour(gen_mod).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/2]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% gen_mod API
-export([start/2]).
-export([stop/1]).
-export([config_spec/0]).
-export([supported_features/0]).

%% Hooks.
-export([does_cached_user_exist/4]).
-export([maybe_put_user_into_cache/4]).
-export([remove_user/3]).
-export([remove_domain/3]).

-include("mongoose_config_spec.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

tbl_name() ->
    mod_cache_users.

-spec tbl_name(mongooseim:host_type()) -> atom().
tbl_name(HostType) ->
    gen_mod:get_module_proc(HostType, tbl_name()).

%%====================================================================
%% API
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    start_server(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    stop_server(HostType),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{
                       <<"time_to_live">> => #option{type = int_or_infinity,
                                                     validate = non_negative,
                                                     format = {kv, ttl}},
                       <<"number_of_segments">> => #option{type = integer}
                      }}.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [
     %% These hooks must run before and after the ejabberd_auth does_user_exist hook
     {does_user_exist, HostType, ?MODULE, does_cached_user_exist, 30},
     {does_user_exist, HostType, ?MODULE, maybe_put_user_into_cache, 70},
     %% It is important that these two handlers happen _after_ ejabberd_auth
     %% but _before_ all other modules
     {remove_user, HostType, ?MODULE, remove_user, 10},
     {remove_domain, HostType, ?MODULE, remove_domain, 20}
    ].

%%====================================================================
%% Hooks
%%====================================================================

-spec remove_user(Acc :: mongoose_acc:t(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver() | string()) -> mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    Key = key(LUser, LServer),
    HostType = mongoose_acc:host_type(Acc),
    send_to_group(HostType, {remove_user, Key}),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    send_to_group(HostType, {remove_domain, Domain}),
    Acc.

send_to_group(HostType, Msg) ->
    ParentName = tbl_name(HostType),
    case pg2:get_members(ParentName) of
        Pids when is_list(Pids) ->
            [gen_server:cast(Pid, Msg) || Pid <- Pids];
        {error, _Reason} ->
            ok
    end.

%%====================================================================
%% Helpers
%%====================================================================

-spec does_cached_user_exist(Status :: boolean(),
                             HostType :: mongooseim:host_type(),
                             Jid :: jid:jid(),
                             RequestType :: ejabberd_auth:exist_type()) ->
    boolean().
does_cached_user_exist(false, HostType, #jid{luser = LUser, lserver = LServer}, stored) ->
    Key = key(LUser, LServer),
    ParentTab = tbl_name(HostType),
    case is_member(tbl_name(HostType), Key, ets:last(ParentTab)) of
        true -> {stop, true};
        false -> false
    end;
does_cached_user_exist(Status, _, _, _) ->
    Status.

-spec maybe_put_user_into_cache(Status :: boolean(),
                                HostType :: mongooseim:host_type(),
                                Jid :: jid:jid(),
                                RequestType :: ejabberd_auth:exist_type()) ->
    boolean().
maybe_put_user_into_cache(true, HostType, #jid{luser = LUser, lserver = LServer}, stored) ->
    Key = key(LUser, LServer),
    ParentTab = tbl_name(HostType),
    put_member(ParentTab, Key, ets:last(ParentTab));
maybe_put_user_into_cache(Status, _, _, _) ->
    Status.

-spec key(jid:luser(), jid:lserver()) -> {jid:lserver(), jid:luser()}.
key(LUser, LServer) ->
    {LServer, LUser}.

is_member(_, _, '$end_of_table') ->
    false;
is_member(ParentTab, Key, SegmentKey) ->
    case ets:lookup(ParentTab, SegmentKey) of
        [{_, EtsSegment}] ->
            case ets:member(EtsSegment, Key) of
                false -> is_member(ParentTab, Key, ets:prev(ParentTab, SegmentKey));
                true -> true
            end;
        [] ->
           is_member(ParentTab, Key, ets:prev(ParentTab, SegmentKey))
    end.

%% There's a chance that by the time we insert in the ets table, this table is not
%% the first anymore because the cleaner has taken action and pushed it behind.
%% That's fine, worst case this record will live a segment less than expected.
put_member(ParentTab, Key, SegmentKey) ->
    EtsSegment = ets:lookup_element(ParentTab, SegmentKey, 2),
    ets:insert(EtsSegment, {Key}).

-spec start_server(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_server(HostType, Opts) ->
    ParentTab = tbl_name(HostType),
    Spec = #{id => ParentTab, start => {?MODULE, start_link, [ParentTab, Opts]},
             restart => permanent, shutdown => 5000,
             type => worker, modules => [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(Spec).

-spec stop_server(mongooseim:host_type()) -> any().
stop_server(HostType) ->
    ok = ejabberd_sup:stop_child(tbl_name(HostType)).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

-record(cache_state, {tab :: atom(),
                      ttl = timer:hours(8) :: timeout()}).

rotate_and_purge_last_segment(#cache_state{tab = ParentTab}) ->
    First = ets:first(ParentTab),
    Last = ets:last(ParentTab),
    EtsSegment = case ets:info(ParentTab, size) of
                     1 ->
                         ets:lookup_element(ParentTab, First, 2);
                     _ ->
                         [{_, E}] = ets:take(ParentTab, First),
                         true = ets:insert_new(ParentTab, {Last + 1, E}),
                         E
                 end,
    ets:delete_all_objects(EtsSegment),
    ok.

iterate_through_tables(ParentTab, Action) when is_atom(ParentTab), is_function(Action, 1) ->
    iterate_(ParentTab, Action, ets:first(ParentTab)).

iterate_(_, _, '$end_of_table') ->
    ok;
iterate_(ParentTab, Action, SegmentKey) when is_atom(ParentTab), is_function(Action, 1) ->
    case ets:lookup(ParentTab, SegmentKey) of
        [{_, EtsSegment}] ->
            Action(EtsSegment),
            iterate_(ParentTab, Action, ets:next(ParentTab, SegmentKey));
        [] ->
            iterate_(ParentTab, Action, ets:next(ParentTab, SegmentKey))
    end.

-spec start_link(atom(), gen_mod:module_opts()) -> {ok, pid()}.
start_link(ParentTab, Opts) ->
    gen_server:start_link(?MODULE, [ParentTab, Opts], []).

init([ParentName, Opts]) ->
    pg2:create(ParentName),
    pg2:join(ParentName, self()),
    TTL0 = gen_mod:get_opt(ttl, Opts, 8 * 60), %% 8h
    N   = gen_mod:get_opt(number_of_segments, Opts, 3),
    EtsOpts = [ordered_set, named_table, protected, {read_concurrency, true}],
    ets:new(ParentName, EtsOpts),
    lists:foreach(
      fun(I) ->
              %% Note, these names are only for debugging purposes,
              %% you're not supposed to use the name as an API
              SegmentTab = list_to_atom(atom_to_list(ParentName) ++ "_" ++ integer_to_list(I)),
              SegmentOpts = [set, named_table, public, {read_concurrency, true}, {write_concurrency, true}],
              Ref = ets:new(SegmentTab, SegmentOpts),
              ets:insert(ParentName, {I, Ref})
      end, lists:seq(1, N)),
    TTL = case TTL0 of
              infinity -> infinity;
              _ ->
                  erlang:send_after(timer:minutes(TTL0), self(), purge),
                  timer:minutes(TTL0)
          end,
    {ok, #cache_state{tab = ParentName, ttl = TTL}}.

handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, ok, State}.

handle_cast({remove_user, Key}, #cache_state{tab = ParentTab} = State) ->
    try
        Action = fun(EtsSegment) -> ets:delete(EtsSegment, Key) end,
        iterate_through_tables(ParentTab, Action)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => remove_user_from_cache_error,
                         class => Class, reason => Reason, stacktrace => Stacktrace})
    end,
    {noreply, State};
handle_cast({remove_domain, Domain}, #cache_state{tab = ParentTab} = State) ->
    try
        Action = fun(EtsSegment) -> ets:match_delete(EtsSegment, {{Domain, '_'}}) end,
        iterate_through_tables(ParentTab, Action)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => remove_domain_from_cache_error,
                         class => Class, reason => Reason, stacktrace => Stacktrace})
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(purge, #cache_state{ttl = TTL} = State) ->
    rotate_and_purge_last_segment(State),
    erlang:send_after(TTL, self(), purge),
    {noreply, State};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.
