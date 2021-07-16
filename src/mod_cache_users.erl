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
-export([terminate/2]).

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

-ignore_xref([does_cached_user_exist/4, maybe_put_user_into_cache/4,
              remove_domain/3, remove_user/3, start_link/2]).

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
                                                     validate = positive,
                                                     format = {kv, ttl}},
                       <<"number_of_segments">> => #option{type = integer,
                                                           validate = positive}
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
    [gen_server:cast({ParentName, Node}, Msg) || Node <- [node() | nodes()]].

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
    ParentName = tbl_name(HostType),
    case is_member(ParentName, Key) of
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
    ParentName = tbl_name(HostType),
    put_member(ParentName, Key);
maybe_put_user_into_cache(Status, _, _, _) ->
    Status.

-compile({inline, [key/2, is_member/2, check_segment/3]}).

-spec key(jid:luser(), jid:lserver()) -> {jid:lserver(), jid:luser()}.
key(LUser, LServer) ->
    {LServer, LUser}.

is_member(ParentName, Key) ->
    Segments = persistent_term:get({ParentName, segments}),
    Size = tuple_size(Segments),
    Index = persistent_term:get({ParentName, index}),
    LastTableToCheck = case Index of
                           1 -> Size;
                           _ -> Index - 1
                       end,
    is_member(Key, Segments, Size, LastTableToCheck, Index).

% if we arrived to the last table we finish here
is_member(Key, Segments, _, LastIndex, LastIndex) ->
    check_segment(Key, Segments, LastIndex);
% if we arrived to the last slot, we check and wrap around
is_member(Key, Segments, Size, LastIndex, Size) ->
    check_segment(Key, Segments, Size) orelse is_member(Key, Segments, Size, LastIndex, 1);
% else we check the current table and if it fails we move forwards
is_member(Key, Segments, Size, LastIndex, Index) ->
    check_segment(Key, Segments, Index) orelse is_member(Key, Segments, Size, LastIndex, Index + 1).

check_segment(Key, Segments, Index) ->
    EtsSegment = element(Index, Segments),
    ets:member(EtsSegment, Key).

%% There's a chance that by the time we insert in the ets table, this table is not
%% the first anymore because the cleaner has taken action and pushed it behind.
%% That's fine, worst case this record will live a segment less than expected.
put_member(ParentName, Key) ->
    Index = persistent_term:get({ParentName, index}),
    Segments = persistent_term:get({ParentName, segments}),
    EtsSegment = element(Index, Segments),
    ets:insert(EtsSegment, {Key}).

-spec start_server(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_server(HostType, Opts) ->
    ParentName = tbl_name(HostType),
    Spec = #{id => ParentName, start => {?MODULE, start_link, [ParentName, Opts]},
             restart => permanent, shutdown => 5000,
             type => worker, modules => [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(Spec).

-spec stop_server(mongooseim:host_type()) -> any().
stop_server(HostType) ->
    ok = ejabberd_sup:stop_child(tbl_name(HostType)).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

-record(cache_state, {key :: atom(), ttl :: timeout()}).

-spec start_link(atom(), gen_mod:module_opts()) -> {ok, pid()}.
start_link(ParentName, Opts) ->
    gen_server:start_link({local, ParentName}, ?MODULE, [ParentName, Opts], []).

init([ParentName, Opts]) ->
    TTL0 = gen_mod:get_opt(ttl, Opts, 8 * 60), %% 8h
    N = gen_mod:get_opt(number_of_segments, Opts, 3),
    SegmentsList = lists:map(
      fun(_) ->
              SegmentOpts = [set, public, {read_concurrency, true}, {write_concurrency, true}],
              ets:new(undefined, SegmentOpts)
      end, lists:seq(1, N)),
    Segments = list_to_tuple(SegmentsList),
    persistent_term:put({ParentName, index}, 1),
    persistent_term:put({ParentName, segments}, Segments),
    TTL = case TTL0 of
              infinity -> infinity;
              _ ->
                  erlang:send_after(timer:minutes(TTL0), self(), purge),
                  timer:minutes(TTL0)
          end,
    {ok, #cache_state{key = ParentName, ttl = TTL}}.

handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, ok, State}.

handle_cast({remove_user, Key}, #cache_state{key = ParentName} = State) ->
    try
        Action = fun(EtsSegment) -> ets:delete(EtsSegment, Key) end,
        iterate_tables(ParentName, Action)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => remove_user_from_cache_error,
                         class => Class, reason => Reason, stacktrace => Stacktrace})
    end,
    {noreply, State};
handle_cast({remove_domain, Domain}, #cache_state{key = ParentName} = State) ->
    try
        Action = fun(EtsSegment) -> ets:match_delete(EtsSegment, {{Domain, '_'}}) end,
        iterate_tables(ParentName, Action)
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

terminate(_Reason, #cache_state{key = ParentName}) ->
    persistent_term:erase({ParentName, index}),
    persistent_term:erase({ParentName, segments}),
    ok.

rotate_and_purge_last_segment(#cache_state{key = ParentName}) ->
    %% Clear the oldest segment (Index-1) and then make it the first
    Index = persistent_term:get({ParentName, index}),
    Segments = persistent_term:get({ParentName, segments}),
    Size = tuple_size(Segments),
    %% If Size was 1, Index would not change, and persistent_term:put would also be a no-op
    NewIndex = case Index of
                   1 -> Size;
                   _ -> Index - 1
               end,
    TableToClear = element(NewIndex, Segments),
    ets:delete_all_objects(TableToClear),
    persistent_term:put({ParentName, index}, NewIndex).

iterate_tables(ParentName, Action) when is_atom(ParentName), is_function(Action, 1) ->
    Segments = persistent_term:get({ParentName, segments}),
    Size = tuple_size(Segments),
    [ begin
          EtsSegment = element(N, Segments),
          Action(EtsSegment)
      end || N <- lists:seq(1, Size) ].
