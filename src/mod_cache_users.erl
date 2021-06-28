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

%% API
-export([start/2]).
-export([stop/1]).
-export([config_spec/0]).
-export([supported_features/0]).

-export([does_user_exist/2]).

%% Hooks.
-export([remove_user/3]).
-export([remove_domain/3]).

-include("mongoose_config_spec.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

tbl_name() ->
    mongoose_users.

-spec tbl_name(mongooseim:host_type()) -> atom().
tbl_name(HostType) ->
    gen_mod:get_module_proc(HostType, tbl_name()).

%%====================================================================
%% API
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    start_cache(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    stop_cache(HostType),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{
                       <<"time_to_live">> => #option{type = integer, format = {kv, ttl}},
                       <<"number_of_segments">> => #option{type = integer}
                      }}.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [
     {remove_user, HostType, ?MODULE, remove_user, 30},
     {remove_domain, HostType, ?MODULE, remove_domain, 30}
    ].

-spec does_user_exist(HostType :: mongooseim:host_type(), JID :: jid:jid()) -> boolean().
does_user_exist(HostType, #jid{luser = LUser, lserver = LServer} = JID) ->
    case does_cached_user_exist(HostType, LServer, LUser) of
        true -> true;
        false ->
            case does_stored_user_exist(HostType, JID) of
                true ->
                    put_user_into_cache(HostType, LServer, LUser),
                    true;
                false -> false
            end
    end.

%%====================================================================
%% Hooks
%%====================================================================

-spec remove_user(Acc :: mongoose_acc:t(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver() | string()) -> mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    HostType = mongoose_acc:host_type(Acc),
    Key = key(LUser, LServer),
    Tab = tbl_name(HostType),
    del_member(Tab, Key, ets:first(Tab)),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    Tab = tbl_name(HostType),
    del_domain(Tab, Domain, ets:first(Tab)),
    Acc.

%%====================================================================
%% Helpers
%%====================================================================

-spec does_stored_user_exist(mongooseim:host_type(), jid:jid()) -> boolean().
does_stored_user_exist(HostType, JID) ->
    true =:= ejabberd_auth:does_stored_user_exist(HostType, JID).

-spec does_cached_user_exist(mongooseim:host_type(), jid:lserver(), jid:luser()) -> boolean().
does_cached_user_exist(HostType, LServer, LUser) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(HostType),
    is_member(Tab, Key, ets:last(Tab)).

-spec put_user_into_cache(mongooseim:host_type(), jid:lserver(), jid:luser()) -> ok.
put_user_into_cache(HostType, LServer, LUser) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(HostType),
    put_member(Tab, Key, ets:last(Tab)).

-spec key(jid:luser(), jid:lserver()) -> {jid:lserver(), jid:luser()}.
key(LUser, LServer) ->
    {LServer, LUser}.

is_member(_, _, '$end_of_table') ->
    false;
is_member(Tab, Key, Table) ->
    case ets:lookup(Tab, Table) of
        [{_, EtsSegment}] ->
            case ets:member(EtsSegment, Key) of
                false -> is_member(Tab, Key, ets:prev(Tab, Table));
                true -> true
            end;
        [] ->
           is_member(Tab, Key, ets:prev(Tab, Table))
    end.

%% There's a chance that by the time we insert in the ets table, this table is not
%% the first anymore because the cleaner has taken action and pushed it behind.
%% That's fine, worst case this record will live a segment less than expected.
put_member(Tab, Key, Table) ->
    EtsSegment = ets:lookup_element(Tab, Table, 2),
    ets:insert(EtsSegment, {Key}),
    ok.

del_member(_, _, '$end_of_table') ->
    ok;
del_member(Tab, Key, Table) ->
    case ets:lookup(Tab, Table) of
        [{_, EtsSegment}] ->
            ets:delete(EtsSegment, Key),
            del_member(Tab, Key, ets:next(Tab, Table));
        [] ->
            del_member(Tab, Key, ets:next(Tab, Table))
    end.

del_domain(_, _, '$end_of_table') ->
    ok;
del_domain(Tab, Domain, Table) ->
    case ets:lookup(Tab, Table) of
        [{_, EtsSegment}] ->
            ets:match_delete(EtsSegment, {{Domain, '_'}}),
            del_domain(Tab, Domain, ets:next(Tab, Table));
        [] ->
            del_domain(Tab, Domain, ets:next(Tab, Table))
    end.

-spec start_cache(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_cache(HostType, Opts) ->
    Tab = tbl_name(HostType),
    Spec = #{id => Tab, start => {?MODULE, start_link, [Tab, Opts]},
             restart => permanent, shutdown => 5000,
             type => worker, modules => [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(Spec).

-spec stop_cache(mongooseim:host_type()) -> any().
stop_cache(HostType) ->
    ok = ejabberd_sup:stop_child(tbl_name(HostType)).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

-record(cache_state, {tab :: atom()}).

purge_tables(#cache_state{tab = Tab}) ->
    First = ets:first(Tab),
    Last = ets:last(Tab),
    EtsSegment = case ets:info(Tab, size) of
                     1 ->
                         ets:lookup_element(Tab, First, 2);
                     _ ->
                         [{_, E}] = ets:take(Tab, First),
                         true = ets:insert_new(Tab, {Last + 1, E}),
                         E
                 end,
    ets:delete_all_objects(EtsSegment),
    ok.

-spec start_link(atom(), gen_mod:module_opts()) -> {ok, pid()}.
start_link(ParentTab, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ParentTab, Opts], []).

init([ParentTab, Opts]) ->
    TTL = gen_mod:get_opt(ttl, Opts, infinity),
    N   = gen_mod:get_opt(number_of_segments, Opts, 1),
    EtsOpts = [ordered_set, named_table, protected, {read_concurrency, true}],
    ParentTab = ets:new(ParentTab, EtsOpts),
    lists:foreach(
      fun(I) ->
              SegmentOpts = [set, public, {read_concurrency, true}, {write_concurrency, true}],
              Ref = ets:new(ParentTab, SegmentOpts),
              ets:insert(ParentTab, {I, Ref})
      end, lists:seq(1, N)),
    erlang:send_after(TTL, self(), {purge, TTL}),
    {ok, #cache_state{tab = ParentTab}}.

handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info({purge, TTL}, State) ->
    purge_tables(State),
    erlang:send_after(TTL, self(), {purge, TTL}),
    {noreply, State};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.
