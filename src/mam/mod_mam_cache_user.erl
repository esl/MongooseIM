%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Stores cache using ETS-table.
%%% This module is a proxy for `mod_mam_rdbms_user' (it should be started).
%%%
%%% There are 2 hooks for `mam_archive_id':
%%% `cached_archive_id/3' and `store_archive_id/3'.
%%%
%%% This module supports several hosts.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_cache_user).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod handlers
-export([start/2, stop/1, supported_features/0]).

%% ejabberd handlers
-export([cached_archive_id/3,
         store_archive_id/3,
         remove_archive/4]).

%% API
-export([clean_cache/1]).

%% Internal exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {}).

srv_name() ->
    mod_mam_cache.

tbl_name_archive_id() ->
    mod_mam_cache_table_archive_id.

group_name() ->
    mod_mam_cache.

-spec su_key(jid:jid()) -> jid:simple_bare_jid().
su_key(#jid{lserver = LServer, luser = LUser}) ->
    {LServer, LUser}.

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives
-spec start(HostType :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    start_server(),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

writer_child_spec() ->
    MFA = {?MODULE, start_link, []},
    {?MODULE, MFA, permanent, 5000, worker, [?MODULE]}.

start_server() ->
    %% TODO make per host server
    supervisor:start_child(ejabberd_sup, writer_child_spec()).

hooks(HostType) ->
    PM = gen_mod:get_module_opt(HostType, ?MODULE, pm, false),
    MUC = gen_mod:get_module_opt(HostType, ?MODULE, muc, false),
    maybe_pm_hooks(PM, HostType) ++ maybe_muc_hooks(MUC, HostType).

maybe_pm_hooks(true, HostType) -> pm_hooks(HostType);
maybe_pm_hooks(false, _HostType) -> [].

maybe_muc_hooks(true, HostType) -> muc_hooks(HostType);
maybe_muc_hooks(false, _HostType) -> [].

pm_hooks(HostType) ->
    [{mam_archive_id, HostType, ?MODULE, cached_archive_id, 30},
     {mam_archive_id, HostType, ?MODULE, store_archive_id, 70},
     {mam_remove_archive, HostType, ?MODULE, remove_archive, 100}].

muc_hooks(HostType) ->
    [{mam_muc_archive_id, HostType, ?MODULE, cached_archive_id, 30},
     {mam_muc_archive_id, HostType, ?MODULE, store_archive_id, 70},
     {mam_muc_remove_archive, HostType, ?MODULE, remove_archive, 100}].

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() ->
    gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

-spec cached_archive_id(undefined, HostType :: mongooseim:host_type(),
                        ArcJID :: jid:jid()) -> mod_mam:archive_id().
cached_archive_id(undefined, _HostType, ArcJID) ->
    case lookup_archive_id(ArcJID) of
        not_found ->
            put(mam_not_cached_flag, true),
            undefined;
        UserID ->
            UserID
    end.

-spec store_archive_id(mod_mam:archive_id(), mongooseim:host_type(), jid:jid())
        -> mod_mam:archive_id().
store_archive_id(UserID, _HostType, ArcJID) ->
    maybe_cache_archive_id(ArcJID, UserID),
    UserID.

-spec remove_archive(Acc :: map(), HostType :: mongooseim:host_type(),
                     UserID :: mod_mam:archive_id(), ArcJID :: jid:jid()) -> map().
remove_archive(Acc, _HostType, _UserID, ArcJID) ->
    clean_cache(ArcJID),
    Acc.

%%====================================================================
%% Internal functions
%%====================================================================

-spec maybe_cache_archive_id(jid:jid(), mod_mam:archive_id()) -> ok.
maybe_cache_archive_id(ArcJID, UserID) ->
    case erase(mam_not_cached_flag) of
        undefined ->
            ok;
        true ->
            cache_archive_id(ArcJID, UserID)
    end.

%% @doc Put the user id into cache.
%% @private
-spec cache_archive_id(jid:jid(), mod_mam:archive_id()) -> ok.
cache_archive_id(ArcJID, UserID) ->
    gen_server:call(srv_name(), {cache_archive_id, ArcJID, UserID}).

-spec lookup_archive_id(jid:jid()) -> mod_mam:archive_id() | not_found.
lookup_archive_id(ArcJID) ->
    try
        ets:lookup_element(tbl_name_archive_id(), su_key(ArcJID), 2)
    catch error:badarg ->
        not_found
    end.

-spec clean_cache(jid:jid()) -> ok.
clean_cache(ArcJID) ->
    %% Send a broadcast message.
    case pg2:get_members(group_name()) of
        Pids when is_list(Pids) ->
            [gen_server:cast(Pid, {remove_user, ArcJID})
            || Pid <- Pids],
            ok;
        {error, _Reason} -> ok
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    pg2:create(group_name()),
    pg2:join(group_name(), self()),
    TOpts = [named_table, protected,
             {write_concurrency, false},
             {read_concurrency, true}],
    ets:new(tbl_name_archive_id(), TOpts),
    {ok, #state{}}.

handle_call({cache_archive_id, ArcJID, UserID}, _From, State) ->
    ets:insert(tbl_name_archive_id(), {su_key(ArcJID), UserID}),
    {reply, ok, State}.

handle_cast({remove_user, ArcJID}, State) ->
    ets:delete(tbl_name_archive_id(), su_key(ArcJID)),
    {noreply, State};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

