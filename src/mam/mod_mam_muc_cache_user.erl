-module(mod_mam_muc_cache_user).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([cached_archive_id/3,
         store_archive_id/3,
         remove_archive/4]).

-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-define(TABLE, mam_muc_user_cache).
-record(state, {}).
-record(mam_muc_user_cache, {key, room_id}).

make_key(HostType, #jid{lserver = LServer, luser = LUser}) ->
    {HostType, LServer, LUser}.

make_record(HostType, ArcJID, UserID) ->
    #mam_muc_user_cache{key = make_key(HostType, ArcJID), room_id = UserID}.

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(HostType, _Opts) ->
    init_mnesia(),
    start_server(HostType),
    start_hooks(HostType),
    ok.

stop(HostType) ->
    stop_hooks(HostType),
    stop_server(HostType),
    clean_mnesia(HostType),
    ok.

%% ----------------------------------------------------------------------
%% Init

start_hooks(HostType) ->
    ejabberd_hooks:add(mam_muc_archive_id, HostType, ?MODULE, cached_archive_id, 30),
    ejabberd_hooks:add(mam_muc_archive_id, HostType, ?MODULE, store_archive_id, 70),
    ejabberd_hooks:add(mam_muc_remove_archive, HostType, ?MODULE, remove_archive, 100).

stop_hooks(HostType) ->
    ejabberd_hooks:delete(mam_muc_archive_id, HostType, ?MODULE, cached_archive_id, 30),
    ejabberd_hooks:delete(mam_muc_archive_id, HostType, ?MODULE, store_archive_id, 70),
    ejabberd_hooks:delete(mam_muc_remove_archive, HostType, ?MODULE, remove_archive, 100).

start_server(HostType) ->
    supervisor:start_child(ejabberd_sup, writer_child_spec(HostType)).

stop_server(HostType) ->
    Proc = srv_name(HostType),
    supervisor:terminate_child(mod_mam_sup, Proc),
    supervisor:delete_child(mod_mam_sup, Proc).

writer_child_spec(HostType) ->
    Id = {?MODULE, HostType},
    MFA = {?MODULE, start_link, [HostType]},
    {Id, MFA, permanent, 5000, worker, [?MODULE]}.

%% ----------------------------------------------------------------------
%% API

start_link(HostType) ->
    gen_server:start_link({local, srv_name(HostType)}, ?MODULE, [], []).

cached_archive_id(undefined, HostType, ArcJID) ->
    case lookup_archive_id(HostType, ArcJID) of
        not_found ->
            put(mam_not_cached_flag, true),
            undefined;
        UserID ->
            UserID
    end.

store_archive_id(UserID, HostType, ArcJID) ->
    maybe_cache_archive_id(HostType, ArcJID, UserID),
    UserID.

remove_archive(Acc, HostType, _UserID, ArcJID) ->
    clean_cache(HostType, ArcJID),
    Acc.

%% ----------------------------------------------------------------------
%% Internal functions

srv_name(HostType) ->
    gen_mod:get_module_proc(HostType, ?MODULE).

init_mnesia() ->
    %% Replicates data across other nodes
    mnesia:create_table(?TABLE, table_opts()),
    mnesia:add_table_copy(?TABLE, node(), ram_copies).

table_opts() ->
    [{ram_copies, [node()]}, {type, ordered_set},
     {attributes, record_info(fields, ?TABLE)}].

clean_mnesia(HostType) ->
    %% Clean mnesia locally
    %% We don't want to replicate this across the cluster for performance reasons
    catch ets:match_delete(?TABLE, {?TABLE, {HostType, '_', '_'}, '_'}).

maybe_cache_archive_id(HostType, ArcJID, UserID) ->
    case {erase(mam_not_cached_flag), is_integer(UserID)} of
        {true, true} ->
            Rec = make_record(HostType, ArcJID, UserID),
            gen_server:cast(srv_name(HostType), {cache_archive_id, Rec});
        _ ->
            ok
    end.

lookup_archive_id(HostType, ArcJID) ->
    try
        ets:lookup_element(?TABLE, make_key(HostType, ArcJID), #mam_muc_user_cache.room_id)
    catch error:badarg ->
        not_found
    end.

clean_cache(HostType, ArcJID) ->
    Key = make_key(HostType, ArcJID),
    gen_server:cast(srv_name(HostType), {clean_cache, Key}).

%% ----------------------------------------------------------------------
%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call(Msg, From, State) ->
    ?LOG_WARNING(#{what => unexpected_call, msg => Msg, call_from => From}),
    {reply, ok, State}.

handle_cast({cache_archive_id, Rec}, State) ->
    mnesia:dirty_write(Rec),
    {noreply, State};
handle_cast({clean_cache, Key}, State) ->
    mnesia:dirty_delete({?TABLE, Key}),
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
