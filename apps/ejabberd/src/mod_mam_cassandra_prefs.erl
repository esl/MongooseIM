%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using Cassandra.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_cassandra_prefs).
-behaviour(mongoose_cassandra).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_prefs).
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/3]).

-export([prepared_queries/0]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(ejabberd:server(), _) -> 'ok'.
start(Host, Opts) ->
    compile_params_module(Opts),
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            start_pm(Host, Opts);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            start_muc(Host, Opts);
        false ->
            ok
    end.


-spec stop(ejabberd:server()) -> 'ok'.
stop(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            stop_pm(Host);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            stop_muc(Host);
        false ->
            ok
    end.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

-spec start_pm(ejabberd:server(), _) -> 'ok'.
start_pm(Host, _Opts) ->
    ejabberd_hooks:add(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_pm(ejabberd:server()) -> 'ok'.
stop_pm(Host) ->
    ejabberd_hooks:delete(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc_muc

-spec start_muc(ejabberd:server(), _) -> 'ok'.
start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_muc(ejabberd:server()) -> 'ok'.
stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.

%% ----------------------------------------------------------------------

prepared_queries() ->
    [
     {set_prefs_ts_query,
      "INSERT INTO mam_config(user_jid, remote_jid, behaviour) VALUES (?, ?, ?) USING TIMESTAMP ?"},
     {get_prefs_query,
      "SELECT remote_jid, behaviour FROM mam_config WHERE user_jid = ?"},
     {get_behaviour_bare_query,
      "SELECT remote_jid, behaviour FROM mam_config WHERE user_jid = ? AND remote_jid IN ('', ?)"},
     {get_behaviour_full_query,
      "SELECT remote_jid, behaviour FROM mam_config WHERE user_jid = ? AND remote_jid",
      "IN ('', ?, ?)"},
     {del_prefs_ts_query,
      "DELETE FROM mam_config USING TIMESTAMP ? WHERE user_jid = ?"}
    ].

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec get_behaviour(Default :: mod_mam:archive_behaviour(),
                    Host :: ejabberd:server(), ArchiveID :: mod_mam:archive_id(),
                    LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid()) -> any().
get_behaviour(DefaultBehaviour, Host, _UserID, LocJID, RemJID) ->
    BUserJID = bare_jid(LocJID),
    BRemBareJID = bare_jid(RemJID),
    BRemJID = full_jid(RemJID),
    case query_behaviour(Host, LocJID, BUserJID, BRemJID, BRemBareJID) of
        {ok, []} ->
            DefaultBehaviour;
        {ok, [_ | _] = Rows} ->
            %% After sort <<>>, <<"a">>, <<"a/b">>
            [_, Behavour] = lists:last(lists:sort(Rows)),
            decode_behaviour(Behavour)
    end.


-spec set_prefs(Result :: any(), Host :: ejabberd:server(),
                ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid(),
                DefaultMode :: mod_mam:archive_behaviour(),
                AlwaysJIDs :: [ejabberd:literal_jid()],
                NeverJIDs :: [ejabberd:literal_jid()]) -> any().
set_prefs(_Result, Host, _UserID, UserJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    try
        set_prefs1(Host, UserJID, DefaultMode, AlwaysJIDs, NeverJIDs)
    catch Type:Error ->
            Stacktrace = erlang:get_stacktrace(),
            ?ERROR_MSG("issue=\"set_prefs failed\", reason=~p:~p, stacktrace=~p",
                       [Type, Error, Stacktrace]),
            {error, Error}
    end.

set_prefs1(_Host, UserJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    PoolName = pool_name(UserJID),
    BUserJID = bare_jid(UserJID),
    %% Force order of operations using timestamps
    %% http://stackoverflow.com/questions/30317877/cassandra-batch-statement-execution-order
    Now = mongoose_cassandra:now_timestamp(),
    Next = Now + 1,
    DelParams = [Now, BUserJID],
    MultiParams = [[BUserJID, <<>>, encode_behaviour(DefaultMode), Next]]
        ++ [[BUserJID, BinJID, <<"A">>, Next] || BinJID <- AlwaysJIDs]
        ++ [[BUserJID, BinJID, <<"N">>, Next] || BinJID <- NeverJIDs],
    DelQuery = {del_prefs_ts_query, DelParams},
    SetQuries = [{set_prefs_ts_query, Params} || Params <- MultiParams],
    Queries = [DelQuery | SetQuries],
    Res = mongoose_cassandra_worker:cql_batch_pool(PoolName, UserJID, ?MODULE, Queries),
    ?DEBUG("issue=set_prefs1, result=~p", [Res]),
    ok.


-spec get_prefs(mod_mam:preference(), _Host :: ejabberd:server(),
                ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid())
               -> mod_mam:preference().
get_prefs({GlobalDefaultMode, _, _}, _Host, _UserID, UserJID) ->
    BUserJID = bare_jid(UserJID),
    PoolName = pool_name(UserJID),
    Params = [BUserJID],
    {ok, Rows} = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE,
                                                          get_prefs_query, Params),
    decode_prefs_rows(Rows, GlobalDefaultMode, [], []).


-spec remove_archive(ejabberd:server(), mod_mam:archive_id(),
                     ejabberd:jid()) -> 'ok'.
remove_archive(_Host, _UserID, UserJID) ->
    PoolName = pool_name(UserJID),
    BUserJID = bare_jid(UserJID),
    Now = mongoose_cassandra:now_timestamp(),
    Params = [Now, BUserJID],
    mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, del_prefs_ts_query,
                                             Params),
    ok.


-spec query_behaviour(ejabberd:server(), UserJID :: ejabberd:jid(), BUserJID :: binary() | string(),
                      BRemJID :: binary() | string(), BRemBareJID :: binary() | string()) -> any().
query_behaviour(_Host, UserJID, BUserJID, BRemJID, BRemBareJID) ->
    PoolName = pool_name(UserJID),
    case BRemJID of
        BRemBareJID ->
            Params = [BUserJID, BRemBareJID],
            mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE,
                                                     get_behaviour_bare_query, Params);
        _ ->
            Params = [BUserJID, BRemJID, BRemBareJID],
            mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE,
                                                     get_behaviour_full_query, Params)
    end.

%% ----------------------------------------------------------------------
%% Helpers

-spec encode_behaviour('always' | 'never' | 'roster') -> binary().
encode_behaviour(roster) -> <<"R">>;
encode_behaviour(always) -> <<"A">>;
encode_behaviour(never) -> <<"N">>.


-spec decode_behaviour(<<_:8>>) -> 'always' | 'never' | 'roster'.
decode_behaviour(<<"R">>) -> roster;
decode_behaviour(<<"A">>) -> always;
decode_behaviour(<<"N">>) -> never.

bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_binary(jid:to_bare(jid:to_lower(JID))).

full_jid(undefined) -> undefined;
full_jid(JID) ->
    jid:to_binary(jid:to_lower(JID)).

-spec decode_prefs_rows([[term()]],
                        DefaultMode :: mod_mam:archive_behaviour(),
                        AlwaysJIDs :: [ejabberd:literal_jid()],
                        NeverJIDs :: [ejabberd:literal_jid()]) -> {
                         mod_mam:archive_behaviour(),
                         [ejabberd:literal_jid()],
                         [ejabberd:literal_jid()]
                        }.
decode_prefs_rows([[<<>>, Behavour] | Rows], _DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, decode_behaviour(Behavour), AlwaysJIDs, NeverJIDs);
decode_prefs_rows([[JID, <<"A">>] | Rows], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, [JID | AlwaysJIDs], NeverJIDs);
decode_prefs_rows([[JID, <<"N">>] | Rows], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, AlwaysJIDs, [JID | NeverJIDs]);
decode_prefs_rows([], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs}.


%% ----------------------------------------------------------------------
%% Dynamic params module

compile_params_module(Params) ->
    CodeStr = params_helper(Params),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_mam_cassandra_prefs_params.erl", Code).

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
                                      "-module(mod_mam_cassandra_prefs_params).~n"
                                      "-compile(export_all).~n"
                                      "pool_name() -> ~p.~n",
                                      [proplists:get_value(pool_name, Params, default)
                                      ]))).

-spec pool_name(ejabberd:jid()) -> term().
pool_name(_UserJID) ->
    mod_mam_cassandra_prefs_params:pool_name().
