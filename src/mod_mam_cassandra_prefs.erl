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
         remove_archive/4]).

-export([prepared_queries/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> 'ok'.
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


-spec stop(jid:server()) -> 'ok'.
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

-spec start_pm(jid:server(), _) -> 'ok'.
start_pm(Host, _Opts) ->
    ejabberd_hooks:add(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_pm(jid:server()) -> 'ok'.
stop_pm(Host) ->
    ejabberd_hooks:delete(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc_muc

-spec start_muc(jid:server(), _) -> 'ok'.
start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_muc(jid:server()) -> 'ok'.
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
      "SELECT remote_jid, behaviour FROM mam_config WHERE user_jid = ? AND remote_jid "
      "IN ('', :start_remote_jid, :end_remote_jid)"},
     {del_prefs_ts_query,
      "DELETE FROM mam_config USING TIMESTAMP ? WHERE user_jid = ?"}
    ].

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec get_behaviour(Default :: mod_mam:archive_behaviour(),
                    Host :: jid:server(), ArchiveID :: mod_mam:archive_id(),
                    LocJID :: jid:jid(), RemJID :: jid:jid()) -> any().
get_behaviour(DefaultBehaviour, Host, _UserID, LocJID, RemJID) ->
    BUserJID = mod_mam_utils:bare_jid(LocJID),
    BRemBareJID = mod_mam_utils:bare_jid(RemJID),
    BRemJID = mod_mam_utils:full_jid(RemJID),
    case query_behaviour(Host, LocJID, BUserJID, BRemJID, BRemBareJID) of
        {ok, []} ->
            DefaultBehaviour;
        {ok, [_ | _] = Rows} ->
            %% After sort <<>>, <<"a">>, <<"a/b">>
            SortedRows = lists:sort(
                fun(#{remote_jid := JID1, behaviour := B1},
                    #{remote_jid := JID2, behaviour := B2}) ->
                    {JID1, B1} < {JID2, B2}
                end, Rows),
            #{behaviour := Behaviour} = lists:last(SortedRows),
            decode_behaviour(Behaviour)
    end.


-spec set_prefs(Result :: any(), Host :: jid:server(),
                ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid(),
                DefaultMode :: mod_mam:archive_behaviour(),
                AlwaysJIDs :: [jid:literal_jid()],
                NeverJIDs :: [jid:literal_jid()]) -> any().
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
    BUserJID = mod_mam_utils:bare_jid(UserJID),
    %% Force order of operations using timestamps
    %% http://stackoverflow.com/questions/30317877/cassandra-batch-statement-execution-order
    Now = mongoose_cassandra:now_timestamp(),
    Next = Now + 1,
    DelParams = #{'[timestamp]' => Now, user_jid => BUserJID},
    MultiParams = [encode_row(BUserJID, <<>>, encode_behaviour(DefaultMode), Next)]
        ++ [encode_row(BUserJID, BinJID, <<"A">>, Next) || BinJID <- AlwaysJIDs]
        ++ [encode_row(BUserJID, BinJID, <<"N">>, Next) || BinJID <- NeverJIDs],
    DelQuery = {del_prefs_ts_query, [DelParams]},
    SetQuery = {set_prefs_ts_query, MultiParams},
    Queries = [DelQuery, SetQuery],
    Res = [mongoose_cassandra:cql_write(PoolName, UserJID, ?MODULE, Query, Params)
           || {Query, Params} <- Queries],
    ?DEBUG("issue=set_prefs1, result=~p", [Res]),
    ok.

encode_row(BUserJID, BRemoteJID, Behaviour, Timestamp) ->
    #{user_jid => BUserJID, remote_jid => BRemoteJID,
      behaviour => Behaviour, '[timestamp]' => Timestamp}.


-spec get_prefs(mod_mam:preference(), _Host :: jid:server(),
                ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid())
               -> mod_mam:preference().
get_prefs({GlobalDefaultMode, _, _}, _Host, _UserID, UserJID) ->
    BUserJID = mod_mam_utils:bare_jid(UserJID),
    PoolName = pool_name(UserJID),
    Params = #{user_jid => BUserJID},
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE,
                                             get_prefs_query, Params),
    decode_prefs_rows(Rows, GlobalDefaultMode, [], []).


-spec remove_archive(any(), jid:server(), mod_mam:archive_id(),
                     jid:jid()) -> any().
remove_archive(Acc, _Host, _UserID, UserJID) ->
    PoolName = pool_name(UserJID),
    BUserJID = mod_mam_utils:bare_jid(UserJID),
    Now = mongoose_cassandra:now_timestamp(),
    Params = #{'[timestamp]' => Now, user_jid => BUserJID},
    mongoose_cassandra:cql_write(PoolName, UserJID, ?MODULE, del_prefs_ts_query, [Params]),
    Acc.


-spec query_behaviour(jid:server(), UserJID :: jid:jid(), BUserJID :: binary() | string(),
                      BRemJID :: binary() | string(), BRemBareJID :: binary() | string()) -> any().
query_behaviour(_Host, UserJID, BUserJID, BRemJID, BRemBareJID) ->
    PoolName = pool_name(UserJID),
    case BRemJID of
        BRemBareJID ->
            Params = #{user_jid => BUserJID, remote_jid => BRemBareJID},
            mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE,
                                        get_behaviour_bare_query, Params);
        _ ->
            Params = #{user_jid => BUserJID, start_remote_jid => BRemJID,
                       end_remote_jid => BRemBareJID},
            mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE,
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

-spec decode_prefs_rows([[term()]],
                        DefaultMode :: mod_mam:archive_behaviour(),
                        AlwaysJIDs :: [jid:literal_jid()],
                        NeverJIDs :: [jid:literal_jid()]) -> {
                             mod_mam:archive_behaviour(),
                         [jid:literal_jid()],
                         [jid:literal_jid()]
                        }.
decode_prefs_rows([], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs};

decode_prefs_rows([#{remote_jid := <<>>, behaviour := Behaviour} | Rows],
                  _DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, decode_behaviour(Behaviour), AlwaysJIDs, NeverJIDs);
decode_prefs_rows([#{remote_jid := JID, behaviour := <<"A">>} | Rows],
                  DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, [JID | AlwaysJIDs], NeverJIDs);
decode_prefs_rows([#{remote_jid := JID, behaviour := <<"N">>} | Rows],
                  DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, AlwaysJIDs, [JID | NeverJIDs]).

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

-spec pool_name(jid:jid()) -> term().
pool_name(_UserJID) ->
    mod_mam_cassandra_prefs_params:pool_name().
