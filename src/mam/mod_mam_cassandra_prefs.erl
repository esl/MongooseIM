%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using Cassandra.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_cassandra_prefs).
-behaviour(mongoose_cassandra).
-behaviour(gen_mod).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_prefs).
-export([get_behaviour/3,
         get_prefs/3,
         set_prefs/3,
         remove_archive/3]).

-export([prepared_queries/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-type host_type() :: mongooseim:host_type().

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    gen_hook:add_handlers(hooks(HostType, Opts)).

-spec stop(host_type()) -> ok.
stop(HostType) ->
    Opts = gen_mod:get_loaded_module_opts(HostType, ?MODULE),
    gen_hook:delete_handlers(hooks(HostType, Opts)).

%% ----------------------------------------------------------------------
%% Hooks

hooks(HostType, Opts) ->
    lists:flatmap(fun(Type) -> hooks(HostType, Type, Opts) end, [pm, muc]).

hooks(HostType, pm, #{pm := true}) ->
    [{mam_get_behaviour, HostType, fun ?MODULE:get_behaviour/3, #{}, 50},
     {mam_get_prefs, HostType, fun ?MODULE:get_prefs/3, #{}, 50},
     {mam_set_prefs, HostType, fun ?MODULE:set_prefs/3, #{}, 50},
     {mam_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 50}];
hooks(HostType, muc, #{muc := true}) ->
    [{mam_muc_get_behaviour, HostType, fun ?MODULE:get_behaviour/3, #{}, 50},
     {mam_muc_get_prefs, HostType, fun ?MODULE:get_prefs/3, #{}, 50},
     {mam_muc_set_prefs, HostType, fun ?MODULE:set_prefs/3, #{}, 50},
     {mam_muc_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 50}];
hooks(_HostType, _Opt, _Opts) ->
    [].

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

-spec get_behaviour(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:archive_behaviour(),
    Params :: ejabberd_gen_mam_prefs:get_behaviour_params(),
    Extra :: gen_hook:extra().
get_behaviour(DefaultBehaviour,
              #{owner := LocJID, remote := RemJID},
              #{host_type := HostType}) ->
    get_behaviour2(DefaultBehaviour, LocJID, RemJID, HostType);
get_behaviour(DefaultBehaviour,
              #{room := LocJID, remote := RemJID},
              #{host_type := HostType}) ->
    get_behaviour2(DefaultBehaviour, LocJID, RemJID, HostType).

get_behaviour2(DefaultBehaviour, LocJID, RemJID, HostType) ->
    BUserJID = mod_mam_utils:bare_jid(LocJID),
    BRemBareJID = mod_mam_utils:bare_jid(RemJID),
    BRemJID = mod_mam_utils:full_jid(RemJID),
    case query_behaviour(HostType, LocJID, BUserJID, BRemJID, BRemBareJID) of
        {ok, []} ->
            {ok, DefaultBehaviour};
        {ok, [_ | _] = Rows} ->
            %% After sort <<>>, <<"a">>, <<"a/b">>
            SortedRows = lists:sort(
                fun(#{remote_jid := JID1, behaviour := B1},
                    #{remote_jid := JID2, behaviour := B2}) ->
                    {JID1, B1} < {JID2, B2}
                end, Rows),
            #{behaviour := Behaviour} = lists:last(SortedRows),
            {ok, decode_behaviour(Behaviour)}
    end.


-spec set_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: ejabberd_gen_mam_prefs:set_prefs_params(),
    Extra :: gen_hook:extra().
set_prefs(_Result,
          #{owner := UserJID, default_mode := DefaultMode, always_jids := AlwaysJIDs,
            never_jids := NeverJIDs},
          #{host_type := HostType}) ->
    try
        {ok, set_prefs1(HostType, UserJID, DefaultMode, AlwaysJIDs, NeverJIDs)}
    catch Type:Error:StackTrace ->
              ?LOG_ERROR(#{what => mam_set_prefs_failed,
                           user_jid => UserJID, default_mode => DefaultMode,
                           always_jids => AlwaysJIDs, never_jids => NeverJIDs,
                           class => Type, reason => Error, stacktrace => StackTrace}),
            {ok, {error, Error}}
    end.

set_prefs1(HostType, UserJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
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
    Res = [mongoose_cassandra:cql_write(pool_name(HostType), UserJID, ?MODULE, Query, Params)
           || {Query, Params} <- Queries],
    ?LOG_DEBUG(#{what => mam_set_prefs, user_jid => UserJID, default_mode => DefaultMode,
                 always_jids => AlwaysJIDs, never_jids => NeverJIDs, result => Res}),
    ok.

encode_row(BUserJID, BRemoteJID, Behaviour, Timestamp) ->
    #{user_jid => BUserJID, remote_jid => BRemoteJID,
      behaviour => Behaviour, '[timestamp]' => Timestamp}.


-spec get_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc ::  mod_mam:preference(),
    Params :: ejabberd_gen_mam_prefs:get_prefs_params(),
    Extra :: gen_hook:extra().
get_prefs({GlobalDefaultMode, _, _}, #{owner := UserJID}, #{host_type := HostType}) ->
    BUserJID = mod_mam_utils:bare_jid(UserJID),
    Params = #{user_jid => BUserJID},
    {ok, Rows} = mongoose_cassandra:cql_read(pool_name(HostType), UserJID, ?MODULE,
                                             get_prefs_query, Params),
    {ok, decode_prefs_rows(Rows, GlobalDefaultMode, [], [])}.


-spec remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, owner => jid:jid(), room => jid:jid()},
    Extra :: gen_hook:extra().
remove_archive(Acc, #{owner := UserJID}, #{host_type := HostType}) ->
    remove_archive(HostType, UserJID),
    {ok, Acc};
remove_archive(Acc, #{room := UserJID}, #{host_type := HostType}) ->
    remove_archive(HostType, UserJID),
    {ok, Acc}.

remove_archive(HostType, UserJID) ->
    BUserJID = mod_mam_utils:bare_jid(UserJID),
    Now = mongoose_cassandra:now_timestamp(),
    Params = #{'[timestamp]' => Now, user_jid => BUserJID},
    mongoose_cassandra:cql_write(pool_name(HostType), UserJID,
                                 ?MODULE, del_prefs_ts_query, [Params]).

-spec query_behaviour(host_type(), UserJID :: jid:jid(), BUserJID :: binary() | string(),
                      BRemJID :: binary() | string(), BRemBareJID :: binary() | string()) -> any().
query_behaviour(HostType, UserJID, BUserJID, BRemJID, BRemBareJID)
  when BRemJID == BRemBareJID ->
    Params = #{user_jid => BUserJID, remote_jid => BRemBareJID},
    mongoose_cassandra:cql_read(pool_name(HostType), UserJID, ?MODULE,
                                get_behaviour_bare_query, Params);
query_behaviour(HostType, UserJID, BUserJID, BRemJID, BRemBareJID) ->
    Params = #{user_jid => BUserJID, start_remote_jid => BRemJID,
               end_remote_jid => BRemBareJID},
    mongoose_cassandra:cql_read(pool_name(HostType), UserJID, ?MODULE,
                                get_behaviour_full_query, Params).

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

-spec decode_prefs_rows([[term()]], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} when
        DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [jid:literal_jid()],
        NeverJIDs :: [jid:literal_jid()].
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
%% Params getters

-spec pool_name(HostType :: host_type()) -> term().
pool_name(_HostType) ->
    default.
