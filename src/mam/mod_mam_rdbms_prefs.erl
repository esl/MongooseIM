%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using RDBMS.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_rdbms_prefs).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-behaviour(gen_mod).
-export([start/2, stop/1, hooks/1, supported_features/0]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_prefs).
-export([get_behaviour/3,
         get_prefs/3,
         set_prefs/3,
         remove_archive/3]).

-import(mongoose_rdbms, [prepare/4]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives
-spec start(mongooseim:host_type(), _) -> ok.
start(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    PM = gen_mod:get_module_opt(HostType, ?MODULE, pm, false),
    MUC = gen_mod:get_module_opt(HostType, ?MODULE, muc, false),
    maybe_pm_hooks(PM, HostType) ++ maybe_muc_hooks(MUC, HostType).

maybe_pm_hooks(true, HostType) -> pm_hooks(HostType);
maybe_pm_hooks(false, _HostType) -> [].

maybe_muc_hooks(true, HostType) -> muc_hooks(HostType);
maybe_muc_hooks(false, _HostType) -> [].

pm_hooks(HostType) ->
    [{mam_get_behaviour, HostType, fun ?MODULE:get_behaviour/3, #{}, 50},
     {mam_get_prefs, HostType, fun ?MODULE:get_prefs/3, #{}, 50},
     {mam_set_prefs, HostType, fun ?MODULE:set_prefs/3, #{}, 50},
     {mam_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 50}].

muc_hooks(HostType) ->
    [{mam_muc_get_behaviour, HostType, fun ?MODULE:get_behaviour/3, #{}, 50},
     {mam_muc_get_prefs, HostType, fun ?MODULE:get_prefs/3, #{}, 50},
     {mam_muc_set_prefs, HostType, fun ?MODULE:set_prefs/3, #{}, 50},
     {mam_muc_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 50}].

%% Prepared queries
prepare_queries(HostType) ->
    prepare(mam_prefs_insert, mam_config, [user_id, remote_jid, behaviour],
            <<"INSERT INTO mam_config(user_id, remote_jid, behaviour) "
              "VALUES (?, ?, ?)">>),
    prepare(mam_prefs_select, mam_config, [user_id],
            <<"SELECT remote_jid, behaviour "
              "FROM mam_config WHERE user_id=?">>),
    prepare(mam_prefs_select_behaviour, mam_config,
            [user_id, remote_jid],
            <<"SELECT remote_jid, behaviour "
              "FROM mam_config "
              "WHERE user_id=? "
                "AND (remote_jid='' OR remote_jid=?)">>),
    prepare(mam_prefs_select_behaviour2, mam_config,
            [user_id, remote_jid, remote_jid],
            <<"SELECT remote_jid, behaviour "
              "FROM mam_config "
              "WHERE user_id=? "
                "AND (remote_jid='' OR remote_jid=? OR remote_jid=?)">>),
    OrdBy = order_by_remote_jid_in_delete(HostType),
    prepare(mam_prefs_delete, mam_config, [user_id],
            <<"DELETE FROM mam_config WHERE user_id=?", OrdBy/binary>>),
    ok.

order_by_remote_jid_in_delete(HostType) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql -> <<" ORDER BY remote_jid">>;
        _ -> <<>>
    end.

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec get_behaviour(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:archive_behaviour(),
    Params :: ejabberd_gen_mam_prefs:get_behaviour_params(),
    Extra :: gen_hook:extra().
get_behaviour(DefaultBehaviour,
              #{archive_id := UserID, remote := RemJID},
              #{host_type := HostType})
        when is_integer(UserID) ->
    RemLJID      = jid:to_lower(RemJID),
    BRemLBareJID = jid:to_bare_binary(RemLJID),
    BRemLJID     = jid:to_binary(RemLJID),
    case query_behaviour(HostType, UserID, BRemLJID, BRemLBareJID) of
        {selected, []} ->
            {ok, DefaultBehaviour};
        {selected, RemoteJid2Behaviour} ->
            DbBehaviour = choose_behaviour(BRemLJID, BRemLBareJID, RemoteJid2Behaviour),
            {ok, decode_behaviour(DbBehaviour)}
    end.

-spec choose_behaviour(binary(), binary(), [{binary(), binary()}]) -> binary().
choose_behaviour(BRemLJID, BRemLBareJID, RemoteJid2Behaviour) ->
    case lists:keyfind(BRemLJID, 1, RemoteJid2Behaviour) of
        {_, Behaviour} ->
            Behaviour;
        false ->
            case lists:keyfind(BRemLBareJID, 1, RemoteJid2Behaviour) of
                {_, Behaviour} ->
                    Behaviour;
                false ->
                    %% Only one key remains
                    {_, Behaviour} = lists:keyfind(<<>>, 1, RemoteJid2Behaviour),
                    Behaviour
            end
    end.

-spec set_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: ejabberd_gen_mam_prefs:set_prefs_params(),
    Extra :: gen_hook:extra().
set_prefs(_Result,
          #{archive_id := UserID, default_mode := DefaultMode,
            always_jids := AlwaysJIDs, never_jids := NeverJIDs},
          #{host_type := HostType}) ->
    try
        {ok, set_prefs1(HostType, UserID, DefaultMode, AlwaysJIDs, NeverJIDs)}
    catch _Type:Error ->
        {ok, {error, Error}}
    end.

set_prefs1(HostType, UserID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    Rows = prefs_to_rows(UserID, DefaultMode, AlwaysJIDs, NeverJIDs),
    %% MySQL sometimes aborts transaction with reason:
    %% "Deadlock found when trying to get lock; try restarting transaction"
    mongoose_rdbms:transaction_with_delayed_retry(HostType, fun() ->
            {updated, _} =
                mongoose_rdbms:execute(HostType, mam_prefs_delete, [UserID]),
            [{updated, 1} = mongoose_rdbms:execute(HostType, mam_prefs_insert, Row) || Row <- Rows],
            ok
        end, #{user_id => UserID, retries => 5, delay => 100}),
    ok.

-spec get_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc ::  mod_mam:preference(),
    Params :: ejabberd_gen_mam_prefs:get_prefs_params(),
    Extra :: gen_hook:extra().
get_prefs({GlobalDefaultMode, _, _}, #{archive_id := UserID}, #{host_type := HostType}) ->
    {selected, Rows} = mongoose_rdbms:execute(HostType, mam_prefs_select, [UserID]),
    {ok, decode_prefs_rows(Rows, GlobalDefaultMode, [], [])}.

-spec remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, owner => jid:jid(), room => jid:jid()},
    Extra :: gen_hook:extra().
remove_archive(Acc, #{archive_id := UserID}, #{host_type := HostType}) ->
    remove_archive(HostType, UserID),
    {ok, Acc}.

remove_archive(HostType, UserID) ->
    {updated, _} =
        mongoose_rdbms:execute(HostType, mam_prefs_delete, [UserID]).

-spec query_behaviour(HostType :: mongooseim:host_type(),
                      UserID :: non_neg_integer(),
                      BRemLJID :: binary(),
                      BRemLBareJID :: binary()
        ) -> any().
query_behaviour(HostType, UserID, BRemLJID, BRemLJID) ->
    mongoose_rdbms:execute(HostType, mam_prefs_select_behaviour,
                           [UserID, BRemLJID]); %% check just bare jid
query_behaviour(HostType, UserID, BRemLJID, BRemLBareJID) ->
    mongoose_rdbms:execute(HostType, mam_prefs_select_behaviour2,
                           [UserID, BRemLJID, BRemLBareJID]).

%% ----------------------------------------------------------------------
%% Helpers

-spec encode_behaviour(always | never | roster) -> binary().
encode_behaviour(roster) -> <<"R">>;
encode_behaviour(always) -> <<"A">>;
encode_behaviour(never)  -> <<"N">>.

-spec decode_behaviour(binary()) -> always | never | roster.
decode_behaviour(<<"R">>) -> roster;
decode_behaviour(<<"A">>) -> always;
decode_behaviour(<<"N">>) -> never.

prefs_to_rows(UserID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    AlwaysRows = [[UserID, JID, encode_behaviour(always)] || JID <- AlwaysJIDs],
    NeverRows  = [[UserID, JID, encode_behaviour(never)] || JID <- NeverJIDs],
    DefaultRow = [UserID, <<>>, encode_behaviour(DefaultMode)],
    %% Lock keys in the same order to avoid deadlock
    [DefaultRow|lists:sort(AlwaysRows ++ NeverRows)].

-spec decode_prefs_rows([{binary() | jid:jid(), binary()}],
        DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [jid:literal_jid()],
        NeverJIDs :: [jid:literal_jid()]) ->
    {mod_mam:archive_behaviour(), [jid:literal_jid()], [jid:literal_jid()]}.
decode_prefs_rows([{<<>>, Behaviour}|Rows], _DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, decode_behaviour(Behaviour), AlwaysJIDs, NeverJIDs);
decode_prefs_rows([{JID, <<"A">>}|Rows], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, [JID|AlwaysJIDs], NeverJIDs);
decode_prefs_rows([{JID, <<"N">>}|Rows], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, AlwaysJIDs, [JID|NeverJIDs]);
decode_prefs_rows([], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs}.
