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
-export([start/2, stop/1, supported_features/0]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_prefs).
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/4]).

-ignore_xref([remove_archive/4, start/2, stop/1, supported_features/0]).

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
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
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
    [{mam_get_behaviour, HostType, ?MODULE, get_behaviour, 50},
     {mam_get_prefs, HostType, ?MODULE, get_prefs, 50},
     {mam_set_prefs, HostType, ?MODULE, set_prefs, 50},
     {mam_remove_archive, HostType, ?MODULE, remove_archive, 50}].

muc_hooks(HostType) ->
    [{mam_muc_get_behaviour, HostType, ?MODULE, get_behaviour, 50},
     {mam_muc_get_prefs, HostType, ?MODULE, get_prefs, 50},
     {mam_muc_set_prefs, HostType, ?MODULE, set_prefs, 50},
     {mam_muc_remove_archive, HostType, ?MODULE, remove_archive, 50}].

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

-spec get_behaviour(Default :: mod_mam:archive_behaviour(),
        HostType :: mongooseim:host_type(), ArchiveID :: mod_mam:archive_id(),
        LocJID :: jid:jid(), RemJID :: jid:jid()) -> any().
get_behaviour(DefaultBehaviour, HostType, UserID, _LocJID, RemJID)
        when is_integer(UserID) ->
    RemLJID      = jid:to_lower(RemJID),
    BRemLBareJID = jid:to_binary(jid:to_bare(RemLJID)),
    BRemLJID     = jid:to_binary(RemLJID),
    case query_behaviour(HostType, UserID, BRemLJID, BRemLBareJID) of
        {selected, []} ->
            DefaultBehaviour;
        {selected, RemoteJid2Behaviour} ->
            DbBehaviour = choose_behaviour(BRemLJID, BRemLBareJID, RemoteJid2Behaviour),
            decode_behaviour(DbBehaviour)
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

-spec set_prefs(Result :: any(), HostType :: mongooseim:host_type(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid(),
        DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [jid:literal_jid()],
        NeverJIDs :: [jid:literal_jid()]) -> any().
set_prefs(_Result, HostType, UserID, _ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    try
        set_prefs1(HostType, UserID, DefaultMode, AlwaysJIDs, NeverJIDs)
    catch _Type:Error ->
        {error, Error}
    end.

set_prefs1(HostType, UserID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    Rows = prefs_to_rows(UserID, DefaultMode, AlwaysJIDs, NeverJIDs),
    %% MySQL sometimes aborts transaction with reason:
    %% "Deadlock found when trying to get lock; try restarting transaction"
    mongoose_rdbms:transaction_with_delayed_retry(HostType, fun() ->
            {updated, _} =
                mongoose_rdbms:execute(HostType, mam_prefs_delete, [UserID]),
            [mongoose_rdbms:execute(HostType, mam_prefs_insert, Row) || Row <- Rows],
            ok
        end, #{user_id => UserID, retries => 5, delay => 100}),
    ok.

-spec get_prefs(mod_mam:preference(), HostType :: mongooseim:host_type(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid())
            -> mod_mam:preference().
get_prefs({GlobalDefaultMode, _, _}, HostType, UserID, _ArcJID) ->
    {selected, Rows} = mongoose_rdbms:execute(HostType, mam_prefs_select, [UserID]),
    decode_prefs_rows(Rows, GlobalDefaultMode, [], []).

-spec remove_archive(mongoose_acc:t(), mongooseim:host_type(),
                     mod_mam:archive_id(), jid:jid()) ->
    mongoose_acc:t().
remove_archive(Acc, HostType, UserID, _ArcJID) ->
    remove_archive(HostType, UserID),
    Acc.

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
