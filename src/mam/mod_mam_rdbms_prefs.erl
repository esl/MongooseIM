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
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_prefs).
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/4]).

-import(mongoose_rdbms,
        [escape_string/1,
         escape_integer/1,
         use_escaped_string/1,
         use_escaped_integer/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> 'ok'.
start(Host, Opts) ->
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
%% Internal functions and callbacks

-spec get_behaviour(Default :: mod_mam:archive_behaviour(),
        Host :: jid:server(), ArchiveID :: mod_mam:archive_id(),
        LocJID :: jid:jid(), RemJID :: jid:jid()) -> any().
get_behaviour(DefaultBehaviour, Host, UserID, _LocJID, RemJID)
        when is_integer(UserID) ->
    RemLJID      = jid:to_lower(RemJID),
    BRemLBareJID = jid:to_binary(jid:to_bare(RemLJID)),
    BRemLJID     = jid:to_binary(RemLJID),
    SRemLBareJID = escape_string(BRemLBareJID),
    SRemLJID     = escape_string(BRemLJID),
    SUserID      = escape_integer(UserID),
    %% CheckBare if resource is not empty
    CheckBare    = RemJID#jid.lresource =/= <<>>,
    case query_behaviour(Host, SUserID, SRemLJID, SRemLBareJID, CheckBare) of
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

-spec set_prefs(Result :: any(), Host :: jid:server(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid(),
        DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [jid:literal_jid()],
        NeverJIDs :: [jid:literal_jid()]) -> any().
set_prefs(_Result, Host, UserID, _ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    try
        set_prefs1(Host, UserID, DefaultMode, AlwaysJIDs, NeverJIDs)
    catch _Type:Error ->
        {error, Error}
    end.


order_by_in_delete(Host) ->
    case mongoose_rdbms:db_engine(Host) of
        mysql ->
                " ORDER BY remote_jid";
        _ ->
                ""
    end.

set_prefs1(Host, UserID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    %% Lock keys in the same order to avoid deadlock
    SUserID = escape_integer(UserID),
    EscapedA = escape_string("A"),
    EscapedN = escape_string("N"),
    JidBehaviourA = [{JID, EscapedA} || JID <- AlwaysJIDs],
    JidBehaviourN = [{JID, EscapedN} || JID <- NeverJIDs],
    JidBehaviour = lists:keysort(1, JidBehaviourA ++ JidBehaviourN),
    ValuesAN = [encode_config_row(SUserID, SBehaviour, escape_string(JID))
                || {JID, SBehaviour} <- JidBehaviour],
    SDefaultMode = escape_string(encode_behaviour(DefaultMode)),
    DefaultValue = encode_first_config_row(SUserID, SDefaultMode, escape_string("")),
    Values = [DefaultValue|ValuesAN],
    DelQuery = ["DELETE FROM mam_config WHERE user_id = ",
                mongoose_rdbms:use_escaped_integer(SUserID),
                order_by_in_delete(Host)],
    InsQuery = ["INSERT INTO mam_config(user_id, behaviour, remote_jid) "
                "VALUES ", Values],

    run_transaction_or_retry_on_abort(fun() ->
            case sql_transaction_map(Host, [DelQuery, InsQuery]) of
                {atomic, [{updated, _}, {updated, _}]} ->
                    {atomic, ok};
                Other ->
                    Other
            end
        end, UserID, 5),
    ok.

%% Possible error with mysql
%% Reason "Deadlock found when trying to get lock; try restarting transaction"
%% triggered mod_mam_utils:error_on_sql_error
run_transaction_or_retry_on_abort(F, UserID, Retries) ->
    Result = F(),
    case Result of
        {atomic, _} ->
            Result;
        {aborted, Reason} when Retries > 0 ->
            ?WARNING_MSG("event=\"Transaction aborted. Restart\", "
                          "user_id=~p, reason=~p, retries=~p",
                         [UserID, Reason, Retries]),
            timer:sleep(100),
            run_transaction_or_retry_on_abort(F, UserID, Retries-1);
        _ ->
            ?ERROR_MSG("event=\"Transaction failed\", "
                        "user_id=~p, reason=~p, retries=~p",
                         [UserID, Result, Retries]),
            erlang:error({transaction_failed, #{user_id => UserID, result => Result}})
    end.

-spec get_prefs(mod_mam:preference(), _Host :: jid:server(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid())
            -> mod_mam:preference().
get_prefs({GlobalDefaultMode, _, _}, Host, UserID, _ArcJID) ->
    SUserID = escape_integer(UserID),
    {selected, Rows} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT remote_jid, behaviour "
       "FROM mam_config "
       "WHERE user_id=", use_escaped_integer(SUserID)]),
    decode_prefs_rows(Rows, GlobalDefaultMode, [], []).


-spec remove_archive(mongoose_acc:t(), jid:server(), mod_mam:archive_id(), jid:jid()) ->
    mongoose_acc:t().
remove_archive(Acc, Host, UserID, _ArcJID) ->
    remove_archive(Host, UserID),
    Acc.

remove_archive(Host, UserID) ->
    SUserID = escape_integer(UserID),
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host, ["DELETE FROM mam_config WHERE user_id=", use_escaped_integer(SUserID)]).

-spec query_behaviour(jid:server(),
                      SUserID :: mongoose_rdbms:escaped_integer(),
                      SRemLJID :: mongoose_rdbms:escaped_string(),
                      SRemLBareJID :: mongoose_rdbms:escaped_string(),
                      CheckBare :: boolean()
        ) -> any().
query_behaviour(Host, SUserID, SRemLJID, SRemLBareJID, CheckBare) ->
    Result =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT remote_jid, behaviour "
       "FROM mam_config "
       "WHERE user_id=", use_escaped_integer(SUserID), " "
         "AND (remote_jid='' OR remote_jid=", use_escaped_string(SRemLJID),
               case CheckBare of
                    false ->
                        "";
                    true ->
                       [" OR remote_jid=", use_escaped_string(SRemLBareJID)]
               end,
         ")"]),
    ?DEBUG("query_behaviour query returns ~p", [Result]),
    Result.

%% ----------------------------------------------------------------------
%% Helpers

-spec encode_behaviour(always | never | roster) -> string().
encode_behaviour(roster) -> "R";
encode_behaviour(always) -> "A";
encode_behaviour(never)  -> "N".


-spec decode_behaviour(binary()) -> always | never | roster.
decode_behaviour(<<"R">>) -> roster;
decode_behaviour(<<"A">>) -> always;
decode_behaviour(<<"N">>) -> never.

-spec encode_first_config_row(SUserID :: mongoose_rdbms:escaped_integer(),
                              SBehaviour :: mongoose_rdbms:escaped_string(),
                              SJID :: mongoose_rdbms:escaped_string()) ->
    mongoose_rdbms:sql_query_part().
encode_first_config_row(SUserID, SBehaviour, SJID) ->
    ["(", use_escaped_integer(SUserID),
     ", ", use_escaped_string(SBehaviour),
     ", ", use_escaped_string(SJID), ")"].


-spec encode_config_row(SUserID :: mongoose_rdbms:escaped_integer(),
                        SBehaviour :: mongoose_rdbms:escaped_string(),
                        SJID :: mongoose_rdbms:escaped_string()) ->
    mongoose_rdbms:sql_query_part().
encode_config_row(SUserID, SBehaviour, SJID) ->
    [", (", use_escaped_integer(SUserID),
     ", ", use_escaped_string(SBehaviour),
     ", ", use_escaped_string(SJID), ")"].


-spec sql_transaction_map(jid:server(), [mongoose_rdbms:sql_query()]) -> any().
sql_transaction_map(LServer, Queries) ->
    AtomicF = fun() ->
        [mod_mam_utils:success_sql_query(LServer, Query) || Query <- Queries]
    end,
    mongoose_rdbms:sql_transaction(LServer, AtomicF).


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
