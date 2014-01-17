%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using ODBC.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_prefs).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/3]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

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

start_pm(Host, _Opts) ->
    ejabberd_hooks:add(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.

stop_pm(Host) ->
    ejabberd_hooks:delete(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc_muc

start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.

stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

get_behaviour(DefaultBehaviour, Host, UserID, _LocJID, RemJID) ->
    RemLJID      = jlib:jid_tolower(RemJID),
    SRemLBareJID = esc_jid(jlib:jid_remove_resource(RemLJID)),
    SRemLJID     = esc_jid(jlib:jid_tolower(RemJID)),
    SUserID      = integer_to_list(UserID),
    case query_behaviour(Host, SUserID, SRemLJID, SRemLBareJID) of
        {selected, ["behaviour"], [{Behavour}]} ->
            decode_behaviour(Behavour);
        _ -> DefaultBehaviour
    end.

set_prefs(Result, Host, UserID, _ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    SUserID = integer_to_list(UserID),
    DelQuery = ["DELETE FROM mam_config WHERE user_id = '", SUserID, "'"],
    InsQuery = ["INSERT INTO mam_config(user_id, behaviour, remote_jid) "
       "VALUES ", encode_first_config_row(SUserID, encode_behaviour(DefaultMode), ""),
       [encode_config_row(SUserID, "A", ejabberd_odbc:escape(JID))
        || JID <- AlwaysJIDs],
       [encode_config_row(SUserID, "N", ejabberd_odbc:escape(JID))
        || JID <- NeverJIDs]],
    %% Run as a transaction
    {atomic, [{updated, _}, {updated, _}]} =
        sql_transaction_map(Host, [DelQuery, InsQuery]),
    Result.

get_prefs({GlobalDefaultMode, _, _}, Host, UserID, _ArcJID) ->
    SUserID = integer_to_list(UserID),
    {selected, _ColumnNames, Rows} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT remote_jid, behaviour "
       "FROM mam_config "
       "WHERE user_id='", SUserID, "'"]),
    decode_prefs_rows(Rows, GlobalDefaultMode, [], []).

remove_archive(Host, UserID, _ArcJID) ->
    SUserID = integer_to_list(UserID),
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE "
       "FROM mam_config "
       "WHERE user_id='", SUserID, "'"]),
    ok.

query_behaviour(Host, SUserID, SRemLJID, SRemLBareJID) ->
    Result =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT behaviour "
       "FROM mam_config "
       "WHERE user_id='", SUserID, "' "
         "AND (remote_jid='' OR remote_jid='", SRemLJID, "'",
               case SRemLBareJID of
                    SRemLJID -> "";
                    _        -> [" OR remote_jid='", SRemLBareJID, "'"]
               end,
         ") "
       "ORDER BY remote_jid DESC "
       "LIMIT 1"]),
    ?DEBUG("query_behaviour query returns ~p", [Result]),
    Result.

%% ----------------------------------------------------------------------
%% Helpers

encode_behaviour(roster) -> "R";
encode_behaviour(always) -> "A";
encode_behaviour(never)  -> "N".

decode_behaviour(<<"R">>) -> roster;
decode_behaviour(<<"A">>) -> always;
decode_behaviour(<<"N">>) -> never.

esc_jid(JID) ->
    ejabberd_odbc:escape(jlib:jid_to_binary(JID)).

encode_first_config_row(SUserID, SBehavour, SJID) ->
    ["('", SUserID, "', '", SBehavour, "', '", SJID, "')"].

encode_config_row(SUserID, SBehavour, SJID) ->
    [", ('", SUserID, "', '", SBehavour, "', '", SJID, "')"].

sql_transaction_map(LServer, Queries) ->
    AtomicF = fun() ->
        [mod_mam_utils:success_sql_query(LServer, Query) || Query <- Queries]
    end,
    ejabberd_odbc:sql_transaction(LServer, AtomicF).

decode_prefs_rows([{<<>>, Behavour}|Rows], _DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, decode_behaviour(Behavour), AlwaysJIDs, NeverJIDs);
decode_prefs_rows([{JID, <<"A">>}|Rows], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, [JID|AlwaysJIDs], NeverJIDs);
decode_prefs_rows([{JID, <<"N">>}|Rows], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    decode_prefs_rows(Rows, DefaultMode, AlwaysJIDs, [JID|NeverJIDs]);
decode_prefs_rows([], DefaultMode, AlwaysJIDs, NeverJIDs) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs}.


