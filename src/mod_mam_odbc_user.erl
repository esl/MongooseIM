%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Assigns archive integer identifiers (multihost version).
%%%
%%% This module supports several hosts.
%%%
%%% Archive id is assigned based on user_name and host.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_user).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([archive_id/3,
         remove_archive/4]).

-include("mongoose.hrl").
-include("jlib.hrl").

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
    ejabberd_hooks:add(mam_archive_id, Host, ?MODULE, archive_id, 50),
    case gen_mod:get_module_opt(Host, ?MODULE, auto_remove, false) of
        true ->
            ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 90),
            ok;
        false ->
            ok
    end,
    ok.


-spec stop_pm(jid:server()) -> 'ok'.
stop_pm(Host) ->
    ejabberd_hooks:delete(mam_archive_id, Host, ?MODULE, archive_id, 50),
    case gen_mod:get_module_opt(Host, ?MODULE, auto_remove, false) of
        true ->
            ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 90),
            ok;
        false ->
            ok
    end,
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

-spec start_muc(jid:server(), _) -> 'ok'.
start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_id, Host, ?MODULE, archive_id, 50),
    case gen_mod:get_module_opt(Host, ?MODULE, auto_remove, false) of
        true ->
            ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 90),
            ok;
        false ->
            ok
    end,
    ok.


-spec stop_muc(jid:server()) -> 'ok'.
stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_id, Host, ?MODULE, archive_id, 50),
    case gen_mod:get_module_opt(Host, ?MODULE, auto_remove, false) of
        true ->
            ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 90),
            ok;
        false ->
            ok
    end,
    ok.


%%====================================================================
%% API
%%====================================================================
-spec archive_id(undefined | mod_mam:archive_id(), jid:server(),
                 jid:jid()) -> mod_mam:archive_id().
archive_id(undefined, Host, _ArcJID=#jid{lserver = Server, luser = UserName}) ->
    query_archive_id(Host, Server, UserName);
archive_id(ArcID, _Host, _ArcJID) ->
    ArcID.

-spec remove_archive(Acc :: map(), Host :: jid:server(),
                     ArchiveID :: mod_mam:archive_id(),
                     ArchiveJID :: jid:jid()) -> map().
remove_archive(Acc, Host, _ArcID, _ArcJID=#jid{lserver = Server, luser = UserName}) ->
    SUserName = mongoose_rdbms:escape(UserName),
    SServer   = mongoose_rdbms:escape(Server),
    {updated, _} =
    mongoose_rdbms:sql_query(
      Host,
      ["DELETE FROM mam_server_user "
       "WHERE server = '", SServer, "' AND user_name = '", SUserName, "'"]),
    Acc.

%%====================================================================
%% Internal functions
%%====================================================================

-spec query_archive_id(jid:server(), jid:lserver(), jid:user()) -> integer().
query_archive_id(Host, Server, UserName) ->
    SServer   = mongoose_rdbms:escape(Server),
    SUserName = mongoose_rdbms:escape(UserName),
    DbType = mongoose_rdbms_type:get(),
    Result = do_query_archive_id(DbType, Host, SServer, SUserName),

    case Result of
        {selected, [{IdBin}]} ->
            mongoose_rdbms:result_to_integer(IdBin);
        {selected, []} ->
            %% The user is not found
            create_user_archive(Host, Server, UserName),
            query_archive_id(Host, Server, UserName)
    end.

-spec create_user_archive(jid:server(), jid:lserver(), jid:user()) -> 'ok'.
create_user_archive(Host, Server, UserName) ->
    SServer   = mongoose_rdbms:escape(Server),
    SUserName = mongoose_rdbms:escape(UserName),
    Res =
    mongoose_rdbms:sql_query(
      Host,
      ["INSERT INTO mam_server_user "
       "(server, user_name) VALUES ('", SServer, "', '", SUserName, "')"]),
    case Res of
        {updated, 1} ->
            ok;
        %% Ignore the race condition Duplicate entry ... for key 'uc_mam_server_user_name'
        {error, duplicate_key} ->
            ok
    end.

do_query_archive_id(mssql, Host, SServer, SUserName) ->
    rdbms_queries_mssql:query_archive_id(Host, SServer, SUserName);
do_query_archive_id(_, Host, SServer, SUserName) ->
    mongoose_rdbms:sql_query(
      Host,
      ["SELECT id "
       "FROM mam_server_user "
       "WHERE server = '", SServer, "' AND user_name = '", SUserName, "' "
       "LIMIT 1"]).
