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
-module(mod_mam_rdbms_user).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([archive_id/3,
         remove_archive/4]).

%% For debugging ONLY
-export([create_user_archive/3]).

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
    prepare_queries(),
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

%% Preparing queries
prepare_queries() ->
    mongoose_rdbms:prepare(mam_user_insert, mam_server_user, [server, user_name],
                            <<"INSERT INTO mam_server_user (server, user_name) VALUES (?, ?)">>),
    mongoose_rdbms:prepare(mam_user_select, mam_server_user, [server, user_name],
                            <<"SELECT id FROM mam_server_user WHERE server=? AND user_name=?">>),
    mongoose_rdbms:prepare(mam_user_remove, mam_server_user, [server, user_name],
                            <<"DELETE FROM mam_server_user WHERE server=? AND user_name=?">>),
    ok.

%%====================================================================
%% API
%%====================================================================
-spec archive_id(undefined | mod_mam:archive_id(), jid:server(),
                 jid:jid()) -> mod_mam:archive_id().
archive_id(undefined, Host, _ArcJID=#jid{lserver = LServer, luser = LUser}) ->
    query_archive_id(Host, LServer, LUser);
archive_id(ArcID, _Host, _ArcJID) ->
    ArcID.

-spec remove_archive(Acc :: map(), Host :: jid:server(),
                     ArchiveID :: mod_mam:archive_id(),
                     ArchiveJID :: jid:jid()) -> map().
remove_archive(Acc, Host, ArcID, ArcJID) ->
    remove_archive(Host, ArcID, ArcJID),
    Acc.

remove_archive(Host, _ArcID, _ArcJID=#jid{lserver = LServer, luser = LUser}) ->
    {updated, _} =
        mongoose_rdbms:execute(Host, mam_user_remove, [LUser, LServer]).


%%====================================================================
%% Internal functions
%%====================================================================

-spec query_archive_id(jid:server(), jid:lserver(), jid:user()) -> integer().
query_archive_id(Host, LServer, LUser) ->
    Tries = 5,
    query_archive_id(Host, LServer, LUser, Tries).

query_archive_id(Host, LServer, LUser, 0) ->
    ?LOG_ERROR(#{what => query_archive_id_failed,
                 host => Host, server => LServer, user => LUser}),
    error(query_archive_id_failed);
query_archive_id(Host, LServer, LUser, Tries) when Tries > 0 ->
    Result = mongoose_rdbms:execute(Host, mam_user_select, [LServer, LUser]),
    case Result of
        {selected, [{IdBin}]} ->
            mongoose_rdbms:result_to_integer(IdBin);
        {selected, []} ->
            %% The user is not found
            create_user_archive(Host, LServer, LUser),
            query_archive_id(Host, LServer, LUser, Tries - 1)
    end.

-spec create_user_archive(jid:server(), jid:lserver(), jid:user()) -> ok.
create_user_archive(Host, LServer, LUser) ->
    Res = mongoose_rdbms:execute(Host, mam_user_insert, [LServer, LUser]),
    case Res of
        {updated, 1} ->
            ok;
        _ ->
            %% There is a common race condition case
            %% Duplicate entry ... for key 'uc_mam_server_user_name'.
            %% In this case Res can de:
            %% - {error, duplicate_key}
            %% - {error, "[FreeTDS][SQL Server]Violation of UNIQUE KEY constraint" ++ _}
            %% Let's ignore the errors and just retry in query_archive_id
            ?LOG_WARNING(#{what => create_user_archive_failed, reason => Res,
                           user => LUser, host => Host, server => LServer}),
            ok
    end.
