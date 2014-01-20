%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Assigns archive integer identifiers.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_user).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([archive_id/3,
         remove_archive/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

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
    ejabberd_hooks:add(mam_archive_id, Host, ?MODULE, archive_id, 50),
    ok.

stop_pm(Host) ->
    ejabberd_hooks:delete(mam_archive_id, Host, ?MODULE, archive_id, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_id, Host, ?MODULE, archive_id, 50),
    ok.

stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_id, Host, ?MODULE, archive_id, 50),
    ok.


%%====================================================================
%% API
%%====================================================================

archive_id(undefined, Host, _ArcJID=#jid{luser = UserName}) ->
    query_archive_id(Host, UserName);
archive_id(ArcID, _Host, _ArcJID) ->
    ArcID.

remove_archive(Host, _ArcID, _ArcJID=#jid{luser = UserName}) ->
    SUserName = ejabberd_odbc:escape(UserName),
    {updated, _} =
    ejabberd_odbc:sql_query(
      Host,
      ["DELETE FROM mam_user "
       "WHERE user_name = '", SUserName, "'"]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

query_archive_id(Host, UserName) ->
    SUserName = ejabberd_odbc:escape(UserName),
    Result =
    ejabberd_odbc:sql_query(
      Host,
      ["SELECT id "
       "FROM mam_user "
       "WHERE user_name='", SUserName, "' "
       "LIMIT 1"]),

    case Result of
        {selected, [<<"id">>], [{IdBin}]} ->
            binary_to_integer(IdBin);
        {selected, [<<"id">>], []} ->
            %% The user is not found
            create_user_archive(Host, UserName),
            query_archive_id(Host, UserName)
    end.
    
create_user_archive(Host, UserName) ->
    SUserName = ejabberd_odbc:escape(UserName),
    {updated, 1} =
    ejabberd_odbc:sql_query(
      Host,
      ["INSERT INTO mam_user "
       "(user_name) VALUES ('", SUserName, "')"]),
    ok.
