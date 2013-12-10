%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Assigns archive integer identifiers.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_user).
-export([archive_id/3,
         remove_archive/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

%%====================================================================
%% API
%%====================================================================

archive_id(Host, _Mod, _ArcJID=#jid{luser = UserName}) ->
    query_archive_id(Host, UserName).

remove_archive(Host, _Mod, _ArcID, _ArcJID=#jid{luser = UserName}) ->
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
        {selected, ["id"], [{IdBin}]} ->
            binary_to_integer(IdBin);
        {selected, ["id"], []} ->
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
