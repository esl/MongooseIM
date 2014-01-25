%%% @author  <marcin.miszczyk@erlang.solutions.com>
%%% @copyright (C) 2014
%%% @doc Mnesia backend for roster module
%%% @see mod_roster
%%%
%%% @end
%%% Created : 22 Jan 2014 by  <marcin.miszczyk@erlang.solutions.com>

-module(mod_roster_mnesia).
-author( 'marcin.miszczyk@erlang.solutions.com').

-behaviour( mod_roster_gen_backend ).
-export( [ init/1,
           roster_version/1,
           write_version/2,
           get_user_server_roster/1,
           get_roster/1,
           write_roster/1,
           remove_roster/1,
           remove_roster_object/1,
           transaction/1
         ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").


%% --interface-----------------------------------------------------------

-spec init ( ModuleOptions ) -> ok when
      ModuleOptions :: list().
init( _Opts ) ->
    mnesia:create_table(roster,[{disc_copies, [node()]},
                                {attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
                                         {attributes, record_info(fields, roster_version)}]),
    update_table(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ok.

-spec roster_version( UserServer ) -> {ok, Version} | not_found when
      UserServer :: us(),
      Version :: version().
roster_version( US ) ->
    case mnesia:dirty_read(roster_version, US) of
        [#roster_version{ version = Version }] ->
            {ok, Version};
        [] ->
            not_found
    end.

-spec write_version( UserServer, RosterVersion ) -> any() when
      UserServer :: us(),
      RosterVersion :: version().
write_version( US, Version ) ->
    mnesia:dirty_write(#roster_version{us = US, version = Version}).


get_user_server_roster( US ) ->
    case catch mnesia:dirty_index_read(roster, US, #roster.us) of
        Items when is_list(Items) ->
            Items;
        _ ->
            []
    end.

get_roster( LoweredUserServerJID) ->
    case mnesia:read(roster, LoweredUserServerJID ) of
        [ Rouster ] ->
            {ok, Rouster};
        [] ->
            not_found
    end.

remove_roster( LoweredUserServerJID ) ->
    mnesia:delete({roster, LoweredUserServerJID}).


remove_roster_object( Roster = #roster{} ) ->
    mnesia:delete_object(Roster).

write_roster( Roster ) ->
    mnesia:write(Roster).


transaction( Function ) ->
    mnesia:transaction( Function ).

%% --private-------------------------------------------------------------

update_table() ->
    Fields = record_info(fields, roster),
    case mnesia:table_info(roster, attributes) of
        Fields ->
            ok;
        [uj, user, jid, name, subscription, ask, groups, xattrs, xs] ->
            convert_table1(Fields);
        [usj, us, jid, name, subscription, ask, groups, xattrs, xs] ->
            convert_table2(Fields);
        _ ->
            ?INFO_MSG("Recreating roster table", []),
            mnesia:transform_table(roster, ignore, Fields)
    end.

%% Convert roster table to support virtual host
convert_table1(Fields) ->
    ?INFO_MSG("Virtual host support: converting roster table from "
              "{uj, user, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    Host = ?MYNAME,
    {atomic, ok} = mnesia:create_table(
                     mod_roster_tmp_table,
                     [{disc_only_copies, [node()]},
                      {type, bag},
                      {local_content, true},
                      {record_name, roster},
                      {attributes, record_info(fields, roster)}]),
    mnesia:del_table_index(roster, user),
    mnesia:transform_table(roster, ignore, Fields),
    F1 = fun() ->
                 mnesia:write_lock_table(mod_roster_tmp_table),
                 mnesia:foldl(
                   fun(#roster{usj = {U, JID}, us = U} = R, _) ->
                           mnesia:dirty_write(
                             mod_roster_tmp_table,
                             R#roster{usj = {U, Host, JID},
                                      us = {U, Host}})
                   end, ok, roster)
         end,
    mnesia:transaction(F1),
    mnesia:clear_table(roster),
    F2 = fun() ->
                 mnesia:write_lock_table(roster),
                 mnesia:foldl(
                   fun(R, _) ->
                           mnesia:dirty_write(R)
                   end, ok, mod_roster_tmp_table)
         end,
    mnesia:transaction(F2),
    mnesia:delete_table(mod_roster_tmp_table).

%% Convert roster table: xattrs fields become
convert_table2(Fields) ->
    ?INFO_MSG("Converting roster table from "
              "{usj, us, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    mnesia:transform_table(roster, ignore, Fields).
