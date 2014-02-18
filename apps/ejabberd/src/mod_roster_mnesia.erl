%%% @author  <marcin.miszczyk@erlang.solutions.com>
%%% @copyright (C) 2014
%%% @doc Mnesia backend for roster module
%%% @see mod_roster
%%%
%%% @end
%%% Created : 22 Jan 2014 by  <marcin.miszczyk@erlang.solutions.com>

-module(mod_roster_mnesia).
-author( 'marcin.miszczyk@erlang.solutions.com').

-behaviour( mod_roster).
-export( [ init/1,
           roster_version/1,
           write_version/2,
           rosters_by_us/1,
           rosters_without_groups/1,
           roster/1,
           write_roster/1,
           write_roster_subscription/1,
           remove_roster/1,
           remove_user/1,
           transaction/2
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
    update_table_scheme(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ok.

-spec roster_version( UserServer ) -> {ok, Version} | not_found when
      UserServer :: us(),
      Version :: version().
roster_version( US ) when size(US) =:= 2 ->
    case mnesia:dirty_read(roster_version, US) of
        [#roster_version{ version = Version }] ->
            {ok, Version};
        [] ->
            not_found
    end.


-spec write_version( UserServer, RosterVersion ) -> any() when
      UserServer :: us(),
      RosterVersion :: version().
write_version( US, Version ) when size(US) =:= 2 ->
    mnesia:dirty_write(#roster_version{us = US, version = Version}).


-spec rosters_by_us( UserServer ) -> Rosters when
      UserServer :: us(),
      Rosters :: list( roster() ).
rosters_by_us( US ) when size(US) =:= 2 ->
    case catch mnesia:dirty_index_read(roster, US, #roster.us) of
        Items when is_list(Items) ->
            Items;
        _ ->
            []
    end.


-spec rosters_without_groups( UserServer ) -> Rousters when
      UserServer :: usj(),
      Rousters :: list( roster() ).
rosters_without_groups( US ) ->
    %% This one can not be optymized in mnesia
    rosters_by_us( US ).


-spec roster( UserServeJid ) -> MightBeRoster when
      UserServeJid :: usj(),
      MightBeRoster :: {ok, roster() } | not_found.
roster( USJ ) when size( USJ ) =:= 3 ->
    case mnesia:dirty_read(roster, USJ ) of
        [ Rouster ] ->
            {ok, Rouster};
        [] ->
            not_found
    end.


-spec remove_roster( UserServerJID ) -> ok when
      UserServerJID :: usj().
remove_roster( USJ ) when size(USJ) =:= 3 ->
    mnesia:delete({roster, USJ}).

-spec remove_user( UserServer ) -> ok when
      UserServer :: usj().
remove_user( US = { _LUser, LServer} ) ->
    F = fun() ->
                lists:foreach(fun remove_roster_object/1,
                              rosters_by_us(US))
        end,
    transaction( LServer, F).

-spec remove_roster_object( Roster ) -> ok when
      Roster :: roster().
remove_roster_object( Roster = #roster{} ) ->
    mnesia:delete_object(Roster).


-spec write_roster( Roster ) -> ok when
      Roster :: roster().
write_roster( Roster = #roster{} ) ->
    mnesia:write(Roster).

-spec write_roster_subscription( Roster ) -> ok when
      Roster :: roster().
write_roster_subscription( Roster = #roster{} ) ->
    %% this might be optymalized somehow
    write_roster( Roster ).


-spec transaction( Server, TransactionFun ) -> FunReturn when
      Server :: server(),
      TransactionFun :: fun ( () -> FunReturn ).
transaction( _Server, Function ) ->
    mnesia:transaction( Function ).

%% --private-------------------------------------------------------------

update_table_scheme() ->
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
