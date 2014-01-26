%%% @author  <marcin.miszczyk@erlang.solutions.com>
%%% @copyright (C) 2014
%%% @doc Mnesia backend for roster module
%%% @see mod_roster
%%%
%%% @end
%%% Created : 22 Jan 2014 by  <marcin.miszczyk@erlang.solutions.com

-module(mod_roster_odbc_back).
-author( 'marcin.miszczyk@erlang.solutions.com').


-behaviour( mod_roster_gen_backend ).
-export( [ init/1,
           roster_version/1,
           write_version/2,
           rosters_by_us/1,
           roster/1,
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
    ok.


-spec roster_version( UserServer ) -> {ok, Version} | not_found when
      UserServer :: us(),
      Version :: version().
roster_version( US ) when size(US) =:= 2 ->
    {LUser, LServer} =  US,
    case odbc_queries:get_roster_version(ejabberd_odbc:escape(LServer),
                                         ejabberd_odbc:escape(LUser)) of
        {selected, ["version"], [{Version}]} ->
            {ok,Version};
        {selected, ["version"], []} ->
            not_found
    end.


-spec write_version( UserServer, RosterVersion ) -> any() when
      UserServer :: us(),
      RosterVersion :: version().
write_version( US, Version ) when size(US) =:= 2 ->
    {LUser, LServer} =  US,
    Transaction = fun () ->
                          odbc_queries:set_roster_version(ejabberd_odbc:escape(LUser), Version)
                  end,
    {atomic, {updated,1}} = odbc_queries:sql_transaction(LServer,
                                                         Transaction).


-spec rosters_by_us( UserServe ) -> Rosters when
      UserServe :: us(),
      Rosters :: list( roster() ).
rosters_by_us( US ) when size(US) =:= 2 ->
    not_implemented.


-spec roster( UserServeJid ) -> MightBeRoster when
      UserServeJid :: usj(),
      MightBeRoster :: {ok, roster() } | not_found.
roster( USJ ) when size( USJ ) =:= 3 ->
    not_implemented.

-spec remove_roster( UserServerJID ) -> ok when
      UserServerJID :: usj().
remove_roster( USJ ) when size(USJ) =:= 3 ->
    not_implemented.


-spec remove_roster_object( Roster ) -> ok when
      Roster :: roster().
remove_roster_object( Roster = #roster{} ) ->
    not_implemented.


-spec write_roster( Roster ) -> ok when
      Roster :: roster().
write_roster( Roster = #roster{} ) ->
    not_implemented.


-spec transaction( TransactionFun ) -> FunReturn when
      TransactionFun :: fun ( () -> FunReturn ).
transaction( Function ) ->
    not_implemented.

%% --private-------------------------------------------------------------
