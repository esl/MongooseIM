%%% @author marcin piotr miszczyk  <marcin.miszczyk@erlang.solutions.com>
%%% @copyright (C) 2014
%%% @doc Bahaviour definition for mod_roster backends
%%% @see mod_roster_mnesia
%%% @see mod_roster_odbc
%%%
%%% @end
%%% Created : 22 Jan 2014 by  <marcin.miszczyk@erlang.solutions.com>

-module(mod_roster_gen_backend).
-author( 'marcin.miszczyk@erlang.solutions.com').

-include("jlib.hrl").
-include("mod_roster.hrl").

-callback init( Options ) -> ok when
      Options :: list().

-callback roster_version( UserServer ) -> {ok, Version} | not_found when
      UserServer :: us(),
      Version :: version().

-callback write_version( UserServer, RosterVersion ) -> any() when
      UserServer :: us(),
      RosterVersion :: version().

-callback rosters_by_us( UserServe ) -> Rosters when
      UserServe :: us(),
      Rosters :: list( roster() ).

-callback roster( UserServeJid ) -> MightBeRoster when
      UserServeJid :: usj(),
      MightBeRoster :: {ok, roster() } | not_found.



