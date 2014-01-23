%%% @author  <marcin.miszczyk@erlang.solutions.com>
%%% @copyright (C) 2014
%%% @doc Mnesia backend for roster module
%%% @see mod_roster
%%%
%%% @end
%%% Created : 22 Jan 2014 by  <marcin.miszczyk@erlang.solutions.com>

-module(mod_roster_mnesia).
-author( 'marcin.miszczyk@erlang.solutions.com').

-export( [ init/1,
           roster_version/1]).

-include("mod_roster.hrl").
-include("ejabberd.hrl").

%% --interface-----------------------------------------------------------

-spec init ( ModuleOptions ) -> ok when
      ModuleOptions :: term().
init( _Opts ) ->
    mnesia:create_table(roster,[{disc_copies, [node()]},
                                {attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
                                         {attributes, record_info(fields, roster_version)}]),
    update_table(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ok.


roster_version( US ) ->
    mnesia:dirty_read(roster_version, US).

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
