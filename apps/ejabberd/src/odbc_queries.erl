%%%----------------------------------------------------------------------
%%% File    : odbc_queries.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : ODBC queries dependind on back-end
%%% Created : by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(odbc_queries).
-author("mremond@process-one.net").

-export([get_db_type/0,
	 sql_transaction/2,
	 get_last/2,
     select_last/3,
	 set_last_t/4,
	 del_last/2,
	 get_password/2,
	 set_password_t/3,
	 add_user/3,
	 del_user/2,
	 del_user_return_password/3,
	 list_users/1,
         list_users/2,
	 users_number/1,
         users_number/2,
	 add_spool_sql/2,
	 add_spool/2,
	 get_and_del_spool_msg_t/2,
	 del_spool_msg/2,
         get_average_roster_size/1,
         get_average_rostergroup_size/1,
         clear_rosters/1,
	 get_roster/2,
	 get_roster_jid_groups/2,
	 get_roster_groups/3,
	 del_user_roster_t/2,
	 get_roster_by_jid/3,
	 get_rostergroup_by_jid/3,
	 del_roster/3,
	 del_roster_sql/2,
	 update_roster/5,
	 update_roster_sql/4,
	 roster_subscribe/4,
	 get_subscription/3,
	 set_private_data/4,
	 set_private_data_sql/3,
	 get_private_data/3,
     multi_get_private_data/3,
	 del_user_private_storage/2,
	 get_default_privacy_list/2,
	 get_default_privacy_list_t/1,
         count_privacy_lists/1,
         clear_privacy_lists/1,
	 get_privacy_list_names/2,
	 get_privacy_list_names_t/1,
	 get_privacy_list_id/3,
	 get_privacy_list_id_t/2,
	 get_privacy_list_data/3,
	 get_privacy_list_data_by_id/2,
	 set_default_privacy_list/2,
	 unset_default_privacy_list/2,
	 remove_privacy_list/2,
	 add_privacy_list/2,
	 set_privacy_list/2,
	 del_privacy_lists/3,
	 set_vcard/26,
	 get_vcard/2,
	 escape_string/1,
	 escape_like_string/1,
	 count_records_where/3,
	 get_roster_version/2,
	 set_roster_version/2]).

%% We have only two compile time options for db queries:
%%-define(generic, true).
%%-define(mssql, true).
-ifndef(mssql).
-undef(generic).
-define(generic, true).
-endif.

-include("ejabberd.hrl").

%% -----------------
%% Common functions

%% Almost a copy of string:join/2.
%% We use this version because string:join/2 is relatively
%% new function (introduced in R12B-0).
join([], _Sep) ->
    [];
join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

%% Note: escape functions (`escape_string/1' and `escape_like_string/1')
%%       are in this module and not in `ejabberd_odbc',
%%       because they are called a lot.
%%       To have both `escape_string/1' and `escape_character/1' in one module
%%       is an optimization.

escape_string(S) when is_binary(S) ->
    list_to_binary(escape_string(binary_to_list(S)));
escape_string(S) when is_list(S) ->
    [escape_character(C) || C <- S].

escape_like_string(S) when is_binary(S) ->
    list_to_binary(escape_like_string(binary_to_list(S)));
escape_like_string(S) when is_list(S) ->
    [escape_like_character(C) || C <- S].

escape_like_character($%) -> "\\%";
escape_like_character($_) -> "\\_";
escape_like_character(C)  -> escape_character(C).


%% -----------------
%% Generic queries
-ifdef(generic).

get_db_type() ->
    generic.

%% Safe atomic update.
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun(A, B) -> [A, "='", B, "'"] end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query_t(
	   [<<"update ">>, Table, <<" set ">>,
	    join(UPairs, ", "),
	    <<" where ">>, Where, ";"]) of
	{updated, 1} ->
	    ok;
	_ ->
	    ejabberd_odbc:sql_query_t(
	      [<<"insert into ">>, Table, "(", join(Fields, ", "),
	       <<") values ('">>, join(Vals, "', '"), "');"])
    end.

%% Safe atomic update.
%% Fields and their values are passed as a list where
%% odd elements are fieldnames and
%% even elements are their values.
%% This function is useful, when there are a lot of fields to update.
update_set_t(Table, FieldsVals, Where) ->
    case ejabberd_odbc:sql_query_t(
	   [<<"update ">>, Table, <<" set ">>,
        join_field_and_values(FieldsVals),
	    <<" where ">>, Where, ";"]) of
	{updated, 1} ->
	    ok;
	_ ->
        Fields = odds(FieldsVals),
        Vals = evens(FieldsVals),
	    ejabberd_odbc:sql_query_t(
	      [<<"insert into ">>, Table, "(", join(Fields, ", "),
	       <<") values ('">>, join(Vals, "', '"), "');"])
    end.

odds([X,_|T]) -> [X|odds(T)];
odds([])      -> [].

evens([_,X|T]) -> [X|evens(T)];
evens([])      -> [].

join_field_and_values([Field, Val|FieldsVals]) ->
    %% Append a field-value pair
    [Field, $=, $', Val, $' | join_field_and_values_1(FieldsVals)].

join_field_and_values_1([Field, Val|FieldsVals]) ->
    %% Append a separater and a field-value pair
    [$,, $ , Field, $=, $', Val, $' | join_field_and_values_1(FieldsVals)];
join_field_and_values_1([]) ->
    [].


update(LServer, Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun(A, B) -> [A, "='", B, "'"] end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query(
	   LServer,
	   [<<"update ">>, Table, <<" set ">>,
	    join(UPairs, ", "),
	    <<" where ">>, Where, ";"]) of
	{updated, 1} ->
	    ok;
	_ ->
	    ejabberd_odbc:sql_query(
	      LServer,
	      [<<"insert into ">>, Table, "(", join(Fields, ", "),
	       <<") values ('">>, join(Vals, "', '"), "');"])
    end.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the ejabberd_odbc module to this one (odbc_queries)
sql_transaction(LServer, F) ->
    ejabberd_odbc:sql_transaction(LServer, F).

get_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select seconds, state from last "
         "where username='">>, Username, "'"]).

select_last(LServer, TStamp, Comparator) ->
    ejabberd_odbc:sql_query(
        LServer,
        [<<"select username, seconds, state from last "
           "where seconds ">>, Comparator, " ", integer_to_list(TStamp), ";"]).

set_last_t(LServer, Username, Seconds, State) ->
    update(LServer, "last", ["username", "seconds", "state"],
	   [Username, Seconds, State],
	   [<<"username='">>, Username, "'"]).

del_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from last where username='">>, Username, "'"]).

get_password(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select password from users "
         "where username='">>, Username, "';"]).

set_password_t(LServer, Username, Pass) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
	      update_t(<<"users">>, [<<"username">>, <<"password">>],
		       [Username, Pass],
		       [<<"username='">>, Username ,"'"])
      end).

add_user(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"insert into users(username, password) "
         "values ('">>, Username, "', '", Pass, "');"]).

del_user(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from users where username='">>, Username ,"';"]).

del_user_return_password(_LServer, Username, Pass) ->
    P = ejabberd_odbc:sql_query_t(
	  [<<"select password from users where username='">>,
	   Username, "';"]),
    ejabberd_odbc:sql_query_t([<<"delete from users "
			       "where username='">>, Username,
			       <<"' and password='">>, Pass, "';"]),
    P.

list_users(LServer) ->
    ejabberd_odbc:sql_query(
      LServer,
      <<"select username from users">>).

list_users(LServer, [{from, Start}, {to, End}]) when is_integer(Start) and
                                                     is_integer(End) ->
    list_users(LServer, [{limit, End-Start+1}, {offset, Start-1}]);
list_users(LServer, [{prefix, Prefix}, {from, Start}, {to, End}]) when is_list(Prefix) and
                                                                       is_integer(Start) and
                                                                       is_integer(End) ->
    list_users(LServer, [{prefix, Prefix}, {limit, End-Start+1}, {offset, Start-1}]);

list_users(LServer, [{limit, Limit}, {offset, Offset}]) when is_integer(Limit) and
                                                             is_integer(Offset) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select username from users "
         "order by username "
         "limit ">>, integer_to_list(Limit), <<" "
         "offset ">>, integer_to_list(Offset)]);
list_users(LServer, [{prefix, Prefix},
                     {limit, Limit},
                     {offset, Offset}]) when is_list(Prefix) and
                                             is_integer(Limit) and
                                             is_integer(Offset) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select username from users "
         "where username like '">>, Prefix, <<"%' "
         "order by username "
         "limit ">>, integer_to_list(Limit), <<" "
         "offset ">>, integer_to_list(Offset)]).

users_number(LServer) ->
    case element(1, ejabberd_config:get_local_option({odbc_server, LServer})) of
        mysql ->
            ejabberd_odbc:sql_query(
              LServer,
              <<"select table_rows from information_schema.tables where table_name='users'">>);
        pgsql ->
            case ejabberd_config:get_local_option({pgsql_users_number_estimate, LServer}) of
                true ->
                    ejabberd_odbc:sql_query(
                      LServer,
                      <<"select reltuples from pg_class where oid = 'users'::regclass::oid">>);
                _ ->
                    ejabberd_odbc:sql_query(
                      LServer,
                      <<"select count(*) from users">>)
            end;
        _ ->
            ejabberd_odbc:sql_query(
              LServer,
              <<"select count(*) from users">>)
    end.

users_number(LServer, [{prefix, Prefix}]) when is_list(Prefix) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select count(*) from users "
         %% Warning: Escape prefix at higher level to prevent SQL
         %%          injection.
         "where username like '">>, Prefix, "%'"]);
users_number(LServer, []) ->
    users_number(LServer).


add_spool_sql(Username, XML) ->
    [<<"insert into spool(username, xml) "
       "values ('">>, Username, "', '", XML, "');"].

add_spool(LServer, Queries) ->
    ejabberd_odbc:sql_transaction(LServer, Queries).

get_and_del_spool_msg_t(LServer, Username) ->
    F = fun() ->
		Result = ejabberd_odbc:sql_query_t(
			   [<<"select username, xml from spool where username='">>, Username, <<"' "
			      "order by seq;">>]),
		ejabberd_odbc:sql_query_t(
		  [<<"delete from spool where username='">>, Username, "';"]),
		Result
	end,
    ejabberd_odbc:sql_transaction(LServer,F).

del_spool_msg(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from spool where username='">>, Username, "';"]).

get_average_roster_size(Server) ->
    ejabberd_odbc:sql_query(
        Server,
        [<<"select avg(items) from "
           "(select count(*) as items from rosterusers group by username) as items;">>]).

get_average_rostergroup_size(Server) ->
    ejabberd_odbc:sql_query(
        Server,
        [<<"select avg(roster) from "
           "(select count(*) as roster from rostergroups group by username) as roster;">>]).

clear_rosters(Server) ->
    ejabberd_odbc:sql_transaction(
      Server,
      fun() ->
	      ejabberd_odbc:sql_query_t(
		[<<"delete from rosterusers;">>]),
	      ejabberd_odbc:sql_query_t(
		[<<"delete from rostergroups;">>])
      end).

get_roster(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select username, jid, nick, subscription, ask, "
         "askmessage, server, subscribe, type from rosterusers "
         "where username='">>, Username, "'"]).

get_roster_jid_groups(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select jid, grp from rostergroups "
         "where username='">>, Username, "'"]).

get_roster_groups(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t(
      [<<"select grp from rostergroups "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]).

del_user_roster_t(LServer, Username) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
	      ejabberd_odbc:sql_query_t(
		[<<"delete from rosterusers "
		   "where username='">>, Username, "';"]),
	      ejabberd_odbc:sql_query_t(
		[<<"delete from rostergroups "
		   "where username='">>, Username, "';"])
      end).

get_roster_by_jid(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t(
      [<<"select username, jid, nick, subscription, "
         "ask, askmessage, server, subscribe, type from rosterusers "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]).

get_rostergroup_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select grp from rostergroups "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "'"]).

del_roster(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t(
      [<<"delete from rosterusers "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]),
    ejabberd_odbc:sql_query_t(
      [<<"delete from rostergroups "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]).

del_roster_sql(Username, SJID) ->
    [[<<"delete from rosterusers "
        "where username='">>, Username, <<"' "
        "and jid='">>, SJID, "';"],
     [<<"delete from rostergroups "
        "where username='">>, Username, <<"' "
        "and jid='">>, SJID, "';"]].

update_roster(_LServer, Username, SJID, ItemVals, ItemGroups) ->
    update_t(<<"rosterusers">>,
	     [<<"username">>, <<"jid">>, <<"nick">>, <<"subscription">>, <<"ask">>,
	      <<"askmessage">>, <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"username='">>, Username, <<"' and jid='">>, SJID, "'"]),
    ejabberd_odbc:sql_query_t(
      [<<"delete from rostergroups "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]),
    lists:foreach(fun(ItemGroup) ->
			  ejabberd_odbc:sql_query_t(
			    [<<"insert into rostergroups(username, jid, grp) "
			       "values ('">>, join(ItemGroup, "', '"), "');"])
		  end,
		  ItemGroups).

update_roster_sql(Username, SJID, ItemVals, ItemGroups) ->
    [[<<"delete from rosterusers "
        "where username='">>, Username, <<"' "
        "and jid='">>, SJID, "';"],
     [<<"insert into rosterusers("
        "username, jid, nick, "
        "subscription, ask, askmessage, "
        "server, subscribe, type) "
        " values ('">>, join(ItemVals, "', '"), "');"],
     [<<"delete from rostergroups "
        "where username='">>, Username, <<"' "
        "and jid='">>, SJID, "';"]] ++
        [[<<"insert into rostergroups(username, jid, grp) "
            "values ('">>, join(ItemGroup, "', '"), "');"] ||
            ItemGroup <- ItemGroups].

roster_subscribe(_LServer, Username, SJID, ItemVals) ->
    update_t(<<"rosterusers">>,
	     [<<"username">>, <<"jid">>, <<"nick">>, <<"subscription">>, <<"ask">>,
	      <<"askmessage">>, <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"username='">>, Username, <<"' and jid='">>, SJID, "'"]).

get_subscription(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select subscription from rosterusers "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "'"]).

set_private_data(_LServer, Username, LXMLNS, SData) ->
    update_t(<<"private_storage">>,
	     [<<"username">>, <<"namespace">>, <<"data">>],
	     [Username, LXMLNS, SData],
	     [<<"username='">>, Username, <<"' and namespace='">>, LXMLNS, "'"]).

set_private_data_sql(Username, LXMLNS, SData) ->
    [[<<"delete from private_storage "
        "where username='">>, Username, <<"' and "
        "namespace='">>, LXMLNS, "';"],
     [<<"insert into private_storage(username, namespace, data) "
        "values ('">>, Username, "', '", LXMLNS, "', '", SData, "');"]].

get_private_data(LServer, Username, LXMLNS) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select data from private_storage "
         "where username='">>, Username, <<"' and "
         "namespace='">>, LXMLNS, "';"]).

multi_get_private_data(LServer, Username, LXMLNSs) when length(LXMLNSs) > 0 ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select namespace, data from private_storage "
         "where username='">>, Username, <<"' and "
         "namespace IN ('">>, join(LXMLNSs, "', '"), "');"]).

del_user_private_storage(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from private_storage where username='">>, Username, "';"]).

set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail, SFN, SFamily, SGiven,
	  SLBDay, SLCTRY, SLEMail, SLFN, SLFamily, SLGiven, SLLocality,
	  SLMiddle, SLNickname, SLOrgName, SLOrgUnit, SLocality, SMiddle,
	  SNickname, SOrgName, SOrgUnit, SVCARD, Username) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
        update_t(<<"vcard">>,
          [<<"username">>, <<"server">>, <<"vcard">>],
          [LUsername, LServer, SVCARD],
          [<<"username='">>, LUsername, <<"' and server='">>, LServer, "'"]),
        update_set_t(<<"vcard_search">>,
          [<<"username">>, Username,
           <<"lusername">>, LUsername,
           <<"server">>, LServer,
           <<"fn">>, SFN,
           <<"lfn">>, SLFN,
           <<"family">>, SFamily,
           <<"lfamily">>, SLFamily,
           <<"given">>, SGiven,
           <<"lgiven">>, SLGiven,
           <<"middle">>, SMiddle,
           <<"lmiddle">>, SLMiddle,
           <<"nickname">>, SNickname,
           <<"lnickname">>, SLNickname,
           <<"bday">>, SBDay,
           <<"lbday">>, SLBDay,
           <<"ctry">>, SCTRY,
           <<"lctry">>, SLCTRY,
           <<"locality">>, SLocality,
           <<"llocality">>, SLLocality,
           <<"email">>, SEMail,
           <<"lemail">>, SLEMail,
           <<"orgname">>, SOrgName,
           <<"lorgname">>, SLOrgName,
           <<"orgunit">>, SOrgUnit,
           <<"lorgunit">>, SLOrgUnit],
          [<<"lusername='">>, LUsername, <<"' and server='">>, LServer, "'"])
      end).

get_vcard(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select vcard from vcard "
         "where username='">>, Username, <<"' and server='">>, LServer, "';"]).

get_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select name from privacy_default_list "
         "where username='">>, Username, "';"]).

get_default_privacy_list_t(Username) ->
    ejabberd_odbc:sql_query_t(
      [<<"select name from privacy_default_list "
         "where username='">>, Username, "';"]).

count_privacy_lists(LServer) ->
    ejabberd_odbc:sql_query(LServer, [<<"select count(*) from privacy_list;">>]).

clear_privacy_lists(LServer) ->
    ejabberd_odbc:sql_query(LServer, [<<"delete from privacy_list;">>]).

get_privacy_list_names(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select name from privacy_list "
         "where username='">>, Username, "';"]).

get_privacy_list_names_t(Username) ->
    ejabberd_odbc:sql_query_t(
      [<<"select name from privacy_list "
         "where username='">>, Username, "';"]).

get_privacy_list_id(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select id from privacy_list "
         "where username='">>, Username, <<"' and name='">>, SName, "';"]).

get_privacy_list_id_t(Username, SName) ->
    ejabberd_odbc:sql_query_t(
      [<<"select id from privacy_list "
         "where username='">>, Username, <<"' and name='">>, SName, "';"]).

get_privacy_list_data(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select t, value, action, ord, match_all, match_iq, "
         "match_message, match_presence_in, match_presence_out "
         "from privacy_list_data "
         "where id = (select id from privacy_list where "
         "username='">>, Username, <<"' and name='">>, SName, <<"') "
         "order by ord;">>]).

get_privacy_list_data_by_id(LServer, ID) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select t, value, action, ord, match_all, match_iq, "
         "match_message, match_presence_in, match_presence_out "
         "from privacy_list_data "
         "where id='">>, ID, <<"' order by ord;">>]).

set_default_privacy_list(Username, SName) ->
    update_t(<<"privacy_default_list">>, [<<"username">>, <<"name">>],
	     [Username, SName], [<<"username='">>, Username, "'"]).

unset_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from privacy_default_list "
         "where username='">>, Username, "';"]).

remove_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t(
      [<<"delete from privacy_list "
         "where username='">>, Username, "' and name='", SName, "';"]).

add_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t(
      [<<"insert into privacy_list(username, name) "
         "values ('">>, Username, "', '", SName, "');"]).

set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t(
      [<<"delete from privacy_list_data "
         "where id='">>, ID, "';"]),
    lists:foreach(fun(Items) ->
			  ejabberd_odbc:sql_query_t(
			    [<<"insert into privacy_list_data("
			       "id, t, value, action, ord, match_all, match_iq, "
			       "match_message, match_presence_in, "
			       "match_presence_out "
			       ") "
			       "values ('">>, ID, "', '",
			     join(Items, "', '"), "');"])
		  end, RItems).

del_privacy_lists(LServer, Server, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from privacy_list where username='">>, Username, "';"]),
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from privacy_list_data where value='">>, Username, $@, Server, "';"]),
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from privacy_default_list where username='">>, Username, "';"]).

%% Characters to escape
escape_character($\0) -> "\\0";
escape_character($\n) -> "\\n";
escape_character($\t) -> "\\t";
escape_character($\b) -> "\\b";
escape_character($\r) -> "\\r";
escape_character($')  -> "\\'";
escape_character($")  -> "\\\"";
escape_character($\\) -> "\\\\";
escape_character(C)   -> C.

%% Count number of records in a table given a where clause
count_records_where(LServer, Table, WhereClause) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select count(*) from ">>, Table, " ", WhereClause, ";"]).


get_roster_version(LServer, LUser) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select version from roster_version where username = '">>, LUser, "'"]).

set_roster_version(LUser, Version) ->
    update_t(
      <<"roster_version">>,
      [<<"username">>, <<"version">>],
      [LUser, Version],
      [<<"username = '">>, LUser, "'"]).

-endif.

%% -----------------
%% MSSQL queries
-ifdef(mssql).

get_db_type() ->
    mssql.

%% Queries can be either a fun or a list of queries
sql_transaction(LServer, Queries) when is_list(Queries) ->
    %% SQL transaction based on a list of queries
    %% This function automatically
    F = fun() ->
                lists:foreach(fun(Query) ->
                                      ejabberd_odbc:sql_query(LServer, Query)
                              end, Queries)
        end,
    {atomic, catch F()};
sql_transaction(_LServer, FQueries) ->
    {atomic, catch FQueries()}.

get_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_last '", Username, "'"]).

set_last_t(LServer, Username, Seconds, State) ->
    Result = ejabberd_odbc:sql_query(
               LServer,
               ["EXECUTE dbo.set_last '", Username, "', '", Seconds,
                "', '", State, "'"]),
    {atomic, Result}.

del_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_last '", Username, "'"]).

get_password(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_password '", Username, "'"]).

set_password_t(LServer, Username, Pass) ->
    Result = ejabberd_odbc:sql_query(
               LServer,
               ["EXECUTE dbo.set_password '", Username, "', '", Pass, "'"]),
    {atomic, Result}.

add_user(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.add_user '", Username, "', '", Pass, "'"]).

del_user(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_user '", Username ,"'"]).

del_user_return_password(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_user_return_password '", Username, "'"]),
    Pass.

list_users(LServer) ->
    ejabberd_odbc:sql_query(
      LServer,
      "EXECUTE dbo.list_users").

list_users(LServer, _) ->
    %% scope listing not supported
    list_users(LServer).

users_number(LServer) ->
    ejabberd_odbc:sql_query(
      LServer,
      "select count(*) from users with (nolock)").

users_number(LServer, _) ->
    %% scope listing not supported
    users_number(LServer).

add_spool_sql(Username, XML) ->
    ["EXECUTE dbo.add_spool '", Username, "' , '",XML,"'"].

add_spool(LServer, Queries) ->
    lists:foreach(fun(Query) ->
			  ejabberd_odbc:sql_query(LServer, Query)
		  end,
		  Queries).

get_and_del_spool_msg_t(LServer, Username) ->
    [Result] = case ejabberd_odbc:sql_query(
                      LServer,
                      ["EXECUTE dbo.get_and_del_spool_msg '", Username, "'"]) of
		   Rs when is_list(Rs) ->
                       lists:filter(fun({selected, _Header, _Row}) ->
                                            true;
                                       ({updated, _N}) ->
                                            false
                                    end,
                                    Rs);
		   Rs -> [Rs]
	       end,
    {atomic, Result}.

del_spool_msg(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_spool_msg '", Username, "'"]).

get_roster(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster '", Username, "'"]).

get_roster_jid_groups(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster_jid_groups '", Username, "'"]).

get_roster_groups(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster_groups '", Username, "' , '", SJID, "'"]).

del_user_roster_t(LServer, Username) ->
    Result = ejabberd_odbc:sql_query(
               LServer,
               ["EXECUTE dbo.del_user_roster '", Username, "'"]),
    {atomic, Result}.

get_roster_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster_by_jid '", Username, "' , '", SJID, "'"]).

get_rostergroup_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_rostergroup_by_jid '", Username, "' , '", SJID, "'"]).

del_roster(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_roster '", Username, "', '", SJID, "'"]).

del_roster_sql(Username, SJID) ->
    ["EXECUTE dbo.del_roster '", Username, "', '", SJID, "'"].

update_roster(LServer, Username, SJID, ItemVals, ItemGroups) ->
    Query1 = ["EXECUTE dbo.del_roster '", Username, "', '", SJID, "' "],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query1)),
    Query2 = ["EXECUTE dbo.add_roster_user ", ItemVals],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query2)),
    Query3 = ["EXECUTE dbo.del_roster_groups '", Username, "', '", SJID, "' "],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query3)),
    lists:foreach(fun(ItemGroup) ->
			  Query = ["EXECUTE dbo.add_roster_group ",
				   ItemGroup],
			  ejabberd_odbc:sql_query(LServer,
						  lists:flatten(Query))
		  end,
		  ItemGroups).

update_roster_sql(Username, SJID, ItemVals, ItemGroups) ->
    ["BEGIN TRANSACTION ",
     "EXECUTE dbo.del_roster_groups '", Username, "','", SJID, "' ",
     "EXECUTE dbo.add_roster_user ", ItemVals, " "] ++
	[lists:flatten("EXECUTE dbo.add_roster_group ", ItemGroup, " ")
	 || ItemGroup <- ItemGroups] ++
	["COMMIT"].

roster_subscribe(LServer, _Username, _SJID, ItemVals) ->
    catch ejabberd_odbc:sql_query(
	    LServer,
	    ["EXECUTE dbo.add_roster_user ", ItemVals]).

get_subscription(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_subscription '", Username, "' , '", SJID, "'"]).

set_private_data(LServer, Username, LXMLNS, SData) ->
    ejabberd_odbc:sql_query(
      LServer,
      set_private_data_sql(Username, LXMLNS, SData)).

set_private_data_sql(Username, LXMLNS, SData) ->
    ["EXECUTE dbo.set_private_data '", Username, "' , '", LXMLNS, "' , '", SData, "'"].

get_private_data(LServer, Username, LXMLNS) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_private_data '", Username, "' , '", LXMLNS, "'"]).

del_user_private_storage(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_user_storage '", Username, "'"]).

set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail, SFN, SFamily, SGiven,
	  SLBDay, SLCTRY, SLEMail, SLFN, SLFamily, SLGiven, SLLocality,
	  SLMiddle, SLNickname, SLOrgName, SLOrgUnit, SLocality, SMiddle,
	  SNickname, SOrgName, SOrgUnit, SVCARD, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.set_vcard '", SVCARD, "' , '", Username, "' , '",
       LUsername, "' , '", LServer, "' , '",
       SFN, "' , '", SLFN, "' , '", SFamily, "' , '", SLFamily, "' , '",
       SGiven, "' , '", SLGiven, "' , '", SMiddle, "' , '", SLMiddle, "' , '",
       SNickname, "' , '", SLNickname, "' , '", SBDay, "' , '", SLBDay, "' , '",
       SCTRY, "' , '", SLCTRY, "' , '", SLocality, "' , '", SLLocality, "' , '",
       SEMail, "' , '", SLEMail, "' , '", SOrgName, "' , '", SLOrgName, "' , '",
       SOrgUnit, "' , '", SLOrgUnit, "'"]).

get_vcard(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_vcard '", Username, "' , '", LServer, "'"]).

get_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_default_privacy_list '", Username, "'"]).

get_default_privacy_list_t(Username) ->
    ejabberd_odbc:sql_query_t(
      ["EXECUTE dbo.get_default_privacy_list '", Username, "'"]).

get_privacy_list_names(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_privacy_list_names '", Username, "'"]).

get_privacy_list_names_t(Username) ->
    ejabberd_odbc:sql_query_t(
      ["EXECUTE dbo.get_privacy_list_names '", Username, "'"]).

get_privacy_list_id(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_privacy_list_id '", Username, "' , '", SName, "'"]).

get_privacy_list_id_t(Username, SName) ->
    ejabberd_odbc:sql_query_t(
      ["EXECUTE dbo.get_privacy_list_id '", Username, "' , '", SName, "'"]).

get_privacy_list_data(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_privacy_list_data '", Username, "' , '", SName, "'"]).

get_privacy_list_data_by_id(LServer, ID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_privacy_list_data_by_id '", ID, "'"]).

set_default_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t(
      ["EXECUTE dbo.set_default_privacy_list '", Username, "' , '", SName, "'"]).

unset_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.unset_default_privacy_list '", Username, "'"]).

remove_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t(
      ["EXECUTE dbo.remove_privacy_list '", Username, "' , '", SName, "'"]).

add_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t(
      ["EXECUTE dbo.add_privacy_list '", Username, "' , '", SName, "'"]).

set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t(
      ["EXECUTE dbo.del_privacy_list_by_id '", ID, "'"]),

    lists:foreach(fun(Items) ->
			  ejabberd_odbc:sql_query_t(
                            ["EXECUTE dbo.set_privacy_list '", ID, "', '", join(Items, "', '"), "'"])
		  end, RItems).

del_privacy_lists(LServer, Server, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_privacy_lists @Server='", Server ,"' @username='", Username, "'"]).

%% @doc Escape a character.
%% Characters to escape.
escape_character($\0) -> "\\0";
escape_character($\t) -> "\\t";
escape_character($\b) -> "\\b";
escape_character($\r) -> "\\r";
escape_character($')  -> "\''";
escape_character($")  -> "\\\"";
escape_character(C)   -> C.

%% Count number of records in a table given a where clause
count_records_where(LServer, Table, WhereClause) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select count(*) from ", Table, " with (nolock) ", WhereClause]).

get_roster_version(LServer, LUser) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster_version '", LUser, "'"]).

set_roster_version(Username, Version) ->
    %% This function doesn't know the vhost, so we hope it's the first one defined:
    LServer = ?MYNAME,
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.set_roster_version '", Username, "', '", Version, "'"]).
-endif.
