%%%----------------------------------------------------------------------
%%% File    : rdbms_queries.erl
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

-module(rdbms_queries).
-author("mremond@process-one.net").

-export([get_db_type/0,
         begin_trans/0,
         get_db_specific_limits/1,
         get_db_specific_offset/2,
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
         get_users_without_scram/2,
         get_users_without_scram_count/1,
         get_average_roster_size/1,
         get_average_rostergroup_size/1,
         clear_rosters/1,
         get_roster/2,
         get_roster_jid_groups/2,
         get_roster_groups/3,
         del_user_roster_t/2,
         get_roster_by_jid/3,
         get_roster_by_jid_t/3,
         get_rostergroup_by_jid/3,
         get_rostergroup_by_jid_t/3,
         del_roster/3,
         del_roster_sql/2,
         update_roster/5,
         update_roster_sql/4,
         roster_subscribe/4,
         get_subscription/3,
         get_subscription_t/3,
         set_private_data/4,
         set_private_data_sql/3,
         get_private_data/3,
         multi_get_private_data/3,
         multi_set_private_data/3,
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
         search_vcard/3,
         escape_string/1,
         escape_like_string/1,
         count_records_where/3,
         get_roster_version/2,
         set_roster_version/2,
         prepare_offline_message/6,
         push_offline_messages/2,
         pop_offline_messages/4,
         count_offline_messages/4,
         remove_old_offline_messages/2,
         remove_expired_offline_messages/2,
         remove_offline_messages/3,
         create_bulk_insert_query/3]).

%% We have only two compile time options for db queries:
%%-define(generic, true).
%%-define(mssql, true).
-ifndef(mssql).
-undef(generic).
-define(generic, true).
-endif.

-define(ODBC_TYPE, (mongoose_rdbms_type:get())).

-include("mongoose.hrl").

%
%% -----------------
%% Common functions

join([], _Sep) ->
    [];
join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

%% Note: escape functions (`escape_string/1' and `escape_like_string/1')
%%       are in this module and not in `mongoose_rdbms',
%%       because they are called a lot.
%%       To have both `escape_string/1' and `escape_character/1' in one module
%%       is an optimization.

-spec escape_string(binary() | string()) -> binary() | string().
escape_string(S) when is_binary(S) ->
    list_to_binary(escape_string(binary_to_list(S)));
escape_string(S) when is_list(S) ->
    [escape_character(C) || C <- S].

-spec escape_like_string(binary() | string()) -> binary() | string().
escape_like_string(S) when is_binary(S) ->
    list_to_binary(escape_like_string(binary_to_list(S)));
escape_like_string(S) when is_list(S) ->
    [escape_like_character(C) || C <- S].

escape_like_character($%) -> "\\%";
escape_like_character($_) -> "\\_";
escape_like_character(C)  -> escape_character(C).


%% -----------------
%% Generic queries

get_db_type() ->
    ?ODBC_TYPE.


%% Safe atomic update.
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun(A, B) -> [A, "='", B, "'"] end,
                           Fields, Vals),
    case mongoose_rdbms:sql_query_t(
           [<<"update ">>, Table, <<" set ">>,
            join(UPairs, ", "),
            <<" where ">>, Where, ";"]) of
        {updated, 1} ->
            ok;
        _ ->
            mongoose_rdbms:sql_query_t(
              [<<"insert into ">>, Table, "(", join(Fields, ", "),
               <<") values ('">>, join(Vals, "', '"), "');"])
    end.

%% Safe atomic update.
%% Fields and their values are passed as a list where
%% odd elements are fieldnames and
%% even elements are their values.
%% This function is useful, when there are a lot of fields to update.
update_set_t(Table, FieldsVals, Where) ->
    case mongoose_rdbms:sql_query_t(
           [<<"update ">>, Table, <<" set ">>,
        join_field_and_values(FieldsVals),
            <<" where ">>, Where, ";"]) of
        {updated, 1} ->
            ok;
        _ ->
        Fields = odds(FieldsVals),
        Vals = evens(FieldsVals),
            mongoose_rdbms:sql_query_t(
              [<<"insert into ">>, Table, "(", join(Fields, ", "),
               <<") values ('">>, join(Vals, "', '"), "');"])
    end.

odds([X, _|T]) -> [X|odds(T)];
odds([])      -> [].

evens([_, X|T]) -> [X|evens(T)];
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
    case mongoose_rdbms:sql_query(
           LServer,
           [<<"update ">>, Table, <<" set ">>,
            join(UPairs, ", "),
            <<" where ">>, Where, ";"]) of
        {updated, 1} ->
            ok;
        _ ->
            mongoose_rdbms:sql_query(
              LServer,
              [<<"insert into ">>, Table, "(", join(Fields, ", "),
               <<") values ('">>, join(Vals, "', '"), "');"])
    end.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the mongoose_rdbms module to this one (rdbms_queries)
sql_transaction(LServer, F) ->
    mongoose_rdbms:sql_transaction(LServer, F).

begin_trans() ->
    begin_trans(?ODBC_TYPE).

begin_trans(mssql) ->
    rdbms_queries_mssql:begin_trans();
begin_trans(_) ->
    [<<"BEGIN;">>].


get_last(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select seconds, state from last "
         "where username='">>, Username, "'"]).

select_last(LServer, TStamp, Comparator) ->
    mongoose_rdbms:sql_query(
        LServer,
        [<<"select username, seconds, state from last "
           "where seconds ">>, Comparator, " ", integer_to_list(TStamp), ";"]).

set_last_t(LServer, Username, Seconds, State) ->
    update(LServer, "last", ["username", "seconds", "state"],
           [Username, Seconds, State],
           [<<"username='">>, Username, "'"]).

del_last(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from last where username='">>, Username, "'"]).

get_password(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select password, pass_details from users "
       "where username='">>, Username, <<"';">>]).

set_password_t(LServer, Username, {Pass, PassDetails}) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
              update_t(<<"users">>, [<<"password">>, <<"pass_details">>],
                       [Pass, PassDetails],
                       [<<"username='">>, Username, <<"'">>])
      end);
set_password_t(LServer, Username, Pass) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
              update_t(<<"users">>, [<<"username">>, <<"password">>],
                       [Username, Pass],
                       [<<"username='">>, Username, <<"'">>])
      end).

add_user(LServer, Username, {Pass, PassDetails}) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"insert into users(username, password, pass_details) "
       "values ('">>, Username, <<"', '">>, Pass, <<"', '">>, PassDetails, <<"');">>]);
add_user(LServer, Username, Pass) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"insert into users(username, password) "
         "values ('">>, Username, <<"', '">>, Pass, <<"');">>]).

del_user(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from users where username='">>, Username, "';"]).

del_user_return_password(_LServer, Username, Pass) ->
    P = mongoose_rdbms:sql_query_t(
          [<<"select password from users where username='">>,
           Username, "';"]),
    mongoose_rdbms:sql_query_t([<<"delete from users "
                               "where username='">>, Username,
                               <<"' and password='">>, Pass, "';"]),
    P.

list_users(LServer) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select username from users">>]).

list_users(LServer, [{from, Start}, {to, End}]) when is_integer(Start) and
                                                     is_integer(End) ->
    list_users(LServer, [{limit, End-Start+1}, {offset, Start-1}]);
list_users(LServer, [{prefix, Prefix}, {from, Start}, {to, End}]) when is_list(Prefix) and
                                                                       is_integer(Start) and
                                                                       is_integer(End) ->
    list_users(LServer, [{prefix, Prefix}, {limit, End-Start+1}, {offset, Start-1}]);

list_users(LServer, [{limit, Limit}, {offset, Offset}]) when is_integer(Limit) and
                                                             is_integer(Offset) ->
    mongoose_rdbms:sql_query(
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
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select username from users "
         "where username like '">>, Prefix, <<"%' "
         "order by username "
         "limit ">>, integer_to_list(Limit), <<" "
         "offset ">>, integer_to_list(Offset)]).

users_number(LServer) ->
    case mongoose_rdbms:db_engine(LServer) of
        mysql ->
            mongoose_rdbms:sql_query(
              LServer,
              <<"select table_rows from information_schema.tables where table_name='users'">>);
        pgsql ->
            case ejabberd_config:get_local_option({pgsql_users_number_estimate, LServer}) of
                true ->
                    mongoose_rdbms:sql_query(
                      LServer,
                      <<"select reltuples from pg_class where oid = 'users'::regclass::oid">>);
                _ ->
                    mongoose_rdbms:sql_query(
                      LServer,
                      <<"select count(*) from users">>)
            end;
        _ ->
            mongoose_rdbms:sql_query(
              LServer,
              [<<"select count(*) from users">>])
    end.

users_number(LServer, [{prefix, Prefix}]) when is_list(Prefix) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from users "
         %% Warning: Escape prefix at higher level to prevent SQL
         %%          injection.
         "where username like '">>, Prefix, "%'"]);
users_number(LServer, []) ->
    users_number(LServer).

get_users_without_scram(LServer, Limit) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select username, password from users where pass_details is null limit ">>,
       integer_to_binary(Limit)]).

get_users_without_scram_count(LServer) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from users where pass_details is null">>]).

get_average_roster_size(Server) ->
    mongoose_rdbms:sql_query(
        Server,
        [<<"select avg(items) from "
           "(select count(*) as items from rosterusers group by username) as items;">>]).

get_average_rostergroup_size(Server) ->
    mongoose_rdbms:sql_query(
        Server,
        [<<"select avg(roster) from "
           "(select count(*) as roster from rostergroups group by username) as roster;">>]).

clear_rosters(Server) ->
    mongoose_rdbms:sql_transaction(
      Server,
      fun() ->
              mongoose_rdbms:sql_query_t(
                [<<"delete from rosterusers;">>]),
              mongoose_rdbms:sql_query_t(
                [<<"delete from rostergroups;">>])
      end).

get_roster(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select username, jid, nick, subscription, ask, "
         "askmessage, server, subscribe, type from rosterusers "
         "where username='">>, Username, "'"]).

get_roster_jid_groups(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select jid, grp from rostergroups "
         "where username='">>, Username, "'"]).

get_roster_groups(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(
      [<<"select grp from rostergroups "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]).

del_user_roster_t(LServer, Username) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
              mongoose_rdbms:sql_query_t(
                [<<"delete from rosterusers "
                   "where username='">>, Username, "';"]),
              mongoose_rdbms:sql_query_t(
                [<<"delete from rostergroups "
                   "where username='">>, Username, "';"])
      end).

q_get_roster(Username, SJID) ->
    [<<"select username, jid, nick, subscription, "
    "ask, askmessage, server, subscribe, type from rosterusers "
    "where username='">>, Username, <<"' "
    "and jid='">>, SJID, "';"].

get_roster_by_jid(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query(LServer, q_get_roster(Username, SJID)).

get_roster_by_jid_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_roster(Username, SJID)).

q_get_rostergroup(Username, SJID) ->
    [<<"select grp from rostergroups "
    "where username='">>, Username, <<"' "
    "and jid='">>, SJID, "'"].

get_rostergroup_by_jid(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query(LServer, q_get_rostergroup(Username, SJID)).

get_rostergroup_by_jid_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_rostergroup(Username, SJID)).

del_roster(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(
      [<<"delete from rosterusers "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]),
    mongoose_rdbms:sql_query_t(
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
    mongoose_rdbms:sql_query_t(
      [<<"delete from rostergroups "
         "where username='">>, Username, <<"' "
         "and jid='">>, SJID, "';"]),
    lists:foreach(fun(ItemGroup) ->
                          mongoose_rdbms:sql_query_t(
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

q_get_subscription(Username, SJID) ->
    [<<"select subscription from rosterusers "
    "where username='">>, Username, <<"' "
    "and jid='">>, SJID, "'"].

get_subscription(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query( LServer, q_get_subscription(Username, SJID)).

get_subscription_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_subscription(Username, SJID)).

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
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select data from private_storage "
         "where username='">>, Username, <<"' and "
         "namespace='">>, LXMLNS, "';"]).

multi_get_private_data(LServer, Username, LXMLNSs) when length(LXMLNSs) > 0 ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select namespace, data from private_storage "
         "where username='">>, Username, <<"' and "
         "namespace IN ('">>, join(LXMLNSs, "', '"), "');"]).

%% set_private_data for multiple queries using MySQL's specific syntax.
multi_set_private_data(LServer, Username, SNS2XML) when length(SNS2XML) > 0 ->
    Rows = [private_data_row(Username, NS, Data) || {NS, Data} <- SNS2XML],
    mongoose_rdbms:sql_query(
      LServer,
      [<<"replace into private_storage (username, namespace, data) "
         "values ">>, join(Rows, "', '")]).

private_data_row(Username, NS, Data) ->
    [<<"('">>, Username, <<"', '">>, NS, <<"', '">>, Data, <<"')">>].

del_user_private_storage(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from private_storage where username='">>, Username, "';"]).

set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail, SFN, SFamily, SGiven,
          SLBDay, SLCTRY, SLEMail, SLFN, SLFamily, SLGiven, SLLocality,
          SLMiddle, SLNickname, SLOrgName, SLOrgUnit, SLocality, SMiddle,
          SNickname, SOrgName, SOrgUnit, SVCARD, Username) ->
    mongoose_rdbms:sql_transaction(
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
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select vcard from vcard "
         "where username='">>, Username, <<"' and server='">>, LServer, "';"]).


search_vcard(LServer, RestrictionSQL, Limit) ->
    Type = ?ODBC_TYPE,
    search_vcard(Type, LServer, RestrictionSQL, Limit).

search_vcard(mssql, LServer, RestrictionSQL, Limit) ->
    rdbms_queries_mssql:search_vcard(LServer, RestrictionSQL, Limit);
search_vcard(_, LServer, RestrictionSQL, Limit) ->
    do_search_vcard(LServer, RestrictionSQL, Limit).


do_search_vcard(LServer, RestrictionSQL, infinity) ->
    do_search_vcard2(LServer, RestrictionSQL, <<"">>);
do_search_vcard(LServer, RestrictionSQL, Limit) when is_integer(Limit) ->
    BinLimit = integer_to_binary(Limit),
    do_search_vcard2(LServer, RestrictionSQL, <<"LIMIT ", BinLimit/binary>>).

do_search_vcard2(LServer, RestrictionSQL, Limit) ->
    mongoose_rdbms:sql_query(
        LServer,
        [<<"select username, server, fn, family, given, middle, "
        "nickname, bday, ctry, locality, "
        "email, orgname, orgunit from vcard_search ">>,
            RestrictionSQL, Limit, ";"]).

get_default_privacy_list(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select name from privacy_default_list "
         "where username='">>, Username, "';"]).

get_default_privacy_list_t(Username) ->
    mongoose_rdbms:sql_query_t(
      [<<"select name from privacy_default_list "
         "where username='">>, Username, "';"]).

count_privacy_lists(LServer) ->
    mongoose_rdbms:sql_query(LServer, [<<"select count(*) from privacy_list;">>]).

clear_privacy_lists(LServer) ->
    mongoose_rdbms:sql_query(LServer, [<<"delete from privacy_list;">>]).

get_privacy_list_names(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select name from privacy_list "
         "where username='">>, Username, "';"]).

get_privacy_list_names_t(Username) ->
    mongoose_rdbms:sql_query_t(
      [<<"select name from privacy_list "
         "where username='">>, Username, "';"]).

get_privacy_list_id(LServer, Username, SName) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select id from privacy_list "
         "where username='">>, Username, <<"' and name='">>, SName, "';"]).

get_privacy_list_id_t(Username, SName) ->
    mongoose_rdbms:sql_query_t(
      [<<"select id from privacy_list "
         "where username='">>, Username, <<"' and name='">>, SName, "';"]).

get_privacy_list_data(LServer, Username, SName) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select t, value, action, ord, match_all, match_iq, "
         "match_message, match_presence_in, match_presence_out "
         "from privacy_list_data "
         "where id = (select id from privacy_list where "
         "username='">>, Username, <<"' and name='">>, SName, <<"') "
         "order by ord;">>]).

get_privacy_list_data_by_id(LServer, ID) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select t, value, action, ord, match_all, match_iq, "
         "match_message, match_presence_in, match_presence_out "
         "from privacy_list_data "
         "where id='">>, ID, <<"' order by ord;">>]).

set_default_privacy_list(Username, SName) ->
    update_t(<<"privacy_default_list">>, [<<"username">>, <<"name">>],
             [Username, SName], [<<"username='">>, Username, "'"]).

unset_default_privacy_list(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_default_list "
         "where username='">>, Username, "';"]).

remove_privacy_list(Username, SName) ->
    mongoose_rdbms:sql_query_t(
      [<<"delete from privacy_list "
         "where username='">>, Username, "' and name='", SName, "';"]).

add_privacy_list(Username, SName) ->
    mongoose_rdbms:sql_query_t(
      [<<"insert into privacy_list(username, name) "
         "values ('">>, Username, "', '", SName, "');"]).

set_privacy_list(ID, RItems) ->
    mongoose_rdbms:sql_query_t(
      [<<"delete from privacy_list_data "
         "where id='">>, ID, "';"]),
    lists:foreach(fun(Items) ->
                          mongoose_rdbms:sql_query_t(
                            [<<"insert into privacy_list_data("
                               "id, t, value, action, ord, match_all, match_iq, "
                               "match_message, match_presence_in, "
                               "match_presence_out "
                               ") "
                               "values ('">>, ID, "', '",
                             join(Items, "', '"), "');"])
                  end, RItems).

del_privacy_lists(LServer, _Server, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_list_data where id in ( select id from privacy_list as pl where pl.username='">>, Username, <<"');">>]),
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_list where username='">>, Username, "';"]),
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_default_list where username='">>, Username, "';"]).

%% Characters to escape
escape_character($\0) -> "\\0";
escape_character($\n) -> "\\n";
escape_character($\t) -> "\\t";
escape_character($\b) -> "\\b";
escape_character($\r) -> "\\r";
escape_character($')  -> "''";
escape_character($")  -> "\\\"";
escape_character($\\) -> "\\\\";
escape_character(C)   -> C.

%% Count number of records in a table given a where clause
count_records_where(LServer, Table, WhereClause) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from ">>, Table, " ", WhereClause, ";"]).


get_roster_version(LServer, LUser) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select version from roster_version where username = '">>, LUser, "'"]).

set_roster_version(LUser, Version) ->
    update_t(
      <<"roster_version">>,
      [<<"username">>, <<"version">>],
      [LUser, Version],
      [<<"username = '">>, LUser, "'"]).


pop_offline_messages(LServer, SUser, SServer, STimeStamp) ->
    SelectSQL = select_offline_messages_sql(SUser, SServer, STimeStamp),
    DeleteSQL = delete_offline_messages_sql(SUser, SServer),
    F = fun() ->
              Res = mongoose_rdbms:sql_query_t(SelectSQL),
          mongoose_rdbms:sql_query_t(DeleteSQL),
          Res
        end,
    mongoose_rdbms:sql_transaction(LServer, F).

select_offline_messages_sql(SUser, SServer, STimeStamp) ->
    [<<"select timestamp, from_jid, packet from offline_message "
            "where server = '">>, SServer, <<"' and "
                  "username = '">>, SUser, <<"' and "
                  "(expire is null or expire > ">>, STimeStamp, <<") "
             "ORDER BY timestamp">>].

delete_offline_messages_sql(SUser, SServer) ->
    [<<"delete from offline_message "
            "where server = '">>, SServer, <<"' and "
                  "username = '">>, SUser, <<"'">>].

remove_old_offline_messages(LServer, STimeStamp) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from offline_message where timestamp < ">>, STimeStamp]).

remove_expired_offline_messages(LServer, STimeStamp) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from offline_message "
            "where expire is not null and expire < ">>, STimeStamp]).

remove_offline_messages(LServer, SUser, SServer) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from offline_message "
            "where server = '">>, SServer, <<"' and "
                  "username = '">>, SUser, <<"'">>]).

prepare_offline_message(SUser, SServer, STimeStamp, SExpire, SFrom, SPacket) ->
    [<<"('">>,   SUser,
     <<"', '">>, SServer,
     <<"', ">>,  STimeStamp,
     <<", ">>,   SExpire,
     <<", '">>,  SFrom,
     <<"', '">>, SPacket,
     <<"')">>].

push_offline_messages(LServer, Rows) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"INSERT INTO offline_message "
              "(username, server, timestamp, expire, from_jid, packet) "
            "VALUES ">>, join(Rows, ", ")]).


count_offline_messages(LServer, SUser, SServer, Limit) ->
    count_offline_messages(?ODBC_TYPE, LServer, SUser, SServer, Limit).

count_offline_messages(mssql, LServer, SUser, SServer, Limit) ->
    rdbms_queries_mssql:count_offline_messages(LServer, SUser, SServer, Limit);
count_offline_messages(_, LServer, SUser, SServer, Limit) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from offline_message "
            "where server = '">>, SServer, <<"' and "
                  "username = '">>, SUser, <<"' "
            "limit ">>, integer_to_list(Limit)]).

-spec create_bulk_insert_query(Table :: iodata() | atom(), Fields :: [iodata() | atom()],
                               RowsNum :: pos_integer()) ->
    iodata().
create_bulk_insert_query(Table, Fields, RowsNum) when is_atom(Table) ->
    create_bulk_insert_query(atom_to_binary(Table, utf8), Fields, RowsNum);
create_bulk_insert_query(Table, [Field | _] = Fields, RowsNum) when is_atom(Field) ->
    create_bulk_insert_query(Table, [atom_to_binary(F, utf8) || F <- Fields], RowsNum);
create_bulk_insert_query(Table, Fields, RowsNum) when RowsNum > 0 ->
    JoinedFields = join(Fields, <<", ">>),
    Placeholders = lists:duplicate(length(Fields), <<"?">>),
    PlaceholderSet = [<<"(">>, join(Placeholders, <<", ">>), <<")">>],
    PlaceholderSets = lists:duplicate(RowsNum, PlaceholderSet),
    JoinedPlaceholderSets = join(PlaceholderSets, <<", ">>),
    [<<"INSERT INTO ">>, Table, <<" (">>, JoinedFields, <<") "
       "VALUES ">>, JoinedPlaceholderSets, <<";">>].

-spec get_db_specific_limits(integer())
        -> {SQL :: nonempty_string(), []} | {[], MSSQL::nonempty_string()}.
get_db_specific_limits(Limit) ->
    LimitStr = integer_to_list(Limit),
    do_get_db_specific_limits(?ODBC_TYPE, LimitStr).

-spec get_db_specific_offset(integer(), integer()) -> iolist().
get_db_specific_offset(Offset, Limit) ->
    do_get_db_specific_offset(?ODBC_TYPE, integer_to_list(Offset), integer_to_list(Limit)).


do_get_db_specific_limits(mssql, LimitStr) ->
    {"", "TOP " ++ LimitStr};
do_get_db_specific_limits(_, LimitStr) ->
    {"LIMIT " ++ LimitStr, ""}.

do_get_db_specific_offset(mssql, Offset, Limit) ->
    [" OFFSET ", Offset, " ROWS"
    " FETCH NEXT ", Limit, " ROWS ONLY"];
do_get_db_specific_offset(_, Offset, _Limit) ->
    [" OFFSET ", Offset].
