%%%----------------------------------------------------------------------
%%% File    : rdbms_queries.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : RDBMS queries dependind on back-end
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
         get_all_roster_namespaces/2,
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
         set_vcard/27,
         get_vcard/3,
         search_vcard/3,
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

-export([join/2,
         prepare_upsert/6,
         execute_upsert/5]).

%% We have only two compile time options for db queries:
%%-define(generic, true).
%%-define(mssql, true).
-ifndef(mssql).
-undef(generic).
-define(generic, true).
-endif.

-define(RDBMS_TYPE, (mongoose_rdbms_type:get())).

-include("mongoose.hrl").

%
%% -----------------
%% Common functions

join([], _Sep) ->
    [];
join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

%% -----------------
%% Generic queries

get_db_type() ->
    ?RDBMS_TYPE.


%% Safe atomic update.
-spec update_t(Table, Fields, Vals, Where) -> term() when
    Table :: binary(),
    Fields :: list(binary()),
    Vals :: list(mongoose_rdbms:escaped_value()),
    Where :: mongoose_rdbms:sql_query_part().
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun(A, B) -> [A, "=", mongoose_rdbms:use_escaped(B)] end,
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
               <<") values (">>, join_escaped(Vals), ");"])
    end.

join_escaped(Vals) ->
    join([mongoose_rdbms:use_escaped(X) || X <- Vals], ", ").

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
               <<") values (">>, join_escaped(Vals), ");"])
    end.

odds([X, _|T]) -> [X|odds(T)];
odds([])      -> [].

evens([_, X|T]) -> [X|evens(T)];
evens([])      -> [].

join_field_and_values([Field, Val|FieldsVals]) ->
    %% Append a field-value pair
    [Field, $=, mongoose_rdbms:use_escaped(Val) | join_field_and_values_1(FieldsVals)].

join_field_and_values_1([Field, Val|FieldsVals]) ->
    %% Append a separater and a field-value pair
    [$,, $ , Field, $=, mongoose_rdbms:use_escaped(Val) | join_field_and_values_1(FieldsVals)];
join_field_and_values_1([]) ->
    [].


update(LServer, Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun(A, B) -> [A, "=", mongoose_rdbms:use_escaped(B)] end,
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
               <<") values (">>, join_escaped(Vals), ");"])
    end.

-spec execute_upsert(Host :: mongoose_rdbms:server(),
                     Name :: atom(),
                     InsertParams :: [any()],
                     UpdateParams :: [any()],
                     UniqueKeyValues :: [any()]) -> mongoose_rdbms:query_result().
execute_upsert(Host, Name, InsertParams, UpdateParams, UniqueKeyValues) ->
    case {mongoose_rdbms:db_engine(Host), mongoose_rdbms_type:get()} of
        {mysql, _} ->
            mongoose_rdbms:execute(Host, Name, InsertParams ++ UpdateParams);
        {pgsql, _} ->
            mongoose_rdbms:execute(Host, Name, InsertParams ++ UpdateParams);
        {odbc, mssql} ->
            mongoose_rdbms:execute(Host, Name, UniqueKeyValues ++ InsertParams ++ UpdateParams);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

%% @doc
%% This functions prepares query for inserting a row or updating it if already exists
-spec prepare_upsert(Host :: mongoose_rdbms:server(),
                     QueryName :: atom(),
                     TableName :: atom(),
                     InsertFields :: [binary()],
                     UpdateFields :: [binary()],
                     UniqueKeyFields :: [binary()]) ->
    {ok, QueryName :: atom()} | {error, already_exists}.
prepare_upsert(Host, Name, Table, InsertFields, UpdateFields, UniqueKeyFields) ->
    SQL = upsert_query(Host, Table, InsertFields, UpdateFields, UniqueKeyFields),
    Query = iolist_to_binary(SQL),
    ?DEBUG("event=upsert_query, query=~s", [Query]),
    Fields = prepared_upsert_fields(InsertFields, UpdateFields, UniqueKeyFields),
    mongoose_rdbms:prepare(Name, Table, Fields, Query).

prepared_upsert_fields(InsertFields, UpdateFields, UniqueKeyFields) ->
    case mongoose_rdbms_type:get() of
        mssql ->
            UniqueKeyFields ++ InsertFields ++ UpdateFields;
        _ -> InsertFields ++ UpdateFields
    end.

upsert_query(Host, Table, InsertFields, UpdateFields, UniqueKeyFields) ->
    case {mongoose_rdbms:db_engine(Host), mongoose_rdbms_type:get()} of
        {mysql, _} ->
            upsert_mysql_query(Table, InsertFields, UpdateFields, UniqueKeyFields);
        {pgsql, _} ->
            upsert_pgsql_query(Table, InsertFields, UpdateFields, UniqueKeyFields);
        {odbc, mssql} ->
            upsert_mssql_query(Table, InsertFields, UpdateFields, UniqueKeyFields);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

mysql_and_pgsql_insert(Table, Columns) ->
    JoinedFields = join(Columns, <<", ">>),
    Placeholders = lists:duplicate(length(Columns), $?),
    ["INSERT INTO ", atom_to_binary(Table, utf8), " (",
     JoinedFields,
     ") VALUES (",
     join(Placeholders, ", "),
     ")"].

upsert_mysql_query(Table, InsertFields, UpdateFields, _) ->
    Insert = mysql_and_pgsql_insert(Table, InsertFields),
    OnConflict = mysql_on_conflict(UpdateFields),
    [Insert, OnConflict].

upsert_pgsql_query(Table, InsertFields, UpdateFields, UniqueKeyFields) ->
    Insert = mysql_and_pgsql_insert(Table, InsertFields),
    OnConflict = pgsql_on_conflict(UpdateFields, UniqueKeyFields),
    [Insert, OnConflict].

mysql_on_conflict(UpdateFields) ->
    [" ON DUPLICATE KEY UPDATE ",
     update_fields_on_conflict(UpdateFields)].

pgsql_on_conflict(UpdateFields, UniqueKeyFields) ->
    JoinedKeys = join(UniqueKeyFields, ", "),
    [" ON CONFLICT (", JoinedKeys, ")"
     " DO UPDATE SET ",
     update_fields_on_conflict(UpdateFields)].

update_fields_on_conflict(UpdateFields) ->
    FieldsWithPlaceHolders = [[Field, " = ?"] || Field <- UpdateFields],
    join(FieldsWithPlaceHolders, ", ").

upsert_mssql_query(Table, InsertFields, UpdateFields, UniqueKeyFields) ->
    UniqueKeysInSelect = [[" ? AS ", Key] || Key <- UniqueKeyFields],
    UniqueConstraint = [["target.", Key, " = source.", Key] || Key <- UniqueKeyFields],
    JoinedInsertFields = join(InsertFields, ", "),
    Placeholders = lists:duplicate(length(InsertFields), $?),
    ["MERGE INTO ", atom_to_binary(Table, utf8), " WITH (SERIALIZABLE) as target"
     " USING (SELECT ", join(UniqueKeysInSelect, ", "), ")"
            " AS source (", join(UniqueKeyFields, ", "), ")"
        " ON (", join(UniqueConstraint, " AND "), ")"
     " WHEN NOT MATCHED THEN INSERT"
       " (", JoinedInsertFields, ")"
         " VALUES (", join(Placeholders, ", "), ")"
     " WHEN MATCHED THEN UPDATE"
       " SET ", update_fields_on_conflict(UpdateFields),";"].

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the mongoose_rdbms module to this one (rdbms_queries)
sql_transaction(LServer, F) ->
    mongoose_rdbms:sql_transaction(LServer, F).

begin_trans() ->
    begin_trans(?RDBMS_TYPE).

begin_trans(mssql) ->
    rdbms_queries_mssql:begin_trans();
begin_trans(_) ->
    [<<"BEGIN;">>].


get_last(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select seconds, state from last "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

select_last(LServer, TStamp, Comparator) ->
    mongoose_rdbms:sql_query(
        LServer,
        [<<"select username, seconds, state from last "
           "where seconds ">>, Comparator, " ",
         mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(TStamp)), ";"]).

set_last_t(LServer, Username, Seconds, State) ->
    update(LServer, "last", ["username", "seconds", "state"],
           [Username, Seconds, State],
           [<<"username=">>, mongoose_rdbms:use_escaped_string(Username)]).

del_last(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from last where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

get_password(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select password, pass_details from users "
       "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<";">>]).

set_password_t(LServer, Username, #{password := Pass, details := PassDetails}) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
              update_t(<<"users">>, [<<"password">>, <<"pass_details">>],
                       [Pass, PassDetails],
                       [<<"username=">>, mongoose_rdbms:use_escaped_string(Username)])
      end);
set_password_t(LServer, Username, Pass) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
              update_t(<<"users">>, [<<"username">>, <<"password">>],
                       [Username, Pass],
                       [<<"username=">>, mongoose_rdbms:use_escaped_string(Username)])
      end).

add_user(LServer, Username, #{password := Pass, details := PassDetails}) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"insert into users(username, password, pass_details) "
       "values (">>, mongoose_rdbms:use_escaped_string(Username),
           <<", ">>, mongoose_rdbms:use_escaped_string(Pass),
           <<", ">>, mongoose_rdbms:use_escaped_string(PassDetails), <<");">>]);
add_user(LServer, Username, Pass) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"insert into users(username, password) "
         "values (">>, mongoose_rdbms:use_escaped_string(Username),
           <<", ">>, mongoose_rdbms:use_escaped_string(Pass), <<");">>]).

del_user(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from users where username=">>,
           mongoose_rdbms:use_escaped_string(Username), ";"]).

del_user_return_password(_LServer, Username, Pass) ->
    P = mongoose_rdbms:sql_query_t(
          [<<"select password from users where username=">>,
           mongoose_rdbms:use_escaped_string(Username), ";"]),
    mongoose_rdbms:sql_query_t([<<"delete from users "
           "where username=">>, mongoose_rdbms:use_escaped_string(Username),
           <<" and password=">>, mongoose_rdbms:use_escaped_string(Pass), ";"]),
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
         "where username like ">>, mongoose_rdbms:use_escaped_like(mongoose_rdbms:escape_like_prefix(Prefix)), <<" "
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
         "where username like ">>, mongoose_rdbms:use_escaped_like(mongoose_rdbms:escape_like_prefix(Prefix)), ""]);
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
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

get_roster_jid_groups(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select jid, grp from rostergroups "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

get_roster_groups(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(
      [<<"select grp from rostergroups "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
         "and jid=">>, mongoose_rdbms:use_escaped_string(SJID), ";"]).

del_user_roster_t(LServer, Username) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
              mongoose_rdbms:sql_query_t(
                [<<"delete from rosterusers "
                   "where username=">>, mongoose_rdbms:use_escaped_string(Username)]),
              mongoose_rdbms:sql_query_t(
                [<<"delete from rostergroups "
                   "where username=">>, mongoose_rdbms:use_escaped_string(Username)])
      end).

q_get_roster(Username, SJID) ->
    [<<"select username, jid, nick, subscription, "
    "ask, askmessage, server, subscribe, type from rosterusers "
    "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
    "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)].

get_roster_by_jid(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query(LServer, q_get_roster(Username, SJID)).

get_roster_by_jid_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_roster(Username, SJID)).

q_get_rostergroup(Username, SJID) ->
    [<<"select grp from rostergroups "
    "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
    "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)].

get_rostergroup_by_jid(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query(LServer, q_get_rostergroup(Username, SJID)).

get_rostergroup_by_jid_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_rostergroup(Username, SJID)).

del_roster(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(
      [<<"delete from rosterusers "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
         "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]),
    mongoose_rdbms:sql_query_t(
      [<<"delete from rostergroups "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
         "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]).

del_roster_sql(Username, SJID) ->
    [[<<"delete from rosterusers "
        "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
        "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)],
     [<<"delete from rostergroups "
        "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
        "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]].

update_roster(_LServer, Username, SJID, ItemVals, ItemGroups) ->
    update_t(<<"rosterusers">>,
             [<<"username">>, <<"jid">>, <<"nick">>, <<"subscription">>, <<"ask">>,
              <<"askmessage">>, <<"server">>, <<"subscribe">>, <<"type">>],
             ItemVals,
             [<<"username=">>, mongoose_rdbms:use_escaped_string(Username),
              <<" and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]),
    mongoose_rdbms:sql_query_t(
      [<<"delete from rostergroups "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
         "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]),
    lists:foreach(fun(ItemGroup) ->
                          mongoose_rdbms:sql_query_t(
                            [<<"insert into rostergroups(username, jid, grp) "
                               "values (">>, join_escaped(ItemGroup), ");"])
                  end,
                  ItemGroups).

update_roster_sql(Username, SJID, ItemVals, ItemGroups) ->
    [[<<"delete from rosterusers "
        "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
        "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)],
     [<<"insert into rosterusers("
        "username, jid, nick, "
        "subscription, ask, askmessage, "
        "server, subscribe, type) "
        " values (">>, join_escaped(ItemVals), ");"],
     [<<"delete from rostergroups "
        "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
        "and jid=">>, mongoose_rdbms:use_escaped_string(SJID), ";"]] ++
        [[<<"insert into rostergroups(username, jid, grp) "
            "values (">>, join_escaped(ItemGroup), ");"] ||
            ItemGroup <- ItemGroups].

roster_subscribe(_LServer, Username, SJID, ItemVals) ->
    update_t(<<"rosterusers">>,
             [<<"username">>, <<"jid">>, <<"nick">>, <<"subscription">>, <<"ask">>,
              <<"askmessage">>, <<"server">>, <<"subscribe">>, <<"type">>],
             ItemVals,
             [<<"username=">>, mongoose_rdbms:use_escaped_string(Username),
              <<" and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]).

q_get_subscription(Username, SJID) ->
    [<<"select subscription from rosterusers "
    "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
    "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)].

get_subscription(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query( LServer, q_get_subscription(Username, SJID)).

get_subscription_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_subscription(Username, SJID)).

set_private_data(_LServer, Username, LXMLNS, SData) ->
    update_t(<<"private_storage">>,
             [<<"username">>, <<"namespace">>, <<"data">>],
             [Username, LXMLNS, SData],
             [<<"username=">>, mongoose_rdbms:use_escaped_string(Username),
              <<" and namespace=">>, mongoose_rdbms:use_escaped_string(LXMLNS)]).

set_private_data_sql(Username, LXMLNS, SData) ->
    [[<<"delete from private_storage "
        "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" and "
        "namespace=">>, mongoose_rdbms:use_escaped_string(LXMLNS), ";"],
     [<<"insert into private_storage(username, namespace, data) "
        "values (">>, mongoose_rdbms:use_escaped_string(Username), ", ",
                      mongoose_rdbms:use_escaped_string(LXMLNS), ", ",
                      mongoose_rdbms:use_escaped_string(SData), ");"]].

get_all_roster_namespaces(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select namespace from private_storage where username=">>,
       mongoose_rdbms:use_escaped_string(Username), " ;"]).

get_private_data(LServer, Username, LXMLNS) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select data from private_storage "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" and "
         "namespace=">>, mongoose_rdbms:use_escaped_string(LXMLNS)]).

multi_get_private_data(LServer, Username, LXMLNSs) when length(LXMLNSs) > 0 ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select namespace, data from private_storage "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" and "
         "namespace IN (">>, join_escaped(LXMLNSs), ");"]).

%% set_private_data for multiple queries using MySQL's specific syntax.
multi_set_private_data(LServer, Username, SNS2XML) when length(SNS2XML) > 0 ->
    Rows = [private_data_row(Username, NS, Data) || {NS, Data} <- SNS2XML],
    mongoose_rdbms:sql_query(
      LServer,
      [<<"replace into private_storage (username, namespace, data) "
         "values ">>, join(Rows, ", ")]).

private_data_row(Username, NS, Data) ->
    [<<"(">>, mongoose_rdbms:use_escaped_string(Username),
     <<", ">>, mongoose_rdbms:use_escaped_string(NS),
     <<", ">>, mongoose_rdbms:use_escaped_string(Data), <<")">>].

del_user_private_storage(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from private_storage where username=">>,
           mongoose_rdbms:use_escaped_string(Username)]).

set_vcard(LServer,
          SLServer, SLUsername,
          SBDay, SCTRY, SEMail, SFN, SFamily, SGiven,
          SLBDay, SLCTRY, SLEMail, SLFN, SLFamily, SLGiven, SLLocality,
          SLMiddle, SLNickname, SLOrgName, SLOrgUnit, SLocality, SMiddle,
          SNickname, SOrgName, SOrgUnit, SVCARD, SUsername) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
        update_t(<<"vcard">>,
          [<<"username">>, <<"server">>, <<"vcard">>],
          [SLUsername, SLServer, SVCARD],
          [<<"username=">>, mongoose_rdbms:use_escaped_string(SLUsername),
           <<" and server=">>, mongoose_rdbms:use_escaped_string(SLServer)]),
        update_set_t(<<"vcard_search">>,
          [<<"username">>, SUsername,
           <<"lusername">>, SLUsername,
           <<"server">>, SLServer,
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
          [<<"lusername=">>, mongoose_rdbms:use_escaped_string(SLUsername),
           <<" and server=">>, mongoose_rdbms:use_escaped_string(SLServer)])
      end).

get_vcard(LServer, Username, SLServer) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select vcard from vcard "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username),
       <<" and server=">>, mongoose_rdbms:use_escaped_string(SLServer)]).


search_vcard(LServer, RestrictionSQL, Limit) ->
    Type = ?RDBMS_TYPE,
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
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

get_default_privacy_list_t(Username) ->
    mongoose_rdbms:sql_query_t(
      [<<"select name from privacy_default_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

count_privacy_lists(LServer) ->
    mongoose_rdbms:sql_query(LServer, [<<"select count(*) from privacy_list;">>]).

clear_privacy_lists(LServer) ->
    mongoose_rdbms:sql_query(LServer, [<<"delete from privacy_list;">>]).

get_privacy_list_names(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select name from privacy_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

get_privacy_list_names_t(Username) ->
    mongoose_rdbms:sql_query_t(
      [<<"select name from privacy_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

get_privacy_list_id(LServer, Username, SName) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select id from privacy_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username),
       <<" and name=">>, mongoose_rdbms:use_escaped_string(SName)]).

get_privacy_list_id_t(Username, SName) ->
    mongoose_rdbms:sql_query_t(
      [<<"select id from privacy_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username),
       <<" and name=">>, mongoose_rdbms:use_escaped_string(SName)]).

get_privacy_list_data(LServer, Username, SName) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select t, value, action, ord, match_all, match_iq, "
         "match_message, match_presence_in, match_presence_out "
         "from privacy_list_data "
         "where id = (select id from privacy_list where "
         "username=">>, mongoose_rdbms:use_escaped_string(Username),
       <<" and name=">>, mongoose_rdbms:use_escaped_string(SName), <<") "
         "order by ord;">>]).

get_privacy_list_data_by_id(LServer, ID) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select t, value, action, ord, match_all, match_iq, "
         "match_message, match_presence_in, match_presence_out "
         "from privacy_list_data "
         "where id=">>, mongoose_rdbms:use_escaped_integer(ID), <<" order by ord;">>]).

set_default_privacy_list(Username, SName) ->
    update_t(<<"privacy_default_list">>, [<<"username">>, <<"name">>],
             [Username, SName],
             [<<"username=">>, mongoose_rdbms:use_escaped_string(Username)]).

unset_default_privacy_list(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_default_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

remove_privacy_list(Username, SName) ->
    mongoose_rdbms:sql_query_t(
      [<<"delete from privacy_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username),
        " and name=", mongoose_rdbms:use_escaped_string(SName)]).

add_privacy_list(Username, SName) ->
    mongoose_rdbms:sql_query_t(
      [<<"insert into privacy_list(username, name) "
         "values (">>, mongoose_rdbms:use_escaped_string(Username),
                 ", ", mongoose_rdbms:use_escaped_string(SName), ");"]).

-spec set_privacy_list(mongoose_rdbms:escaped_integer(),
                       list(list(mongoose_rdbms:escaped_value()))) -> ok.
set_privacy_list(ID, RItems) ->
    mongoose_rdbms:sql_query_t(
      [<<"delete from privacy_list_data "
         "where id=">>, mongoose_rdbms:use_escaped_integer(ID), ";"]),
    lists:foreach(fun(Items) ->
                          mongoose_rdbms:sql_query_t(
                            [<<"insert into privacy_list_data("
                               "id, t, value, action, ord, match_all, match_iq, "
                               "match_message, match_presence_in, "
                               "match_presence_out "
                               ") "
                               "values (">>, mongoose_rdbms:use_escaped_integer(ID), ", ",
                                 join_escaped(Items), ");"])
                  end, RItems).

del_privacy_lists(LServer, _Server, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_list_data where id in "
         "( select id from privacy_list as pl where pl.username=">>,
           mongoose_rdbms:use_escaped_string(Username), <<";">>]),
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_list "
          "where username=">>, mongoose_rdbms:use_escaped_string(Username)]),
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from privacy_default_list "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

%% Count number of records in a table given a where clause
count_records_where(LServer, Table, WhereClause) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from ">>, Table, " ", WhereClause, ";"]).


get_roster_version(LServer, LUser) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select version from roster_version "
         "where username=">>, mongoose_rdbms:use_escaped_string(LUser)]).

set_roster_version(LUser, Version) ->
    update_t(
      <<"roster_version">>,
      [<<"username">>, <<"version">>],
      [LUser, Version],
      [<<"username = ">>, mongoose_rdbms:use_escaped_string(LUser)]).


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
            "where server = ">>, mongoose_rdbms:use_escaped_string(SServer), <<" and "
                  "username = ">>, mongoose_rdbms:use_escaped_string(SUser), <<" and "
                  "(expire is null or expire > ">>, mongoose_rdbms:use_escaped_integer(STimeStamp), <<") "
             "ORDER BY timestamp">>].

delete_offline_messages_sql(SUser, SServer) ->
    [<<"delete from offline_message "
            "where server = ">>, mongoose_rdbms:use_escaped_string(SServer), <<" and "
                  "username = ">>, mongoose_rdbms:use_escaped_string(SUser)].

remove_old_offline_messages(LServer, STimeStamp) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from offline_message where timestamp < ">>,
       mongoose_rdbms:use_escaped_integer(STimeStamp)]).

remove_expired_offline_messages(LServer, STimeStamp) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from offline_message "
            "where expire is not null and expire < ">>,
       mongoose_rdbms:use_escaped_integer(STimeStamp)]).

remove_offline_messages(LServer, SUser, SServer) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"delete from offline_message "
            "where server = ">>, mongoose_rdbms:use_escaped_string(SServer), <<" and "
                  "username = ">>, mongoose_rdbms:use_escaped_string(SUser)]).

-spec prepare_offline_message(SUser, SServer, STimeStamp, SExpire, SFrom, SPacket) ->
    mongoose_rdbms:sql_query_part() when
      SUser :: mongoose_rdbms:escaped_string(),
      SServer :: mongoose_rdbms:escaped_string(),
      STimeStamp :: mongoose_rdbms:escaped_timestamp(),
      SExpire :: mongoose_rdbms:escaped_timestamp() | mongoose_rdbms:escaped_null(),
      SFrom :: mongoose_rdbms:escaped_string(),
      SPacket :: mongoose_rdbms:escaped_string().
prepare_offline_message(SUser, SServer, STimeStamp, SExpire, SFrom, SPacket) ->
    [<<"(">>,  mongoose_rdbms:use_escaped_string(SUser),
     <<", ">>, mongoose_rdbms:use_escaped_string(SServer),
     <<", ">>, mongoose_rdbms:use_escaped_integer(STimeStamp),
     <<", ">>, mongoose_rdbms:use_escaped(SExpire),
     <<", ">>, mongoose_rdbms:use_escaped_string(SFrom),
     <<", ">>, mongoose_rdbms:use_escaped_string(SPacket),
     <<")">>].

push_offline_messages(LServer, Rows) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"INSERT INTO offline_message "
              "(username, server, timestamp, expire, from_jid, packet) "
            "VALUES ">>, join(Rows, ", ")]).


count_offline_messages(LServer, SUser, SServer, Limit) ->
    count_offline_messages(?RDBMS_TYPE, LServer, SUser, SServer, Limit).

count_offline_messages(mssql, LServer, SUser, SServer, Limit) ->
    rdbms_queries_mssql:count_offline_messages(LServer, SUser, SServer, Limit);
count_offline_messages(_, LServer, SUser, SServer, Limit) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from offline_message "
            "where server = ">>, mongoose_rdbms:use_escaped_string(SServer), <<" and "
                  "username = ">>, mongoose_rdbms:use_escaped_string(SUser), <<" "
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
    do_get_db_specific_limits(?RDBMS_TYPE, LimitStr).

-spec get_db_specific_offset(integer(), integer()) -> iolist().
get_db_specific_offset(Offset, Limit) ->
    do_get_db_specific_offset(?RDBMS_TYPE, integer_to_list(Offset), integer_to_list(Limit)).


do_get_db_specific_limits(mssql, LimitStr) ->
    {"", "TOP " ++ LimitStr};
do_get_db_specific_limits(_, LimitStr) ->
    {"LIMIT " ++ LimitStr, ""}.

do_get_db_specific_offset(mssql, Offset, Limit) ->
    [" OFFSET ", Offset, " ROWS"
    " FETCH NEXT ", Limit, " ROWS ONLY"];
do_get_db_specific_offset(_, Offset, _Limit) ->
    [" OFFSET ", Offset].
