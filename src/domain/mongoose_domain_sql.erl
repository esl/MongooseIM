-module(mongoose_domain_sql).

-export([start/1]).

-export([insert_domain/2,
         delete_domain/2,
         disable_domain/1,
         enable_domain/1]).

-export([select_domain_admin/1,
         set_domain_admin/2,
         delete_domain_admin/1]).

-export([select_domain/1,
         get_minmax_event_id/0,
         count_events_between_ids/2,
         get_event_ids_between/2,
         select_from/2,
         select_updates_between/2,
         get_enabled_dynamic/0,
         delete_events_older_than/1,
         insert_dummy_event/1]).

%% interfaces only for integration tests
-export([prepare_test_queries/1,
         erase_database/1,
         insert_full_event/2,
         insert_domain_settings_without_event/2]).

-ignore_xref([erase_database/1, prepare_test_queries/1, get_enabled_dynamic/0,
              insert_full_event/2, insert_domain_settings_without_event/2]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

-type event_id() :: non_neg_integer().
-type domain() :: binary().
-type row() :: {event_id(), domain(), mongooseim:host_type() | null}.
-export_type([row/0]).

start(#{db_pool := Pool}) ->
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(),
    True = sql_true(Pool),
    %% Settings
    prepare(domain_insert_settings, domain_settings, [domain, host_type],
            <<"INSERT INTO domain_settings (domain, host_type) "
              "VALUES (?, ?)">>),
    prepare(domain_update_settings_enabled, domain_settings,
            [enabled, domain],
            <<"UPDATE domain_settings "
              "SET enabled = ? "
              "WHERE domain = ?">>),
    prepare(domain_delete_settings, domain_settings, [domain],
            <<"DELETE FROM domain_settings WHERE domain = ?">>),
    prepare(domain_select, domain_settings, [domain],
            <<"SELECT host_type, enabled "
              "FROM domain_settings WHERE domain = ?">>),
    prepare(domain_select_from, domain_settings,
            rdbms_queries:add_limit_arg(limit, [id]),
            <<"SELECT ", LimitMSSQL/binary,
              " id, domain, host_type "
              " FROM domain_settings "
              " WHERE id > ? AND enabled = ", True/binary, " "
              " ORDER BY id ",
              LimitSQL/binary>>),
    %% Events
    prepare(domain_insert_event, domain_events, [domain],
            <<"INSERT INTO domain_events (domain) VALUES (?)">>),
    prepare(domain_insert_full_event, domain_events, [id, domain],
            <<"INSERT INTO domain_events (id, domain) VALUES (?, ?)">>),
    prepare(domain_events_minmax, domain_events, [],
            <<"SELECT MIN(id), MAX(id) FROM domain_events">>),
    prepare(domain_count_event_between, domain_events, [id, id],
            <<"SELECT COUNT(*) FROM domain_events  WHERE id >= ? AND id <= ?">>),
    prepare(domain_event_ids_between, domain_events, [id, id],
            <<"SELECT id FROM domain_events  WHERE id >= ? AND id <= ? ORDER BY id">>),
    prepare(domain_events_delete_older_than, domain_events, [id],
            <<"DELETE FROM domain_events WHERE id < ?">>),
    prepare(domain_select_events_between, domain_events, [id, id],
            <<"SELECT "
              " domain_events.id, domain_events.domain, domain_settings.host_type "
              " FROM domain_events "
              " LEFT JOIN domain_settings ON "
                  "(domain_settings.domain = domain_events.domain AND "
                   "domain_settings.enabled = ", True/binary, ") "
              " WHERE domain_events.id >= ? AND domain_events.id <= ? "
              " ORDER BY domain_events.id ">>),
    %% Admins
    prepare(domain_insert_admin, domain_admin, [domain, password],
            <<"INSERT INTO domain_admin (domain, password) VALUES (?, ?)">>),
    prepare(domain_update_admin, domain_admin, [password, domain],
            <<"UPDATE domain_admin"
              " SET password = ? "
              " WHERE domain = ?">>),
    prepare(domain_delete_admin, domain_admin, [domain],
            <<"DELETE FROM domain_admin WHERE domain = ?">>),
    prepare(domain_select_admin, domain_admin, [domain],
            <<"SELECT domain, password"
              " FROM domain_admin WHERE domain = ?">>),
    ok.

prepare_test_queries(Pool) ->
    True = sql_true(Pool),
    prepare(domain_erase_settings, domain_settings, [],
            <<"DELETE FROM domain_settings">>),
    prepare(domain_erase_events, domain_events, [],
            <<"DELETE FROM domain_events">>),
    prepare(domain_get_enabled_dynamic, domain_settings, [],
            <<"SELECT "
              " domain, host_type "
              " FROM domain_settings "
              " WHERE enabled = ", True/binary, " "
              " ORDER BY id">>),
    prepare(domain_events_get_all, domain_events, [],
            <<"SELECT id, domain FROM domain_events ORDER BY id">>).

sql_true(Pool) ->
    case mongoose_rdbms:db_engine(Pool) of
        pgsql -> <<"true">>;
        _ -> <<"1">>
    end.

%% ----------------------------------------------------------------------------
%% API
insert_domain(Domain, HostType) ->
    transaction(fun(Pool) ->
            case select_domain(Domain) of
                {ok, #{host_type := HT}} when HT =:= HostType ->
                    ok; %% ignore second call
                {error, not_found} ->
                    insert_domain_settings(Pool, Domain, HostType),
                    insert_domain_event(Pool, Domain),
                    ok;
                {ok, _} ->
                    {error, duplicate}
            end
        end).

select_domain(Domain) ->
    Pool = get_db_pool(),
    case execute_successfully(Pool, domain_select, [Domain]) of
        {selected, []} ->
            {error, not_found};
        {selected, [Row]} ->
             {ok, row_to_map(Row)}
    end.

delete_domain(Domain, HostType) ->
    transaction(fun(Pool) ->
            case select_domain(Domain) of
                {ok, #{host_type := HT}} when HT =:= HostType ->
                    {updated, 1} = delete_domain_settings(Pool, Domain),
                    insert_domain_event(Pool, Domain),
                    ok;
                {ok, _} ->
                    {error, wrong_host_type};
                {error, not_found} ->
                    ok
            end
        end).

disable_domain(Domain) ->
    set_enabled(Domain, false).

enable_domain(Domain) ->
    set_enabled(Domain, true).

select_domain_admin(Domain) ->
    Pool = get_db_pool(),
    case execute_successfully(Pool, domain_select_admin, [Domain]) of
        {selected, []} ->
            {error, not_found};
        {selected, [Row]} ->
             {ok, Row}
    end.

set_domain_admin(Domain, Password) ->
    transaction(fun(Pool) ->
                    case select_domain_admin(Domain) of
                        {ok, _} ->
                            update_domain_admin(Pool, Domain, Password),
                            ok;
                        {error, not_found} ->
                            insert_domain_admin(Pool, Domain, Password),
                            ok
                    end
                end).

delete_domain_admin(Domain) ->
    transaction(fun(Pool) ->
                    case select_domain_admin(Domain) of
                        {ok, _} ->
                            {updated, 1} = delete_domain_admin(Pool, Domain),
                            ok;
                        {error, not_found} ->
                            ok
                    end
                end).

insert_domain_admin(Pool, Domain, Password) ->
    execute_successfully(Pool, domain_insert_admin, [Domain, Password]).

update_domain_admin(Pool, Domain, Password) ->
    execute_successfully(Pool, domain_update_admin, [Domain, Password]).

delete_domain_admin(Pool, Domain) ->
    execute_successfully(Pool, domain_delete_admin, [Domain]).

%% Returns smallest id first
select_from(FromId, Limit) ->
    Pool = get_db_pool(),
    Args = rdbms_queries:add_limit_arg(Limit, [FromId]),
    {selected, Rows} = execute_successfully(Pool, domain_select_from, Args),
    Rows.

get_enabled_dynamic() ->
    Pool = get_db_pool(),
    prepare_test_queries(Pool),
    {selected, Rows} = execute_successfully(Pool, domain_get_enabled_dynamic, []),
    Rows.

%% FromId, ToId are included into the result
-spec select_updates_between(event_id(), event_id()) -> [row()].
select_updates_between(FromId, ToId) ->
    Pool = get_db_pool(),
    Args = [FromId, ToId],
    {selected, Rows} = execute_successfully(Pool, domain_select_events_between, Args),
    Rows.

get_minmax_event_id() ->
    Pool = get_db_pool(),
    {selected, [{Min, Max}]} = execute_successfully(Pool, domain_events_minmax, []),
    case Min of
        null ->
            {null, null};
        _ ->
            {mongoose_rdbms:result_to_integer(Min),
             mongoose_rdbms:result_to_integer(Max)}
    end.

count_events_between_ids(Min, Max) ->
    Pool = get_db_pool(),
    Selected = execute_successfully(Pool, domain_count_event_between, [Min, Max]),
    mongoose_rdbms:selected_to_integer(Selected).

%% Min and Max are included in the result set
get_event_ids_between(Min, Max) ->
    Pool = get_db_pool(),
    {selected, Rows} = execute_successfully(Pool, domain_event_ids_between, [Min, Max]),
    [mongoose_rdbms:result_to_integer(Id) || {Id} <- Rows].

delete_events_older_than(Id) ->
    transaction(fun(Pool) ->
            execute_successfully(Pool, domain_events_delete_older_than, [Id])
        end).

insert_dummy_event(EventId) ->
    insert_full_event(EventId, <<>>).

insert_full_event(EventId, Domain) ->
    case mongoose_rdbms:db_type() of
        mssql ->
            insert_full_event_mssql(EventId, Domain);
        _ ->
            Pool = get_db_pool(),
            execute_successfully(Pool, domain_insert_full_event, [EventId, Domain])
    end.

insert_full_event_mssql(EventId, Domain) ->
    %% MSSQL does not allow to specify ids,
    %% that are supposed to be autoincremented, easily
    %% https://docs.microsoft.com/pl-pl/sql/t-sql/statements/set-identity-insert-transact-sql
    transaction(fun(Pool) ->
            %% This query could not be a prepared query
            %% You will get an error:
            %% "No SQL-driver information available."
            %% when trying to execute
            mongoose_rdbms:sql_query(Pool, <<"SET IDENTITY_INSERT domain_events ON">>),
            try
                execute_successfully(Pool, domain_insert_full_event, [EventId, Domain])
            after
                mongoose_rdbms:sql_query(Pool, <<"SET IDENTITY_INSERT domain_events OFF">>)
            end
        end).

%% ----------------------------------------------------------------------------
%% For testing

erase_database(Pool) ->
    execute_successfully(Pool, domain_erase_events, []),
    execute_successfully(Pool, domain_erase_settings, []).

insert_domain_settings_without_event(Domain, HostType) ->
    Pool = get_db_pool(),
    execute_successfully(Pool, domain_insert_settings, [Domain, HostType]).

%% ----------------------------------------------------------------------------

%% Inserts a new event with an autoincremented EventId.
%% Rows would not appear in the EventId order, RDBMS likes to rearrange them.
%% Example of rearranging:
%% Events with ids [1, 2, 3] could appear as:
%% 1. [1]
%% 2. [1, 3] - at this step record with EventId=2 is not visible yet.
%% 3. [1, 2, 3] - and finally everything is fine.
insert_domain_event(Pool, Domain) ->
    execute_successfully(Pool, domain_insert_event, [Domain]).

insert_domain_settings(Pool, Domain, HostType) ->
    execute_successfully(Pool, domain_insert_settings, [Domain, HostType]).

delete_domain_settings(Pool, Domain) ->
    execute_successfully(Pool, domain_delete_settings, [Domain]).

set_enabled(Domain, Enabled) when is_boolean(Enabled) ->
    transaction(fun(Pool) ->
            case select_domain(Domain) of
                {error, Reason} ->
                    {error, Reason};
                {ok, #{enabled := En, host_type := HostType}} ->
                    case mongoose_domain_core:is_host_type_allowed(HostType) of
                        false ->
                            {error, unknown_host_type};
                        true when Enabled =:= En ->
                            ok;
                        true ->
                            update_domain_enabled(Pool, Domain, Enabled),
                            insert_domain_event(Pool, Domain),
                            ok
                    end
            end
        end).

update_domain_enabled(Pool, Domain, Enabled) ->
    ExtEnabled = bool_to_ext(Pool, Enabled),
    execute_successfully(Pool, domain_update_settings_enabled, [ExtEnabled, Domain]).

%% MySQL needs booleans as integers
bool_to_ext(Pool, Bool) when is_boolean(Bool) ->
    case mongoose_rdbms:db_engine(Pool) of
        pgsql ->
            Bool;
        _ ->
            bool_to_int(Bool)
    end.

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

row_to_map({HostType, Enabled}) ->
    #{host_type => HostType, enabled => mongoose_rdbms:to_bool(Enabled)}.

get_db_pool() ->
    mongoose_config:get_opt([services, service_domain_db, db_pool]).

transaction(F) ->
    transaction(F, 3, []).

%% MSSQL especially likes to kill a connection deadlocked by tablock connections.
%% But that's fine, we could just restart
%% (there is no logic, that would suffer by a restart of a transaction).
transaction(_F, 0, Errors) ->
    {error, {db_error, Errors}};
transaction(F, Retries, Errors) when Retries > 0 ->
    Pool = get_db_pool(),
    Result = rdbms_queries:sql_transaction(Pool, fun() -> F(Pool) end),
    case Result of
        {aborted, _} -> %% Restart any rolled back transaction
            timer:sleep(100), %% Small break before retry
            transaction(F, Retries - 1, [Result|Errors]);
        _ ->
            simple_result(Result)
    end.

simple_result({atomic, Result}) -> Result;
simple_result(Other) -> {error, {db_error, Other}}.
