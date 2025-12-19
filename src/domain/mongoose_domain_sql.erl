-module(mongoose_domain_sql).

-export([start/1]).

-export([insert_domain/2,
         delete_domain/2,
         set_domain_for_deletion/2,
         set_status/2]).

-export([select_domain_admin/1,
         set_domain_admin/2,
         delete_domain_admin/1]).

-export([select_domain/1,
         select_all_domains/0,
         get_minmax_event_id/0,
         count_events_between_ids/2,
         get_event_ids_between/2,
         select_from/2,
         select_updates_between/2,
         get_enabled_dynamic/0,
         delete_events_older_than/1,
         insert_dummy_events/1]).

%% interfaces only for integration tests
-export([prepare_test_queries/0,
         erase_database/1,
         insert_full_event/2,
         insert_domain_settings_without_event/2]).

-ignore_xref([erase_database/1, prepare_test_queries/0, get_enabled_dynamic/0,
              insert_full_event/2, insert_domain_settings_without_event/2]).

-import(mongoose_rdbms, [prepare/4]).
-include("mongoose_logger.hrl").

-type event_id() :: non_neg_integer().
-type domain() :: jid:lserver().
-type row() :: {event_id(), domain(), mongooseim:host_type() | null}.
-export_type([row/0]).

start(_) ->
    LimitSQL = rdbms_queries:limit(),
    Enabled = integer_to_binary(status_to_int(enabled)),
    %% Settings
    prepare(domain_insert_settings, domain_settings, [domain, host_type],
            <<"INSERT INTO domain_settings (domain, host_type) "
              "VALUES (?, ?)">>),
    prepare(domain_update_settings_status, domain_settings,
            [status, domain],
            <<"UPDATE domain_settings "
              "SET status = ? "
              "WHERE domain = ?">>),
    prepare(domain_delete_settings, domain_settings, [domain],
            <<"DELETE FROM domain_settings WHERE domain = ?">>),
    prepare(domain_select, domain_settings, [domain],
            <<"SELECT host_type, status "
              "FROM domain_settings WHERE domain = ?">>),
    prepare(domain_select_from, domain_settings,
            [id, limit],
            <<"SELECT id, domain, host_type "
              " FROM domain_settings "
              " WHERE id > ? AND status = ", Enabled/binary, " "
              " ORDER BY id",
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
                   "domain_settings.status = ", Enabled/binary, ") "
              " WHERE domain_events.id >= ? AND domain_events.id <= ? "
              " ORDER BY domain_events.id ">>),
    prepare(domain_select_all, domain_settings, [],
            <<"SELECT domain, host_type, status FROM domain_settings order by domain">>),
    %% Admins
    prepare(domain_insert_admin, domain_admins, [domain, pass_details],
            <<"INSERT INTO domain_admins (domain, pass_details) VALUES (?, ?)">>),
    prepare(domain_update_admin, domain_admins, [pass_details, domain],
            <<"UPDATE domain_admins"
              " SET pass_details = ? "
              " WHERE domain = ?">>),
    prepare(domain_delete_admin, domain_admins, [domain],
            <<"DELETE FROM domain_admins WHERE domain = ?">>),
    prepare(domain_select_admin, domain_admins, [domain],
            <<"SELECT domain, pass_details"
              " FROM domain_admins WHERE domain = ?">>),
    ok.

prepare_test_queries() ->
    Enabled = integer_to_binary(status_to_int(enabled)),
    prepare(domain_erase_admins, domain_admins, [],
            <<"DELETE FROM domain_admins">>),
    prepare(domain_erase_settings, domain_settings, [],
            <<"DELETE FROM domain_settings">>),
    prepare(domain_erase_events, domain_events, [],
            <<"DELETE FROM domain_events">>),
    prepare(domain_get_status_dynamic, domain_settings, [],
            <<"SELECT "
              " domain, host_type "
              " FROM domain_settings "
              " WHERE status = ", Enabled/binary, " "
              " ORDER BY id">>),
    prepare(domain_events_get_all, domain_events, [],
            <<"SELECT id, domain FROM domain_events ORDER BY id">>).

%% ----------------------------------------------------------------------------
%% API
insert_domain(Domain, HostType) ->
    transaction(fun(Pool) ->
            case select_domain(Domain) of
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

select_all_domains() ->
    Pool = get_db_pool(),
    {selected, Rows} = execute_successfully(Pool, domain_select_all, []),
    [#{domain => Domain,
       host_type => HostType,
       status => int_to_status(mongoose_rdbms:result_to_integer(Status))} || {Domain, HostType, Status} <- Rows].

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

set_domain_for_deletion(Domain, HostType) ->
    transaction(fun(Pool) ->
            case select_domain(Domain) of
                {ok, #{host_type := HT}} when HT =:= HostType ->
                    {updated, 1} = set_domain_for_deletion_settings(Pool, Domain),
                    insert_domain_event(Pool, Domain),
                    ok;
                {ok, _} ->
                    {error, wrong_host_type};
                {error, not_found} ->
                    {error, not_found}
            end
        end).

select_domain_admin(Domain) ->
    Pool = get_db_pool(),
    case execute_successfully(Pool, domain_select_admin, [Domain]) of
        {selected, []} ->
            {error, not_found};
        {selected, [Row]} ->
            {ok, Row}
    end.

set_domain_admin(Domain, Password) ->
    Iterations = mongoose_scram:iterations(),
    HashMap = mongoose_scram:password_to_scram_sha(Password, Iterations, sha512),
    PassDetails = mongoose_scram:serialize(HashMap),
    transaction(fun(Pool) ->
                    case select_domain_admin(Domain) of
                        {ok, _} ->
                            update_domain_admin(Pool, Domain, PassDetails),
                            ok;
                        {error, not_found} ->
                            insert_domain_admin(Pool, Domain, PassDetails),
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
                            {error, not_found}
                    end
                end).

insert_domain_admin(Pool, Domain, PassDetails) ->
    execute_successfully(Pool, domain_insert_admin, [Domain, PassDetails]).

update_domain_admin(Pool, Domain, PassDetails) ->
    execute_successfully(Pool, domain_update_admin, [PassDetails, Domain]).

delete_domain_admin(Pool, Domain) ->
    execute_successfully(Pool, domain_delete_admin, [Domain]).

%% Returns smallest id first
select_from(FromId, Limit) ->
    Pool = get_db_pool(),
    Args =  [FromId, Limit],
    {selected, Rows} = execute_successfully(Pool, domain_select_from, Args),
    Rows.

get_enabled_dynamic() ->
    Pool = get_db_pool(),
    prepare_test_queries(),
    {selected, Rows} = execute_successfully(Pool, domain_get_status_dynamic, []),
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

insert_dummy_events(EventIds) ->
    insert_full_events([{EventId, <<>>} || EventId <- EventIds]).

insert_full_event(EventId, Domain) ->
    [Res] = insert_full_events([{EventId, Domain}]),
    Res.

insert_full_events(EventIdDomain) ->
    Pool = get_db_pool(),
    [catch execute_successfully(Pool, domain_insert_full_event, [EventId, Domain])
     || {EventId, Domain} <- EventIdDomain].

%% ----------------------------------------------------------------------------
%% For testing

erase_database(Pool) ->
    execute_successfully(Pool, domain_erase_events, []),
    execute_successfully(Pool, domain_erase_settings, []),
    execute_successfully(Pool, domain_erase_admins, []).

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

set_domain_for_deletion_settings(Pool, Domain) ->
    ExtStatus = status_to_int(deleting),
    execute_successfully(Pool, domain_update_settings_status, [ExtStatus, Domain]).

-spec set_status(domain(), enabled | disabled) -> ok | {error, term()}.
set_status(Domain, Status) ->
    transaction(fun(Pool) ->
            case select_domain(Domain) of
                {error, Reason} ->
                    {error, Reason};
                {ok, #{status := CurrentStatus, host_type := HostType}} ->
                    case mongoose_domain_core:is_host_type_allowed(HostType) of
                        false ->
                            {error, unknown_host_type};
                        true when deleting =:= CurrentStatus ->
                            {error, domain_deleted};
                        true when Status =:= CurrentStatus ->
                            ok;
                        true ->
                            update_domain_enabled(Pool, Domain, Status),
                            insert_domain_event(Pool, Domain),
                            ok
                    end
            end
        end).

update_domain_enabled(Pool, Domain, Status) ->
    ExtStatus = status_to_int(Status),
    execute_successfully(Pool, domain_update_settings_status, [ExtStatus, Domain]).

row_to_map({HostType, Status}) ->
    IntStatus = mongoose_rdbms:result_to_integer(Status),
    #{host_type => HostType, status => int_to_status(IntStatus)}.

-spec int_to_status(0..2) -> mongoose_domain_api:status().
int_to_status(0) -> disabled;
int_to_status(1) -> enabled;
int_to_status(2) -> deleting.

-spec status_to_int(mongoose_domain_api:status()) -> 0..2.
status_to_int(disabled) -> 0;
status_to_int(enabled) -> 1;
status_to_int(deleting) -> 2.

get_db_pool() ->
    mongoose_config:get_opt([services, service_domain_db, db_pool]).

transaction(F) ->
    transaction(F, 3, []).

transaction(_F, 0, Errors) ->
    {error, {db_error, Errors}};
transaction(F, Tries, Errors) when Tries > 0 ->
    Pool = get_db_pool(),
    Result = rdbms_queries:sql_transaction(Pool, fun() -> F(Pool) end),
    case Result of
        {aborted, _} -> %% Restart any rolled back transaction
            put(debug_last_transaction_error, Result),
            timer:sleep(100), %% Small break before retry
            transaction(F, Tries - 1, [Result|Errors]);
        _ ->
            erase(debug_last_transaction_error),
            simple_result(Result)
    end.

simple_result({atomic, Result}) -> Result;
simple_result(Other) -> {error, {db_error, Other}}.

execute_successfully(Pool, StatementName, Args) ->
    {Time, Res} = timer:tc(fun() -> mongoose_rdbms:execute_successfully(Pool, StatementName, Args) end),
    %% Convert args to tuple, because Erlang formats list as a string for domain_event_ids_between
    ?LOG_DEBUG(#{what => domain_sql_execute,
                 statement_name => StatementName, args => list_to_tuple(Args), result => Res,
                 duration => round(Time / 1000)}),
    Res.
