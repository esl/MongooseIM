-module(mongoose_domain_sql).

-export([start/1]).

-export([insert_domain/2,
         delete_domain/2,
         disable_domain/1,
         enable_domain/1]).

-export([select_domain/1,
         get_max_event_id_or_set_dummy/0,
         select_from/2,
         select_updates_from/2,
         delete_events_older_than/1]).

%% interfaces only for integration tests
-export([prepare_test_queries/0,
         get_min_event_id/0,
         erase_database/0]).

-ignore_xref([erase_database/0, get_min_event_id/0, prepare_test_queries/0]).

-include("mongoose_logger.hrl").

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

start(_Opts) ->
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(),
    Pool = get_db_pool(),
    True = case mongoose_rdbms:db_engine(Pool) of
               pgsql -> <<"true">>;
               _ -> <<"1">>
           end,
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
    prepare(domain_events_max, domain_events, [],
            <<"SELECT MAX(id) FROM domain_events">>),
    prepare(domain_events_delete_older_than, domain_events, [id],
            <<"DELETE FROM domain_events WHERE id < ?">>),
    prepare(domain_select_events_from, domain_events,
            rdbms_queries:add_limit_arg(limit, [id]),
            <<"SELECT ", LimitMSSQL/binary,
              " domain_events.id, domain_events.domain, domain_settings.host_type "
              " FROM domain_events "
              " LEFT JOIN domain_settings ON "
                  "(domain_settings.domain = domain_events.domain AND "
                   "domain_settings.enabled = ", True/binary, ") "
              " WHERE domain_events.id >= ? "
              " ORDER BY domain_events.id ",
              LimitSQL/binary>>),
    ok.

prepare_test_queries() ->
    prepare(domain_erase_settings, domain_settings, [],
            <<"DELETE FROM domain_settings">>),
    prepare(domain_erase_events, domain_events, [],
            <<"DELETE FROM domain_events">>),
    prepare(domain_events_min, domain_events, [],
            <<"SELECT MIN(id) FROM domain_events">>).

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

erase_database() ->
    Pool = get_db_pool(),
    execute_successfully(Pool, domain_erase_events, []),
    execute_successfully(Pool, domain_erase_settings, []).

get_min_event_id() ->
    %% use this function for integration tests only
    Pool = get_db_pool(),
    Selected = execute_successfully(Pool, domain_events_min, []),
    mongoose_rdbms:selected_to_integer(Selected).

%% Returns smallest id first
select_from(FromId, Limit) ->
    Pool = get_db_pool(),
    Args = rdbms_queries:add_limit_arg(Limit, [FromId]),
    {selected, Rows} = execute_successfully(Pool, domain_select_from, Args),
    Rows.

select_updates_from(FromId, Limit) ->
    select_updates_from(FromId, Limit, 2).

select_updates_from(_FromId, _Limit, 0) ->
    %% this should never happen, but just in case returning
    %% an empty list here and trying to restart service
    service_domain_db:restart(),
    [];
select_updates_from(FromId, Limit, NoOfRetries) ->
    Pool = get_db_pool(),
    Args = rdbms_queries:add_limit_arg(Limit, [FromId]),
    case execute_successfully(Pool, domain_select_events_from, Args) of
        {selected, []} ->
            %% get MaxID, this inserts the dummy record
            %% in the events table if required.
            MaxId = get_max_event_id_or_set_dummy(),
            if
                MaxId > FromId ->
                    select_updates_from(FromId, Limit, NoOfRetries - 1);
                true ->
                    %% This must never happen though, unless the events table
                    %% is modified externally. this is critical error!
                    ?LOG_ERROR(#{what => select_updates_from_failed, limit => Limit,
                                 from_id => FromId, max_id => MaxId}),
                    service_domain_db:restart(),
                    []
            end;
        {selected, Rows} -> Rows
    end.

get_max_event_id_or_set_dummy() ->
    get_max_event_id_or_set_dummy(2).

get_max_event_id_or_set_dummy(0) ->
    %% this should never happen, but just in case
    %% returning 0 here and trying to restart service
    service_domain_db:restart(),
    0;
get_max_event_id_or_set_dummy(NoOfRetries) ->
    Pool = get_db_pool(),
    case execute_successfully(Pool, domain_events_max, []) of
        {selected, [{null}]} ->
            %% ensure that we have at least one record
            %% in the table, even if it's dummy one.
            insert_domain_event(Pool, <<"dummy.test.domain">>),
            get_max_event_id_or_set_dummy(NoOfRetries - 1);
        NonNullSelection ->
            mongoose_rdbms:selected_to_integer(NonNullSelection)
    end.

delete_events_older_than(Id) ->
    transaction(fun(Pool) ->
            MaxId = get_max_event_id_or_set_dummy(),
            if
                MaxId < Id ->
                    %% Removal would erase all the events, which we don't want.
                    %% We want to keep at least one event.
                    %% This must never happen though, unless the events table
                    %% is modified externally. this is critical error!
                    ?LOG_ERROR(#{what => domain_delete_events_older_than_failed,
                                 max_id => MaxId, older_than_id => Id}),
                    service_domain_db:restart(),
                    skipped;
                true ->
                    execute_successfully(Pool, domain_events_delete_older_than, [Id])
            end
        end).
%% ----------------------------------------------------------------------------
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
    hd(ejabberd_config:get_global_option(hosts)).

transaction(F) ->
    Pool = get_db_pool(),
    Result = rdbms_queries:sql_transaction(Pool, fun() -> F(Pool) end),
    simple_result(Result).

simple_result({atomic, Result}) -> Result;
simple_result(Other) -> {error, {db_error, Other}}.
