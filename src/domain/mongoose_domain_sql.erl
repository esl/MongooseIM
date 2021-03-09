-module(mongoose_domain_sql).

-export([start/1]).

-export([insert_domain/2,
         remove_domain/1,
         disable_domain/1,
         enable_domain/1]).

-export([select_domain/1,
         get_min_event_id/0,
         get_max_event_id/0,
         select_from/2,
         select_updates_from/2]).

-export([prepare_erase/0,
         erase_database/0]).

-import(mongoose_rdbms, [prepare/4]).

start(_Opts) ->
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(),
    %% Settings
    prepare(domain_insert_settings, domain_settings, [domain, host_type],
            <<"INSERT INTO domain_settings (domain, host_type, enabled) "
              "VALUES (?, ?, true)">>),
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
              " WHERE id > ? AND enabled = true "
              " ORDER BY id ",
              LimitSQL/binary>>),
    %% Events
    prepare(domain_insert_event, domain_events, [domain],
            <<"INSERT INTO domain_events (domain) VALUES (?)">>),
    prepare(domain_events_max, domain_events, [],
            <<"SELECT MAX(id) FROM domain_events">>),
    prepare(domain_events_min, domain_events, [],
            <<"SELECT MIN(id) FROM domain_events">>),
    prepare(domain_select_events_from, domain_events,
            rdbms_queries:add_limit_arg(limit, [id]),
            <<"SELECT ", LimitMSSQL/binary,
              " domain_events.id, domain_events.domain, domain_settings.host_type "
              " FROM domain_events "
              " LEFT JOIN domain_settings ON "
                  "(domain_settings.domain = domain_events.domain AND "
                   "domain_settings.enabled = true) "
              " WHERE domain_events.id > ? "
              " ORDER BY domain_events.id ",
              LimitSQL/binary>>),
    ok.

prepare_erase() ->
    prepare(domain_erase_settings, domain_settings, [],
            <<"TRUNCATE TABLE domain_settings">>),
    prepare(domain_erase_events, domain_events, [],
            <<"TRUNCATE TABLE domain_events">>).

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
    case mongoose_rdbms:execute(Pool, domain_select, [Domain]) of
        {selected, []} ->
            {error, not_found};
        {selected, [Row]} ->
             {ok, row_to_map(Row)}
    end.

remove_domain(Domain) ->
    transaction(fun(Pool) ->
            case delete_domain_settings(Pool, Domain) of
                {updated, 1} ->
                    insert_domain_event(Pool, Domain),
                    ok;
                {updated, 0} ->
                    ok
            end
        end).

disable_domain(Domain) ->
    set_enabled(Domain, false).

enable_domain(Domain) ->
    set_enabled(Domain, true).

erase_database() ->
    Pool = get_db_pool(),
    mongoose_rdbms:execute(Pool, domain_erase_events, []),
    mongoose_rdbms:execute(Pool, domain_erase_settings, []).

%% Returns smallest id first
select_from(FromNum, Limit) ->
    Pool = get_db_pool(),
    Args = rdbms_queries:add_limit_arg(Limit, [FromNum]),
    {selected, Rows} = mongoose_rdbms:execute(Pool, domain_select_from, Args),
    Rows.

select_updates_from(FromNum, Limit) ->
    Pool = get_db_pool(),
    Args = rdbms_queries:add_limit_arg(Limit, [FromNum]),
    {selected, Rows} = mongoose_rdbms:execute(Pool, domain_select_events_from, Args),
    Rows.

get_min_event_id() ->
    Pool = get_db_pool(),
    selected_to_int(mongoose_rdbms:execute(Pool, domain_events_min, [])).

get_max_event_id() ->
    Pool = get_db_pool(),
    selected_to_int(mongoose_rdbms:execute(Pool, domain_events_max, [])).

%% ----------------------------------------------------------------------------
insert_domain_event(Pool, Domain) ->
    mongoose_rdbms:execute(Pool, domain_insert_event, [Domain]).

insert_domain_settings(Pool, Domain, HostType) ->
    mongoose_rdbms:execute(Pool, domain_insert_settings, [Domain, HostType]).

delete_domain_settings(Pool, Domain) ->
    mongoose_rdbms:execute(Pool, domain_delete_settings, [Domain]).

set_enabled(Domain, Enabled) when is_boolean(Enabled) ->
    transaction(fun(Pool) ->
            case select_domain(Domain) of
                {error, Reason} ->
                    {error, Reason};
                {ok, #{enabled := X}} when Enabled =:= X ->
                    ok;
                {ok, #{}} ->
                    update_domain_enabled(Pool, Domain, Enabled),
                    insert_domain_event(Pool, Domain),
                    ok
            end
        end).

update_domain_enabled(Pool, Domain, Enabled) ->
    En = bool_to_int(Enabled),
    mongoose_rdbms:execute(Pool, domain_update_settings_enabled, [En, Domain]).

%% MySQL needs booleans as integers
bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

row_to_map({HostType, Enabled}) ->
    #{host_type => HostType, enabled => mongoose_rdbms:to_bool(Enabled)}.

get_db_pool() ->
    hd(ejabberd_config:get_global_option(hosts)).

selected_to_int({selected, [{null}]}) -> 0;
selected_to_int({selected, [{UpdateNum}]}) ->
    mongoose_rdbms:result_to_integer(UpdateNum).

transaction(F) ->
    Pool = get_db_pool(),
    Result = rdbms_queries:sql_transaction(Pool, fun() -> F(Pool) end),
    simple_result(Result).

simple_result({atomic, Result}) -> Result;
simple_result(Other) -> {error, {db_error, Other}}.
