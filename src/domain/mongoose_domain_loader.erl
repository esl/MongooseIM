-module(mongoose_domain_loader).
-export([load_data_from_base/2]).
-export([check_for_updates/2]).

-include("mongoose_logger.hrl").

load_data_from_base(FromId, PageSize) ->
    try
        load_data_from_base_loop(FromId, PageSize)
    catch Class:Reason:Stacktrace ->
              Text = <<"Loading initial domains from RDBMS failed">>,
              ?LOG_CRITICAL(#{what => load_domains_from_base_failed,
                              text => Text,
                              from_id => FromId,
                              class => Class, reason => Reason,
                              stacktrace => Stacktrace}),
              mongoose_domain_utils:halt_node(Text)
    end.

load_data_from_base_loop(FromId, PageSize) ->
    %% Crash on init if select fails.
    case mongoose_domain_sql:select_from(FromId, PageSize) of
        [] -> ok;
        Rows ->
            PageMaxId = row_to_id(lists:last(Rows)),
            insert_rows_to_core(Rows),
            load_data_from_base_loop(PageMaxId, PageSize)
    end.

check_for_updates(FromId, PageSize) ->
    check_if_from_num_still_relevant(FromId),
    %% Ordered by the earlist events first
    try mongoose_domain_sql:select_updates_from(FromId, PageSize) of
        [] -> FromId;
        Rows ->
            PageMaxId = row_to_id(lists:last(Rows)),
            apply_changes(Rows),
            check_for_updates(PageMaxId, PageSize)
    catch Class:Reason:StackTrace ->
            %% Don't allow to crash in the critical code,
            %% once we've started.
            ?LOG_ERROR(#{what => domain_check_for_updates_failed,
                         class => Class, reason => Reason,
                         stacktrace => StackTrace}),
            FromId
    end.

%% Be aware that for this check to work, the cleaner should keep at least
%% one record in domain_events table.
check_if_from_num_still_relevant(0) ->
    ok;
check_if_from_num_still_relevant(FromId) ->
    Min = mongoose_domain_sql:get_min_event_id(),
    if Min =:= 0 ->
            %% Nothing to do, there were no updates done ever in the DB
            ok;
       Min > FromId ->
           %% Looks like this node has no DB connection for a long time.
           %% But the event log in the DB has been truncated by some other node
           %% meanwhile. We have to load the whole set of data from DB.
           ?LOG_ERROR(#{what => events_log_out_of_sync,
                        text => <<"DB domain log had some updates to domains deleted, "
                                  " which we have not applied yet. Have to crash.">>,
                        min_db => Min,
                        from_id => FromId}),
           error(check_if_from_num_still_relevant_failed);
       true ->
           ok
    end.

apply_changes(Rows) ->
    lists:foreach(fun apply_change/1, Rows).

apply_change({_Id, Domain, null}) ->
    %% Removed or disabled domain.
    %% According to the SQL query, the HostType is null when:
    %% - There is no record for the domain in the domain_settings table.
    %% - Or domain_settings.enabled equals false.
    mongoose_domain_core:delete(Domain);
apply_change({_Id, Domain, HostType}) ->
    %% Inserted or enabled record.
    mongoose_domain_core:insert(Domain, HostType).

insert_rows_to_core(Rows) ->
    lists:foreach(fun insert_row_to_core/1, Rows).

insert_row_to_core({_Id, Domain, HostType}) ->
    mongoose_domain_core:insert(Domain, HostType).

row_to_id({Id, _Domain, _HostType}) ->
    Id.
