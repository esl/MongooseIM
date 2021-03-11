-module(mongoose_domain_loader).
-export([load_data_from_base/2]).
-export([check_for_updates/2]).

-include("mongoose_logger.hrl").

load_data_from_base(FromNum, PageSize) ->
    %% Crash on init if select fails.
    case mongoose_domain_sql:select_from(FromNum, PageSize) of
        [] -> ok;
        Rows ->
            PageMaxId = row_to_id(lists:last(Rows)),
            insert_rows_to_core(Rows),
            load_data_from_base(PageMaxId, PageSize)
    end.

check_for_updates(FromNum, PageSize) ->
    check_if_from_num_still_relevant(FromNum),
    %% Ordered by the earlist events first
    try mongoose_domain_sql:select_updates_from(FromNum, PageSize) of
        [] -> FromNum;
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
            FromNum
    end.

%% Be aware that for this check to work, the cleaner should keep at least
%% one record in domain_events table.
check_if_from_num_still_relevant(0) ->
    ok;
check_if_from_num_still_relevant(FromNum) ->
    Min = mongoose_domain_sql:get_min_event_id(),
    if Min =:= 0 ->
            %% Nothing to do, there were no updates done ever in the DB
            ok;
       Min > FromNum ->
           %% Looks like this node has no DB connection for a long time.
           %% But the event log in the DB has been truncated by some other node
           %% meanwhile. We have to load the whole set of data from DB.
           ?LOG_ERROR(#{what => events_log_out_of_sync,
                        text => <<"DB domain log had some updates to domains removed, "
                                  " which we have not applied yet. Have to crash.">>,
                        min_db => Min,
                        from_num => FromNum}),
           error(check_if_from_num_still_relevant_failed);
       true ->
           ok
    end.

apply_changes([Row|Rows]) ->
    apply_change(Row),
    apply_changes(Rows);
apply_changes([]) ->
    ok.

apply_change({_Id, Domain, null}) ->
    %% Missing record in domain_settings table.
    %% Or enabled field is false.
    mongoose_domain_core:remove_unlocked(Domain);
apply_change({_Id, Domain, HostType}) ->
    %% Inserted or updated record
    mongoose_domain_core:insert_unlocked(Domain, HostType).

insert_rows_to_core([Row|Rows]) ->
    insert_row_to_core(Row),
    insert_rows_to_core(Rows);
insert_rows_to_core([]) ->
    ok.

insert_row_to_core({_Id, Domain, HostType}) ->
    mongoose_domain_core:insert_unlocked(Domain, HostType).

row_to_id({Id, _Domain, _HostType}) ->
    Id.
