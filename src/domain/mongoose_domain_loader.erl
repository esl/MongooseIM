-module(mongoose_domain_loader).
-export([load_data_from_base/2,
         check_for_updates/2,
         remove_outdated_domains_from_core/0]).

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
              service_domain_db:restart()
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

remove_outdated_domains_from_core() ->
    CurrentSource = self(),
    OutdatedDomains = mongoose_domain_core:get_all_outdated(CurrentSource),
    remove_domains(OutdatedDomains).

check_for_updates(FromId, PageSize) ->
    %% Ordered by the earliest events first
    try mongoose_domain_sql:select_updates_from(FromId, PageSize) of
        [] -> %% Skipping this time
            ?LOG_WARNING(#{what => domain_check_for_updates_skipped,
                           from_id => FromId}),
            FromId;
        Rows ->
            case check_for_gaps(FromId, Rows) of
                ok ->
                    case check_if_id_is_still_relevant(FromId, Rows) of
                        [] -> FromId;
                        RowsToApply ->
                            PageMaxId = row_to_id(lists:last(RowsToApply)),
                            apply_changes(RowsToApply),
                            check_for_updates(PageMaxId, PageSize)
                    end;
                Other ->
                    Other
            end
    catch Class:Reason:StackTrace ->
        %% Don't allow to crash in the critical code,
        %% once we've started.
        ?LOG_ERROR(#{what => domain_check_for_updates_failed,
                     from_id => FromId,
                     class => Class, reason => Reason,
                     stacktrace => StackTrace}),
        FromId
    end.

check_for_gaps(FromId, Rows) ->
    Ids = rows_to_ids(Rows),
    case find_gaps(Ids, 10000) of
        too_many_gaps ->
            ?LOG_ERROR(#{what => domain_check_for_gaps_failed,
                         reason => too_many_gaps,
                         from_id => FromId, rows => Rows}),
            %% Just reread all domains ignoring the event table
            service_domain_db:restart(),
            FromId;
        [] -> %% No gaps, good
            ok;
        Gaps ->
            case mongoose_domain_gaps:handle_gaps(Gaps) of
                wait -> FromId;
                skip -> ok
            end
    end.

rows_to_ids(Rows) ->
    [element(1, Row) || Row <- Rows].

find_gaps(Ids, MaxGaps) ->
    find_gaps(Ids, [], MaxGaps).

find_gaps(_, _Missing, _MaxGaps = 0) ->
    too_many_gaps;
find_gaps([H|T], Missing, MaxGaps) ->
    Expected = H + 1,
    case T of
        [Expected|_] ->
            find_gaps(T, Missing, MaxGaps);
         [_|_] ->
            find_gaps([Expected|T], [Expected|Missing], MaxGaps - 1);
         [] ->
             Missing
     end;
find_gaps([], Missing, _MaxGaps) ->
     Missing.

check_if_id_is_still_relevant(FromId, Rows) ->
    MinId = row_to_id(hd(Rows)),
    %% FromID should be always equal or less than MinID,
    %% see check_for_updates/2 function for more details
    if
        FromId =:= MinId ->
            tl(Rows);
        FromId =:= MinId - 1 ->
            %% looks like someone completely erased the events table
            %% this should not happen, but we are still fine.
            Rows;
        FromId < MinId - 1 ->
            %% Looks like this node has no DB connection for a long time.
            %% But the event log in the DB has been truncated by some other node
            %% meanwhile. We have to load the whole set of data from DB.
            Text = <<"DB domain log had some updates to domains deleted,"
                     " which we have not applied yet. Have to crash.">>,
            ?LOG_CRITICAL(#{what => events_log_out_of_sync,
                            text => Text, min_db => MinId, from_id => FromId}),
            service_domain_db:restart(),
            []
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
    %% Inserted, reinserted (removed & inserted) or enabled record.
    maybe_insert_to_core(Domain, HostType).

insert_rows_to_core(Rows) ->
    lists:foreach(fun insert_row_to_core/1, Rows).

insert_row_to_core({_Id, Domain, HostType}) ->
    maybe_insert_to_core(Domain, HostType).

maybe_insert_to_core(Domain, HostType) ->
    Source = self(),
    case mongoose_domain_core:insert(Domain, HostType, Source) of
        {error, bad_insert} ->
            %% we already have such dynamic domain paired with
            %% another host type, enforce update of the domain.
            mongoose_domain_core:delete(Domain),
            mongoose_domain_core:insert(Domain, HostType, Source);
        _ -> ok %% ignore other errors
    end.

remove_domains(DomainsWithHostTypes) ->
    lists:foreach(fun remove_domain/1, DomainsWithHostTypes).

remove_domain({Domain, _HostType}) ->
    mongoose_domain_core:delete(Domain).

row_to_id({Id, _Domain, _HostType}) ->
    Id.
