%% This module is designed assuming the fact, that records inserted
%% into domains or events table could appear in any order.
%% I.e. events with ids [1, 2, 3] could appear as [1, 3] for a short amount of time.
%% We also assume, event ids are never reused.
-module(mongoose_domain_loader).
-export([initial_load/0,
         check_for_updates/0]).

%% For tests
-export([find_gaps_between/2]).
-ignore_xref([find_gaps_between/2]).

-include("mongoose_logger.hrl").

%% There are two important functions, called by service_domain_db:
%% - initial_load
%% - check_for_updates
-spec initial_load() -> skip | ok.
initial_load() ->
    case mongoose_loader_state:get(undefined) of
        undefined ->
            %% If mongoose_loader_state is undefined,
            %% this means we start for the first time with this core process
            cold_load();
        reset ->
            %% Case when state has been reset without restarting core
            %% For example, when we detected out-of-sync situation
            cold_load(),
            remove_outdated_domains_from_core(),
            ok;
        _ ->
            %% Already synced to some point.
            %% Just read updates from the event table.
            skip
    end.

%% Load from the domain table
cold_load() ->
    %% We assume that to sync successfully we need conditions:
    %% - events table does not contain gaps (if it contain a gap, a record could be missing).
    %% - we have to check the whole event table for gaps.
    %% - we don't care about gaps in the domain table - even if some state
    %%   is not visible yet in the domain table, it would be visible once
    %%   we try to fix the event gap.
    {MinEventId, MaxEventId} = mongoose_domain_sql:get_minmax_event_id(),
    %% It's important to get gaps info before the loading of domains
    Gaps = find_gaps_between(MinEventId, MaxEventId),
    %% Do domain loading from the main domain table
    load_data_from_base(0, 1000),
    %% Try to fix gaps
    fix_gaps(Gaps),
    State = #{min_event_id => MinEventId, max_event_id => MaxEventId},
    mongoose_loader_state:set(State),
    ok.

load_data_from_base(FromId, PageSize) ->
    try
        load_data_from_base_loop(FromId, PageSize, 0)
    catch Class:Reason:Stacktrace ->
              Text = <<"Loading initial domains from RDBMS failed">>,
              ?LOG_CRITICAL(#{what => load_domains_from_base_failed,
                              text => Text,
                              from_id => FromId,
                              class => Class, reason => Reason,
                              stacktrace => Stacktrace}),
              service_domain_db:restart()
    end.

load_data_from_base_loop(FromId, PageSize, Loaded) ->
    %% Crash on init if select fails.
    case mongoose_domain_sql:select_from(FromId, PageSize) of
        [] -> {ok, #{count => Loaded}};
        Rows ->
            PageMaxId = row_to_id(lists:last(Rows)),
            insert_rows_to_core(Rows),
            ?LOG_INFO(#{what => load_data_from_base,
                        count => length(Rows)}),
            load_data_from_base_loop(PageMaxId, PageSize, Loaded + length(Rows))
    end.

remove_outdated_domains_from_core() ->
    CurrentSource = self(),
    OutdatedDomains = mongoose_domain_core:get_all_outdated(CurrentSource),
    remove_domains(OutdatedDomains),
    ?LOG_WARNING(#{what => remove_outdated_domains_from_core,
                   count => length(OutdatedDomains)}),
    ok.

%% If this function fails
%% (for example, if the database is not available at this moment),
%% it is safe to just call it again
-spec check_for_updates() -> empty_db | no_new_updates | ok.
check_for_updates() ->
    MinMax = mongoose_domain_sql:get_minmax_event_id(),
    State = mongoose_loader_state:get(undefined),
    case check_for_updates(MinMax, State) of
        more_pages -> check_for_updates();
        Other -> Other
    end.

check_for_updates({null, null}, _State) ->
    empty_db; %% empty db
check_for_updates({Min, Max},
                  #{min_event_id := Min, max_event_id := Max}) ->
    no_new_updates; %% no new updates
check_for_updates(MinMax = {Min, Max},
                  #{min_event_id := OldMin, max_event_id := OldMax})
  when is_integer(Min), is_integer(Max) ->
    {MinEventId, MaxEventId} = limit_max_id(OldMax, MinMax, 1000),
    check_if_id_is_still_relevant(OldMax, MinEventId),
    NewGapsFromBelow =
        case {OldMin, OldMax} of
            {MinEventId, _} ->
                []; %% MinEventId is the same
            {null, null} ->
                []; %% Starting from an empty table
            _ when MinEventId > OldMin ->
                []; %% someone cleaned event table by removing some events
            _ -> % MinEventId < OldMin
                 %% Race condition detected, check for new gaps
                 lists:seq(MinEventId, OldMin)
           end,
    FromId = case {OldMin, OldMax} of
                 {null, null} -> MinEventId;
                 _ -> OldMax + 1
             end,
    NewGapsFromThePage =
        case OldMax of
            MaxEventId ->
                [];
            _ ->
                Rows = mongoose_domain_sql:select_updates_between(FromId, MaxEventId),
                apply_changes(Rows),
                Ids = rows_to_ids(Rows),
                ids_to_gaps(FromId, MaxEventId, Ids)
        end,
    fix_gaps(NewGapsFromBelow ++ NewGapsFromThePage),
    State2 = #{min_event_id => MinEventId, max_event_id => MaxEventId},
    mongoose_loader_state:set(State2),
    case MaxEventId < Max of
        true -> more_pages;
        false -> ok
    end.

limit_max_id(null, {MinEventId, MaxEventId}, PageSize) ->
    {MinEventId, min(MaxEventId, MinEventId + PageSize)};
limit_max_id(OldMax, {MinEventId, MaxEventId}, PageSize) ->
    {MinEventId, min(MaxEventId, OldMax + PageSize)}.

rows_to_ids(Rows) ->
    [row_to_id(Row) || Row <- Rows].

check_if_id_is_still_relevant(null, _MinEventId) ->
    %% Starting from the empty event table
    ok;
check_if_id_is_still_relevant(OldMax, MinEventId) when OldMax < MinEventId ->
    %% Looks like this node has no DB connection for a long time.
    %% But the event log in the DB has been truncated by some other node
    %% meanwhile. We have to load the whole set of data from DB.
    Text = <<"DB domain log had some updates to domains deleted,"
             " which we have not applied yet. Have to crash.">>,
    ?LOG_CRITICAL(#{what => events_log_out_of_sync,
                    text => Text}),
    service_domain_db:restart();
check_if_id_is_still_relevant(_OldMax, _MinEventId) ->
    ok.

apply_changes([]) ->
    ok;
apply_changes(Rows) ->
    Results = lists:map(fun apply_change/1, Rows),
    ?LOG_INFO(#{what => load_updated_domains,
                skips => count(skip, Results),
                deletes => count(delete, Results),
                inserts => count(insert, Results)}).

count(X, List) ->
    count(X, List, 0).

count(X, [X|T], Count) ->
    count(X, T, Count + 1);
count(X, [_|T], Count) ->
    count(X, T, Count);
count(_, [], Count) ->
    Count.

apply_change({_Id, <<>>, null}) -> %% Skip dummy domain
    skip;
apply_change({_Id, Domain, null}) ->
    %% Removed or disabled domain.
    %% According to the SQL query, the HostType is null when:
    %% - There is no record for the domain in the domain_settings table.
    %% - Or domain_settings.enabled equals false.
    mongoose_domain_core:delete(Domain),
    delete;
apply_change({_Id, Domain, HostType}) ->
    %% Inserted, reinserted (removed & inserted) or enabled record.
    maybe_insert_to_core(Domain, HostType),
    insert.

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
    mongoose_rdbms:result_to_integer(Id).

find_gaps_between(null, null) ->
    [];
find_gaps_between(MinEventId, MaxEventId) when (MaxEventId - MinEventId) < 100 ->
    %% For small sets just grab ids without aggregating
    Ids = mongoose_domain_sql:get_event_ids_between(MinEventId, MaxEventId),
    ids_to_gaps(MinEventId, MaxEventId, Ids);
find_gaps_between(MinEventId, MaxEventId) ->
    Expected = MaxEventId - MinEventId + 1,
    Count = mongoose_domain_sql:count_events_between_ids(MinEventId, MaxEventId),
    case Count of
        Expected -> [];
        _ ->
            %% Recursive binary search using COUNT
            Mid = MinEventId + (MaxEventId - MinEventId) div 2,
            find_gaps_between(MinEventId, Mid) ++ find_gaps_between(Mid + 1, MaxEventId)
    end.

ids_to_gaps(MinEventId, MaxEventId, Ids) ->
    AllIds = lists:seq(MinEventId, MaxEventId),
    %% Find missing ids
    ordsets:subtract(AllIds, Ids).

fix_gaps(Gaps) ->
    %% Retries are only for extra safety, one try would be enough usually
    fix_gaps(Gaps, 3).

fix_gaps([], _Retries) ->
    ok;
fix_gaps(Gaps, Retries) when Retries > 0 ->
    %% A gap is an event id without a record. But it has records above and below.
    %% It occurs pretty rarely.
    %%
    %% There are two reasons for it:
    %% - a transaction is very slow, and not committed yet (but the key is already
    %% autoincremented, so a gap appears).
    %% - a transaction is aborted, so the key would never be used.
    %%
    %% There is no easy way to check for a reason.
    %%
    %% fix_gaps tries to insert_dummy_event with a gap event id.
    %% This makes the state of transaction for gap events obvious:
    %% - if this insert fails, this means the actual record finally
    %%   appears and we can read it.
    %% - if this insert passes - the transaction, that initially used this id has failed.
    %%   (or that transaction would get aborted, which is still fine for a consistent sync.
    %%    The transactions are restarted in mongoose_domain_sql:transaction/1.
    %%    But it should rarely happen)
    %%
    %% RDBMS servers do not overwrite data when INSERT operation is used.
    %% i.e. only one insert for a key succeeded.
    [catch mongoose_domain_sql:insert_dummy_event(Id) || Id <- Gaps],
    %% The gaps should be filled at this point
    Rows = lists:append([mongoose_domain_sql:select_updates_between(Id, Id) || Id <- Gaps]),
    ?LOG_WARNING(#{what => domain_fix_gaps, gaps => Gaps, rows => Rows}),
    apply_changes(lists:usort(Rows)),
    Ids = rows_to_ids(Rows),
    %% We still retry to fill the gaps, in case insert_dummy_event fails
    %% It could fail, if there is a database connectivity issues, for example
    fix_gaps(Gaps -- Ids, Retries - 1).
