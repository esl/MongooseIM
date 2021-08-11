-module(mongoose_domain_gaps).
-export([init/0]).
-export([check_for_gaps/1]).

-include("mongoose_logger.hrl").

-define(TABLE, ?MODULE).

init() ->
    ets:new(?TABLE, [set, named_table, public]).

-spec check_for_gaps([tuple()]) -> ok | restart | wait.
check_for_gaps(Rows) ->
    Ids = rows_to_ids(Rows),
    case find_gaps(Ids, 10000) of
        too_many_gaps ->
            ?LOG_ERROR(#{what => domain_check_for_gaps_failed,
                         reason => too_many_gaps,
                         rows => Rows}),
            restart;
        [] -> %% No gaps, good
            ok;
        Gaps ->
            handle_gaps(Gaps)
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

handle_gaps(Gaps) ->
    remember_gaps(Gaps),
    Now = erlang:monotonic_time(seconds),
    MaxTimeToWait = 30, %% seconds
    case [Gap || Gap <- Gaps, not is_expired_gap(Gap, Now, MaxTimeToWait)] of
        [] ->
            ok; %% Skip any gaps checking
        [_|_] = WaitingGaps ->
            ?LOG_WARNING(#{what => wait_for_gaps, gaps => WaitingGaps}),
            wait
    end.

remember_gaps(Gaps) ->
    Now = erlang:monotonic_time(seconds),
    [ets:insert_new(?TABLE, {Gap, Now}) || Gap <- Gaps],
    ok.

is_expired_gap(Gap, Now, MaxTimeToWait) ->
    case ets:lookup(?TABLE, Gap) of
        [] ->
            ?LOG_WARNING(#{what => gap_not_found, gap => Gap}),
            true;
        [{Gap, Inserted}] ->
            (Now - Inserted) > MaxTimeToWait
    end.
