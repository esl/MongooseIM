-module(mongoose_domain_gaps).
-export([init/0]).
-export([handle_gaps/1]).

-include("mongoose_logger.hrl").

-define(TABLE, ?MODULE).

init() ->
    ets:new(?TABLE, [set, named_table, public]).

handle_gaps(Gaps) ->
    remember_gaps(Gaps),
    Now = erlang:monotonic_time(seconds),
    MaxTimeToWait = 30, %% seconds
    case [Gap || Gap <- Gaps, not is_expired_gap(Gap, Now, MaxTimeToWait)] of
        [] ->
            skip;
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
