-module(mod_inbox_rdbms_async).
-compile([inline_list_funcs]).

-include("mod_inbox.hrl").

-behaviour(mod_inbox_backend).

-define(PER_MESSAGE_FLUSH_TIME, [?MODULE, per_message_flush_time]).
-define(FLUSH_TIME, [?MODULE, flush_time]).
-define(MESSAGES_PER_ENTRY, [?MODULE, messages_per_entry]).

-type task() ::
    {set_inbox, mod_inbox:entry_key(), content(), pos_integer(), id(), integer()} |
    {set_inbox_incr_unread, mod_inbox:entry_key(), content(), id(), integer(), Incrs :: pos_integer()} |
    {remove_inbox_row, mod_inbox:entry_key()} |
    {reset_unread, mod_inbox:entry_key(), id()}.

%% API
-export([init/2,
         set_inbox/6,
         set_inbox_incr_unread/5,
         reset_unread/3,
         remove_inbox_row/2,
         remove_domain/2,
         clear_inbox/3,
         get_inbox/4,
         get_inbox_unread/2,
         get_entry_properties/2,
         set_entry_properties/3]).
-export([stop/1]).

%% Async callback
-export([flush_inbox/2]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mod_inbox_rdbms:init(HostType, Opts),
    AsyncOpts = prepare_pool_opts(Opts),
    start_pool(HostType, AsyncOpts),
    ensure_metrics(HostType),
    ok.

stop(HostType) ->
    mongoose_async_pools:stop_pool(HostType, inbox).

prepare_pool_opts(Opts) ->
    AsyncOpts = gen_mod:get_opt(async_writer, Opts),
    AsyncOpts1 = AsyncOpts#{flush_extra => AsyncOpts},
    AsyncOpts1#{flush_callback => fun ?MODULE:flush_inbox/2}.

start_pool(HostType, Opts) ->
    catch mongoose_async_pools:start_pool(HostType, inbox, Opts).

ensure_metrics(HostType) ->
    mongoose_metrics:ensure_metric(HostType, ?MESSAGES_PER_ENTRY, histogram),
    mongoose_metrics:ensure_metric(HostType, ?PER_MESSAGE_FLUSH_TIME, histogram),
    mongoose_metrics:ensure_metric(HostType, ?FLUSH_TIME, histogram).

-spec flush_inbox([task()], mongoose_async_pools:pool_extra()) -> ok | {error, term()}.
flush_inbox(Acc, Extra = #{host_type := HostType, queue_length := MessageCount}) ->
    {FlushTime, Result} = timer:tc(fun do_flush_inbox/2, [Acc, Extra]),
    mongoose_metrics:update(HostType, ?PER_MESSAGE_FLUSH_TIME, round(FlushTime / MessageCount)),
    mongoose_metrics:update(HostType, ?FLUSH_TIME, FlushTime),
    Result.

-spec do_flush_inbox([task()], mongoose_async_pools:pool_extra()) -> ok | {error, term()}.
do_flush_inbox(Acc, #{host_type := HostType, queue_length := MessageCount}) ->
    Entries = classify_by_entry(Acc),
    mongoose_metrics:update(HostType, ?MESSAGES_PER_ENTRY, MessageCount / maps:size(Entries)),
    AccEntries = accumulate_entries(Entries),
    Results = apply_entries(AccEntries, HostType),
    verify_results(Results).

%% Here tasks are ordered from oldest to newest
-spec classify_by_entry([task()]) ->
    #{mod_inbox:entry_key() => [task()]}.
classify_by_entry(Tasks) ->
    lists:foldl(fun(Task, Classification) ->
                        Entry = element(2, Task),
                        TaskForEntry = maps:get(Entry, Classification, []),
                        Classification#{Entry => [Task | TaskForEntry]}
                end, #{}, Tasks).

-spec accumulate_entries(#{mod_inbox:entry_key() => [task()]}) -> [task()].
accumulate_entries(Entries) ->
    maps:fold(fun(_Entry, Tasks, Acc) -> [ aggregate_tasks(Tasks) | Acc] end, [], Entries).

%% Here tasks are ordered from newest to oldest, so we reverse the lists again
aggregate_tasks(Tasks) ->
    lists:foldl(fun aggregate/2, undefined, lists:reverse(Tasks)).

-spec apply_entries([task()], mongooseim:host_type()) -> [mod_inbox:write_res()].
apply_entries(Tasks, HostType) ->
    [ flush_one(HostType, Task) || Task <- Tasks].

-spec verify_results([mod_inbox:write_res()]) -> ok | {error, term()}.
verify_results(Results) ->
    case lists:filter(fun(Res) -> Res =/= ok end, Results) of
        [] -> ok;
        Errors -> {error, Errors}
    end.

%% async callbacks
-spec set_inbox(mongooseim:host_type(), mod_inbox:entry_key(),
                content(), Count :: integer(), id(), Timestamp :: integer()) ->
    mod_inbox:write_res().
set_inbox(HostType, Entry, Content, Count, MsgId, Timestamp) ->
    Params = {set_inbox, Entry, Content, Count, MsgId, Timestamp},
    mongoose_async_pools:put_task(HostType, inbox, Entry, Params).

-spec set_inbox_incr_unread(mongooseim:host_type(), mod_inbox:entry_key(),
                            Content :: binary(), MsgId :: binary(), Timestamp :: integer()) ->
    mod_inbox:count_res().
set_inbox_incr_unread(HostType, Entry, Content, MsgId, Timestamp) ->
    Params = {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, 1},
    mongoose_async_pools:put_task(HostType, inbox, Entry, Params).

-spec reset_unread(mongooseim:host_type(), mod_inbox:entry_key(), binary() | undefined) ->
    mod_inbox:write_res().
reset_unread(HostType, Entry, MsgId) ->
    Params = {reset_unread, Entry, MsgId},
    mongoose_async_pools:put_task(HostType, inbox, Entry, Params).

-spec remove_inbox_row(mongooseim:host_type(), mod_inbox:entry_key()) -> mod_inbox:write_res().
remove_inbox_row(HostType, Entry) ->
    Params = {remove_inbox_row, Entry},
    mongoose_async_pools:put_task(HostType, inbox, Entry, Params).

%% synchronous callbacks
-spec get_inbox(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_inbox:get_inbox_params()) ->
    get_inbox_res().
get_inbox(HostType, LUser, LServer, Params) ->
    mod_inbox_rdbms:get_inbox(HostType, LUser, LServer, Params).

-spec get_inbox_unread(mongooseim:host_type(), mod_inbox:entry_key()) ->
    {ok, integer()}.
get_inbox_unread(HostType, Entry) ->
    mod_inbox_rdbms:get_inbox_unread(HostType, Entry).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    mod_inbox_rdbms:remove_domain(HostType, LServer).

-spec clear_inbox(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    mod_inbox:write_res().
clear_inbox(HostType, LUser, LServer) ->
    mod_inbox_rdbms:clear_inbox(HostType, LUser, LServer).

-spec get_entry_properties(mongooseim:host_type(), mod_inbox:entry_key()) ->
    entry_properties() | nil().
get_entry_properties(HostType, Entry) ->
    mod_inbox_rdbms:get_entry_properties(HostType, Entry).

-spec set_entry_properties(mongooseim:host_type(), mod_inbox:entry_key(), entry_properties()) ->
    entry_properties() | {error, binary()}.
set_entry_properties(HostType, Entry, Properties) ->
    mod_inbox_rdbms:set_entry_properties(HostType, Entry, Properties).

%% Internal functions

flush_one(HostType, {set_inbox, Entry, Content, Count, MsgId, Timestamp}) ->
    mod_inbox_rdbms:set_inbox(HostType, Entry, Content, Count, MsgId, Timestamp);
flush_one(HostType, {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs}) ->
    mod_inbox_rdbms:set_inbox_incr_unread(HostType, Entry, Content, MsgId, Timestamp, Incrs);
flush_one(HostType, {remove_inbox_row, Entry}) ->
    mod_inbox_rdbms:remove_inbox_row(HostType, Entry);
flush_one(HostType, {reset_unread, Entry, MsgId}) ->
    mod_inbox_rdbms:reset_unread(HostType, Entry, MsgId).

-spec aggregate(NewTask :: task(), CurrentlyAccumulatedTask :: task()) -> FinalTask :: term().

%%% First task being processed, just take that one
aggregate(Task, undefined) ->
    Task;

%%% if new task is remove_inbox, ignore all previous requests and just remove
aggregate({remove_inbox_row, Entry}, _) ->
    {remove_inbox_row, Entry};

%%% if the last task was remove_row, this task should now only be an insert
aggregate({reset_unread, _, _}, {remove_inbox_row, _} = OldTask) ->
    OldTask;
aggregate({set_inbox, _, _, _, _, _} = NewTask, {remove_inbox_row, _}) ->
    NewTask;
aggregate({set_inbox_incr_unread, _, _, _, _, _} = NewTask, {remove_inbox_row, _}) ->
    NewTask;

%%% If the last task was a reset_unread,
%   we prefer explicit resets,
%   then adhoc newer resets,
%   then we accumulate inserts
%% an undefined means an explicit request to reset, it has priority
aggregate({reset_unread, _, undefined} = NewTask, {reset_unread, _, _}) ->
    NewTask;
%% an undefined means an explicit request to reset, it has priority
aggregate({reset_unread, _, _}, {reset_unread, _, undefined} = OldTask) ->
    OldTask;
%% both are adhoc, we prefer the newer
aggregate({reset_unread, _, _} = NewTask, {reset_unread, _, _}) ->
    NewTask;
aggregate({set_inbox, _, _, _, _, _} = NewTask, {reset_unread, _, _}) ->
    NewTask;
%% Here `Count` becomes an absolute value instead of an increment
aggregate({set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs}, {reset_unread, _, _}) ->
    {set_inbox, Entry, Content, Incrs, MsgId, Timestamp};

%%% If the last task was a set_inbox
%% Reset is an explicit reset-to-zero, so do reset the counter
aggregate({reset_unread, _, undefined}, {set_inbox, Entry, Content, _, MsgId, Timestamp}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp};
%% Reset refers to that same set_inbox
aggregate({reset_unread, _, MsgId}, {set_inbox, Entry, Content, _, MsgId, Timestamp}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp};
%% Reset refers to some other set_inbox
aggregate({reset_unread, _, _}, {set_inbox, _, _, _, _, _} = OldTask) ->
    OldTask;
aggregate({set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs},
          {set_inbox, _, _, Count, _, _, _}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Count + Incrs};

%%% If the last task was a set_inbox_incr_unread
% we're resetting on this message:
aggregate({reset_unread, _, MsgId}, {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, _}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp};
aggregate({reset_unread, _, _}, {set_inbox_incr_unread, _, _, _, _, _} = OldTask) ->
    OldTask;
% prefer newest row, but accumulate increment
aggregate({set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs1},
          {set_inbox_incr_unread, _, _, _, _, Incrs2}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs1 + Incrs2};

aggregate({set_inbox, _, _, _, MsgId, _} = NewTask, {set_inbox_incr_unread, _, _, MsgId, _, _}) ->
    NewTask;

aggregate(NewTask, _OldTask) ->
    NewTask.
