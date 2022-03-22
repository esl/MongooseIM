-module(mod_inbox_rdbms_async).

-include("mod_inbox.hrl").
-include("mongoose_logger.hrl").

-behaviour(mod_inbox_backend).
-behaviour(mongoose_aggregator_worker).

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
         get_full_entry/2,
         get_entry_properties/2,
         set_entry_properties/3]).
-export([stop/1]).

%% Worker callbacks
-export([request/2, aggregate/3, verify/3]).

%% Initialisation
-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    AsyncOpts = prepare_pool_opts(Opts),
    mod_inbox_rdbms:init(HostType, Opts),
    start_pool(HostType, AsyncOpts),
    ok.

stop(HostType) ->
    mongoose_async_pools:stop_pool(HostType, inbox).

prepare_pool_opts(#{async_writer := AsyncOpts}) ->
    AsyncOpts#{pool_type => aggregate,
               request_callback => fun ?MODULE:request/2,
               aggregate_callback => fun ?MODULE:aggregate/3,
               verify_callback => fun ?MODULE:verify/3}.

-spec start_pool(mongooseim:host_type(), mongoose_async_pools:pool_opts()) -> term().
start_pool(HostType, Opts) ->
    mongoose_async_pools:start_pool(HostType, inbox, Opts).

%% Worker callbacks
-spec request(task(), mongoose_async_pools:pool_extra()) -> reference().
request(Task, _Extra = #{host_type := HostType}) ->
    request_one(HostType, Task).

request_one(HostType, {set_inbox, {LUser, LServer, LToBareJid}, Content, Count, MsgId, Timestamp}) ->
    InsertParams = [LUser, LServer, LToBareJid, Content, Count, MsgId, Timestamp],
    UpdateParams = [Content, Count, MsgId, Timestamp, false],
    UniqueKeyValues  = [LUser, LServer, LToBareJid],
    rdbms_queries:request_upsert(HostType, inbox_upsert, InsertParams, UpdateParams, UniqueKeyValues);

request_one(HostType, {set_inbox_incr_unread, {LUser, LServer, LToBareJid}, Content, MsgId, Timestamp, Incrs}) ->
    InsertParams = [LUser, LServer, LToBareJid, Content, Incrs, MsgId, Timestamp],
    UpdateParams = [Content, MsgId, Timestamp, false, Incrs],
    UniqueKeyValues  = [LUser, LServer, LToBareJid],
    rdbms_queries:request_upsert(HostType, inbox_upsert_incr_unread, InsertParams, UpdateParams, UniqueKeyValues);

request_one(HostType, {remove_inbox_row, {LUser, LServer, LToBareJid}}) ->
    mongoose_rdbms:execute_request(HostType, inbox_delete_row, [LUser, LServer, LToBareJid]);

request_one(HostType, {reset_unread, {LUser, LServer, LToBareJid}, undefined}) ->
    mongoose_rdbms:execute_request(HostType, inbox_reset_unread, [LUser, LServer, LToBareJid]);
request_one(HostType, {reset_unread, {LUser, LServer, LToBareJid}, MsgId}) ->
    mongoose_rdbms:execute_request(HostType, inbox_reset_unread_msg, [LUser, LServer, LToBareJid, MsgId]).

-spec aggregate(task(), task(), mongoose_async_pools:pool_extra()) -> {ok, task()}.
aggregate(Current, NewTask, _Extra) ->
    {ok, aggregate(Current, NewTask)}.

-spec verify(term(), task(), mongoose_async_pools:pool_extra()) -> ok.
verify(Answer, InboxTask, _Extra) ->
    case mod_inbox_rdbms:check_result(Answer) of
        {error, Reason} ->
            {LU, LS, LRem} = element(2, InboxTask),
            ?LOG_WARNING(#{what => inbox_process_message_failed, reason => Reason,
                           from_jid => jid:to_binary({LU, LS}), to_jid => LRem});
        _ -> ok
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
    [mod_inbox:inbox_res()].
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

-spec get_full_entry(mongooseim:host_type(), mod_inbox:entry_key()) ->
    inbox_res() | nil().
get_full_entry(HostType, Entry) ->
    mod_inbox_rdbms:get_full_entry(HostType, Entry).

-spec get_entry_properties(mongooseim:host_type(), mod_inbox:entry_key()) ->
    entry_properties() | nil().
get_entry_properties(HostType, Entry) ->
    mod_inbox_rdbms:get_entry_properties(HostType, Entry).

-spec set_entry_properties(mongooseim:host_type(), mod_inbox:entry_key(), entry_properties()) ->
    entry_properties() | {error, binary()}.
set_entry_properties(HostType, Entry, Properties) ->
    mod_inbox_rdbms:set_entry_properties(HostType, Entry, Properties).

-spec aggregate(CurrentlyAccumulatedTask :: task(), NewTask :: task()) -> FinalTask :: task().
%%% First task being processed, just take that one
aggregate(undefined, Task) ->
    Task;

%%% if new task is remove_inbox, ignore all previous requests and just remove
aggregate(_, {remove_inbox_row, Entry}) ->
    {remove_inbox_row, Entry};

%%% if the last task was remove_row, this task should now only be an insert
aggregate({remove_inbox_row, _} = OldTask, {reset_unread, _, _}) ->
    OldTask;
aggregate({remove_inbox_row, _}, {set_inbox, _, _, _, _, _} = NewTask) ->
    NewTask;
aggregate({remove_inbox_row, _}, {set_inbox_incr_unread, _, _, _, _, _} = NewTask) ->
    NewTask;

%%% If the last task was a reset_unread,
%   we prefer explicit resets,
%   then adhoc newer resets,
%   then we accumulate inserts
%% an undefined means an explicit request to reset, it has priority
aggregate({reset_unread, _, _}, {reset_unread, _, undefined} = NewTask) ->
    NewTask;
%% an undefined means an explicit request to reset, it has priority
aggregate({reset_unread, _, undefined} = OldTask, {reset_unread, _, _}) ->
    OldTask;
%% both are adhoc, we prefer the newer
aggregate({reset_unread, _, _}, {reset_unread, _, _} = NewTask) ->
    NewTask;
aggregate({reset_unread, _, _}, {set_inbox, _, _, _, _, _} = NewTask) ->
    NewTask;
%% Here `Count` becomes an absolute value instead of an increment
aggregate({reset_unread, _, _}, {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs}) ->
    {set_inbox, Entry, Content, Incrs, MsgId, Timestamp};

%%% If the last task was a set_inbox
%% Reset is an explicit reset-to-zero, so do reset the counter
aggregate({set_inbox, Entry, Content, _, MsgId, Timestamp}, {reset_unread, _, undefined}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp};
%% Reset refers to that same set_inbox
aggregate({set_inbox, Entry, Content, _, MsgId, Timestamp}, {reset_unread, _, MsgId}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp};
%% Reset refers to some other set_inbox
aggregate({set_inbox, _, _, _, _, _} = OldTask, {reset_unread, _, _}) ->
    OldTask;
aggregate({set_inbox, _, _, Count, _, _, _},
          {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Count + Incrs};

%%% If the last task was a set_inbox_incr_unread
% we're resetting on this message:
aggregate({set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, _}, {reset_unread, _, MsgId}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp};
aggregate({set_inbox_incr_unread, _, _, _, _, _} = OldTask, {reset_unread, _, _}) ->
    OldTask;
% prefer newest row, but accumulate increment
aggregate({set_inbox_incr_unread, _, _, _, _, Incrs2},
          {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs1}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs1 + Incrs2};

aggregate({set_inbox_incr_unread, _, _, MsgId, _, _}, {set_inbox, _, _, _, MsgId, _} = NewTask) ->
    NewTask;

aggregate(_OldTask, NewTask) ->
    NewTask.
