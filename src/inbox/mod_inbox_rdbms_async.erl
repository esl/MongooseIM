-module(mod_inbox_rdbms_async).

-include("mod_inbox.hrl").
-include("mongoose_logger.hrl").

-behaviour(mod_inbox_backend).
-behaviour(mongoose_aggregator_worker).

-type box() :: binary().
-type task() ::
    {set_inbox, mod_inbox:entry_key(), exml:element(), pos_integer(), id(), integer(), box()} |
    {set_inbox_incr_unread, mod_inbox:entry_key(), exml:element(), id(), integer(), pos_integer(), box()} |
    {remove_inbox_row, mod_inbox:entry_key()} |
    {reset_unread, mod_inbox:entry_key(), id(), integer()}.

%% API
-export([init/2,
         set_inbox/6,
         set_inbox_incr_unread/5,
         reset_unread/4,
         remove_inbox_row/2,
         empty_user_bin/4,
         empty_global_bin/2,
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
    prepare_deletes(HostType, Opts),
    start_pool(HostType, AsyncOpts),
    ok.

stop(HostType) ->
    mongoose_async_pools:stop_pool(HostType, inbox).

prepare_pool_opts(#{async_writer := AsyncOpts}) ->
    AsyncOpts#{pool_type => aggregate,
               request_callback => fun ?MODULE:request/2,
               aggregate_callback => fun ?MODULE:aggregate/3,
               verify_callback => fun ?MODULE:verify/3}.

prepare_deletes(_HostType, _Opts) ->
    mongoose_rdbms:prepare(inbox_move_conversation_to_bin, inbox,
                           [luser, lserver, remote_bare_jid],
                           <<"UPDATE inbox SET box='bin'",
                             " WHERE luser = ? AND lserver = ? AND remote_bare_jid = ?">>),
    ok.

-spec start_pool(mongooseim:host_type(), mongoose_async_pools:pool_opts()) -> term().
start_pool(HostType, Opts) ->
    mongoose_async_pools:start_pool(HostType, inbox, Opts).

%% Worker callbacks
-spec request(task(), mongoose_async_pools:pool_extra()) -> reference().
request(Task, _Extra = #{host_type := HostType}) ->
    request_one(HostType, Task).

request_one(HostType, {set_inbox, {LUser, LServer, LToBareJid}, Packet, Count, MsgId, Timestamp, Box}) ->
    Content = exml:to_binary(Packet),
    Unique = [LUser, LServer, LToBareJid],
    Update = [MsgId, Box, Content, Count, Timestamp],
    Insert = [LUser, LServer, LToBareJid, MsgId, Box, Content, Count, Timestamp],
    rdbms_queries:request_upsert(HostType, inbox_upsert, Insert, Update, Unique);
request_one(HostType, {set_inbox_incr_unread, {LUser, LServer, LToBareJid}, Packet, MsgId, Timestamp, Incrs, Box}) ->
    Content = exml:to_binary(Packet),
    Unique = [LUser, LServer, LToBareJid],
    Update = [MsgId, Box, Content, Incrs, Timestamp],
    Insert = [LUser, LServer, LToBareJid, MsgId, Box, Content, Incrs, Timestamp],
    rdbms_queries:request_upsert(HostType, inbox_upsert_incr_unread, Insert, Update, Unique);
request_one(HostType, {reset_unread, {LUser, LServer, LToBareJid}, undefined, TS}) ->
    mongoose_rdbms:execute_request(HostType, inbox_reset_unread, [LUser, LServer, LToBareJid, TS]);
request_one(HostType, {reset_unread, {LUser, LServer, LToBareJid}, MsgId, TS}) ->
    mongoose_rdbms:execute_request(HostType, inbox_reset_unread_msg, [LUser, LServer, LToBareJid, MsgId, TS]);
request_one(HostType, {remove_inbox_row, {LUser, LServer, LToBareJid}}) ->
    mongoose_rdbms:execute_request(HostType, inbox_move_conversation_to_bin, [LUser, LServer, LToBareJid]).

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
                exml:element(), Count :: integer(), id(), Timestamp :: integer()) ->
    mod_inbox:write_res().
set_inbox(HostType, Entry, Packet, Count, MsgId, Timestamp) ->
    Params = {set_inbox, Entry, Packet, Count, MsgId, Timestamp, <<"inbox">>},
    mongoose_async_pools:put_task(HostType, inbox, Entry, Params).

-spec set_inbox_incr_unread(mongooseim:host_type(), mod_inbox:entry_key(),
                            exml:element(), MsgId :: binary(), Timestamp :: integer()) ->
    mod_inbox:count_res().
set_inbox_incr_unread(HostType, Entry, Packet, MsgId, Timestamp) ->
    Params = {set_inbox_incr_unread, Entry, Packet, MsgId, Timestamp, 1, <<"inbox">>},
    mongoose_async_pools:put_task(HostType, inbox, Entry, Params).

-spec reset_unread(mongooseim:host_type(), mod_inbox:entry_key(), binary() | undefined, integer()) ->
    mod_inbox:write_res().
reset_unread(HostType, Entry, MsgId, TS) ->
    Params = {reset_unread, Entry, MsgId, TS},
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

-spec empty_user_bin(HostType :: mongooseim:host_type(),
                     LServer :: jid:lserver(),
                     LUser :: jid:luser(),
                     TS :: integer()) -> non_neg_integer().
empty_user_bin(HostType, LServer, LUser, TS) ->
    mod_inbox_rdbms:empty_user_bin(HostType, LServer, LUser, TS).

-spec empty_global_bin(HostType :: mongooseim:host_type(),
                       TS :: integer()) -> non_neg_integer().
empty_global_bin(HostType, TS) ->
    mod_inbox_rdbms:empty_global_bin(HostType, TS).

-spec aggregate(CurrentlyAccumulatedTask :: task(), NewTask :: task()) -> FinalTask :: task().
%%% if new task is remove_row, do the previous with an updated box
aggregate({set_inbox, Entry, Content, Count, MsgId, Timestamp, _},
          {remove_inbox_row, _}) ->
    {set_inbox, Entry, Content, Count, MsgId, Timestamp, <<"bin">>};
aggregate({set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs, _},
          {remove_inbox_row, _}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs, <<"bin">>};
aggregate(_, {remove_inbox_row, _} = NewTask) ->
    NewTask;

%%% if the last task was remove_row, this task should now only be an insert
aggregate({remove_inbox_row, _} = OldTask, {reset_unread, _, _, _}) ->
    OldTask;
aggregate({remove_inbox_row, _},
          {set_inbox, Entry, Content, Count, MsgId, Timestamp, _}) ->
    {set_inbox, Entry, Content, Count, MsgId, Timestamp, <<"bin">>};
aggregate({remove_inbox_row, _},
          {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs, _}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs, <<"bin">>};

%%% If the last task was a reset_unread,
%   we prefer explicit resets,
%   then adhoc newer resets,
%   then we accumulate inserts
%% an undefined means an explicit request to reset, it has priority
aggregate({reset_unread, _, _, _}, {reset_unread, _, undefined, _} = NewTask) ->
    NewTask;
%% an undefined means an explicit request to reset, it has priority
aggregate({reset_unread, _, undefined, _} = OldTask, {reset_unread, _, _, _}) ->
    OldTask;
%% both are adhoc, we prefer the newer
aggregate({reset_unread, _, _, _}, {reset_unread, _, _, _} = NewTask) ->
    NewTask;
aggregate({reset_unread, _, _, _}, {set_inbox, _, _, _, _, _, _} = NewTask) ->
    NewTask;
%% Here `Count` becomes an absolute value instead of an increment
aggregate({reset_unread, _, _, _},
          {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs, Box}) ->
    {set_inbox, Entry, Content, Incrs, MsgId, Timestamp, Box};

%%% If the last task was a set_inbox
%% Reset is an explicit reset-to-zero, so do reset the counter
aggregate({set_inbox, Entry, Content, _, MsgId, Timestamp, Box},
          {reset_unread, _, undefined, _}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp, Box};
%% Reset refers to that same set_inbox
aggregate({set_inbox, Entry, Content, _, MsgId, Timestamp, Box},
          {reset_unread, _, MsgId, _}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp, Box};
%% Reset refers to some other set_inbox
aggregate({set_inbox, _, _, _, _, _, _} = OldTask,
          {reset_unread, _, _, _}) ->
    OldTask;
aggregate({set_inbox, _, _, Count, _, _, _, _},
          {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs, Box}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Count + Incrs, Box};

%%% If the last task was a set_inbox_incr_unread
% we're resetting on this message:
aggregate({set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, _, Box},
          {reset_unread, _, MsgId, _}) ->
    {set_inbox, Entry, Content, 0, MsgId, Timestamp, Box};
aggregate({set_inbox_incr_unread, _, _, _, _, _, _} = OldTask,
          {reset_unread, _, _, _}) ->
    OldTask;
% prefer newest row, but accumulate increment
aggregate({set_inbox_incr_unread, _, _, _, _, Incrs2, _},
          {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs1, Box}) ->
    {set_inbox_incr_unread, Entry, Content, MsgId, Timestamp, Incrs1 + Incrs2, Box};

aggregate({set_inbox_incr_unread, _, _, MsgId, _, _, _},
          {set_inbox, _, _, _, MsgId, _, _} = NewTask) ->
    NewTask;

aggregate(_OldTask, NewTask) ->
    NewTask.
