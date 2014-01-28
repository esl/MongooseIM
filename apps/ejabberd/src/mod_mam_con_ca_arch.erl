%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_con_ca_arch).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([archive_size/4,
         archive_message/9,
         lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Internal exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ----------------------------------------------------------------------
%% Imports

%% UID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

%% JID serialization
-import(mod_mam_utils,
        [jid_to_opt_binary/2,
         expand_minified_jid/2]).

%% Other
-import(mod_mam_utils,
        [maybe_min/2,
         maybe_max/2,
         apply_start_border/2,
         apply_end_border/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(state, {
    host,
    conn,
    insert_query_id,
    insert_query_types,
    calc_count_handler,
    extract_messages_handler,
    extract_messages_r_handler,
    query_refs,
    query_refs_count}).

-record(mam_ca_filter, {
    lower_jid,
    upper_jid,
    start_id,
    end_id
}).

-type worker() :: pid() | atom().

%% ----------------------------------------------------------------------
%% Types

-type filter() :: iolist().
-type message_id() :: non_neg_integer().
-type user_id() :: non_neg_integer().
-type server_hostname() :: binary().
-type server_host() :: binary().
-type unix_timestamp() :: non_neg_integer().


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    create_worker_pool(Host),
    start_servers(Host, servers(Host)),
    start_muc(Host, Opts).

stop(Host) ->
    stop_muc(Host),
    delete_worker_pool(Host),
    stop_servers(Host, servers(Host)).

servers(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, servers, [{"localhost", 9042, 1}]).

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

stop_muc(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

start_servers(Host, Addresses) ->
    [start_server(Host, Addr, Port, WorkerNumber)
     || {Addr, Port, WorkerCount} <- Addresses,
        WorkerNumber <- lists:seq(1, WorkerCount)].

stop_servers(Host, Addresses) ->
    [stop_server(Host, Addr, Port, WorkerNumber)
     || {Addr, Port, WorkerCount} <- Addresses,
        WorkerNumber <- lists:seq(1, WorkerCount)].

start_server(Host, Addr, Port, WorkerNumber) ->
    Tag = worker_tag(Host, Addr, Port, WorkerNumber),
    Spec = server_child_spec(Tag, Host, Addr, Port, WorkerNumber),
    supervisor:start_child(ejabberd_sup, Spec).

stop_server(Host, Addr, Port, WorkerNumber) ->
    Tag = worker_tag(Host, Addr, Port, WorkerNumber),
    supervisor:terminate_child(ejabberd_sup, Tag),
    supervisor:delete_child(ejabberd_sup, Tag).

create_worker_pool(Host) ->
    pg2:create(group_name(Host)).

delete_worker_pool(Host) ->
    pg2:delete(group_name(Host)).

register_worker(Host, WorkerPid) ->
    pg2:join(group_name(Host), WorkerPid).

select_worker(Host, BLowerJID, BUpperJID) ->
    case pg2:get_local_members(group_name(Host)) of
        [] ->
            error({no_worker, Host});
        Workers ->
            N = erlang:phash2({BLowerJID, BUpperJID}, length(Workers)) + 1,
            lists:nth(N, Workers)
    end.

group_name(Host) ->
    {mam_ca, Host}.

worker_tag(Host, Addr, Port, WorkerNumber) ->
    {mod_mam_ca_arch, Host, Addr, Port, WorkerNumber}.

server_child_spec(Tag, Host, Addr, Port, _WorkerNumber) ->
    {Tag,
     {mod_mam_ca_arch, start_link, [Host, Addr, Port]},
     permanent,
     5000,
     worker,
     [mod_mam_ca_arch]}.

start_link(Host, Addr, Port) ->
    gen_server:start_link(?MODULE, [Host, Addr, Port], []).


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

archive_size(Size, _Host, _UserID, _UserJID) when is_integer(Size) ->
    %% TODO
    Size.

archive_message(_Result, Host, MessID, _UserID,
                 LocJID=#jid{},
                 RemJID=#jid{},
                _SrcJID=#jid{}, incoming, Packet) ->
    BLocJID = prepare_jid(LocJID),
    BRemJID = prepare_jid(RemJID),
    case BLocJID < BRemJID of
        true ->
            IsFromLower = false,
            BLowerJID = BLocJID,
            BUpperJID = BRemJID,
            Worker = select_worker(Host, BLowerJID, BUpperJID),
            Data = term_to_binary(Packet),
            write_message(Worker, Host, MessID, BLowerJID, BUpperJID, IsFromLower, Data);
        false ->
            IsFromLower = true,
            BLowerJID = BRemJID,
            BUpperJID = BLocJID,
            Worker = select_worker(Host, BLowerJID, BUpperJID),
            Data = term_to_binary(Packet),
            write_message(Worker, Host, MessID, BLowerJID, BUpperJID, IsFromLower, Data)
    end;
archive_message(_Result, _Host, _MessID, _UserID,
                _LocJID=#jid{},
                _RemJID=#jid{},
                _SrcJID=#jid{}, outgoing, _Packet) ->
    %% Will be archived by remote jid.
    ok.

write_message(Worker, Host, MessID, BLowerJID, BUpperJID, IsFromLower, Data) ->
    gen_server:cast(Worker, {write_message, MessID, BLowerJID, BUpperJID, IsFromLower, Data}).

-spec lookup_messages(Result, Host,
                      _UserID, UserJID, RSM, Borders,
                      Start, End, Now, WithJID,
                      PageSize, LimitPassed, MaxResultLimit,
                      IsSimple) -> Result when
    Host    :: server_host(),
    UserJID :: #jid{},
    _UserID  :: user_id(),
    RSM     :: #rsm_in{} | undefined,
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    PageSize :: non_neg_integer(),
    WithJID :: #jid{} | undefined,
    LimitPassed :: boolean(),
    MaxResultLimit :: non_neg_integer(),
    IsSimple :: boolean(),
    Result :: {ok, {TotalCount, Offset, MessageRows}} | {error, 'policy-violation'},
    TotalCount :: non_neg_integer(),
    Offset  :: non_neg_integer(),
    MessageRows :: list(tuple()).

lookup_messages(_Result, _Host, _UserID, _UserJID,
                _RSM, Borders,
                Start, End, _Now, undefined,
                _PageSize, _LimitPassed, _MaxResultLimit, _) ->
    error(missing_with_jid);
lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    MessageRows = extract_messages(Worker, Host, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    MessageRows = extract_messages(Worker, Host, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    MessageRows = extract_messages(Worker, Host, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    MessageRows = extract_messages(Worker, Host, Filter, 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};



%% Cannot be optimized:
%% - #rsm_in{direction = aft, id = ID} 
%% - #rsm_in{direction = before, id = ID} 

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                #rsm_in{direction = before, id = undefined}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% Last page
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    MessageRows = extract_messages(Worker, Host, Filter, 0, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Worker, Host, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% By offset
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    MessageRows = extract_messages(Worker, Host, Filter, Offset, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Worker, Host, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% First page
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    MessageRows = extract_messages(Worker, Host, Filter, 0, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Worker, Host, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;


lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                RSM = #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    TotalCount = calc_count(Worker, Host, Filter),
    Offset     = calc_offset(Worker, Host, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Worker, Host, after_id(ID, Filter), 0, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                RSM = #rsm_in{direction = before, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    TotalCount = calc_count(Worker, Host, Filter),
    Offset     = calc_offset(Worker, Host, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Worker, Host, before_id(ID, Filter), 0, PageSize, true),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, _UserID, UserJID = #jid{},
                RSM, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    BUserJID = prepare_jid(UserJID),
    BWithJID = prepare_jid(WithJID),
    BLowerJID = min(BUserJID, BWithJID),
    BUpperJID = max(BUserJID, BWithJID),
    Worker = select_worker(Host, BLowerJID, BUpperJID),
    Filter = prepare_filter(BLowerJID, BUpperJID, Borders, Start, End),
    TotalCount = calc_count(Worker, Host, Filter),
    Offset     = calc_offset(Worker, Host, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Worker, Host, Filter, Offset, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end.

after_id(ID, Filter=#mam_ca_filter{start_id = AfterID}) ->
    Filter#mam_ca_filter{start_id = maybe_max(ID + 1, AfterID)}.

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID - 1, BeforeID)}.

to_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID, BeforeID)}.

rows_to_uniform_format(Host, UserJID, MessageRows) ->
    [row_to_uniform_format(UserJID, Row) || Row <- MessageRows].

row_to_uniform_format(UserJID, [MessID,BNick,Data]) ->
    SrcJID = jlib:jid_replace_resource(UserJID, BNick),
    Packet = binary_to_term(Data),
    {MessID, SrcJID, Packet}.

row_to_message_id([MessID,_,_]) ->
    MessID.

remove_archive(Host, _UserID, _UserJID) ->
    %% TODO
    ok.

-spec purge_single_message(_Result, Host, MessID, _UserID, UserJID, Now) ->
    ok | {error, 'not-allowed' | 'not-found'} when
    Host    :: server_host(),
    MessID  :: message_id(),
    _UserID  :: user_id(),
    UserJID :: #jid{},
    Now     :: unix_timestamp().
purge_single_message(_Result, Host, MessID, _UserID, _UserJID, _Now) ->
   {error, 'not-supported'}.


-spec purge_multiple_messages(_Result, Host,
                              _UserID, UserJID, Borders,
                              Start, End, Now, WithJID) ->
    ok | {error, 'not-allowed'} when
    Host    :: server_host(),
    _UserID  :: user_id(),
    UserJID :: #jid{},
    Borders :: #mam_borders{},
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: #jid{} | undefined.
purge_multiple_messages(_Result, Host, _UserID, UserJID, Borders,
                        Start, End, _Now, WithJID) ->
   {error, 'not-supported'}.


%% Each record is a tuple of form 
%% `{<<"13663125233">>,<<"bob@localhost">>,<<"res1">>,<<binary>>}'.
%% Columns are `["id","from_jid","message"]'.
-spec extract_messages(Worker, Host, Filter, IOffset, IMax, ReverseLimit) ->
    [Record] when
    Worker  :: worker(),
    Host    :: server_hostname(),
    Filter  :: filter(),
    IOffset :: non_neg_integer(),
    IMax    :: pos_integer(),
    ReverseLimit :: boolean(),
    Record :: tuple().
extract_messages(_Worker, _Host, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Worker, Host, Filter, 0, IMax, false) ->
    ResultF = gen_server:call(Worker,
        {extract_messages, {Filter, IMax}}),
    {ok, Result} = ResultF(),
    seestar_result:rows(Result);
extract_messages(Worker, Host, Filter, 0, IMax, true) ->
    ResultF = gen_server:call(Worker,
        {extract_messages_r, {Filter, IMax}}),
    {ok, Result} = ResultF(),
    lists:reverse(seestar_result:rows(Result));
extract_messages(_Worker, _Host, _Filter, _IOffset, _IMax, _) ->
    error(offset_not_supported).


%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
-spec calc_index(Worker, Host, Filter, MessID) -> Count
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    MessID       :: message_id(),
    Count        :: non_neg_integer().
calc_index(Worker, Host, Filter, MessID) ->
    calc_count(Worker, Host, to_id(MessID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
-spec calc_before(Worker, Host, Filter, MessID) -> Count
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    MessID       :: message_id(),
    Count        :: non_neg_integer().
calc_before(Worker, Host, Filter, MessID) ->
    calc_count(Worker, Host, before_id(MessID, Filter)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_con_message WHERE "
-spec calc_count(Worker, Host, Filter) -> Count
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    Count        :: non_neg_integer().
calc_count(Worker, Host, Filter) ->
    ResultF = gen_server:call(Worker, {calc_count, Filter}),
    {ok, Result} = ResultF(),
    [[Count]] = seestar_result:rows(Result),
    Count.


-spec prepare_filter(BLowerJID, BUpperJID, Borders, Start, End) -> filter()
    when
    BLowerJID :: binary(),
    BUpperJID :: binary(),
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined.
prepare_filter(BLowerJID, BUpperJID, Borders, Start, End) ->
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filter_params(BLowerJID, BUpperJID, StartID2, EndID2).

prepare_filter_params(BLowerJID, BUpperJID, StartID, EndID) ->
    #mam_ca_filter{
        lower_jid = BLowerJID,
        upper_jid = BUpperJID,
        start_id = StartID,
        end_id = EndID
    }.

eval_filter_params(#mam_ca_filter{
        lower_jid = BLowerJID,
        upper_jid = BUpperJID,
        start_id = StartID,
        end_id = EndID
    }) ->
    [BLowerJID, BUpperJID | prepare_filter_opt_params(StartID, EndID)].

select_filter(#mam_ca_filter{
        start_id = StartID,
        end_id = EndID
    }) ->
    select_filter(StartID, EndID).


-spec select_filter(StartID, EndID) -> filter()
    when
    StartID :: integer() | undefined,
    EndID   :: integer() | undefined.
select_filter(undefined, undefined) ->
    all;
select_filter(undefined, _) ->
    'end';
select_filter(_, undefined) ->
    start;
select_filter(_, _) ->
    start_end.

prepare_filter_sql(StartID, EndID) ->
    case StartID of
       undefined -> "";
       _         -> " AND id >= ?"
    end ++
    case EndID of
       undefined -> "";
       _         -> " AND id <= ?"
    end.

prepare_filter_opt_params(StartID, EndID) ->
    [Value || Value <- [StartID, EndID], Value =/= undefined].

filter_to_sql() ->
    [{select_filter(StartID, EndID),
      prepare_filter_sql(StartID, EndID)}
     || StartID        <- [undefined, 0],
          EndID        <- [undefined, 0]].

-spec calc_offset(Worker, Host, Filter, PageSize, TotalCount, RSM) -> Offset
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    PageSize     :: non_neg_integer(),
    TotalCount   :: non_neg_integer(),
    RSM          :: #rsm_in{} | undefined,
    Offset       :: non_neg_integer().
calc_offset(_W, _LS, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
    when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_W, _LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Worker, Host, F, PS, _TC, #rsm_in{direction = before, id = ID})
    when is_integer(ID) ->
    max(0, calc_before(Worker, Host, F, ID) - PS);
calc_offset(Worker, Host, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
    when is_integer(ID) ->
    calc_index(Worker, Host, F, ID);
calc_offset(_W, _LS, _F, _PS, _TC, _RSM) ->
    0.

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

prepare_jid(JID) ->
    jlib:jid_to_binary(jlib:jid_tolower(jlib:jid_remove_resource(JID))).


%%====================================================================
%% Internal SQL part
%%====================================================================

extract_messages_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, extract_messages_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_r_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, extract_messages_r_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

calc_count_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, calc_count_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_sql(Filter) ->
    "SELECT id, is_from_lower, message FROM mam_con_message "
        "WHERE lower_jid = ? AND upper_jid = ? " ++
        Filter ++ " ORDER BY id LIMIT ?".

extract_messages_r_sql(Filter) ->
    "SELECT id, nick_name, message FROM mam_con_message "
        "WHERE lower_jid = ? AND upper_jid = ? " ++
        Filter ++ " ORDER BY id DESC LIMIT ?".

calc_count_sql(Filter) ->
    "SELECT COUNT(*) FROM mam_con_message "
        "WHERE lower_jid = ? AND upper_jid = ? " ++ Filter.

execute_extract_messages(ConnPid, Handler, {Filter, IMax}) when is_integer(IMax) ->
    Params = eval_filter_params(Filter) ++ [IMax],
    FilterName = select_filter(Filter),
    PreparedQuery = dict:fetch(FilterName, Handler),
    execute_prepared_query(ConnPid, PreparedQuery, Params).

execute_calc_count(ConnPid, Handler, Filter) ->
    Params = eval_filter_params(Filter),
    FilterName = select_filter(Filter),
    PreparedQuery = dict:fetch(FilterName, Handler),
    execute_prepared_query(ConnPid, PreparedQuery, Params).
    
prepare_query(ConnPid, Query) ->
    {ok, Res} = seestar_session:prepare(ConnPid, Query),
    Types = seestar_result:types(Res),
    QueryID = seestar_result:query_id(Res),
    {QueryID, Types}.

execute_prepared_query(ConnPid, {QueryID, Types}, Params) ->
    seestar_session:execute_async(ConnPid, QueryID, Types, Params, one).


save_query_ref(From, QueryRef, State=#state{query_refs=Refs, query_refs_count=RefsCount}) ->
    Refs2 = dict:store(QueryRef, From, Refs),
    put(query_refs_count, RefsCount+1),
    State#state{query_refs=Refs2, query_refs_count=RefsCount+1}.

forward_query_respond(ResultF, QueryRef,
    State=#state{query_refs=Refs, query_refs_count=RefsCount}) ->
    case dict:find(QueryRef, Refs) of
        {ok, From} ->
            Refs2 = dict:erase(QueryRef, Refs),
            gen_server:reply(From, ResultF),
            put(query_refs_count, RefsCount-1),
            State#state{query_refs=Refs2, query_refs_count=RefsCount-1};
        error ->
%           lager:warning("Ignore response ~p ~p", [QueryRef, ResultF()]),
            State
    end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Addr, Port]) ->
    register_worker(Host, self()),
    ClientOptions = [{keyspace, "mam"}],
    {ok, ConnPid} = seestar_session:start_link(Addr, Port, ClientOptions),
    InsertQuery = "INSERT INTO mam_con_message "
        "(id, lower_jid, upper_jid, is_from_lower, message) "
        "VALUES (?, ?, ?, ?, ?)",
    {ok, InsertQueryRes} = seestar_session:prepare(ConnPid, InsertQuery),
    InsertQueryTypes = seestar_result:types(InsertQueryRes),
    InsertQueryID = seestar_result:query_id(InsertQueryRes),
    ExHandler = extract_messages_handler(ConnPid),
    RevExHandler = extract_messages_r_handler(ConnPid),
    CountHandler = calc_count_handler(ConnPid),
    put(query_refs_count, 0),
    State = #state{
        host=Host,
        conn=ConnPid,
        query_refs=dict:new(),
        query_refs_count=0,
        insert_query_id=InsertQueryID,
        insert_query_types=InsertQueryTypes,
        extract_messages_handler=ExHandler,
        extract_messages_r_handler=RevExHandler,
        calc_count_handler=CountHandler},
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({extract_messages, Args}, From,
    State=#state{conn=ConnPid, extract_messages_handler=Handler}) ->
    QueryRef = execute_extract_messages(ConnPid, Handler, Args),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({extract_messages_r, Args}, From,
    State=#state{conn=ConnPid, extract_messages_r_handler=Handler}) ->
    QueryRef = execute_extract_messages(ConnPid, Handler, Args),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({calc_count, Filter}, From,
    State=#state{conn=ConnPid, calc_count_handler=Handler}) ->
    QueryRef = execute_calc_count(ConnPid, Handler, Filter),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call(_, _From, State=#state{}) ->
    {reply, ok, State}.
 

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({write_message, MessID, BLowerJID, BUpperJID, IsFromLower, Data},
    State=#state{
        conn=ConnPid,
        insert_query_id=InsertQueryID,
        insert_query_types=InsertQueryTypes}) ->
    Row = [MessID, BLowerJID, BUpperJID, IsFromLower, Data],
    seestar_session:execute_async(ConnPid, InsertQueryID, InsertQueryTypes, Row, one),
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Strange cast message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------


handle_info({seestar_response, QueryRef, ResultF}, State) ->
    {noreply, forward_query_respond(ResultF, QueryRef, State)};
handle_info(Msg, State) ->
    ?WARNING_MSG("Strange info message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

