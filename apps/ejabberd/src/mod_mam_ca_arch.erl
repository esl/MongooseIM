%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_ca_arch).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([archive_size/4,
         safe_archive_message/9,
         safe_lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Internal exports
-export([start_link/2]).

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
        [apply_start_border/2,
         apply_end_border/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(state, {
    host,
    conn,
    insert_query_id,
    insert_query_types,
    extract_messages_handler,
    extract_messages_r_handler,
    query_refs}).

-record(mam_ca_filter, {
    user_id,
    start_id,
    end_id,
    with_bare_jid,
    with_resource
}).
%% @see srv_name/1
srv_name() ->
    ejabberd_mod_mam_ca.

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
    start_server(Host),
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            start_pm(Host, Opts);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            start_muc(Host, Opts);
        false ->
            ok
    end.

stop(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            stop_pm(Host);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            stop_muc(Host);
        false ->
            ok
    end,
    stop_server(Host).


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

start_pm(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, safe_archive_message, 50)
    end,
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, safe_lookup_messages, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

stop_pm(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, safe_archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, safe_lookup_messages, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, safe_archive_message, 50)
    end,
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, safe_lookup_messages, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

stop_muc(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, safe_archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, safe_lookup_messages, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

start_server(Host) ->
    Proc = srv_name(Host),
    supervisor:start_child(ejabberd_sup, server_child_spec(Proc, Host)).

stop_server(Host) ->
    Proc = srv_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).

server_child_spec(Proc, Host) ->
    {Proc,
     {mod_mam_ca_arch, start_link, [Proc, Host]},
     permanent,
     5000,
     worker,
     [mod_mam_ca_arch]}.

start_link(ProcName, Host) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host], []).

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

is_incoming(incoming) -> true;
is_incoming(outgoing) -> false.

%% @doc Apply pseudo-random permutation using perfect hash function.
%% Used as an uniform randomize prefix for keys.
prepare_user_id(UserID) when is_integer(UserID) ->
    fmix32(UserID).


archive_size(Size, Host, UserID, _UserJID) when is_integer(Size) ->
    %% TODO
    Size.

safe_archive_message(Result, Host, MessID, UserID,
                     LocJID, RemJID, SrcJID, Dir, Packet) ->
    try
        archive_message(Result, Host, MessID, UserID,
                        LocJID, RemJID, SrcJID, Dir, Packet)
    catch _Type:Reason ->
        {error, Reason}
    end.

archive_message(_Result, Host, MessID, UserID,
                LocJID=#jid{},
                RemJID=#jid{lresource=RemLResource},
                SrcJID, Dir, Packet) ->
    PUserID = prepare_user_id(UserID),
    BBareRemJID = minify_bare_jid(LocJID, RemJID),
    BSrcJID = minify_jid(LocJID, SrcJID),
    IsIncoming = is_incoming(Dir),
    Data = term_to_binary(Packet),
    write_message(Host, MessID, PUserID, BBareRemJID, RemLResource, BSrcJID, IsIncoming, Data).

write_message(Host, MessID, PUserID, BBareRemJID, RemLResource, BSrcJID, IsIncoming, Data) ->
    gen_server:cast(srv_name(Host), {write_message, MessID, PUserID, BBareRemJID, RemLResource, BSrcJID, IsIncoming, Data}).

safe_lookup_messages({error, Reason}=Result, _Host,
                     _UserID, _UserJID, _RSM, _Borders,
                     _Start, _End, _Now, _WithJID,
                     _PageSize, _LimitPassed, _MaxResultLimit,
                     _IsSimple) ->
    Result;
safe_lookup_messages(Result, Host,
                     UserID, UserJID, RSM, Borders,
                     Start, End, Now, WithJID,
                     PageSize, LimitPassed, MaxResultLimit,
                     IsSimple) ->
    try
        lookup_messages(Result, Host,
                        UserID, UserJID, RSM, Borders,
                        Start, End, Now, WithJID,
                        PageSize, LimitPassed, MaxResultLimit,
                        IsSimple)
    catch _Type:Reason ->
        {error, Reason}
    end.

-spec lookup_messages(Result, Host,
                      UserID, UserJID, RSM, Borders,
                      Start, End, Now, WithJID,
                      PageSize, LimitPassed, MaxResultLimit,
                      IsSimple) -> Result when
    Host    :: server_host(),
    UserJID :: #jid{},
    UserID  :: user_id(),
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

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, PUserID, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, PUserID, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, PUserID, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, PUserID, Filter, 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};



%% Cannot be optimized:
%% - #rsm_in{direction = aft, id = ID} 
%% - #rsm_in{direction = before, id = ID} 

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = before, id = undefined}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% Last page
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, PUserID, Filter, 0, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Host, PUserID, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% By offset
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, PUserID, Filter, Offset, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, PUserID, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    PUserID = prepare_user_id(UserID),
    %% First page
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, PUserID, Filter, 0, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, PUserID, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;


lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                RSM = #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    TotalCount = calc_count(Host, PUserID, Filter),
    Offset     = calc_offset(Host, PUserID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, PUserID, after_id(ID, Filter), 0, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                RSM = #rsm_in{direction = before, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    TotalCount = calc_count(Host, PUserID, Filter),
    Offset     = calc_offset(Host, PUserID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, PUserID, before_id(ID, Filter), 0, PageSize, true),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                RSM, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    PUserID = prepare_user_id(UserID),
    Filter = prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID),
    TotalCount = calc_count(Host, PUserID, Filter),
    Offset     = calc_offset(Host, PUserID, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, PUserID, Filter, Offset, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end.

after_id(ID, Filter=#mam_ca_filter{start_id = AfterID}) ->
    Filter#mam_ca_filter{start_id = max(ID + 1, AfterID)}.

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = min(ID - 1, BeforeID)}.

to_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = min(ID, BeforeID)}.

rows_to_uniform_format(Host, UserJID, MessageRows) ->
    [row_to_uniform_format(UserJID, Row) || Row <- MessageRows].

row_to_uniform_format(UserJID, {BMessID,BSrcJID,Data}) ->
    MessID = list_to_integer(binary_to_list(BMessID)),
    SrcJID = jlib:binary_to_jid(expand_minified_jid(UserJID, BSrcJID)),
    Packet = binary_to_term(Data),
    {MessID, SrcJID, Packet}.

row_to_message_id({BMessID,_,_}) ->
    list_to_integer(binary_to_list(BMessID)).


remove_archive(Host, UserID, _UserJID) ->
    %% TODO
    ok.

-spec purge_single_message(_Result, Host, MessID, UserID, UserJID, Now) ->
    ok | {error, 'not-allowed' | 'not-found'} when
    Host    :: server_host(),
    MessID  :: message_id(),
    UserID  :: user_id(),
    UserJID :: #jid{},
    Now     :: unix_timestamp().
purge_single_message(_Result, Host, MessID, UserID, _UserJID, _Now) ->
    %% TODO
    ok.


-spec purge_multiple_messages(_Result, Host,
                              UserID, UserJID, Borders,
                              Start, End, Now, WithJID) ->
    ok | {error, 'not-allowed'} when
    Host    :: server_host(),
    UserID  :: user_id(),
    UserJID :: #jid{},
    Borders :: #mam_borders{},
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: #jid{} | undefined.
purge_multiple_messages(_Result, Host, UserID, UserJID, Borders,
                        Start, End, _Now, WithJID) ->
    %% TODO
    ok.


%% Each record is a tuple of form 
%% `{<<"13663125233">>,<<"bob@localhost">>,<<"res1">>,<<binary>>}'.
%% Columns are `["id","from_jid","message"]'.
-spec extract_messages(Host, PUserID, Filter, IOffset, IMax, ReverseLimit) ->
    [Record] when
    Host :: server_hostname(),
    PUserID :: integer(),
    Filter  :: filter(),
    IOffset :: non_neg_integer(),
    IMax    :: pos_integer(),
    ReverseLimit :: boolean(),
    Record :: tuple().
extract_messages(_Host, _PUserID, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Host, PUserID, Filter, 0, IMax, false) ->
    ResultF = gen_server:call(srv_name(Host),
        {extract_messages, {PUserID, Filter, IMax}}),
    ResultF();
extract_messages(Host, PUserID, Filter, 0, IMax, true) ->
    ResultF = gen_server:call(srv_name(Host),
        {extract_messages_r, {PUserID, Filter, IMax}}),
    lists:reverse(ResultF());
extract_messages(_Host, _PUserID, _Filter, _IOffset, _IMax, _) ->
    error(offset_not_supported).

%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
%% "SELECT COUNT(*) as "index" FROM mam_message WHERE id <= '",  UID
-spec calc_index(Host, PUserID, Filter, MessID) -> Count
    when
    Host         :: server_hostname(),
    PUserID       :: user_id(),
    Filter       :: filter(),
    MessID       :: message_id(),
    Count        :: non_neg_integer().
calc_index(Host, PUserID, Filter, MessID) ->
    calc_count(Host, PUserID, to_id(MessID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE id < '",  UID
-spec calc_before(Host, PUserID, Filter, MessID) -> Count
    when
    Host         :: server_hostname(),
    PUserID       :: user_id(),
    Filter       :: filter(),
    MessID       :: message_id(),
    Count        :: non_neg_integer().
calc_before(Host, PUserID, Filter, MessID) ->
    calc_count(Host, PUserID, before_id(MessID, Filter)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(Host, PUserID, Filter) -> Count
    when
    PUserID       :: user_id(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    Count        :: non_neg_integer().
calc_count(Host, PUserID, Filter) ->
    0.
    %% TODO
%   {selected, _ColumnNames, [{BCount}]} =
%   mod_mam_utils:success_sql_query(
%     Host,
%     ["SELECT COUNT(*) FROM mam_message ", Filter]),
%   list_to_integer(binary_to_list(BCount)).


-spec prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID) -> filter()
    when
    PUserID  :: user_id(),
    UserJID :: #jid{},
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    WithJID :: #jid{} | undefined.
prepare_filter(PUserID, UserJID, Borders, Start, End, WithJID) ->
    {WithBareJID, WithResource} =
    case WithJID of
        undefined -> {undefined, undefined};
        #jid{lresource = <<>>} ->
            {minify_bare_jid(UserJID, WithJID), undefined};
        #jid{lresource = WithLResource} ->
            {minify_bare_jid(UserJID, WithJID), WithLResource}
    end,
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filter_params(PUserID, StartID2, EndID2, WithBareJID, WithResource).

prepare_filter_params(PUserID, StartID, EndID, WithBareJID, WithResource) ->
    #mam_ca_filter{
        user_id = PUserID,
        start_id = StartID,
        end_id = EndID,
        with_bare_jid = WithBareJID,
        with_resource = WithResource
    }.

eval_filter_params(#mam_ca_filter{
        user_id = PUserID,
        start_id = StartID,
        end_id = EndID,
        with_bare_jid = WithJID,
        with_resource = WithResource
    }) ->
    [PUserID | prepare_filter_opt_params(StartID, EndID, WithJID, WithResource)].

select_filter(#mam_ca_filter{
        start_id = StartID,
        end_id = EndID,
        with_bare_jid = WithJID,
        with_resource = WithResource
    }) ->
    select_filter(StartID, EndID, WithJID, WithResource).


-spec select_filter(StartID, EndID, WithJID, WithResource) -> filter()
    when
    StartID :: integer() | undefined,
    EndID   :: integer() | undefined,
    WithJID :: binary() | undefined,
    WithResource :: binary() | undefined.
select_filter(undefined, undefined, undefined, undefined) ->
    all;
select_filter(StartID, EndID, WithJID, WithResource) ->
    list_to_atom(tl(select_filter_s(StartID, EndID, WithJID, WithResource))).

select_filter_s(StartID, EndID, WithJID, WithResource) ->
    case StartID of undefined -> ""; _ -> "_start" end ++
    case EndID   of undefined -> ""; _ -> "_end" end ++
    case WithJID of undefined -> ""; _ -> "_with" ++
       case WithResource of undefined -> "_bare"; _ -> "" end ++ "_jid"
    end.

prepare_filter_sql(StartID, EndID, WithJID, WithResource) ->
    case StartID of
       undefined -> "";
       _         -> " AND id >= ?"
    end ++
    case EndID of
       undefined -> "";
       _         -> " AND id <= ?"
    end ++
    case WithJID of
       undefined -> "";
       _         -> " AND remote_bare_jid = ?"
    end ++
    case WithResource of
       undefined -> "";
       _         -> " AND remote_resource = ?"
    end.

prepare_filter_opt_params(StartID, EndID, WithJID, WithResource) ->
    [Value ||
     Value <- [StartID, EndID, WithJID, WithResource],
     Value =/= undefined].

filter_to_sql() ->
    [{select_filter(StartID, EndID, WithJID, WithResource),
      prepare_filter_sql(StartID, EndID, WithJID, WithResource)}
     || StartID        <- [undefined, 0],
          EndID        <- [undefined, 0],
          WithJID      <- [undefined, <<"x@y">>],
          WithResource <- [undefined, <<"z">>],
        %% Ignore a bad query
        not is_resource_without_bare_jid(WithJID, WithResource)].

is_resource_without_bare_jid(undefined, WithResource) ->
    WithResource =/= undefined;
is_resource_without_bare_jid(_, _) ->
    false.

-spec calc_offset(Host, PUserID, Filter, PageSize, TotalCount, RSM) -> Offset
    when
    Host         :: server_hostname(),
    PUserID       :: user_id(),
    Filter       :: filter(),
    PageSize     :: non_neg_integer(),
    TotalCount   :: non_neg_integer(),
    RSM          :: #rsm_in{} | undefined,
    Offset       :: non_neg_integer().
calc_offset(_LS, _PUserID, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
    when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_LS, _PUserID, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Host, PUserID, F, PS, _TC, #rsm_in{direction = before, id = ID})
    when is_integer(ID) ->
    max(0, calc_before(Host, PUserID, F, ID) - PS);
calc_offset(Host, PUserID, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
    when is_integer(ID) ->
    calc_index(Host, PUserID, F, ID);
calc_offset(_LS, _PUserID, _F, _PS, _TC, _RSM) ->
    0.

%% @doc Strip resource, minify JID.
minify_bare_jid(LocJID, JID) ->
    jid_to_opt_binary(LocJID, jlib:jid_remove_resource(JID)).

minify_jid(LocJID, JID) ->
    jid_to_opt_binary(LocJID, JID).

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

%%====================================================================
%% Internal SQL part
%%====================================================================

extract_messages_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, extract_messages_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_r_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, extract_messages_r_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_sql(Filter) ->
    "SELECT id, from_jid, message FROM mam_message WHERE user_id = ? " ++
        Filter ++ " ORDER BY id LIMIT ?".

extract_messages_r_sql(Filter) ->
    "SELECT id, from_jid, message FROM mam_message WHERE user_id = ? " ++
        Filter ++ " ORDER BY id DESC LIMIT ?".

execute_extract_messages(ConnPid, Handler, {PUserID, Filter, IMax}) ->
    Params = [PUserID|eval_filter_params(Filter)] ++ [IMax],
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


save_query_ref(From, QueryRef, State=#state{query_refs=Refs}) ->
    Refs2 = dict:store(QueryRef, From, Refs),
    State#state{query_refs=Refs2}.

forward_query_respond(ResultF, QueryRef, State=#state{query_refs=Refs}) ->
    case dict:find(QueryRef, Refs) of
        {ok, From} ->
            Refs2 = dict:erase(QueryRef, From, Refs),
            gen_server:reply(From, ResultF),
            State#state{query_refs=Refs2};
        error ->
            lager:warning("Ignore response ~p ~p", [QueryRef, ResultF()]),
            State
    end.


%%====================================================================
%% MurmurHash3
%%====================================================================

%% MurmurHash3 finalizer.
%% No collisions possible for 4-byte keys.
fmix32(H0) when is_integer(H0) ->
    H1 = mask_32(xorbsr(H0, 16) * 16#85ebca6b),
    H2 = mask_32(xorbsr(H1, 13) * 16#c2b2ae35),
    xorbsr(H2, 16).

xorbsr(H, V) when is_integer(H), is_integer(V) ->
    H bxor (H bsr V).

mask_32(X) when is_integer(X) ->
    X band 16#FFFFFFFF.


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
init([Host]) ->
    ClientOptions = [{keyspace, "mam"}],
    {ok, ConnPid} = seestar_session:start_link("localhost", 9042, ClientOptions),
    InsertQuery = "INSERT INTO mam_message "
        "(id, user_id, remote_bare_jid, remote_resource, from_jid, message) "
        "VALUES (?, ?, ?, ?, ?, ?)",
    {ok, InsertQueryRes} = seestar_session:prepare(ConnPid, InsertQuery),
    InsertQueryTypes = seestar_result:types(InsertQueryRes),
    InsertQueryID = seestar_result:query_id(InsertQueryRes),
    ExHandler = extract_messages_handler(ConnPid),
    RevExHandler = extract_messages_r_handler(ConnPid),
    State = #state{
        host=Host,
        conn=ConnPid,
        query_refs=dict:new(),
        insert_query_id=InsertQueryID,
        insert_query_types=InsertQueryTypes,
        extract_messages_handler=ExHandler,
        extract_messages_r_handler=RevExHandler},
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
handle_call({extract_messages_r, Args}, _From, State=#state{}) ->
    {noreply, ok, State};
handle_call(_, _From, State=#state{}) ->
    {reply, ok, State}.
 

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({write_message, MessID, PUserID, BBareRemJID, RemLResource, BSrcJID, IsIncoming, Data},
    State=#state{
        conn=ConnPid,
        insert_query_id=InsertQueryID,
        insert_query_types=InsertQueryTypes}) ->
    Row = [MessID, PUserID, BBareRemJID, RemLResource, BSrcJID, Data],
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

