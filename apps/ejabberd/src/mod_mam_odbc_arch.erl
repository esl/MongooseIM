%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_arch).
%% Use few separate tables.
%% Check `apps/ejabberd/priv/mysql-part.sql' for schema.
%%
%% Hand made partitions do not work with `mod_mam_odbc_async_writer',
%% but work with `mod_mam_odbc_async_pool_writer'.
%%-define(HAND_MADE_PARTITIONS, true).

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

%% Called from mod_mam_odbc_async_writer
-export([prepare_message/8,
         archive_messages/2,
         archive_messages/3]).


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

%% ----------------------------------------------------------------------
%% Types

-type filter() :: iolist().
-type message_id() :: non_neg_integer().
-type user_id() :: non_neg_integer().
-type escaped_message_id() :: binary().
-type escaped_jid() :: binary().
-type escaped_resource() :: binary().
-type server_hostname() :: binary().
-type server_host() :: binary().
-type unix_timestamp() :: non_neg_integer().


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
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
    end.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

start_pm(Host, _Opts) ->
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

stop_pm(Host) ->
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


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

stop_muc(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-ifdef(HAND_MADE_PARTITIONS).
select_table(N) ->
    io_lib:format("mam_message_~2..0B", [N rem partition_count()]).

partition_count() ->
    16.
-else.
select_table(_) ->
    "mam_message".
-endif.

encode_direction(incoming) -> "I";
encode_direction(outgoing) -> "O".

archive_size(Size, Host, UserID, _UserJID) when is_integer(Size) ->
    IndexHintSQL = index_hint_sql(Host),
    {selected, _ColumnNames, [{BSize}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM ", select_table(UserID), " ",
       IndexHintSQL,
       "WHERE user_id = '", escape_user_id(UserID), "'"]),
    Size + list_to_integer(binary_to_list(BSize)).

index_hint_sql(Host) ->
    case ejabberd_odbc:db_engine(Host) of
        mysql ->
            "USE INDEX(i_mam_message_uid, i_mam_message_rem) ";
        _ ->
            ""
    end.

insert_ignore(Host) ->
    case ejabberd_odbc:db_engine(Host) of
        mysql ->
            "IGNORE ";
        _ ->
            ""
    end.

archive_message(_Result, Host, MessID, UserID,
                LocJID=#jid{},
                RemJID=#jid{lresource=RemLResource},
                SrcJID, Dir, Packet) ->
    SUserID = integer_to_list(UserID),
    SBareRemJID = minify_and_escape_bare_jid(LocJID, RemJID),
    SSrcJID = minify_and_escape_jid(LocJID, SrcJID),
    SDir = encode_direction(Dir),
    SRemLResource = ejabberd_odbc:escape(RemLResource),
    Data = term_to_binary(Packet),
    EscFormat = ejabberd_odbc:escape_format(Host),
    SData = ejabberd_odbc:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    Table = select_table(UserID),
    write_message(Host, Table, SMessID, SUserID, SBareRemJID,
                  SRemLResource, SDir, SSrcJID, SData).

write_message(Host, Table, SMessID, SUserID, SBareRemJID,
              SRemLResource, SDir, SSrcJID, SData) ->
    {updated, 1} =
    mod_mam_utils:success_sql_query(
      Host,
      ["INSERT INTO ", Table, " (id, user_id, remote_bare_jid, "
                                 "remote_resource, direction, "
                                 "from_jid, message) "
       "VALUES ('", SMessID, "', '", SUserID, "', '", SBareRemJID, "', "
               "'", SRemLResource, "', '", SDir, "', ",
               "'", SSrcJID, "', '", SData, "');"]),
    ok.

prepare_message(Host, MessID, UserID,
                LocJID=#jid{},
                RemJID=#jid{lresource=RemLResource}, SrcJID, Dir, Packet) ->
    SUserID = integer_to_list(UserID),
    SBareRemJID = minify_and_escape_bare_jid(LocJID, RemJID),
    SSrcJID = minify_and_escape_jid(LocJID, SrcJID),
    SDir = encode_direction(Dir),
    SRemLResource = ejabberd_odbc:escape(RemLResource),
    Data = term_to_binary(Packet, [compressed]),
    EscFormat = ejabberd_odbc:escape_format(Host),
    SData = ejabberd_odbc:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    [SMessID, SUserID, SBareRemJID, SRemLResource, SDir, SSrcJID, SData].

archive_messages(LServer, Acc) ->
    mod_mam_utils:success_sql_query(
      LServer,
      ["INSERT INTO mam_message(id, user_id, remote_bare_jid, "
                                "remote_resource, direction, "
                                "from_jid, message) "
       "VALUES ", tuples(Acc)]).

%% @doc N is a group id (partition number).
archive_messages(LServer, Acc, N) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["INSERT ", insert_ignore(LServer), "INTO ", select_table(N),
                             " (id, user_id, remote_bare_jid, "
                                "remote_resource, direction, "
                                "from_jid, message) "
       "VALUES ", tuples(Acc)]).

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
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, UserID, after_id(ID, Filter), 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = before, id = ID},
                Borders, Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, UserID, before_id(ID, Filter), 0, PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, UserID, Filter, Offset, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, true) ->
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, UserID, Filter, 0, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(Host, UserJID, MessageRows)}};



%% Cannot be optimized:
%% - #rsm_in{direction = aft, id = ID} 
%% - #rsm_in{direction = before, id = ID} 

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = before, id = undefined}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% Last page
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, UserID, Filter, 0, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            IndexHintSQL = index_hint_sql(Host),
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Host, UserID, before_id(FirstID, Filter), IndexHintSQL),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                #rsm_in{direction = undefined, index = Offset}, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% By offset
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, UserID, Filter, Offset, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            IndexHintSQL = index_hint_sql(Host),
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, UserID, after_id(LastID, Filter), IndexHintSQL),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                undefined, Borders,
                Start, End, _Now, WithJID,
                PageSize, _LimitPassed, _MaxResultLimit, opt_count) ->
    %% First page
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    MessageRows = extract_messages(Host, UserID, Filter, 0, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}};
        false ->
            IndexHintSQL = index_hint_sql(Host),
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Host, UserID, after_id(LastID, Filter), IndexHintSQL),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;


lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                RSM = #rsm_in{direction = aft, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    IndexHintSQL = index_hint_sql(Host),
    TotalCount = calc_count(Host, UserID, Filter, IndexHintSQL),
    Offset     = calc_offset(Host, UserID, Filter, IndexHintSQL, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, UserID, after_id(ID, Filter), 0, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                RSM = #rsm_in{direction = before, id = ID}, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    IndexHintSQL = index_hint_sql(Host),
    TotalCount = calc_count(Host, UserID, Filter, IndexHintSQL),
    Offset     = calc_offset(Host, UserID, Filter, IndexHintSQL, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, UserID, before_id(ID, Filter), 0, PageSize, true),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end;

lookup_messages(_Result, Host, UserID, UserJID = #jid{},
                RSM, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, _) ->
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    IndexHintSQL = index_hint_sql(Host),
    TotalCount = calc_count(Host, UserID, Filter, IndexHintSQL),
    Offset     = calc_offset(Host, UserID, Filter, IndexHintSQL, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, UserID, Filter, Offset, PageSize, false),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, UserJID, MessageRows)}}
    end.

after_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id > '", SID, "'"].

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter) ->
    SID = escape_message_id(ID),
    [Filter, " AND id < '", SID, "'"].

rows_to_uniform_format(Host, UserJID, MessageRows) ->
    EscFormat = ejabberd_odbc:escape_format(Host),
    [row_to_uniform_format(UserJID, EscFormat, Row) || Row <- MessageRows].

row_to_uniform_format(UserJID, EscFormat, {BMessID,BSrcJID,SData}) ->
    MessID = list_to_integer(binary_to_list(BMessID)),
    SrcJID = jlib:binary_to_jid(expand_minified_jid(UserJID, BSrcJID)),
    Data = ejabberd_odbc:unescape_binary(EscFormat, SData),
    Packet = binary_to_term(Data),
    {MessID, SrcJID, Packet}.

row_to_message_id({BMessID,_,_}) ->
    list_to_integer(binary_to_list(BMessID)).


remove_archive(Host, UserID, _UserJID) ->
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(UserID), " "
       "WHERE user_id = '", escape_user_id(UserID), "'"]),
    ok.

-spec purge_single_message(_Result, Host, MessID, UserID, UserJID, Now) ->
    ok | {error, 'not-allowed' | 'not-found'} when
    Host    :: server_host(),
    MessID  :: message_id(),
    UserID  :: user_id(),
    UserJID :: #jid{},
    Now     :: unix_timestamp().
purge_single_message(_Result, Host, MessID, UserID, _UserJID, _Now) ->
    Result =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(UserID), " "
       "WHERE user_id = '", escape_user_id(UserID), "' "
       "AND id = '", escape_message_id(MessID), "'"]),
    case Result of
        {updated, 0} -> {error, 'not-found'};
        {updated, 1} -> ok
    end.

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
    Filter = prepare_filter(UserID, UserJID, Borders, Start, End, WithJID),
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM ", select_table(UserID), " ", Filter]),
    ok.



%% Each record is a tuple of form 
%% `{<<"13663125233">>,<<"bob@localhost">>,<<"res1">>,<<binary>>}'.
%% Columns are `["id","from_jid","message"]'.
-spec extract_messages(Host, _UserID, Filter, IOffset, IMax, ReverseLimit) ->
    [Record] when
    Host :: server_hostname(),
    Filter  :: filter(),
    IOffset :: non_neg_integer(),
    IMax    :: pos_integer(),
    ReverseLimit :: boolean(),
    Record :: tuple().
extract_messages(_Host, _UserID, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Host, UserID, Filter, IOffset, IMax, false) ->
    {selected, _ColumnNames, MessageRows} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT id, from_jid, message "
       "FROM ", select_table(UserID), " ",
        Filter,
       " ORDER BY id"
       " LIMIT ", integer_to_list(IMax),
         case IOffset of
             0 -> "";
             _ -> [" OFFSET ", integer_to_list(IOffset)]
         end]),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    MessageRows;
extract_messages(Host, UserID, Filter, IOffset, IMax, true) ->
    {selected, _ColumnNames, MessageRows} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT id, from_jid, message "
       "FROM ", select_table(UserID), " ",
        Filter,
       " ORDER BY id DESC"
       " LIMIT ", integer_to_list(IMax),
         case IOffset of
             0 -> "";
             _ -> [" OFFSET ", integer_to_list(IOffset)]
         end]),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    lists:reverse(MessageRows).

%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
%% "SELECT COUNT(*) as "index" FROM mam_message WHERE id <= '",  UID
-spec calc_index(Host, UserID, Filter, IndexHintSQL, SUID) -> Count
    when
    Host         :: server_hostname(),
    UserID       :: user_id(),
    Filter       :: filter(),
    IndexHintSQL :: string(),
    SUID         :: escaped_message_id(),
    Count        :: non_neg_integer().
calc_index(Host, UserID, Filter, IndexHintSQL, SUID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) FROM ", select_table(UserID), " ",
       IndexHintSQL, Filter, " AND id <= '", SUID, "'"]),
    list_to_integer(binary_to_list(BIndex)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE id < '",  UID
-spec calc_before(Host, UserID, Filter, IndexHintSQL, SUID) -> Count
    when
    Host         :: server_hostname(),
    UserID       :: user_id(),
    Filter       :: filter(),
    IndexHintSQL :: string(),
    SUID         :: escaped_message_id(),
    Count        :: non_neg_integer().
calc_before(Host, UserID, Filter, IndexHintSQL, SUID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) FROM ", select_table(UserID), " ",
       IndexHintSQL, Filter, " AND id < '", SUID, "'"]),
    list_to_integer(binary_to_list(BIndex)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(Host, UserID, Filter, IndexHintSQL) -> Count
    when
    UserID       :: user_id(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    IndexHintSQL :: string(),
    Count        :: non_neg_integer().
calc_count(Host, UserID, Filter, IndexHintSQL) ->
    {selected, _ColumnNames, [{BCount}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) FROM ", select_table(UserID), " ",
       IndexHintSQL, Filter]),
    list_to_integer(binary_to_list(BCount)).


-spec prepare_filter(UserID, UserJID, Borders, Start, End, WithJID) -> filter()
    when
    UserID  :: user_id(),
    UserJID :: #jid{},
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    WithJID :: #jid{} | undefined.
prepare_filter(UserID, UserJID, Borders, Start, End, WithJID) ->
    {SWithJID, SWithResource} =
    case WithJID of
        undefined -> {undefined, undefined};
        #jid{lresource = <<>>} ->
            {minify_and_escape_bare_jid(UserJID, WithJID), undefined};
        #jid{lresource = WithLResource} ->
            {minify_and_escape_bare_jid(UserJID, WithJID),
             ejabberd_odbc:escape(WithLResource)}
    end,
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filter_sql(UserID, StartID2, EndID2, SWithJID, SWithResource).

-spec prepare_filter_sql(UserID, StartID, EndID, SWithJID, SWithResource) -> filter()
    when
    UserID  :: non_neg_integer(),
    StartID :: message_id() | undefined,
    EndID   :: message_id() | undefined,
    SWithJID :: escaped_jid() | undefined,
    SWithResource :: escaped_resource() | undefined.
prepare_filter_sql(UserID, StartID, EndID, SWithJID, SWithResource) ->
   ["WHERE user_id='", escape_user_id(UserID), "'",
     case StartID of
        undefined -> "";
        _         -> [" AND id >= ", integer_to_list(StartID)]
     end,
     case EndID of
        undefined -> "";
        _         -> [" AND id <= ", integer_to_list(EndID)]
     end,
     case SWithJID of
        undefined -> "";
        _         -> [" AND remote_bare_jid = '", SWithJID, "'"]
     end,
     case SWithResource of
        undefined -> "";
        _         -> [" AND remote_resource = '", SWithResource, "'"]
     end].


%% #rsm_in{
%%    max = non_neg_integer() | undefined,
%%    direction = before | aft | undefined,
%%    id = binary() | undefined,
%%    index = non_neg_integer() | undefined}
-spec calc_offset(Host, UserID, Filter, IndexHintSQL, PageSize, TotalCount, RSM) -> Offset
    when
    Host         :: server_hostname(),
    UserID       :: user_id(),
    Filter       :: filter(),
    IndexHintSQL :: string(),
    PageSize     :: non_neg_integer(),
    TotalCount   :: non_neg_integer(),
    RSM          :: #rsm_in{} | undefined,
    Offset       :: non_neg_integer().
calc_offset(_LS, _UserID, _F, _IH, _PS, _TC, #rsm_in{direction = undefined, index = Index})
    when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_LS, _UserID, _F, _IH, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Host, UserID, F, IH, PS, _TC, #rsm_in{direction = before, id = ID})
    when is_integer(ID) ->
    SID = escape_message_id(ID),
    max(0, calc_before(Host, UserID, F, IH, SID) - PS);
calc_offset(Host, UserID, F, IH, _PS, _TC, #rsm_in{direction = aft, id = ID})
    when is_integer(ID) ->
    SID = escape_message_id(ID),
    calc_index(Host, UserID, F, IH, SID);
calc_offset(_LS, _UserID, _F, _IH, _PS, _TC, _RSM) ->
    0.

escape_message_id(MessID) when is_integer(MessID) ->
    integer_to_list(MessID).

escape_user_id(UserID) when is_integer(UserID) ->
    integer_to_list(UserID).

%% @doc Strip resource, minify and escape JID.
minify_and_escape_bare_jid(LocJID, JID) ->
    ejabberd_odbc:escape(jid_to_opt_binary(LocJID, jlib:jid_remove_resource(JID))).

minify_and_escape_jid(LocJID, JID) ->
    ejabberd_odbc:escape(jid_to_opt_binary(LocJID, JID)).

join([H|T]) ->
    [H, [", " ++ X || X <- T]].

tuples(Rows) ->
    join([tuple(Row) || Row <- Rows]).

tuple([H|T]) ->
    ["('", H, "'", [[", '", X, "'"] || X <- T], ")"].

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).
