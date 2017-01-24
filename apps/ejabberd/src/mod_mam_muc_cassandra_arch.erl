%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_cassandra_arch).
-behaviour(mongoose_cassandra).

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([archive_size/4,
         archive_message/9,
         lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% mongoose_cassandra callbacks
-export([prepared_queries/0]).

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

-record(mam_muc_ca_filter, {
          room_jid,
          with_nick,
          nick_name,
          start_id,
          end_id
         }).

-record(mam_muc_message, {
          id :: non_neg_integer(),
          room_jid :: binary(),
          nick_name :: binary(),
          with_nick :: binary(),
          message :: binary()
         }).

-callback encode(binary()) -> binary().
-callback decode(binary()) -> binary().

%% ----------------------------------------------------------------------
%% Types

-type filter() :: #mam_muc_ca_filter{}.
-type message_id() :: non_neg_integer().
-type user_id() :: non_neg_integer().
-type server_hostname() :: binary().
-type server_host() :: binary().
-type unix_timestamp() :: non_neg_integer().


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    compile_params_module(Opts),
    start_muc(Host, Opts).

stop(Host) ->
    stop_muc(Host).

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
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages,
                          50),
    ok.

%% ----------------------------------------------------------------------
%% mongoose_cassandra_worker callbacks

prepared_queries() ->
    [
     {insert_offset_hint_query, insert_offset_hint_query_cql()},
     {prev_offset_query, prev_offset_query_cql()},
     {insert_query, insert_query_cql()},
     {delete_query, delete_query_cql()},
     {select_for_removal_query, select_for_removal_query_cql()},
     {remove_archive_query, remove_archive_query_cql()},
     {remove_archive_offsets_query, remove_archive_offsets_query_cql()},
     {message_id_to_nick_name_query, message_id_to_nick_name_cql()}]
        ++ extract_messages_queries()
        ++ extract_messages_r_queries()
        ++ calc_count_queries()
        ++ list_message_ids_queries().

%%====================================================================
%% Internal functions
%%====================================================================

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

archive_size(Size, Host, _RoomID, RoomJID) when is_integer(Size) ->
    PoolName = pool_name(RoomJID),
    Borders = Start = End = WithNick = undefined,
    Filter = prepare_filter(RoomJID, Borders, Start, End, WithNick),
    calc_count(PoolName, RoomJID, Host, Filter).


%% ----------------------------------------------------------------------
%% INSERT MESSAGE

insert_query_cql() ->
    "INSERT INTO mam_muc_message "
        "(id, room_jid, nick_name, with_nick, message) "
        "VALUES (?, ?, ?, ?, ?)".

archive_message(Result, Host, MessID, _RoomID,
                LocJID, NickName, NickName, Dir, Packet) ->
    try
        archive_message2(Result, Host, MessID,
                         LocJID, NickName, NickName, Dir, Packet)
    catch _Type:Reason ->
            {error, Reason}
    end.

archive_message2(_Result, _Host, MessID,
                 LocJID = #jid{},
                 _RemJID = #jid{},
                 _SrcJID = #jid{lresource = BNick}, _Dir, Packet) ->
    BLocJID = bare_jid(LocJID),
    BPacket = packet_to_stored_binary(Packet),
    Message = #mam_muc_message{
                 id        = MessID,
                 room_jid  = BLocJID,
                 nick_name = BNick,
                 message   = BPacket
                },
    WithNicks = [<<>>, BNick],
    Messages = [Message#mam_muc_message{with_nick = BWithNick} || BWithNick <- WithNicks],
    PoolName = pool_name(LocJID),
    write_messages(PoolName, Messages).

write_messages(RoomJID, Messages) ->
    PoolName = pool_name(RoomJID),
    MultiParams = [message_to_params(M) || M <- Messages],
    mongoose_cassandra:cql_write_async(PoolName, RoomJID, ?MODULE, insert_query, MultiParams).

message_to_params(#mam_muc_message{
                     id        = MessID,
                     room_jid  = BLocJID,
                     nick_name = BNick,
                     with_nick = BWithNick,
                     message   = BPacket
                    }) ->
    #{id => MessID, room_jid => BLocJID, nick_name => BNick,
      with_nick => BWithNick, message => BPacket}.


%% ----------------------------------------------------------------------
%% DELETE MESSAGE

delete_query_cql() ->
    "DELETE FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? AND id = ?".

delete_messages(PoolName, RoomJID, Messages) ->
    MultiParams = [delete_message_to_params(M) || M <- Messages],
    mongoose_cassandra:cql_write(PoolName, RoomJID, ?MODULE, delete_query,
                                 MultiParams).

delete_message_to_params(#mam_muc_message{
                            id        = MessID,
                            room_jid  = BLocJID,
                            with_nick = BWithNick
                           }) ->
    #{room_jid => BLocJID, with_nick => BWithNick, id => MessID}.


%% ----------------------------------------------------------------------
%% REMOVE ARCHIVE

remove_archive_query_cql() ->
    "DELETE FROM mam_muc_message WHERE room_jid = ? AND with_nick = ?".

remove_archive_offsets_query_cql() ->
    "DELETE FROM mam_muc_message_offset WHERE room_jid = ? AND with_nick = ?".

select_for_removal_query_cql() ->
    "SELECT DISTINCT room_jid, with_nick FROM mam_muc_message WHERE room_jid = ?".

remove_archive(_Host, _RoomID, RoomJID) ->
    BRoomJID = bare_jid(RoomJID),
    PoolName = pool_name(RoomJID),
    Params = #{room_jid => BRoomJID},
    %% Wait until deleted

    DeleteFun =
        fun(Rows, _AccIn) ->
                mongoose_cassandra:cql_write(PoolName, RoomJID, ?MODULE,
                                             remove_archive_query, Rows),
                mongoose_cassandra:cql_write(PoolName, RoomJID, ?MODULE,
                                             remove_archive_offsets_query, Rows)
        end,

    mongoose_cassandra:cql_foldl(PoolName, RoomJID, ?MODULE,
                                 select_for_removal_query, Params, DeleteFun, []),
    ok.

%% ----------------------------------------------------------------------
%% GET NICK NAME

message_id_to_nick_name_cql() ->
    "SELECT nick_name FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = '' AND id = ?".

message_id_to_nick_name(PoolName, RoomJID, BRoomJID, MessID) ->
    Params = #{room_jid => BRoomJID, id => MessID},
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE,
                                             message_id_to_nick_name_query, Params),
    case Rows of
        [] ->
            {error, not_found};
        [#{nick_name := NickName}] ->
            {ok, NickName}
    end.


%% ----------------------------------------------------------------------
%% SELECT MESSAGES

-spec lookup_messages(Result :: any(), Host :: ejabberd:server(),
                      ArchiveID :: mod_mam:archive_id(),
                      ArchiveJID :: ejabberd:jid(),
                      RSM :: jlib:rsm_in()  | undefined,
                      Borders :: mod_mam:borders()  | undefined,
                      Start :: mod_mam:unix_timestamp()  | undefined,
                      End :: mod_mam:unix_timestamp()  | undefined,
                      Now :: mod_mam:unix_timestamp(),
                      WithJID :: ejabberd:jid()  | undefined,
                      PageSize :: non_neg_integer(), LimitPassed :: boolean(),
                      MaxResultLimit :: non_neg_integer(),
                      IsSimple :: boolean()  | opt_count) ->
                             {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.
lookup_messages({error, _Reason} = Result, _Host,
                _RoomID, _RoomJID, _RSM, _Borders,
                _Start, _End, _Now, _WithJID,
                _PageSize, _LimitPassed, _MaxResultLimit,
                _IsSimple) ->
    Result;
lookup_messages(_Result, Host,
                _RoomID, RoomJID, RSM, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit,
                IsSimple) ->
    try
        WithNick = maybe_jid_to_nick(WithJID),
        PoolName = pool_name(RoomJID),
        lookup_messages2(PoolName, Host,
                         RoomJID, RSM, Borders,
                         Start, End, WithNick,
                         PageSize, LimitPassed, MaxResultLimit,
                         IsSimple)
    catch _Type:Reason ->
            S = erlang:get_stacktrace(),
            {error, {Reason, {stacktrace, S}}}
    end.

maybe_jid_to_nick(#jid{lresource = BNick}) -> BNick;
maybe_jid_to_nick(undefined) -> undefined.


lookup_messages2(PoolName, Host,
                 RoomJID = #jid{}, RSM, Borders,
                 Start, End, WithNick,
                 PageSize, _LimitPassed, _MaxResultLimit,
                 _IsSimple = true) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(RoomJID, Borders, Start, End, WithNick),
    lookup_messages_simple(PoolName, Host, RoomJID, RSM, PageSize, Filter);
lookup_messages2(PoolName, Host,
                 RoomJID = #jid{}, RSM, Borders,
                 Start, End, WithNick,
                 PageSize, LimitPassed, MaxResultLimit,
                 _IsSimple) ->
    %% Query with offset calculation
    %% We cannot just use ODBC code because "LIMIT X, Y" is not supported by cassandra
    %% Not all queries are optimal. You would like to disable something for production
    %% once you know how you will call bd
    Strategy = rsm_to_strategy(RSM),
    Filter = prepare_filter(RoomJID, Borders, Start, End, WithNick),
    Result =
        case Strategy of
            last_page ->
                lookup_messages_last_page(PoolName, Host, RoomJID, RSM, PageSize, Filter);
            by_offset ->
                lookup_messages_by_offset(PoolName, Host, RoomJID, RSM, PageSize, Filter);
            first_page ->
                lookup_messages_first_page(PoolName, Host, RoomJID, RSM, PageSize, Filter);
            before_id ->
                lookup_messages_before_id(PoolName, Host, RoomJID, RSM, PageSize, Filter);
            after_id ->
                lookup_messages_after_id(PoolName, Host, RoomJID, RSM, PageSize, Filter)
        end,
    check_result_for_policy_violation(Result, MaxResultLimit, LimitPassed).

rsm_to_strategy(#rsm_in{direction = before, id = undefined}) ->
    last_page;
rsm_to_strategy(#rsm_in{direction = undefined, index = 0}) ->
    first_page;
rsm_to_strategy(#rsm_in{direction = undefined, index = Offset}) when is_integer(Offset) ->
    by_offset;
rsm_to_strategy(#rsm_in{direction = before, id = Id}) when is_integer(Id) ->
    before_id;
rsm_to_strategy(#rsm_in{direction = aft, id = Id}) when is_integer(Id) ->
    after_id;
rsm_to_strategy(#rsm_in{}) ->
    first_page;
rsm_to_strategy(undefined) ->
    first_page.

lookup_messages_simple(PoolName, Host, RoomJID,
                       #rsm_in{direction = aft, id = ID},
                       PageSize, Filter) ->
    %% Get last rows from result set
    MessageRows = extract_messages(PoolName, RoomJID, Host, after_id(ID, Filter), PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, RoomJID)}};
lookup_messages_simple(PoolName, Host, RoomJID,
                       #rsm_in{direction = before, id = ID},
                       PageSize, Filter) ->
    MessageRows = extract_messages(PoolName, RoomJID, Host, before_id(ID, Filter), PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, RoomJID)}};
lookup_messages_simple(PoolName, Host, RoomJID,
                       #rsm_in{direction = undefined, index = Offset},
                       PageSize, Filter) ->
    %% Apply offset
    StartId = offset_to_start_id(PoolName, RoomJID, Filter,
                                 Offset), %% POTENTIALLY SLOW AND NOT SIMPLE :)
    MessageRows = extract_messages(PoolName, RoomJID, Host, from_id(StartId, Filter), PageSize,
                                   false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, RoomJID)}};
lookup_messages_simple(PoolName, Host, RoomJID,
                       _,
                       PageSize, Filter) ->
    MessageRows = extract_messages(PoolName, RoomJID, Host, Filter, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows, RoomJID)}}.

lookup_messages_last_page(PoolName, Host, RoomJID,
                          #rsm_in{direction = before, id = undefined},
                          0, Filter) ->
    %% Last page
    TotalCount = calc_count(PoolName, RoomJID, Host, Filter),
    {ok, {TotalCount, TotalCount, []}};
lookup_messages_last_page(PoolName, Host, RoomJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(PoolName, RoomJID, Host, Filter, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows, RoomJID)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(PoolName, RoomJID, Host, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows, RoomJID)}}
    end.

lookup_messages_by_offset(PoolName, Host, RoomJID,
                          #rsm_in{direction = undefined, index = Offset},
                          0, Filter) when is_integer(Offset) ->
    %% By offset
    TotalCount = calc_count(PoolName, RoomJID, Host, Filter),
    {ok, {TotalCount, Offset, []}};
lookup_messages_by_offset(PoolName, Host, RoomJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) when is_integer(Offset) ->
    %% By offset
    StartId = offset_to_start_id(PoolName, RoomJID, Filter, Offset), %% POTENTIALLY SLOW
    MessageRows = extract_messages(PoolName, RoomJID, Host, from_id(StartId, Filter), PageSize,
                                   false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(PoolName, RoomJID, Host, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(MessageRows, RoomJID)}}
    end.

lookup_messages_first_page(PoolName, Host, RoomJID,
                           _,
                           0, Filter) ->
    %% First page, just count
    TotalCount = calc_count(PoolName, RoomJID, Host, Filter),
    {ok, {TotalCount, 0, []}};
lookup_messages_first_page(PoolName, Host, RoomJID,
                           _,
                           PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(PoolName, RoomJID, Host, Filter, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows, RoomJID)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(PoolName, RoomJID, Host, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(MessageRows, RoomJID)}}
    end.

lookup_messages_before_id(PoolName, Host, RoomJID,
                          RSM = #rsm_in{direction = before, id = ID},
                          PageSize, Filter) ->
    TotalCount = calc_count(PoolName, RoomJID, Host, Filter),
    Offset = calc_offset(PoolName, RoomJID, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(PoolName, RoomJID, Host, before_id(ID, Filter), PageSize, true),
    {ok, {TotalCount, Offset, rows_to_uniform_format(MessageRows, RoomJID)}}.

lookup_messages_after_id(PoolName, Host, RoomJID,
                         RSM = #rsm_in{direction = aft, id = ID},
                         PageSize, Filter) ->
    PoolName = pool_name(RoomJID),
    TotalCount = calc_count(PoolName, RoomJID, Host, Filter),
    Offset = calc_offset(PoolName, RoomJID, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(PoolName, RoomJID, Host, after_id(ID, Filter), PageSize, false),
    {ok, {TotalCount, Offset, rows_to_uniform_format(MessageRows, RoomJID)}}.

check_result_for_policy_violation(Result = {ok, {TotalCount, Offset, _}},
                                  MaxResultLimit, LimitPassed)
  when is_integer(TotalCount), is_integer(Offset) ->
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) of
        true ->
            {error, 'policy-violation'};
        false ->
            Result
    end;
check_result_for_policy_violation(Result, _MaxResultLimit, _LimitPassed) ->
    Result.

is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) ->
    TotalCount - Offset > MaxResultLimit andalso not LimitPassed.


after_id(ID, Filter = #mam_muc_ca_filter{start_id = AfterID}) ->
    Filter#mam_muc_ca_filter{start_id = maybe_max(ID + 1, AfterID)}.

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter = #mam_muc_ca_filter{end_id = BeforeID}) ->
    Filter#mam_muc_ca_filter{end_id = maybe_min(ID - 1, BeforeID)}.

to_id(ID, Filter = #mam_muc_ca_filter{end_id = BeforeID}) ->
    Filter#mam_muc_ca_filter{end_id = maybe_min(ID, BeforeID)}.

from_id(ID, Filter = #mam_muc_ca_filter{start_id = AfterID}) ->
    Filter#mam_muc_ca_filter{start_id = maybe_max(ID, AfterID)}.


rows_to_uniform_format(MessageRows, RoomJID) ->
    [row_to_uniform_format(Row, RoomJID) || Row <- MessageRows].

row_to_uniform_format(#{nick_name := BNick, message := Data, id := MessID}, RoomJID) ->
    SrcJID = jid:replace_resource(RoomJID, BNick),
    Packet = stored_binary_to_packet(Data),
    {MessID, SrcJID, Packet}.

row_to_message_id(#{id := MsgID}) ->
    MsgID.

-spec purge_single_message(_Result, Host, MessID, _RoomID, RoomJID,
                           Now) ->
                                  ok  | {error, 'not-supported'} when
      Host :: server_host(), MessID :: message_id(),
      _RoomID :: user_id(), RoomJID :: jid(),
      Now :: unix_timestamp().
purge_single_message(_Result, _Host, MessID, _RoomID, RoomJID, _Now) ->
    PoolName = pool_name(RoomJID),
    BRoomJID = bare_jid(RoomJID),
    Result = message_id_to_nick_name(PoolName, RoomJID, BRoomJID, MessID),
    case Result of
        {ok, BNick} ->
            BWithNicks = lists:usort([BNick, <<>>]),
            %% Set some fields
            %% To remove record we need to know room_jid, with_nick and id.
            Messages = [#mam_muc_message{
                           id        = MessID,
                           room_jid  = BRoomJID,
                           nick_name = BNick, %% set the field for debugging
                           with_nick = BWithNick
                          }           || BWithNick <- BWithNicks],
            delete_messages(PoolName, RoomJID, Messages),
            ok;
        {error, _} ->
            ok
    end.

-spec purge_multiple_messages(_Result, Host, _RoomID, RoomJID, Borders,
                              Start, End, Now, WithNick) ->
                                     ok when
      Host :: server_host(), _RoomID :: user_id(),
      RoomJID :: jid(), Borders :: mam_borders(),
      Start :: unix_timestamp()  | undefined,
      End :: unix_timestamp()  | undefined,
      Now :: unix_timestamp(),
      WithNick :: jid()  | undefined.
purge_multiple_messages(_Result, Host, RoomID, RoomJID, Borders,
                        Start, End, Now, WithNick) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(RoomJID, Borders, Start, End, WithNick),
    PoolName = pool_name(RoomJID),
    Limit = 100, %% TODO something smarter
    QueryName = {list_message_ids_query, select_filter(Filter)},
    Params = maps:put('[limit]', Limit, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName,
                                             Params),
    %% TODO can be faster
    %% TODO rate limiting
    [purge_single_message(ok, Host, Id, RoomID, RoomJID, Now)
     || #{id := Id} <- Rows],
    ok.


%% Offset is not supported
%% Each record is a tuple of form
%% `{<<"13663125233">>, <<"bob@localhost">>, <<"res1">>, <<binary>>}'.
%% Columns are `["id", "nick_name", "message"]'.
-spec extract_messages(PoolName, RoomJID, Host, Filter, IMax, ReverseLimit) ->
                              [Row] when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      IMax :: pos_integer(),
      ReverseLimit :: boolean(),
      Row :: mongoose_cassandra:row().
extract_messages(_Worker, _RoomJID, _Host, _Filter, 0, _) ->
    [];
extract_messages(PoolName, RoomJID, _Host, Filter, IMax, false) ->
    QueryName = {extract_messages_query, select_filter(Filter)},
    Params = maps:put('[limit]', IMax, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName, Params),
    Rows;
extract_messages(PoolName, RoomJID, _Host, Filter, IMax, true) ->
    QueryName = {extract_messages_r_query, select_filter(Filter)},
    Params = maps:put('[limit]', IMax, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName, Params),
    lists:reverse(Rows).


%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
-spec calc_index(PoolName, RoomJID, Host, Filter, MessID) -> Count
                                                                 when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_index(PoolName, RoomJID, Host, Filter, MessID) ->
    calc_count(PoolName, RoomJID, Host, to_id(MessID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
-spec calc_before(PoolName, RoomJID, Host, Filter, MessID) -> Count
                                                                  when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_before(PoolName, RoomJID, Host, Filter, MessID) ->
    calc_count(PoolName, RoomJID, Host, before_id(MessID, Filter)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_muc_message WHERE "
-spec calc_count(PoolName, RoomJID, Host, Filter) -> Count
                                                         when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      Count :: non_neg_integer().
calc_count(PoolName, RoomJID, _Host, Filter) ->
    QueryName = {calc_count_query, select_filter(Filter)},
    Params = eval_filter_params(Filter),
    {ok, [#{count := Count}]} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName,
                                              Params),
    Count.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
%% Uses previously calculated offsets to speed up queries
-spec offset_to_start_id(PoolName, RoomJID, Filter, Offset) -> Id when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jlib:jid(),
      Offset :: non_neg_integer(),
      Filter :: filter(),
      Id :: non_neg_integer() | undefined.
offset_to_start_id(PoolName, RoomJID, Filter, Offset) when is_integer(Offset), Offset >= 0,
                                                           Offset =< 100 ->
    calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset);
offset_to_start_id(PoolName, RoomJID, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    Params = maps:put(offset, Offset, eval_filter_params(Filter)),
    %% Try to find already calculated nearby offset to reduce query size
    case mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, prev_offset_query, Params) of
        {ok, []} -> %% No hints, just calculate offset sloooowly
            StartId = calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset),
            maybe_save_offset_hint(PoolName, RoomJID, Filter, 0, Offset, StartId);
        {ok, [#{offset := PrevOffset, id := PrevId}]} ->
            %% Offset hint found, use it to reduce query size
            case Offset of
                PrevOffset -> PrevId;
                _ ->
                    StartId = calc_offset_to_start_id(PoolName, RoomJID,
                                                      Filter#mam_muc_ca_filter{start_id = PrevId},
                                                      Offset - PrevOffset + 1),
                    maybe_save_offset_hint(PoolName, RoomJID, Filter, PrevOffset, Offset, StartId)
            end
    end.

%% @doc Saves offset hint for future use in order to speed up queries with similar offset
%% Hint is save only if previous offset hint was 50+ entires from current query
%% This function returns given StartId as passthrough for convenience
-spec maybe_save_offset_hint(PoolName :: mongoose_cassandra:pool_name(), RoomJID :: jlib:jid(),
                             Filter :: filter(), HintOffset :: non_neg_integer(),
                             NewOffset :: non_neg_integer(),
                             StartId :: non_neg_integer() | undefined) ->
    StartId :: non_neg_integer() | undefined.
maybe_save_offset_hint(_PoolName, _UserJID, _Filter, _HintOffset, _NewOffset,
                       StartId = undefined) ->
    StartId;
maybe_save_offset_hint(PoolName, RoomJID, Filter, HintOffset, NewOffset, StartId) ->
    case abs(NewOffset - HintOffset) > 50 of
        true ->
            #mam_muc_ca_filter{room_jid = FRoomJID, with_nick = FWithNick} = Filter,
            Row = #{room_jid => FRoomJID, with_nick => FWithNick,
                    offset => NewOffset, id => StartId},
            mongoose_cassandra:cql_write(PoolName, RoomJID, ?MODULE,
                                         insert_offset_hint_query, [Row]);
        false ->
            skip
    end,
    StartId.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
-spec calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset) -> Id
                                                                        when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jlib:jid(),
      Offset :: non_neg_integer(),
      Filter :: filter(),
      Id :: non_neg_integer() | undefined.
calc_offset_to_start_id(PoolName, RoomJID, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    QueryName = {list_message_ids_query, select_filter(Filter)},
    Params = maps:put('[limit]', Offset + 1, eval_filter_params(Filter)),
    {ok, RowsIds} = mongoose_cassandra:cql_read(PoolName, RoomJID, ?MODULE, QueryName, Params),
    case RowsIds of
        [] -> undefined;
        [_ | _] ->
            maps:get(id, lists:last(RowsIds))
    end.

%% @doc Get closest offset -> message id 'hint' for specified offset
prev_offset_query_cql() ->
    "SELECT id, offset FROM mam_muc_message_offset WHERE room_jid = ? and with_nick = ?"
    " and offset <= ? LIMIT 1".

%% @doc Insert offset -> message id 'hint'
insert_offset_hint_query_cql() ->
    "INSERT INTO mam_muc_message_offset(room_jid, with_nick, id, offset) VALUES(?, ?, ?, ?)".

prepare_filter(RoomJID, Borders, Start, End, WithNick) ->
    BRoomJID = bare_jid(RoomJID),
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2 = apply_end_border(Borders, EndID),
    BWithNick = maybe_nick(WithNick),
    prepare_filter_params(BRoomJID, BWithNick, StartID2, EndID2).

prepare_filter_params(BRoomJID, BWithNick, StartID, EndID) ->
    #mam_muc_ca_filter{
       room_jid  = BRoomJID,
       with_nick = BWithNick,
       start_id  = StartID,
       end_id    = EndID
      }.

eval_filter_params(#mam_muc_ca_filter{
                      room_jid  = BRoomJID,
                      with_nick = BWithNick,
                      start_id  = StartID,
                      end_id    = EndID
                     }) ->
    Optional = maps:filter(fun(_K, V) -> V =/= undefined end,
                           #{start_id => StartID, end_id =>EndID}),
    maps:merge(#{room_jid => BRoomJID, with_nick => BWithNick}, Optional).

select_filter(#mam_muc_ca_filter{
                 start_id = StartID,
                 end_id   = EndID
                }) ->
    select_filter(StartID, EndID).


-spec select_filter(StartID, EndID) ->
                           all  | 'end'  | start  | start_end when
      StartID :: integer()  | undefined,
      EndID :: integer()  | undefined.
select_filter(undefined, undefined) ->
    all;
select_filter(undefined, _) ->
    'end';
select_filter(_, undefined) ->
    start;
select_filter(_, _) ->
    start_end.

prepare_filter_cql(StartID, EndID) ->
    case StartID of
        undefined -> "";
        _ -> " AND id >= :start_id"
    end ++
        case EndID of
            undefined -> "";
            _ -> " AND id <= :end_id"
        end.

filter_to_cql() ->
    [{select_filter(StartID, EndID),
      prepare_filter_cql(StartID, EndID)}
     || StartID <- [undefined, 0],
        EndID <- [undefined, 0]].

-spec calc_offset(PoolName, RoomJID, Host, Filter, PageSize, TotalCount, RSM) -> Offset
                                                                                     when
      PoolName :: mongoose_cassandra:pool_name(),
      RoomJID :: jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      PageSize :: non_neg_integer(),
      TotalCount :: non_neg_integer(),
      RSM :: rsm_in() | undefined,
      Offset :: non_neg_integer().
%% Requesting the Last Page in a Result Set
calc_offset(_W, _RoomJID, _LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(PoolName, RoomJID, Host, F, PS, _TC, #rsm_in{direction = before, id = ID})
  when is_integer(ID) ->
    max(0, calc_before(PoolName, RoomJID, Host, F, ID) - PS);
calc_offset(PoolName, RoomJID, Host, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
  when is_integer(ID) ->
    calc_index(PoolName, RoomJID, Host, F, ID);
calc_offset(_W, _RoomJID, _LS, _F, _PS, _TC, _RSM) ->
    0.

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

maybe_nick(undefined) ->
    <<>>;
maybe_nick(WithNick) when is_binary(WithNick) ->
    WithNick.

bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_binary(jid:to_bare(jid:to_lower(JID))).


%%====================================================================
%% Internal SQL part
%%====================================================================

extract_messages_queries() ->
    [{{extract_messages_query, FilterName}, extract_messages_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

extract_messages_r_queries() ->
    [{{extract_messages_r_query, FilterName}, extract_messages_r_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

calc_count_queries() ->
    [{{calc_count_query, FilterName}, calc_count_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

list_message_ids_queries() ->
    [{{list_message_ids_query, FilterName}, list_message_ids_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

extract_messages_cql(Filter) ->
    "SELECT id, nick_name, message FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++
        Filter ++ " ORDER BY id LIMIT ?".

extract_messages_r_cql(Filter) ->
    "SELECT id, nick_name, message FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++
        Filter ++ " ORDER BY id DESC LIMIT ?".

calc_count_cql(Filter) ->
    "SELECT COUNT(*) FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++ Filter.

list_message_ids_cql(Filter) ->
    "SELECT id FROM mam_muc_message "
        "WHERE room_jid = ? AND with_nick = ? " ++ Filter ++
        " ORDER BY id LIMIT ?".

%% ----------------------------------------------------------------------
%% Optimizations

packet_to_stored_binary(Packet) ->
    %% Module implementing mam_muc_message behaviour
    Module = db_message_format(),
    Module:encode(Packet).

stored_binary_to_packet(Bin) ->
    %% Module implementing mam_muc_message behaviour
    Module = db_message_format(),
    Module:decode(Bin).

%% ----------------------------------------------------------------------
%% Dynamic params module

%% compile_params_module([
%%      {db_message_format, module()}
%%      ])
compile_params_module(Params) ->
    CodeStr = params_helper(expand_simple_param(Params)),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_mam_muc_cassandra_arch_params.erl", Code).

expand_simple_param(Params) ->
    lists:flatmap(fun(simple) -> simple_params();
                     ({simple, true}) -> simple_params();
                     (Param) -> [Param]
                  end, Params).

simple_params() ->
    [{db_message_format, mam_muc_message_xml}].

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
                                      "-module(mod_mam_muc_cassandra_arch_params).~n"
                                      "-compile(export_all).~n"
                                      "db_message_format() -> ~p.~n"
                                      "pool_name() -> ~p.~n",
                                      [proplists:get_value(db_message_format, Params,
                                                           mam_message_compressed_eterm),
                                       proplists:get_value(pool_name, Params, default)
                                      ]))).

-spec db_message_format() -> module().
db_message_format() ->
    mod_mam_muc_cassandra_arch_params:db_message_format().

-spec pool_name(jid()) -> term().
pool_name(_UserJid) ->
    mod_mam_muc_cassandra_arch_params:pool_name().
