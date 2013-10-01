%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_arch).
-export([archive_size/4,
         wait_flushing/4,
         archive_message/9,
         lookup_messages/12,
         remove_archive/4,
         purge_single_message/6,
         purge_multiple_messages/8]).

%% UID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-type filter() :: iolist().
-type message_id() :: non_neg_integer().
-type user_id() :: non_neg_integer().
-type escaped_message_id() :: binary().
-type escaped_jid() :: binary().
-type escaped_resource() :: binary().
-type server_hostname() :: binary().
-type server_host() :: binary().
-type unix_timestamp() :: non_neg_integer().

encode_direction(incoming) -> "I";
encode_direction(outgoing) -> "O".

archive_size(Host, _Mod, UserID, _UserJID) ->
    {selected, _ColumnNames, [{BSize}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) "
       "FROM mam_message "
       "WHERE user_id = '", escape_user_id(UserID), "'"]),
    list_to_integer(binary_to_list(BSize)).

wait_flushing(_Host, _Mod, _UserID, _UserJID) ->
    ok.

archive_message(Host, _Mod, MessID, UserID,
               _LocJID=#jid{},
                RemJID=#jid{lresource=RemLResource},
                SrcJID, Dir, Packet) ->
    SUserID = integer_to_list(UserID),
    SBareRemJID = esc_jid(jlib:jid_tolower(jlib:jid_remove_resource(RemJID))),
    SSrcJID = esc_jid(SrcJID),
    SDir = encode_direction(Dir),
    SRemLResource = ejabberd_odbc:escape(RemLResource),
    Data = term_to_binary(Packet),
    EscFormat = ejabberd_odbc:escape_format(Host),
    SData = ejabberd_odbc:escape_binary(EscFormat, Data),
    SMessID = integer_to_list(MessID),
    write_message(Host, SMessID, SUserID, SBareRemJID,
                  SRemLResource, SDir, SSrcJID, SData).

write_message(Host, SMessID, SUserID, SBareRemJID,
              SRemLResource, SDir, SSrcJID, SData) ->
    {updated, 1} =
    mod_mam_utils:success_sql_query(
      Host,
      ["INSERT INTO mam_message(id, user_id, remote_bare_jid, "
                                "remote_resource, direction, "
                                "from_jid, message) "
       "VALUES ('", SMessID, "', '", SUserID, "', '", SBareRemJID, "', "
               "'", SRemLResource, "', '", SDir, "', ",
               "'", SSrcJID, "', '", SData, "');"]),
    ok.


-spec lookup_messages(Host, _Mod,
                      UserID, UserJID, RSM, Start, End, Now, WithJID,
                      PageSize, LimitPassed, MaxResultLimit) ->
    {ok, {TotalCount, Offset, MessageRows}} | {error, 'policy-violation'}
			     when
    Host    :: server_host(),
    UserJID :: #jid{},
    UserID  :: user_id(),
    RSM     :: #rsm_in{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    PageSize :: non_neg_integer(),
    WithJID :: #jid{} | undefined,
    LimitPassed :: boolean(),
    MaxResultLimit :: non_neg_integer(),
    TotalCount :: non_neg_integer(),
    Offset  :: non_neg_integer(),
    MessageRows :: list(tuple()).
lookup_messages(Host, _Mod, UserID, _UserJID = #jid{},
                RSM, Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit) ->
    Filter = prepare_filter(UserID, Start, End, WithJID),
    TotalCount = calc_count(Host, Filter),
    Offset     = calc_offset(Host, Filter, PageSize, TotalCount, RSM),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client. 
    case TotalCount - Offset > MaxResultLimit andalso not LimitPassed of
        true ->
            {error, 'policy-violation'};

        false ->
            MessageRows = extract_messages(Host, Filter, Offset, PageSize),
            {ok, {TotalCount, Offset, rows_to_uniform_format(Host, MessageRows)}}
    end.


rows_to_uniform_format(Host, MessageRows) ->
    [row_to_uniform_format(Host, Row) || Row <- MessageRows].

row_to_uniform_format(Host, {BMessID,BSrcJID,SData}) ->
    MessID = list_to_integer(binary_to_list(BMessID)),
    SrcJID = jlib:binary_to_jid(BSrcJID),
    EscFormat = ejabberd_odbc:escape_format(Host),
    Data = ejabberd_odbc:unescape_binary(EscFormat, SData),
    Packet = binary_to_term(Data),
    {MessID, SrcJID, Packet}.


remove_archive(Host, _Mod, UserID, _UserJID) ->
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM mam_message "
       "WHERE user_id = '", escape_user_id(UserID), "'"]),
    ok.

-spec purge_single_message(Host, Mod, MessID, UserID, UserJID, Now) ->
    ok | {error, 'not-allowed' | 'not-found'} when
    Host    :: server_host(),
    Mod     :: module(),
    MessID  :: message_id(),
    UserID  :: user_id(),
    UserJID :: #jid{},
    Now     :: unix_timestamp().
purge_single_message(Host, _Mod, MessID, UserID, _UserJID, _Now) ->
    Result =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM mam_message "
       "WHERE user_id = '", escape_user_id(UserID), "' "
       "AND id = '", escape_message_id(MessID), "'"]),
    case Result of
        {updated, 0} -> {error, 'not-found'};
        {updated, 1} -> ok
    end.

-spec purge_multiple_messages(Host, Mod,
                              UserID, UserJID, Start, End, Now, WithJID) ->
    ok | {error, 'not-allowed'} when
    Host    :: server_host(),
    Mod     :: module(),
    UserID  :: user_id(),
    UserJID :: #jid{},
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: #jid{} | undefined.
purge_multiple_messages(Host, _Mod, UserID, _UserJID,
                        Start, End, _Now, WithJID) ->
    Filter = prepare_filter(UserID, Start, End, WithJID),
    {updated, _} =
    mod_mam_utils:success_sql_query(
      Host,
      ["DELETE FROM mam_message ", Filter]),
    ok.

%% Each record is a tuple of form 
%% `{<<"13663125233">>,<<"bob@localhost">>,<<"res1">>,<<binary>>}'.
%% Columns are `["id","from_jid","message"]'.
-spec extract_messages(Host, Filter, IOffset, IMax) ->
    [Record] when
    Host :: server_hostname(),
    Filter  :: filter(),
    IOffset :: non_neg_integer(),
    IMax    :: pos_integer(),
    Record :: tuple().
extract_messages(_Host, _Filter, _IOffset, 0) ->
    [];
extract_messages(Host, Filter, IOffset, IMax) ->
    {selected, _ColumnNames, MessageRows} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT id, from_jid, message "
       "FROM mam_message ",
        Filter,
       " ORDER BY id"
       " LIMIT ", integer_to_list(IMax),
         case IOffset of
             0 -> "";
             _ -> [" OFFSET ", integer_to_list(IOffset)]
         end]),
    ?DEBUG("extract_messages query returns ~p", [MessageRows]),
    MessageRows.

%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
%% "SELECT COUNT(*) as "index" FROM mam_message WHERE id <= '",  UID
-spec calc_index(Host, Filter, SUID) -> Count
    when
    Host  :: server_hostname(),
    Filter   :: filter(),
    SUID     :: escaped_message_id(),
    Count    :: non_neg_integer().
calc_index(Host, Filter, SUID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) FROM mam_message ", Filter, " AND id <= '", SUID, "'"]),
    list_to_integer(binary_to_list(BIndex)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE id < '",  UID
-spec calc_before(Host, Filter, SUID) -> Count
    when
    Host  :: server_hostname(),
    Filter   :: filter(),
    SUID     :: escaped_message_id(),
    Count    :: non_neg_integer().
calc_before(Host, Filter, SUID) ->
    {selected, _ColumnNames, [{BIndex}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) FROM mam_message ", Filter, " AND id < '", SUID, "'"]),
    list_to_integer(binary_to_list(BIndex)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(Host, Filter) -> Count
    when
    Host  :: server_hostname(),
    Filter   :: filter(),
    Count    :: non_neg_integer().
calc_count(Host, Filter) ->
    {selected, _ColumnNames, [{BCount}]} =
    mod_mam_utils:success_sql_query(
      Host,
      ["SELECT COUNT(*) FROM mam_message ", Filter]),
    list_to_integer(binary_to_list(BCount)).


%% prepare_filter/4
-spec prepare_filter(UserID, Start, End, WithJID) -> filter()
    when
    UserID  :: user_id(),
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    WithJID :: #jid{} | undefined.
prepare_filter(UserID, Start, End, WithJID) ->
    {SWithJID, SWithResource} =
    case WithJID of
        undefined -> {undefined, undefined};
        #jid{lresource = <<>>} ->
            {secure_escaped_jid(WithJID), undefined};
        #jid{lresource = WithLResource} ->
            WithBareJID = jlib:jid_remove_resource(WithJID),
            {secure_escaped_jid(WithBareJID),
             ejabberd_odbc:escape(WithLResource)}
    end,
    prepare_filter(UserID, Start, End, SWithJID, SWithResource).

%% prepare_filter/5
-spec prepare_filter(UserID, IStart, IEnd, SWithJID, SWithResource) -> filter()
    when
    UserID  :: non_neg_integer(),
    IStart  :: unix_timestamp() | undefined,
    IEnd    :: unix_timestamp() | undefined,
    SWithJID :: escaped_jid() | undefined,
    SWithResource :: escaped_resource() | undefined.
prepare_filter(UserID, IStart, IEnd, SWithJID, SWithResource) ->
   ["WHERE user_id='", escape_user_id(UserID), "'",
     case IStart of
        undefined -> "";
        _         -> [" AND id >= ",
                      escape_message_id(encode_compact_uuid(IStart, 0))]
     end,
     case IEnd of
        undefined -> "";
        _         -> [" AND id <= ",
                      escape_message_id(encode_compact_uuid(IEnd, 255))]
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
-spec calc_offset(Host, Filter, PageSize, TotalCount, RSM) -> Offset
    when
    Host  :: server_hostname(),
    Filter   :: filter(),
    PageSize :: non_neg_integer(),
    TotalCount :: non_neg_integer(),
    RSM      :: #rsm_in{} | undefined,
    Offset   :: non_neg_integer().
calc_offset(_LS, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
    when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Host, F, PS, _TC, #rsm_in{direction = before, id = ID})
    when is_integer(ID) ->
    SID = escape_message_id(ID),
    max(0, calc_before(Host, F, SID) - PS);
calc_offset(Host, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
    when is_integer(ID) ->
    SID = escape_message_id(ID),
    calc_index(Host, F, SID);
calc_offset(_LS, _F, _PS, _TC, _RSM) ->
    0.

escape_message_id(MessID) when is_integer(MessID) ->
    integer_to_list(MessID).

escape_user_id(UserID) when is_integer(UserID) ->
    integer_to_list(UserID).

secure_escaped_jid(JID) ->
    ejabberd_odbc:escape(jlib:binary_to_jid(JID)).

esc_jid(JID) ->
    ejabberd_odbc:escape(jlib:jid_to_binary(JID)).
