-module(mod_muc_db_odbc).
-behaviour(mod_muc_db).
-export([init/2,
         store_room/4,
         restore_room/3,
         forget_room/3,
         get_rooms/2,
         can_use_nick/4,
         get_nick/3,
         set_nick/4,
         unset_nick/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("ejabberd/include/mod_muc.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Host, _Opts) ->
    ok.

store_room(ServerHost, MucHost, RoomName, Opts) ->
    SHost = mongoose_rdbms:escape(MucHost),
    SName = mongoose_rdbms:escape(RoomName),
    BinOpts = jlib:term_to_expr(Opts),
    SBinOpts = mongoose_rdbms:escape(BinOpts),
    Result = mongoose_rdbms:sql_transaction(
      ServerHost,
      fun() ->
          rdbms_queries:update_t(<<"muc_room">>,
               [<<"name">>, <<"host">>, <<"opts">>],
               [SName, SHost, SBinOpts],
               [<<"name='">>, SName, <<"' AND host='">>, SHost, <<"'">>])
      end),
    case Result of
        {atomic, _} ->
            ok;
        _ ->
            ?ERROR_MSG("issue=store_room_failed room=~ts reason=~1000p",
                       [RoomName, Result])
    end,
    Result.

restore_room(ServerHost, MucHost, RoomName) ->
    SHost = mongoose_rdbms:escape(MucHost),
    SName = mongoose_rdbms:escape(RoomName),
    Query = ["select opts from muc_room "
              "where name='", SName, "' "
                "and host='", SHost, "'"],
    case (catch mongoose_rdbms:sql_query(ServerHost, Query)) of
        {selected, [{Opts}]} ->
            {ok, jlib:expr_to_term(Opts)};
        {selected, []} ->
            {error, room_not_found};
        Reason ->
            ?ERROR_MSG("issue=restore_room_failed room=~ts reason=~1000p",
                       [RoomName, Reason]),
            {error, Reason}
    end.

forget_room(ServerHost, MucHost, RoomName) ->
    SHost = mongoose_rdbms:escape(MucHost),
    SName = mongoose_rdbms:escape(RoomName),
    Query = ["delete from muc_room "
              "where name='", SName, "' "
                "and host='", SHost, "'"],
    F = fun () ->
        mongoose_rdbms:sql_query_t(Query)
    end,
    Result = mongoose_rdbms:sql_transaction(ServerHost, F),
    case Result of
        {atomic, _} ->
            ok;
        _ ->
            ?ERROR_MSG("issue=forget_room_failed room=~ts reason=~1000p",
                       [RoomName, Result])
    end,
    Result.

can_use_nick(ServerHost, MucHost, JID, Nick) ->
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(JID))),
    SHost = mongoose_rdbms:escape(MucHost),
    SNick = mongoose_rdbms:escape(Nick),
    Query = ["select jid from muc_registered "
              "where nick='", SNick, "' "
                "and host='", SHost, "'"],
    case catch mongoose_rdbms:sql_query(ServerHost, Query) of
        {selected, [{DbBinJID}]} ->
            BinJID == DbBinJID;
        {selected, []} ->
            true;
        Error ->
            ?ERROR_MSG("issue=can_use_nick_failed jid=~ts nick=~ts reason=~1000p",
                       [BinJID, Nick, Error]),
            false
    end.

get_rooms(ServerHost, MucHost) ->
    SHost = mongoose_rdbms:escape(MucHost),
    Query = ["select name, opts from muc_room"
              " where host='", SHost, "'"],
    case catch mongoose_rdbms:sql_query(ServerHost, Query) of
    {selected, RoomOpts} ->
        lists:map(
          fun({Room, Opts}) ->
              #muc_room{name_host = {Room, MucHost},
                opts = jlib:expr_to_term(Opts)}
          end, RoomOpts);
    Err ->
        ?ERROR_MSG("issue=get_rooms_failed reason=~1000p", [Err]),
        []
    end.

get_nick(ServerHost, MucHost, From) ->
    SHost = mongoose_rdbms:escape(MucHost),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    SBinJID = mongoose_rdbms:escape(BinJID),
    Query = ["select nick from muc_registered where"
              " jid='", SBinJID, "' and host='", SHost, "'"],
    case catch mongoose_rdbms:sql_query(ServerHost, Query) of
        {selected, [{Nick}]} ->
            {ok, Nick};
        {selected, []} ->
            %% not found
            {error, not_registered};
        Error ->
            ?ERROR_MSG("issue=get_nick_failed jid=~ts reason=~1000p",
                       [BinJID, Error]),
            {error, Error}
    end.

set_nick(_ServerHost, _MucHost, _From, <<>>) ->
    {error, should_not_be_empty};
set_nick(ServerHost, MucHost, From, Nick) ->
    SHost = mongoose_rdbms:escape(MucHost),
    SNick = mongoose_rdbms:escape(Nick),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    SBinJID = mongoose_rdbms:escape(BinJID),
    JidQuery = ["select jid from muc_registered "
                   " where nick='", SNick, "'"
                     " and host='", SHost, "'"],
    DelQuery = ["delete from muc_registered "
              "where jid='", SBinJID, "' "
                "and host='", SHost, "'"],
    F = fun () ->
        case mongoose_rdbms:sql_query_t(JidQuery) of
            {selected, [{J}]} when J == BinJID ->
                %% Already inserted
                ok;
            {selected, [{_}]} ->
                %% Busy nick
                {error, conflict};
            {selected, []} ->
                mongoose_rdbms:sql_query_t(DelQuery), %% unset old nick
                %% Available nick
                SBinJID = mongoose_rdbms:escape(BinJID),
                InsQuery = ["insert into muc_registered (jid, host, nick) "
                             " values ('", SBinJID, "', "
                                      "'", SHost, "', "
                                      "'", SNick, "')"],
                mongoose_rdbms:sql_query_t(InsQuery),
                ok
        end
    end,
    case mongoose_rdbms:sql_transaction(ServerHost, F) of
        {atomic, Result} ->
            Result;
        ErrorResult ->
            ?ERROR_MSG("issue=set_nick_failed jid=~ts nick=~ts reason=~1000p",
                       [BinJID, Nick, ErrorResult]),
            {error, ErrorResult}
    end.


unset_nick(ServerHost, MucHost, From) ->
    SHost = mongoose_rdbms:escape(MucHost),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    SBinJID = mongoose_rdbms:escape(BinJID),
    Query = ["delete from muc_registered "
              "where jid='", SBinJID, "' "
                "and host='", SHost, "'"],
    F = fun () -> mongoose_rdbms:sql_query_t(Query) end,
    case mongoose_rdbms:sql_transaction(ServerHost, F) of
        {atomic, _} ->
            ok;
        ErrorResult ->
            ?ERROR_MSG("issue=unset_nick_failed jid=~ts reason=~1000p",
                       [BinJID, ErrorResult]),
            {error, ErrorResult}
    end.
