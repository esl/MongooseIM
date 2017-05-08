-module(mod_muc_db_odbc).
-behaviour(mod_muc_db).
-export([init/2,
         store_room/4,
         restore_room/3,
         forget_room/3,
         get_rooms/2,
         can_use_nick/4,
         get_nick/3,
         set_nick/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("ejabberd/include/mod_muc.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Host, _Opts) ->
    ok.

store_room(LServer, Host, Name, Opts) ->
    SHost = mongoose_rdbms:escape(Host),
    SName = mongoose_rdbms:escape(Name),
    BinOpts = jlib:term_to_expr(Opts),
    SBinOpts = mongoose_rdbms:escape(BinOpts),
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
          rdbms_queries:update_t(<<"muc_room">>,
               [<<"name">>, <<"host">>, <<"opts">>],
               [SName, SHost, SBinOpts],
               [<<"name='">>, SName, <<"' AND host='">>, SHost, <<"'">>])
      end).

restore_room(LServer, Host, Name) ->
    SHost = mongoose_rdbms:escape(Host),
    SName = mongoose_rdbms:escape(Name),
    Query = ["select opts from muc_room "
              "where name='", SName, "' "
                "and host='", SHost, "'"],
    case (catch mongoose_rdbms:sql_query(LServer, Query)) of
    {selected, [{Opts}]} ->
            jlib:expr_to_term(Opts);
    {selected, []} ->
            error;
    Reason ->
        ?ERROR_MSG("issue=restore_room_failed room=~ts reason=~1000p",
                   [Name, Reason]),
        error
    end.

forget_room(LServer, Host, Name) ->
    SHost = mongoose_rdbms:escape(Host),
    SName = mongoose_rdbms:escape(Name),
    Query = ["delete from muc_room "
              "where name='", SName, "' "
                "and host='", SHost, "'"],
    F = fun () ->
        mongoose_rdbms:sql_query_t(Query)
    end,
    mongoose_rdbms:sql_transaction(LServer, F).

can_use_nick(LServer, Host, JID, Nick) ->
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(JID))),
    SHost = mongoose_rdbms:escape(Host),
    SNick = mongoose_rdbms:escape(Nick),
    Query = ["select jid from muc_registered "
              "where nick='", SNick, "' "
                "and host='", SHost, "'"],
    case catch mongoose_rdbms:sql_query(LServer, Query) of
    {selected, [{DbBinJID}]} -> BinJID == DbBinJID;
    _ -> true
    end.

get_rooms(LServer, Host) ->
    SHost = mongoose_rdbms:escape(Host),
    Query = ["select name, opts from muc_room"
              " where host='", SHost, "'"],
    case catch mongoose_rdbms:sql_query(LServer, Query) of
    {selected, RoomOpts} ->
        lists:map(
          fun({Room, Opts}) ->
              #muc_room{name_host = {Room, Host},
                opts = jlib:expr_to_term(Opts)}
          end, RoomOpts);
    Err ->
        ?ERROR_MSG("failed to get rooms: ~p", [Err]),
        []
    end.

get_nick(LServer, Host, From) ->
    SHost = mongoose_rdbms:escape(Host),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    SBinJID = mongoose_rdbms:escape(BinJID),
    Query = ["select nick from muc_registered where"
              " jid='", SBinJID, "' and host='", SHost, "'"],
    case catch mongoose_rdbms:sql_query(LServer, Query) of
    {selected, [{Nick}]} -> Nick;
    _ -> error
    end.

set_nick(LServer, Host, From, <<>>) ->
    unset_nick(LServer, Host, From);
set_nick(LServer, Host, From, Nick) ->
    SHost = mongoose_rdbms:escape(Host),
    SNick = mongoose_rdbms:escape(Nick),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    JidQuery = ["select jid from muc_registered "
                   " where nick='", SNick, "'"
                     " and host='", SHost, "'"],

    F = fun () ->
        case mongoose_rdbms:sql_query_t(JidQuery) of
            {selected, [{J}]} when J == BinJID ->
                %% Already inserted
                ok;
            {selected, [{_}]} ->
                %% Busy nick
                false;
            {selected, []} ->
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
    mongoose_rdbms:sql_transaction(LServer, F).


unset_nick(LServer, Host, From) ->
    SHost = mongoose_rdbms:escape(Host),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    SBinJID = mongoose_rdbms:escape(BinJID),
    Query = ["delete from muc_registered "
              "where jid='", SBinJID, "' "
                "and host='", SHost, "'"],
    F = fun () ->
            mongoose_rdbms:sql_query_t(Query),
            ok
    end,
    mongoose_rdbms:sql_transaction(LServer, F).
