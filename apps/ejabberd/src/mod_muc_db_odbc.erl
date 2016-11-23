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
    SHost = ejabberd_odbc:escape(Host),
    SName = ejabberd_odbc:escape(Name),
    BinOpts = jlib:term_to_expr(Opts),
    SBinOpts = ejabberd_odbc:escape(BinOpts),
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
          odbc_queries:update_t(<<"muc_room">>,
               [<<"name">>, <<"host">>, <<"opts">>],
               [SName, SHost, SBinOpts],
               [<<"name='">>, SName, <<"' AND host='">>, SHost, <<"'">>])
      end).

restore_room(LServer, Host, Name) ->
    SHost = ejabberd_odbc:escape(Host),
    SName = ejabberd_odbc:escape(Name),
    Query = ["select opts from muc_room "
              "where name='", SName, "' "
                "and host='", SHost, "'"],
    case (catch ejabberd_odbc:sql_query(LServer, Query)) of
    {selected, _, [{Opts}]} ->
            jlib:expr_to_term(Opts);
    _ ->
        error
    end.

forget_room(LServer, Host, Name) ->
    SHost = ejabberd_odbc:escape(Host),
    SName = ejabberd_odbc:escape(Name),
    Query = ["delete from muc_room "
              "where name='", SName, "' "
                "and host='", SHost, "'"],
    F = fun () ->
        ejabberd_odbc:sql_query_t(Query)
    end,
    ejabberd_odbc:sql_transaction(LServer, F).

can_use_nick(LServer, Host, JID, Nick) ->
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(JID))),
    SHost = ejabberd_odbc:escape(Host),
    SNick = ejabberd_odbc:escape(Nick),
    Query = ["select jid from muc_registered "
              "where nick='", SNick, "' "
                "and host='", SHost, "'"],
    case catch ejabberd_odbc:sql_query(LServer, Query) of
    {selected, _, [{DbBinJID}]} -> BinJID == DbBinJID;
    _ -> true
    end.

get_rooms(LServer, Host) ->
    SHost = ejabberd_odbc:escape(Host),
    Query = ["select name, opts from muc_room"
              " where host='", SHost, "'"],
    case catch ejabberd_odbc:sql_query(LServer, Query) of
    {selected, _, RoomOpts} ->
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
    SHost = ejabberd_odbc:escape(Host),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    SBinJID = ejabberd_odbc:escape(BinJID),
    Query = ["select nick from muc_registered where"
              " jid='", SBinJID, "' and host='", SHost, "'"],
    case catch ejabberd_odbc:sql_query(LServer, Query) of
    {selected, _, [{Nick}]} -> Nick;
    _ -> error
    end.

set_nick(LServer, Host, From, <<>>) ->
    unset_nick(LServer, Host, From);
set_nick(LServer, Host, From, Nick) ->
    SHost = ejabberd_odbc:escape(Host),
    SNick = ejabberd_odbc:escape(Nick),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    JidQuery = ["select jid from muc_registered "
                   " where nick='", SNick, "'"
                     " and host='", SHost, "'"],

    F = fun () ->
        case ejabberd_odbc:sql_query_t(JidQuery) of
            {selected, _, [{J}]} when J == BinJID ->
                %% Already inserted
                ok;
            {selected, _, [{_}]} ->
                %% Busy nick
                false;
            {selected, _, []} ->
                %% Available nick
                SBinJID = ejabberd_odbc:escape(BinJID),
                InsQuery = ["insert into muc_registered (jid, host, nick) "
                             " values ('", SBinJID, "', "
                                      "'", SHost, "', "
                                      "'", SNick, "')"],
                ejabberd_odbc:sql_query_t(InsQuery),
                ok
        end
    end,
    ejabberd_odbc:sql_transaction(LServer, F).


unset_nick(LServer, Host, From) ->
    SHost = ejabberd_odbc:escape(Host),
    BinJID = jid:to_binary(jid:to_lower(jid:to_bare(From))),
    SBinJID = ejabberd_odbc:escape(BinJID),
    Query = ["delete from muc_registered "
              "where jid='", SBinJID, "' "
                "and host='", SHost, "'"],
    F = fun () ->
            ejabberd_odbc:sql_query_t(Query),
            ok
    end,
    ejabberd_odbc:sql_transaction(LServer, F).
