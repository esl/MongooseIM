%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 16:59
%%%-------------------------------------------------------------------
-module(mod_inbox_odbc).
-author("ludwikbukowski").
-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_inbox.hrl").

%% API
-export([get_inbox/2,
         init/2,
         set_inbox/6,
         set_inbox_incr_unread/5,
         reset_unread/4,
         remove_inbox/3,
         clear_inbox/2]).

%% For specific backends
-export([esc_string/1]).

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

init(VHost, _Options) ->
    %% To verify if current ODBC backend is supported
    odbc_specific_backend(VHost),
    ok.

-spec get_inbox(LUsername :: jid:luser(),
                LServer :: jid:lserver()) -> get_inbox_res().
get_inbox(LUsername, LServer) ->
    case get_inbox_rdbms(LUsername, LServer) of
        {selected, []} ->
            [];
        {selected, Res} ->
            [decode_row(LServer, R) || R <- Res]
    end.


-spec get_inbox_rdbms(LUser :: jid:luser(), Server :: jid:lserver()) -> query_result().
get_inbox_rdbms(LUser, Server) ->
    mongoose_rdbms:sql_query(
        Server,
        ["select remote_bare_jid, content, unread_count from inbox "
        "where luser=", esc_string(LUser), " and lserver=", esc_string(Server), ";"]).

-spec set_inbox(Username, Server, ToBareJid, Content, Count, MsgId) -> inbox_write_res() when
                Username :: jid:luser(),
                Server :: jid:lserver(),
                ToBareJid :: binary(),
                Content :: binary(),
                Count :: binary(),
                MsgId :: binary().
set_inbox(Username, Server, ToBareJid, Content, Count, MsgId) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    BackendModule = odbc_specific_backend(Server),
    Res = BackendModule:set_inbox(LUsername, LServer, LToBareJid, Content, Count, MsgId),
    ok = check_result(Res, 1).

-spec remove_inbox(User :: binary(),
    Server :: binary(),
    ToBareJid :: binary()) -> ok.
remove_inbox(Username, Server, ToBareJid) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    Res = remove_inbox_rdbms(LUsername, LServer, LToBareJid),
    check_result(Res).

-spec remove_inbox_rdbms(Username :: jid:luser(),
                         Server :: jid:lserver(),
                         ToBareJid :: binary()) -> query_result().
remove_inbox_rdbms(Username, Server, ToBareJid) ->
    mongoose_rdbms:sql_query(Server, ["delete from inbox where luser=",
        esc_string(Username), " and lserver=", esc_string(Server),
        " and remote_bare_jid=",
        esc_string(ToBareJid), ";"]).

-spec set_inbox_incr_unread(Username :: binary(),
                            Server :: binary(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary()) -> ok.
set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    BackendModule = odbc_specific_backend(Server),
    Res = BackendModule:set_inbox_incr_unread(LUsername, LServer, LToBareJid, Content, MsgId),
    %% psql will always return {updated, 1} but mysql will return {updated, 2} if it overwrites the row
    check_result(Res,[1,2]).

-spec reset_unread(User :: binary(),
                   Server :: binary(),
                   BareJid :: binary(),
                   MsgId :: binary()) -> ok.
reset_unread(Username, Server, ToBareJid, MsgId) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    Res = reset_inbox_unread_rdbms(LUsername, LServer, LToBareJid, MsgId),
    check_result(Res).

-spec reset_inbox_unread_rdbms(Username :: jid:luser(),
                               Server :: jid:lserver(),
                               ToBareJid :: binary(),
                               MsgId :: binary()) -> query_result().
reset_inbox_unread_rdbms(Username, Server, ToBareJid, MsgId) ->
    mongoose_rdbms:sql_query(Server, ["update inbox set unread_count=0 where luser=",
        esc_string(Username), " and lserver=", esc_string(Server), " and remote_bare_jid=",
        esc_string(ToBareJid), " and msg_id=", esc_string(MsgId), ";"]).

-spec clear_inbox(Username :: binary(), Server :: binary()) -> ok.
clear_inbox(Username, Server) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    Res = clear_inbox_rdbms(LUsername, LServer),
    check_result(Res).

-spec esc_string(binary() | string()) -> mongoose_rdbms:sql_query_part().
esc_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec clear_inbox_rdbms(Username :: jid:luser(), Server :: jid:lserver()) -> query_result().
clear_inbox_rdbms(Username, Server) ->
    mongoose_rdbms:sql_query(Server, ["delete from inbox where luser=",
        esc_string(Username), " and lserver=", esc_string(Server), ";"]).

-spec decode_row(host(), {username(), binary(), count()}) -> inbox_res().
decode_row(LServer, {Username, Content, Count}) ->
    Pool = mongoose_rdbms_sup:pool(LServer),
    Data = mongoose_rdbms:unescape_binary(Pool, Content),
    BCount = count_to_bin(Count),
    {Username, Data, BCount}.


odbc_specific_backend(Host) ->
    case {mongoose_rdbms:db_engine(Host), mongoose_rdbms_type:get()} of
        {mysql, _} -> mod_inbox_odbc_mysql;
        {pgsql, _} -> mod_inbox_odbc_pgsql;
        {odbc, mssql} -> mod_inbox_odbc_mssql;
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

count_to_bin(Count) when is_integer(Count) -> integer_to_binary(Count);
count_to_bin(Count) when is_binary(Count) -> Count.

check_result({updated, Val}, ValList) when is_list(ValList) ->
    case lists:member(Val, ValList) of
        true ->
            ok;
        _ ->
            {error, {expected_does_not_match, Val, ValList}}
    end;
check_result({updated, Val}, Val) ->
    ok;
check_result({updated, Res}, Exp) ->
    {error, {expected_does_not_match, Exp, Res}};
check_result(Result, _) ->
    {error, {bad_result, Result}}.

check_result({updated, _}) ->
    ok;
check_result(Result) ->
    {error, {bad_result, Result}}.
