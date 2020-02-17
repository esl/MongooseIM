-module(mod_muc_db_rdbms).
-include("mod_muc.hrl").
-include("mongoose_logger.hrl").

-export([init/2,
         store_room/4,
         restore_room/3,
         forget_room/3,
         get_rooms/2,
         can_use_nick/4,
         get_nick/3,
         set_nick/4,
         unset_nick/3
        ]).

-define(ESC(T), mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(T))).

%% Defines which RDBMS pool to use
%% Parent host of the MUC service
-type server_host() :: ejabberd:server().

%% Host of MUC service
-type muc_host() :: ejabberd:server().

%% User's JID. Can be on another domain accessable over FED.
%% Only bare part (user@host) is important.
-type client_jid() :: ejabberd:jid().

-type room_opts() :: [{OptionName :: atom(), OptionValue :: term()}].

-type aff() :: atom().


%% ------------------------ Conversions ------------------------

-spec aff_atom2db(aff()) -> string().
aff_atom2db(owner) -> "1";
aff_atom2db({owner, _}) -> "1";
aff_atom2db(member) -> "2";
aff_atom2db({member, _}) -> "2";
aff_atom2db(admin) -> "3";
aff_atom2db({admin, _}) -> "3";
aff_atom2db(outcast) -> "4";
aff_atom2db({outcast, _}) -> "4";
aff_atom2db(_Other) -> "5".

-spec aff_db2atom(binary() | pos_integer()) -> aff().
aff_db2atom(1) -> owner;
aff_db2atom(2) -> member;
aff_db2atom(3) -> admin;
aff_db2atom(4) -> outcast;
aff_db2atom(5) -> none;
aff_db2atom(Bin) -> aff_db2atom(mongoose_rdbms:result_to_integer(Bin)).

-spec bin(integer() | binary()) -> binary().
bin(Int) when is_integer(Int) -> integer_to_binary(Int);
bin(Bin) when is_binary(Bin) -> Bin.

-spec init(server_host(), ModuleOpts :: list()) -> ok.
init(ServerHost, _Opts) ->
    rdbms_queries:prepare_upsert(ServerHost, muc_nick_upsert, muc_registered,
                                 [<<"muc_host">>, <<"luser">>, <<"lserver">>, <<"nick">>],
                                 [<<"nick">>],
                                 [<<"muc_host">>, <<"luser">>, <<"lserver">>]),
    ok.

-spec store_room(server_host(), muc_host(), mod_muc:room(), room_opts()) ->
    ok | {error, term()}.
store_room(ServerHost, MucHost, RoomName, Opts) ->
    forget_room(ServerHost, MucHost, RoomName),
    Affs = proplists:get_value(affiliations, Opts),
    NewOpts = proplists:delete(affiliations, Opts),
    EncodedOpts = jiffy:encode({NewOpts}),
    {atomic, Res} = mongoose_rdbms:sql_transaction(
        ServerHost, fun() ->
        store_room_transaction(ServerHost, MucHost, RoomName, EncodedOpts, Affs) end),
    Res.

store_room_transaction(ServerHost, MucHost, RoomName, Opts, Affs) ->
    InsertRoom = insert_room(MucHost, RoomName, Opts),
    case catch mongoose_rdbms:sql_query_t(InsertRoom) of
        {aborted, Reason} ->
            mongoose_rdbms:sql_query_t("ROLLBACK;"),
            {error, Reason};
        {updated, _ } ->
            {selected, [{RoomID}]} = mongoose_rdbms:sql_query(ServerHost,
                                          select_room_id(MucHost, RoomName)),
            store_aff(RoomID, Affs),
            ok;
        _Other ->
                mongoose_rdbms:sql_query_t("ROLLBACK;"),
                {error, unexpected}
    end.

store_aff( _, undefined) ->
     ok;
store_aff(RoomID, Affs) ->
    lists:foreach(
        fun({{UserU, UserS, Resource}, Aff}) ->
            {updated, _} = mongoose_rdbms:sql_query_t(
                insert_aff(RoomID, UserU, UserS, Resource, Aff))
            end, Affs).

get_full_options(ServerHost, Opts, RoomID) ->
    {selected, Affs} = mongoose_rdbms:sql_query(ServerHost, select_aff(RoomID)),
    {DecodedOpts} = jiffy:decode(Opts),
    RestoredOpts = [{binary_to_existing_atom(Key, utf8), Value}||{Key, Value} <- DecodedOpts],
    AffsList = [{{UserU, UserS, Res}, aff_db2atom(Aff)} || {UserU, UserS, Res, Aff} <- Affs],
    lists:flatten([ RestoredOpts| [{affiliations, AffsList}]]).

-spec restore_room(server_host(), muc_host(), mod_muc:room()) ->
    {ok, room_opts()} | {error, room_not_found} | {error, term()}.
restore_room(ServerHost, MucHost, RoomName) ->
    case mongoose_rdbms:sql_query(ServerHost, select_room(MucHost, RoomName)) of
        {selected, [{RoomID, Opts}]} ->
                    FullOpts = get_full_options(ServerHost, Opts, RoomID),
                    {ok, FullOpts};
        {selected, []} ->
            {error, room_not_found};
        Other ->
            {error, Other}
     end.

-spec forget_room(server_host(), muc_host(), mod_muc:room()) ->
    ok | {error, term()}.
forget_room(ServerHost, MucHost, RoomName) ->
    {atomic, _Res}
        = mongoose_rdbms:sql_transaction(ServerHost,
            fun() -> forget_room_transaction(ServerHost, MucHost, RoomName) end),
    ok.

forget_room_transaction(ServerHost, MucHost, RoomName) ->
    case mongoose_rdbms:sql_query(ServerHost, select_room_id( MucHost, RoomName)) of
        {selected, [{RoomID}]} ->
            {updated, _} = mongoose_rdbms:sql_query_t(delete_affs(RoomID)),
            {updated, _} = mongoose_rdbms:sql_query_t(delete_room(MucHost, RoomName)),
            ok;
        {selected, []} ->
            {error, not_exists}
    end.

-spec get_rooms(server_host(), muc_host()) -> {ok, [#muc_room{}]} | {error, term()}.
get_rooms(ServerHost, MucHost) ->
    SelectRooms = select_rooms(MucHost),
    case mongoose_rdbms:sql_query(ServerHost, SelectRooms) of
        {selected, Reply} ->
            Return = [#muc_room{name_host = {RoomName, MucHost},
                                opts = get_full_options(ServerHost, Opts, RoomID)}
                                || {RoomID, RoomName, Opts} <- Reply],
            {ok, Return};
        Other -> {error, Other}
    end.

-spec can_use_nick(server_host(), muc_host(), client_jid(), mod_muc:nick()) ->
    boolean().
can_use_nick(ServerHost, MucHost, Jid, Nick) ->
    {UserU, UserS} = jid:to_lus(Jid),
    SelectQuery = select_nick_user(MucHost, UserS, Nick),
    case mongoose_rdbms:sql_query(ServerHost, SelectQuery) of
        {selected, []} -> true;
        {selected, [{U}]} -> U == UserU
    end.

%% Get nick associated with jid client_jid() across muc_host() domain
-spec get_nick(server_host(), muc_host(), client_jid()) ->
    {ok, mod_muc:nick()} | {error, not_registered} | {error, term()}.
get_nick(ServerHost, MucHost, Jid) ->
    {UserU, UserS} = jid:to_lus(Jid),
    SelectQuery = select_nick(MucHost, UserU, UserS),
    case mongoose_rdbms:sql_query(ServerHost, SelectQuery) of
        {selected, []} -> {error, not_registered};
        {selected, [{Nick}]} -> {ok, Nick}
    end.

%% Register nick
-spec set_nick(server_host(), muc_host(), client_jid(), mod_muc:nick()) -> ok | {error, term()}.
set_nick(ServerHost, MucHost, Jid, Nick) when is_binary(Nick), Nick =/= <<>> ->
    CanUseNick = can_use_nick(ServerHost, MucHost, Jid, Nick),
    store_nick_transaction(ServerHost, MucHost, Jid, Nick, CanUseNick).

store_nick_transaction(_ServerHost, _MucHost, _Jid, _Nick, false) ->
    {error, conflict};
store_nick_transaction(ServerHost, MucHost, Jid, Nick, true) ->
    {LU, LS} = jid:to_lus(Jid),
    InsertParams = [MucHost, LU, LS, Nick],
    UpdateParams = [Nick],
    UniqueKeyValues  = [MucHost, LU, LS],
    case rdbms_queries:execute_upsert(ServerHost, muc_nick_upsert,
                                       InsertParams, UpdateParams, UniqueKeyValues) of
        {updated, _} -> ok;
        Error -> Error
    end.

%% Unregister nick
%% Unregistered nicks can be used by someone else
-spec unset_nick(server_host(), muc_host(), client_jid()) -> ok | {error, term()}.
unset_nick(ServerHost, MucHost, Jid) ->
    {UserU, UserS} = jid:to_lus(Jid),
    case mongoose_rdbms:sql_query(ServerHost, delete_nick(MucHost, UserU, UserS)) of
        {updated, N} when 0 =< N, N =< 1 -> ok;
        T -> {error, T}
    end.

insert_room(MucHost, RoomName, Opts) ->
    ["INSERT INTO muc_rooms (muc_host, room, options)"
     " VALUES (", ?ESC(MucHost), ", ", ?ESC(RoomName), ",", ?ESC(Opts) ,")"].

-spec insert_aff(RoomID :: integer() | binary(), UserU :: jid:luser(),
                 UserS :: jid:lserver(), Res ::iolist(), Aff :: aff()) -> iolist().
insert_aff(RoomID, UserU, UserS, Res, Aff) ->
    ["INSERT INTO muc_room_aff (room_id, luser, lserver, resource, aff)"
     " VALUES(", bin(RoomID), ", ", ?ESC(UserU), ", ", ?ESC(UserS), ", ",
      ?ESC(Res), ", ", aff_atom2db(Aff), ")"].

select_aff(RoomID) ->
    ["SELECT luser, lserver, resource, aff FROM muc_room_aff WHERE room_id = ",
     bin(RoomID)].

select_room_id(MucHost, RoomName) ->
    ["SELECT id FROM muc_rooms WHERE muc_host = ", ?ESC(MucHost),
    " AND room = ", ?ESC(RoomName)].

select_room(MucHost, RoomName) ->
    ["SELECT id, options FROM muc_rooms WHERE muc_host = ", ?ESC(MucHost),
    " AND room = ", ?ESC(RoomName)].

delete_affs(RoomID) ->
     ["DELETE FROM muc_room_aff WHERE room_id = ", bin(RoomID)].

delete_room(MucHost, RoomName) ->
    ["DELETE FROM muc_rooms"
     " WHERE muc_host = ", ?ESC(MucHost), " AND room = ", ?ESC(RoomName)].

select_rooms(MucHost) ->
    ["SELECT id, room, options FROM muc_rooms WHERE muc_host = ", ?ESC(MucHost)].

select_nick_user(MucHost, UserS, Nick) ->
    ["SELECT luser FROM muc_registered WHERE muc_host = ", ?ESC(MucHost),
    " AND lserver = ", ?ESC(UserS), "AND nick =", ?ESC(Nick)].

select_nick(MucHost, UserU, UserS) ->
    ["SELECT nick FROM muc_registered WHERE muc_host = ", ?ESC(MucHost),
    " AND luser = ", ?ESC(UserU), "AND lserver =", ?ESC(UserS)].

delete_nick(MucHost, UserU, UserS) ->
    ["DELETE FROM muc_registered WHERE muc_host = ", ?ESC(MucHost),
     " AND luser = ", ?ESC(UserU), "AND lserver =", ?ESC(UserS)].
