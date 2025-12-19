-module(mod_muc_rdbms).
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
         unset_nick/3,
         remove_domain/3
        ]).

-ignore_xref([can_use_nick/4, forget_room/3, get_nick/3, get_rooms/2, remove_domain/3, init/2,
              restore_room/3, set_nick/4, store_room/4, unset_nick/3]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

%% Host of MUC service
-type muc_host() :: jid:server().

%% User's JID. Can be on another domain accessible over FED.
%% Only bare part (user@host) is important.
-type client_jid() :: jid:jid().
-type room_id() :: pos_integer().
-type room_opts() :: [{OptionName :: atom(), OptionValue :: term()}].
-type aff() :: atom().


-spec init(mongooseim:host_type(), ModuleOpts :: list()) -> ok.
init(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

prepare_queries(HostType) ->
    %% Queries to muc_rooms table
    prepare(muc_insert_room, muc_rooms,
            [muc_host, room_name, options],
            <<"INSERT INTO muc_rooms (muc_host, room_name, options)"
              " VALUES (?, ?, ?)">>),
    prepare(muc_select_room_id, muc_rooms,
            [muc_host, room_name],
            <<"SELECT id FROM muc_rooms "
              "WHERE muc_host = ? AND room_name = ?">>),
    prepare(muc_select_room, muc_rooms,
            [muc_host, room_name],
            <<"SELECT id, options FROM muc_rooms "
              "WHERE muc_host = ? AND room_name = ?">>),
    prepare(muc_delete_room, muc_rooms,
            [muc_host, room_name],
            <<"DELETE FROM muc_rooms WHERE muc_host = ? AND room_name = ?">>),
    prepare(muc_rooms_remove_domain, muc_rooms,
            [muc_host],
            <<"DELETE FROM muc_rooms WHERE muc_host = ?">>),
    prepare(muc_select_rooms, muc_rooms, [muc_host],
            <<"SELECT id, room_name, options FROM muc_rooms WHERE muc_host = ?">>),
    %% Queries to muc_room_aff table
    prepare(muc_insert_aff, muc_room_aff,
            [room_id, luser, lserver, resource, aff],
            <<"INSERT INTO muc_room_aff"
              " (room_id, luser, lserver, resource, aff)"
              " VALUES(?, ?, ?, ?, ?)">>),
    prepare(muc_select_aff, muc_room_aff,
            [room_id],
            <<"SELECT luser, lserver, resource, aff "
              "FROM muc_room_aff WHERE room_id = ?">>),
    prepare(muc_delete_aff, muc_room_aff, [room_id],
            <<"DELETE FROM muc_room_aff WHERE room_id = ?">>),
    prepare(muc_room_aff_remove_room_domain, muc_room_aff,
            ['muc_rooms.muc_host'],
            <<"DELETE FROM muc_room_aff WHERE room_id IN "
              "(SELECT id FROM muc_rooms WHERE muc_host = ?)">>),
    prepare(muc_room_aff_remove_user_domain, muc_room_aff,
            [lserver],
            <<"DELETE FROM muc_room_aff WHERE lserver = ?">>),
    %% Queries to muc_registered table
    prepare(muc_select_nick_user, muc_registered,
            [muc_host, lserver, nick],
            <<"SELECT luser FROM muc_registered WHERE muc_host = ?"
             " AND lserver = ? AND nick = ?">>),
    prepare(muc_select_nick, muc_registered,
            [muc_host, lserver, luser],
            <<"SELECT nick FROM muc_registered WHERE muc_host = ?"
              " AND lserver = ? AND luser = ?">>),
    prepare(muc_delete_nick, muc_registered,
            [muc_host, lserver, luser],
            <<"DELETE FROM muc_registered WHERE muc_host = ?"
              " AND lserver = ? AND luser = ?">>),
    prepare(muc_registered_remove_room_domain, muc_registered,
            [muc_host],
            <<"DELETE FROM muc_registered WHERE muc_host = ?">>),
    prepare(muc_registered_remove_user_domain, muc_registered,
            [lserver],
            <<"DELETE FROM muc_room_aff WHERE lserver = ?">>),
    rdbms_queries:prepare_upsert(HostType, muc_nick_upsert, muc_registered,
                                 [<<"muc_host">>, <<"luser">>, <<"lserver">>, <<"nick">>],
                                 [<<"nick">>],
                                 [<<"muc_host">>, <<"luser">>, <<"lserver">>]),
    ok.

%% Room API functions

-spec remove_domain(mongooseim:host_type(), muc_host(), jid:lserver()) -> ok.
remove_domain(HostType, MucHost, Domain) ->
    F = fun() ->
        mongoose_rdbms:execute_successfully(
            HostType, muc_registered_remove_room_domain, [MucHost]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_registered_remove_user_domain, [Domain]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_room_aff_remove_room_domain, [MucHost]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_room_aff_remove_user_domain, [Domain]),
        mongoose_rdbms:execute_successfully(
            HostType, muc_rooms_remove_domain, [MucHost]),
        ok
        end,
    {atomic, ok} = mongoose_rdbms:sql_transaction(HostType, F),
    ok.

-spec store_room(mongooseim:host_type(), muc_host(), mod_muc:room(), room_opts()) ->
    ok | {error, term()}.
store_room(HostType, MucHost, RoomName, Opts) ->
    Affs = proplists:get_value(affiliations, Opts),
    NewOpts = proplists:delete(affiliations, Opts),
    ExtOpts = jiffy:encode({NewOpts}),
    F = fun() ->
            forget_room_transaction(HostType, MucHost, RoomName),
            store_room_transaction(HostType, MucHost, RoomName, ExtOpts, Affs)
        end,
    {atomic, Res} = mongoose_rdbms:sql_transaction(HostType, F),
    Res.

-spec restore_room(mongooseim:host_type(), muc_host(), mod_muc:room()) ->
    {ok, room_opts()} | {error, room_not_found} | {error, term()}.
restore_room(HostType, MucHost, RoomName) ->
    case execute_select_room(HostType, MucHost, RoomName) of
        {selected, [{ExtRoomID, ExtOpts}]} ->
            RoomID = mongoose_rdbms:result_to_integer(ExtRoomID),
            FullOpts = get_full_options(HostType, ExtOpts, RoomID),
            {ok, FullOpts};
        {selected, []} ->
            {error, room_not_found}
    end.

-spec forget_room(mongooseim:host_type(), muc_host(), mod_muc:room()) ->
    ok | {error, term()}.
forget_room(HostType, MucHost, RoomName) ->
    F = fun() -> forget_room_transaction(HostType, MucHost, RoomName) end,
    {atomic, _Res} = mongoose_rdbms:sql_transaction(HostType, F),
    ok.

%% Room helper functions

-spec get_rooms(mongooseim:host_type(), muc_host()) -> {ok, [#muc_room{}]}.
get_rooms(HostType, MucHost) ->
    {selected, RoomRows} = execute_select_rooms(HostType, MucHost),
    RoomRecs = [handle_room_row(HostType, MucHost, Row) || Row <- RoomRows],
    {ok, RoomRecs}.

handle_room_row(HostType, MucHost, {ExtRoomID, RoomName, ExtOpts}) ->
    RoomID = mongoose_rdbms:result_to_integer(ExtRoomID),
    FullOpts = get_full_options(HostType, ExtOpts, RoomID),
    #muc_room{name_host = {RoomName, MucHost}, opts = FullOpts}.

get_full_options(HostType, ExtOpts, RoomID) ->
    {selected, Affs} = execute_select_aff(HostType, RoomID),
    decode_opts(ExtOpts, Affs).

%% Nick API functions

-spec can_use_nick(mongooseim:host_type(), muc_host(), client_jid(), mod_muc:nick()) -> boolean().
can_use_nick(HostType, MucHost, Jid, Nick) ->
    {UserU, UserS} = jid:to_lus(Jid),
    case execute_select_nick_user(HostType, MucHost, UserS, Nick) of
        {selected, []} -> true;
        {selected, [{U}]} -> U == UserU
    end.

%% Get nick associated with jid client_jid() across muc_host() domain
-spec get_nick(mongooseim:host_type(), muc_host(), client_jid()) ->
    {ok, mod_muc:nick()} | {error, not_registered}.
get_nick(HostType, MucHost, Jid) ->
    {UserU, UserS} = jid:to_lus(Jid),
    case execute_select_nick(HostType, MucHost, UserU, UserS) of
        {selected, []} -> {error, not_registered};
        {selected, [{Nick}]} -> {ok, Nick}
    end.

%% Register nick
-spec set_nick(mongooseim:host_type(), muc_host(), client_jid(), mod_muc:nick()) -> ok | {error, term()}.
set_nick(HostType, MucHost, Jid, Nick) when is_binary(Nick), Nick =/= <<>> ->
    CanUseNick = can_use_nick(HostType, MucHost, Jid, Nick),
    store_nick_transaction(HostType, MucHost, Jid, Nick, CanUseNick).

%% Unregister nick
%% Unregistered nicks can be used by someone else
-spec unset_nick(mongooseim:host_type(), muc_host(), client_jid()) -> ok.
unset_nick(HostType, MucHost, Jid) ->
    {UserU, UserS} = jid:to_lus(Jid),
    execute_delete_nick(HostType, MucHost, UserU, UserS),
    ok.

%% Transaction body functions

store_nick_transaction(_HostType, _MucHost, _Jid, _Nick, false) ->
    {error, conflict};
store_nick_transaction(HostType, MucHost, Jid, Nick, true) ->
    {LU, LS} = jid:to_lus(Jid),
    InsertParams = [MucHost, LU, LS, Nick],
    UpdateParams = [Nick],
    case rdbms_queries:execute_upsert(HostType, muc_nick_upsert, InsertParams, UpdateParams) of
        {updated, _} -> ok;
        Error -> Error
    end.

-spec store_room_transaction(mongooseim:host_type(), muc_host(), jid:luser(), iodata(), term()) -> ok.
store_room_transaction(HostType, MucHost, RoomName, ExtOpts, Affs) ->
    execute_insert_room(HostType, MucHost, RoomName, ExtOpts),
    Result = execute_select_room_id(HostType, MucHost, RoomName),
    RoomID = mongoose_rdbms:selected_to_integer(Result),
    store_aff(HostType, RoomID, Affs),
    ok.

store_aff(_HostType, _, undefined) ->
    ok;
store_aff(HostType, RoomID, Affs) ->
    F = fun({{UserU, UserS, Resource}, Aff}) ->
            ExtAff = aff_atom2db(Aff),
            execute_insert_aff(HostType, RoomID, UserU, UserS, Resource, ExtAff)
        end,
    lists:foreach(F, Affs).

forget_room_transaction(HostType, MucHost, RoomName) ->
    case execute_select_room_id(HostType, MucHost, RoomName) of
        {selected, [{ExtRoomID}]} ->
            RoomID = mongoose_rdbms:result_to_integer(ExtRoomID),
            execute_delete_affs(HostType, RoomID),
            execute_delete_room(HostType, MucHost, RoomName),
            ok;
        {selected, []} ->
            {error, not_exists}
    end.

%% Execute call functions

-spec execute_insert_room(mongooseim:host_type(), muc_host(), jid:luser(), iodata()) -> ok.
execute_insert_room(HostType, MucHost, RoomName, ExtOpts) ->
    Args = [MucHost, RoomName, ExtOpts],
    execute_successfully(HostType, muc_insert_room, Args),
    ok.

-spec execute_insert_aff(mongooseim:host_type(), RoomID :: room_id(),
                         UserU :: jid:luser(), UserS :: jid:lserver(),
                         Res :: binary(), ExtAff :: pos_integer()) -> ok.
execute_insert_aff(HostType, RoomID, UserU, UserS, Res, ExtAff) ->
    Args = [RoomID, UserU, UserS, Res, ExtAff],
    execute_successfully(HostType, muc_insert_aff, Args),
    ok.

-spec execute_select_aff(mongooseim:host_type(), room_id()) -> term().
execute_select_aff(HostType, RoomID) ->
    execute_successfully(HostType, muc_select_aff, [RoomID]).

-spec execute_select_room_id(mongooseim:host_type(), muc_host(), jid:luser()) -> term().
execute_select_room_id(HostType, MucHost, RoomName) ->
    execute_successfully(HostType, muc_select_room_id, [MucHost, RoomName]).

-spec execute_select_room(mongooseim:host_type(), muc_host(), jid:luser()) -> term().
execute_select_room(HostType, MucHost, RoomName) ->
    execute_successfully(HostType, muc_select_room, [MucHost, RoomName]).

-spec execute_delete_affs(mongooseim:host_type(), room_id()) -> term().
execute_delete_affs(HostType, RoomID) ->
    execute_successfully(HostType, muc_delete_aff, [RoomID]).

-spec execute_delete_room(mongooseim:host_type(), muc_host(), jid:luser()) -> term().
execute_delete_room(HostType, MucHost, RoomName) ->
    execute_successfully(HostType, muc_delete_room, [MucHost, RoomName]).

-spec execute_select_rooms(mongooseim:host_type(), muc_host()) -> term().
execute_select_rooms(HostType, MucHost) ->
    execute_successfully(HostType, muc_select_rooms, [MucHost]).

-spec execute_select_nick_user(mongooseim:host_type(), muc_host(), jid:luser(), mod_muc:nick()) -> term().
execute_select_nick_user(HostType, MucHost, UserS, Nick) ->
    execute_successfully(HostType, muc_select_nick_user, [MucHost, UserS, Nick]).

-spec execute_select_nick(mongooseim:host_type(), muc_host(), jid:luser(), jid:lserver()) -> term().
execute_select_nick(HostType, MucHost, UserU, UserS) ->
    execute_successfully(HostType, muc_select_nick, [MucHost, UserS, UserU]).

-spec execute_delete_nick(mongooseim:host_type(), muc_host(), jid:luser(), jid:lserver()) -> term().
execute_delete_nick(HostType, MucHost, UserU, UserS) ->
    execute_successfully(HostType, muc_delete_nick, [MucHost, UserS, UserU]).

%% Conversion functions

-spec aff_atom2db(aff()) -> pos_integer().
aff_atom2db(owner) -> 1;
aff_atom2db({owner, _}) -> 1;
aff_atom2db(member) -> 2;
aff_atom2db({member, _}) -> 2;
aff_atom2db(admin) -> 3;
aff_atom2db({admin, _}) -> 3;
aff_atom2db(outcast) -> 4;
aff_atom2db({outcast, _}) -> 4;
aff_atom2db(_Other) -> 5.

-spec aff_db2atom(pos_integer()) -> aff().
aff_db2atom(1) -> owner;
aff_db2atom(2) -> member;
aff_db2atom(3) -> admin;
aff_db2atom(4) -> outcast;
aff_db2atom(5) -> none.

decode_opts(ExtOpts, Affs) ->
    {Opts} = jiffy:decode(ExtOpts),
    [{affiliations, decode_affs(Affs)} | keys_as_atoms(Opts)].

decode_affs(Affs) ->
    [{{UserU, UserS, Res}, aff_db2atom(mongoose_rdbms:result_to_integer(Aff))}
     || {UserU, UserS, Res, Aff} <- Affs].

keys_as_atoms(KVs) ->
    [{binary_to_existing_atom(Key, utf8), Value}
     || {Key, Value} <- KVs].
