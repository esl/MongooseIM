-module(mongoose_muc_online_cets).
-behaviour(mongoose_muc_online_backend).

-export([start/2,
         register_room/4,
         room_destroyed/4,
         find_room_pid/3,
         get_online_rooms/2,
         node_cleanup/2]).

-export([handle_conflict/2]).

-include_lib("mod_muc.hrl").

%% Use MucHost first for prefix select optimization in get_online_rooms
%% We also don't want to send HostType in muc_online_room.host_type between CETS nodes
%% or store it
-type muc_tuple() :: {{MucHost :: jid:lserver(), Room :: mod_muc:room()}, Pid :: pid()}.

table_name(HostType) ->
    binary_to_atom(<<"cets_muc_online_room_", HostType/binary>>).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    Tab = table_name(HostType),
    %% Non-random, non-node-specific keys
    %% This means that default merging would not work
    cets:start(Tab, #{handle_conflict => fun ?MODULE:handle_conflict/2}),
    cets_discovery:add_table(mongoose_cets_discovery, Tab),
    ok.

%% We should keep one room and stop another room
%% But stopping logic needs to be tested heavily and designed
%% because we would need to figure out how to send presences to participants
%% (and maybe document how to rejoin the kicked room)
-spec handle_conflict(muc_tuple(), muc_tuple()) -> muc_tuple().
handle_conflict(Rec1, Rec2) when Rec1 > Rec2 ->
    Rec1;
handle_conflict(_Rec1, Rec2) ->
    Rec2.

-spec register_room(
        HostType :: mongooseim:host_type(),
        MucHost :: jid:lserver(),
        Room :: mod_muc:room(),
        Pid :: pid()) -> ok | {exists, pid()} | {error, term()}.
register_room(HostType, MucHost, Room, Pid) ->
    Tab = table_name(HostType),
    Rec = {{MucHost, Room}, Pid},
    case find_room_pid(HostType, MucHost, Room) of
        %% Race condition is possible
        %% TODO use cets:insert_new/2 once available
        %% Otherwise cets:insert could overwrite an existing registration
        {ok, OtherPid} ->
            {exists, OtherPid};
        {error, not_found} ->
            cets:insert(Tab, Rec),
            ok
    end.

%% Race condition is possible between register and room_destroyed
%% (Because register is outside of the room process)
-spec room_destroyed(mongooseim:host_type(), jid:lserver(), mod_muc:room(), pid()) -> ok.
room_destroyed(HostType, MucHost, Room, Pid) ->
    Tab = table_name(HostType),
    Rec = {{MucHost, Room}, Pid},
    cets:delete_object(Tab, Rec),
    ok.

-spec find_room_pid(mongooseim:host_type(), jid:server(), mod_muc:room()) ->
    {ok, pid()} | {error, not_found}.
find_room_pid(HostType, MucHost, Room) ->
    Tab = table_name(HostType),
    case ets:lookup(Tab, {MucHost, Room}) of
        [{_, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

%% This is used by MUC discovery but it is not very scalable.
%% This function should look like get_online_rooms(HostType, MucHost, AfterRoomName, Limit)
%% to reduce the load and still have pagination working.
-spec get_online_rooms(mongooseim:host_type(), jid:lserver()) ->
    [mod_muc:muc_online_room()].
get_online_rooms(HostType, MucHost) ->
    Tab = table_name(HostType),
    [#muc_online_room{name_host = {Room, MucHost}, pid = Pid, host_type = HostType}
     || [Room, Pid] <- ets:match(Tab, {{MucHost, '$1'}, '$2'})].

-spec node_cleanup(mongooseim:host_type(), node()) -> ok.
node_cleanup(HostType, Node) ->
    Tab = table_name(HostType),
    Pattern = {'_', '$1'},
    Guard = {'==', {node, '$1'}, Node},
    ets:select_delete(Tab, [{Pattern, [Guard], [true]}]),
    ok.
