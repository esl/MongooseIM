-module(mongoose_muc_online_mnesia).
-export([start/2,
         room_destroyed/4]).

-include_lib("mod_muc.hrl").

start(_HostType, _Opts) ->
    mnesia:create_table(muc_online_room,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, muc_online_room)}]),
    mnesia:add_table_copy(muc_online_room, node(), ram_copies),
    ok.

%% Race condition is possible between register and room_destroyed
%% (Because register is outside of the room process)
-spec room_destroyed(mongooseim:host_type(), jid:server(), mod_muc:room(), pid()) -> ok.
room_destroyed(HostType, MucHost, Room, Pid) ->
    Obj = #muc_online_room{name_host = {Room, MucHost},
                           host_type = HostType, pid = Pid},
    F = fun() -> mnesia:delete_object(Obj) end,
    {atomic, ok} = mnesia:transaction(F),
    ok.
