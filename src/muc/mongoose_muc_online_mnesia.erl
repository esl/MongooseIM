-module(mongoose_muc_online_mnesia).
-export([start/2,
         register_room/4,
         room_destroyed/4,
         find_room_pid/3,
         get_online_rooms/2,
         node_cleanup/2]).

-include_lib("mod_muc.hrl").

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    mnesia:create_table(muc_online_room,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, muc_online_room)}]),
    mnesia:add_table_copy(muc_online_room, node(), ram_copies),
    ok.

-spec register_room(
        HostType :: mongooseim:host_type(),
        MucHost :: jid:lserver(),
        Room :: mod_muc:room(),
        Pid :: pid()) -> ok | {exists, pid()} | {error, term()}.
register_room(HostType, MucHost, Room, Pid) ->
    F = fun() ->
            case mnesia:read(muc_online_room,  {Room, MucHost}, write) of
                [] ->
                    mnesia:write(#muc_online_room{name_host = {Room, MucHost},
                                                  host_type = HostType,
                                                  pid = Pid});
                [R] ->
                    {exists, R#muc_online_room.pid}
            end
        end,
    simple_transaction_result(mnesia:transaction(F)).

%% Race condition is possible between register and room_destroyed
%% (Because register is outside of the room process)
-spec room_destroyed(mongooseim:host_type(), jid:server(), mod_muc:room(), pid()) -> ok.
room_destroyed(HostType, MucHost, Room, Pid) ->
    Obj = #muc_online_room{name_host = {Room, MucHost},
                           host_type = HostType, pid = Pid},
    F = fun() -> mnesia:delete_object(Obj) end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

simple_transaction_result({atomic, Res}) ->
    Res;
simple_transaction_result({aborted, Reason}) ->
    {error, Reason}.

find_room_pid(_HostType, MucHost, Room) ->
    case mnesia:dirty_read(muc_online_room, {Room, MucHost}) of
        [R] ->
            {ok, R#muc_online_room.pid};
        [] ->
            {error, not_found}
    end.

get_online_rooms(_HostType, MucHost) ->
    mnesia:dirty_select(muc_online_room,
                        [{#muc_online_room{name_host = '$1', _ = '_'},
                          [{'==', {element, 2, '$1'}, MucHost}],
                          ['$_']}]).

node_cleanup(HostType, Node) ->
    F = fun() ->
                Es = mnesia:select(
                       muc_online_room,
                       [{#muc_online_room{pid = '$1',
                                          host_type = HostType,
                                          _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F),
    ok.
