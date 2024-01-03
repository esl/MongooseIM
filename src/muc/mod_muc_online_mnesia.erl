-module(mod_muc_online_mnesia).
-behaviour(mod_muc_online_backend).

-export([start/2,
         stop/1,
         register_room/4,
         room_destroyed/4,
         find_room_pid/3,
         get_online_rooms/2,
         node_cleanup/2,
         clear_table/1]).

-include_lib("mod_muc.hrl").

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    mongoose_mnesia:create_table(muc_online_room,
        [{ram_copies, [node()]},
         {attributes, record_info(fields, muc_online_room)}]),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) -> ok.

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
-spec room_destroyed(mongooseim:host_type(), jid:lserver(), mod_muc:room(), pid()) -> ok.
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

-spec find_room_pid(mongooseim:host_type(), jid:server(), mod_muc:room()) ->
    {ok, pid()} | {error, not_found}.
find_room_pid(_HostType, MucHost, Room) ->
    case mnesia:dirty_read(muc_online_room, {Room, MucHost}) of
        [R] ->
            {ok, R#muc_online_room.pid};
        [] ->
            {error, not_found}
    end.

-spec get_online_rooms(mongooseim:host_type(), jid:lserver()) ->
    [mod_muc:muc_online_room()].
get_online_rooms(_HostType, MucHost) ->
    mnesia:dirty_select(muc_online_room,
                        [{#muc_online_room{name_host = '$1', _ = '_'},
                          [{'==', {element, 2, '$1'}, MucHost}],
                          ['$_']}]).

-spec node_cleanup(mongooseim:host_type(), node()) -> ok.
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

%% Clear table for tests
-spec clear_table(mongooseim:host_type()) -> ok.
clear_table(_HostType) ->
    mnesia:clear_table(muc_online_room),
    ok.
