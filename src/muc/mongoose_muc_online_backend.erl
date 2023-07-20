-module(mongoose_muc_online_backend).

-export([start/2,
         register_room/4,
         room_destroyed/4,
         find_room_pid/3]).

-define(MAIN_MODULE, mongoose_muc_online).

%% Callbacks

%% API Functions

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts = #{online_backend := Backend}) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, tracked_funs(), #{backend => Backend}),
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Opts]).

-spec tracked_funs() -> atom().
tracked_funs() ->
    [register_room,
     room_destroyed].

register_room(HostType, MucHost, Room, Pid) ->
    Args = [HostType, MucHost, Room, Pid],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec room_destroyed(mongooseim:host_type(), jid:server(), mod_muc:room(), pid()) -> ok.
room_destroyed(HostType, MucHost, Room, Pid) ->
    Args = [HostType, MucHost, Room, Pid],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

find_room_pid(HostType, MucHost, Room) ->
    Args = [HostType, MucHost, Room],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
