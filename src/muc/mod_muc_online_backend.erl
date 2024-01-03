-module(mod_muc_online_backend).

-export([start/2,
         stop/1,
         register_room/4,
         room_destroyed/4,
         find_room_pid/3,
         get_online_rooms/2,
         node_cleanup/2,
         clear_table/1]).

%% Used in tests
-ignore_xref([clear_table/1]).

-define(MAIN_MODULE, mod_muc_online).

%% Callbacks

-callback start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback stop(mongooseim:host_type()) -> ok.

-callback register_room(
        HostType :: mongooseim:host_type(),
        MucHost :: jid:lserver(),
        Room :: mod_muc:room(),
        Pid :: pid()) -> ok | {exists, pid()} | {error, term()}.

-callback room_destroyed(mongooseim:host_type(), jid:lserver(), mod_muc:room(), pid()) -> ok.

-callback find_room_pid(mongooseim:host_type(), jid:lserver(), mod_muc:room()) ->
    {ok, pid()} | {error, not_found}.

-callback get_online_rooms(mongooseim:host_type(), jid:lserver()) ->
    [mod_muc:muc_online_room()].

-callback node_cleanup(mongooseim:host_type(), node()) -> ok.

-callback clear_table(mongooseim:host_type()) -> ok.

%% API Functions

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts = #{online_backend := Backend}) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, tracked_funs(), #{backend => Backend}),
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Opts]).

stop(HostType) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]).

-spec tracked_funs() -> [atom()].
tracked_funs() ->
    [register_room,
     room_destroyed,
     get_online_rooms].

-spec register_room(
        HostType :: mongooseim:host_type(),
        MucHost :: jid:lserver(),
        Room :: mod_muc:room(),
        Pid :: pid()) -> ok | {exists, pid()} | {error, term()}.
register_room(HostType, MucHost, Room, Pid) ->
    Args = [HostType, MucHost, Room, Pid],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec room_destroyed(mongooseim:host_type(), jid:lserver(), mod_muc:room(), pid()) -> ok.
room_destroyed(HostType, MucHost, Room, Pid) ->
    Args = [HostType, MucHost, Room, Pid],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec find_room_pid(mongooseim:host_type(), jid:lserver(), mod_muc:room()) ->
    {ok, pid()} | {error, not_found}.
find_room_pid(HostType, MucHost, Room) ->
    Args = [HostType, MucHost, Room],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_online_rooms(mongooseim:host_type(), jid:lserver()) ->
    [mod_muc:muc_online_room()].
get_online_rooms(HostType, MucHost) ->
    Args = [HostType, MucHost],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec node_cleanup(mongooseim:host_type(), node()) -> ok.
node_cleanup(HostType, Node) ->
    Args = [HostType, Node],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

clear_table(HostType) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]).
