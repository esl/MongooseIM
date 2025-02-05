%% Get information about S2S connections on this node.
-module(mongoose_s2s_info).

%% ejabberd API
-export([get_connections/1]).
-ignore_xref([get_connections/1]).

-include("mongoose_logger.hrl").

-type direction() :: in | out.
-type supervisor_child_spec() :: { undefined, pid(), worker, [module()] }.
-type connection_info() :: mongoose_s2s_in:connection_info() | ejabberd_s2s_out:connection_info().

%% @doc Get information about S2S connections of the specified type.
-spec get_connections(direction()) -> [connection_info()].
get_connections(in) ->
    Children = supervisor:which_children(mongoose_listener_sup),
    Listeners = [Ref || {Ref, _, _, [mongoose_s2s_listener | _]} <- Children],
    Pids = lists:flatten([ranch:procs(Ref, connections) || Ref <- Listeners]),
    [Conn || Pid <- Pids, Conn <- get_state_info(in, Pid)];
get_connections(out) ->
    Specs = supervisor:which_children(ejabberd_s2s_out_sup),
    [Conn || Spec <- Specs, Conn <- get_state_info(out, child_to_pid(Spec))].

-spec child_to_pid(supervisor_child_spec()) -> pid().
child_to_pid({_, Pid, _, _}) -> Pid.

-spec get_state_info(direction(), pid()) -> [connection_info()].
get_state_info(in, Pid) when is_pid(Pid) ->
    case mongoose_s2s_in:get_state_info(Pid) of
        Info when is_map(Info) ->
            [Info];
        Other ->
            ?LOG_ERROR(#{what => s2s_get_state_info_failed, pid => Pid, reason => Other}),
            []
    end;
get_state_info(out, Pid) when is_pid(Pid) ->
    case gen_fsm_compat:sync_send_all_state_event(Pid, get_state_info) of
        Info when is_map(Info) ->
            [Info];
        Other ->
            ?LOG_ERROR(#{what => s2s_get_state_info_failed, pid => Pid, reason => Other}),
            []
    end.
