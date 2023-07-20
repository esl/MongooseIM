%% Get information about S2S connections on this node.
-module(mongoose_s2s_info).

%% ejabberd API
-export([get_connections/1]).
-ignore_xref([get_connections/1]).

-include("mongoose_logger.hrl").

-type direction() :: in | out.
-type supervisor_child_spec() :: { undefined, pid(), worker, [module()] }.
-type connection_info() :: ejabberd_s2s_in:connection_info() | ejabberd_s2s_out:connection_info().

%% @doc Get information about S2S connections of the specified type.
-spec get_connections(direction()) -> [connection_info()].
get_connections(Type) ->
    Specs = supervisor:which_children(type_to_supervisor(Type)),
    [Conn || Spec <- Specs, Conn <- get_state_info(child_to_pid(Spec))].

%% Both supervisors are simple_one_for_one with temporary children processes.
-spec type_to_supervisor(direction()) -> atom().
type_to_supervisor(in) -> ejabberd_s2s_in_sup;
type_to_supervisor(out) -> ejabberd_s2s_out_sup.

-spec child_to_pid(supervisor_child_spec()) -> pid().
child_to_pid({_, Pid, _, _}) -> Pid.

-spec get_state_info(pid()) -> [connection_info()].
get_state_info(Pid) when is_pid(Pid) ->
    case gen_fsm_compat:sync_send_all_state_event(Pid, get_state_info) of
        Info when is_map(Info) ->
            [Info];
        Other ->
            ?LOG_ERROR(#{what => s2s_get_state_info_failed, pid => Pid, reason => Other}),
            []
    end.
