%% Some ugly code only used in tests.
%% It was originally in ejabberd_s2s, but it was moved out to improve readability.
-module(mongoose_s2s_info).

%% ejabberd API
-export([get_info_s2s_connections/1]).
-ignore_xref([get_info_s2s_connections/1]).

-type connstate() :: 'restarting' | 'undefined' | pid().
-type conn() :: { any(), connstate(), 'supervisor' | 'worker', 'dynamic' | [_] }.

%% @doc Get information about S2S connections of the specified type.
-spec get_info_s2s_connections('in' | 'out') -> [[{atom(), any()}, ...]].
get_info_s2s_connections(Type) ->
    ChildType = case Type of
                    in -> ejabberd_s2s_in_sup;
                    out -> ejabberd_s2s_out_sup
                end,
    Connections = supervisor:which_children(ChildType),
    get_s2s_info(Connections, Type).

-spec get_s2s_info(Connections :: [conn()],
                  Type :: 'in' | 'out'
                  ) -> [[{any(), any()}, ...]]. % list of lists
get_s2s_info(Connections, Type)->
    complete_s2s_info(Connections, Type, []).

-spec complete_s2s_info(Connections :: [conn()],
                        Type :: 'in' | 'out',
                        Result :: [[{any(), any()}, ...]] % list of lists
                        ) -> [[{any(), any()}, ...]]. % list of lists
complete_s2s_info([], _, Result)->
    Result;
complete_s2s_info([Connection|T], Type, Result)->
    {_, PID, _, _} = Connection,
    State = get_s2s_state(PID),
    complete_s2s_info(T, Type, [State|Result]).

-spec get_s2s_state(connstate()) -> [{atom(), any()}, ...].
get_s2s_state(S2sPid) ->
    Infos = case gen_fsm_compat:sync_send_all_state_event(S2sPid, get_state_infos) of
                {state_infos, Is} -> [{status, open} | Is];
                {noproc, _} -> [{status, closed}]; %% Connection closed
                {badrpc, _} -> [{status, error}]
            end,
    [{s2s_pid, S2sPid} | Infos].
