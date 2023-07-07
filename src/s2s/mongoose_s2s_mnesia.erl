-module(mongoose_s2s_mnesia).
-behaviour(mongoose_s2s_backend).

-export([init/1,
         get_s2s_out_pids/1,
         try_register/2,
         remove_connection/2,
         node_cleanup/1]).

-export([register_secret/2,
         get_shared_secret/1]).

-record(s2s, {
          fromto :: ejabberd_s2s:fromto() | '_',
          pid :: pid() | '$1'
         }).

-record(s2s_secret, {host_type, secret}).

-include("mongoose_logger.hrl").

init(_) ->
    init_pids(),
    init_secrets().

%% Pid lists
init_pids() ->
    Opts = [{ram_copies, [node()]}, {type, bag},
            {attributes, record_info(fields, s2s)}],
    mnesia:create_table(s2s, Opts),
    mnesia:add_table_copy(s2s, node(), ram_copies).

get_s2s_out_pids(FromTo) ->
    s2s_to_pids(mnesia:dirty_read(s2s, FromTo)).

try_register(Pid, FromTo) ->
    F = fun() ->
                Pids = get_s2s_out_pids(FromTo),
                case mongoose_s2s_lib:need_more_connections(FromTo, Pids) of
                    true ->
                        mnesia:write(#s2s{fromto = FromTo, pid = Pid}),
                        true;
                    false ->
                        false
                end
        end,
    case mnesia:transaction(F) of
        {atomic, Bool} ->
            Bool;
        Other ->
            ?LOG_ERROR(#{what => s2s_try_register_failed,
                         s2s_pid => Pid, from_to => FromTo,
                         reason => Other}),
            false
    end.

remove_connection(FromTo, Pid) ->
    Rec = #s2s{fromto = FromTo, pid = Pid},
    F = fun() ->
                mnesia:delete_object(Rec)
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            ok;
        Other ->
            ?LOG_ERROR(#{what => s2s_remove_connection,
                         from_to => FromTo, reason => Other}),
            ok
    end.

node_cleanup(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       s2s,
                       [{#s2s{pid = '$1', _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F).

s2s_to_pids(List) ->
    [Pid || #s2s{pid = Pid} <- List].

%% Secrets
init_secrets() ->
    Opts = [{ram_copies, [node()]}, {attributes, record_info(fields, s2s_secret)}],
    mnesia:create_table(s2s_secret, Opts),
    mnesia:add_table_copy(s2s_secret, node(), ram_copies).

register_secret(HostType, Secret) ->
    Rec = #s2s_secret{host_type = HostType, secret = Secret},
    {atomic, _} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
    ok.

get_shared_secret(HostType) ->
    case mnesia:dirty_read(s2s_secret, HostType) of
        [#s2s_secret{secret = Secret}] ->
            {ok, Secret};
        [] ->
            {error, not_found}
    end.
