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

-record(s2s_shared, {
           host_type :: mongooseim:host_type(),
           secret :: ejabberd_s2s:base16_secret()
       }).

-include("mongoose_logger.hrl").

-spec init(map()) -> ok.
init(_) ->
    init_pids(),
    init_secrets(),
    ok.

%% Pid lists
init_pids() ->
    Opts = [{ram_copies, [node()]}, {type, bag},
            {attributes, record_info(fields, s2s)}],
    mongoose_mnesia:create_table(s2s, Opts).

-spec get_s2s_out_pids(ejabberd_s2s:fromto()) -> ejabberd_s2s:s2s_pids().
get_s2s_out_pids(FromTo) ->
    s2s_to_pids(mnesia:dirty_read(s2s, FromTo)).

-spec try_register(Pid :: pid(),
                   FromTo :: ejabberd_s2s:fromto()) -> boolean().
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

-spec remove_connection(FromTo :: ejabberd_s2s:fromto(), Pid :: pid()) -> ok.
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

-spec node_cleanup(Node :: node()) -> ok.
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
    mnesia:async_dirty(F),
    ok.

s2s_to_pids(List) ->
    [Pid || #s2s{pid = Pid} <- List].

%% Secrets
init_secrets() ->
    Opts = [{ram_copies, [node()]}, {attributes, record_info(fields, s2s_shared)}],
    mongoose_mnesia:create_table(s2s_shared, Opts).

-spec register_secret(HostType :: mongooseim:host_type(),
                      Secret :: ejabberd_s2s:base16_secret()) -> ok.
register_secret(HostType, Secret) ->
    Rec = #s2s_shared{host_type = HostType, secret = Secret},
    {atomic, _} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
    ok.

-spec get_shared_secret(mongooseim:host_type()) ->
    {ok, ejabberd_s2s:base16_secret()} | {error, not_found}.
get_shared_secret(HostType) ->
    case mnesia:dirty_read(s2s_shared, HostType) of
        [#s2s_shared{secret = Secret}] ->
            {ok, Secret};
        [] ->
            {error, not_found}
    end.
