-module(ejabberd_s2s_mnesia).
-export([init/1,
         dirty_read_s2s_list_pids/1,
         try_register/3,
         remove_connection/2,
         node_cleanup/1]).

-export([register_secret/3,
         get_shared_secret/1]).

-record(s2s, {
          fromto :: ejabberd_s2s:fromto(),
          pid :: pid()
         }).

-record(s2s_secret, {host_type, source, secret}).

-include("mongoose_logger.hrl").

init(_) ->
    init_pids(),
    init_secrets().

%% Pid lists
init_pids() ->
    Opts = [{ram_copies, [node()]}, {type, bag},
            {attributes, record_info(fields, s2s)}],
    mongoose_lib:create_mnesia_table(s2s, Opts).

dirty_read_s2s_list_pids(FromTo) ->
    {ok, s2s_to_pids(mnesia:dirty_read(s2s, FromTo))}.

try_register(Pid, ShouldWriteF, FromTo) ->
    F = fun() ->
                L = s2s_to_pids(mnesia:read({s2s, FromTo})),
                case ShouldWriteF(L) of
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
    {atomic, _} = mnesia:transaction(F),
    ok.

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
    mongoose_lib:create_mnesia_table(s2s_secret, Opts).

register_secret(HostType, Source, Secret) ->
    Rec = #s2s_secret{host_type = HostType, source = Source, secret = Secret},
    {atomic, _} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
    ok.

get_shared_secret(HostType) ->
    case mnesia:dirty_read(s2s_secret, HostType) of
        [#s2s_secret{source = Source, secret = Secret}] ->
            {ok, {Source, Secret}};
        [] ->
            {error, not_found}
    end.
