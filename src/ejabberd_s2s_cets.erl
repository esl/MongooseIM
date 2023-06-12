-module(ejabberd_s2s_cets).
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

-define(TABLE, cets_s2s_session).
-define(SECRET_TABLE, cets_s2s_secret).

init(_) ->
    cets:start(?TABLE, #{}),
    cets:start(?SECRET_TABLE, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE),
    cets_discovery:add_table(mongoose_cets_discovery, ?SECRET_TABLE).

%% Pid lists
dirty_read_s2s_list_pids(FromTo) ->
    R = {{FromTo, '$1'}},
    Pids = ets:select(?TABLE, [{R, [], ['$1']}]),
    {ok, Pids}.

try_register(Pid, ShouldWriteF, FromTo) ->
    L = dirty_read_s2s_list_pids(FromTo),
    case ShouldWriteF(L) of
        true ->
            cets:insert(?TABLE, {{FromTo, Pid}}),
            true;
        false ->
            false
    end.

remove_connection(FromTo, Pid) ->
    cets:delete(?TABLE, {FromTo, Pid}),
    ok.

%% node_cleanup is called on each node in the cluster, when Node is down
node_cleanup(Node) ->
    KeyPattern = {'_', '$1'},
    R = {KeyPattern},
    Guard = {'==', {node, '$1'}, Node},
    ets:select_delete(?TABLE, [{R, [Guard], [true]}]).

s2s_to_pids(List) ->
    [Pid || #s2s{pid = Pid} <- List].

%% Secrets
register_secret(HostType, Source, Secret) ->
    cets:insert(?SECRET_TABLE, {HostType, Source, Secret}),
    ok.

get_shared_secret(HostType) ->
    case ets:lookup(?SECRET_TABLE, HostType) of
        [{_HostType, Source, Secret}] ->
            {ok, {Source, Secret}};
        [] ->
            {error, not_found}
    end.
