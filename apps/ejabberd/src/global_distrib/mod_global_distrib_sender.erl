-module(mod_global_distrib_sender).

-behaviour(gen_mod).
-behaviour(supervisor).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([start/2, stop/1, send/2, get_process_for/1, start_link/0, init/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    Opts = [{listen_port, 5555}, {num_of_connections, 1} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: ejabberd:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% supervisor API
%%--------------------------------------------------------------------

init(_) ->
    Child = {cpool, {cpool, new_pool_sup, []}, temporary, 5000, supervisor, dynamic},
    {ok, {{simple_one_for_one, 1000000, 1}, [Child]}}.

start_pool(Name, Server) ->
    supervisor:start_child(?MODULE, [Name, {mod_global_distrib_connection, start_link, [Server]},
                                     [{pool_size, opt(num_of_connections)}]]).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start() -> any().
start() ->
    opt(tls_opts), %% Check for required tls_opts
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MESSAGES_SENT, spiral),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_SEND_QUEUE_TIME, histogram),
    ChildSpec = {?MODULE, {?MODULE, start_link, []}, permanent, 1000, supervisor, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

-spec stop() -> any().
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

-spec send(ejabberd:lserver() | pid(), {jid(), jid(), mongoose_acc:t()}) -> ok.
send(Server, Packet) when is_binary(Server) ->
    Worker = get_process_for(Server),
    send(Worker, Packet);
send(Worker, {From, _To, _Acc} = Packet) ->
    BinPacket = term_to_binary(Packet),
    BinFrom = from_to_streamid(From),
    Data = <<(byte_size(BinFrom)):16, BinFrom/binary, BinPacket/binary>>,
    Stamp = erlang:monotonic_time(),
    ok = mod_global_distrib_utils:cast_or_call(Worker, {data, Stamp, Data}).

-spec from_to_streamid(From :: jid()) -> binary().
from_to_streamid(From) ->
    GlobalHost = opt(global_host),
    %% TODO: dedup: It's possible that there are other places with this kind of construct.
    case jid:to_lower(From) of
        {_, GlobalHost, _} = LJid -> jid:to_binary(LJid);
        {_, Domain, _} -> Domain
    end.

-spec get_process_for(ejabberd:lserver()) -> pid().
get_process_for(Server) ->
    Name = mod_global_distrib_utils:any_binary_to_atom(Server),
    case whereis(Name) of
        undefined -> start_pool(Name, Server);
        _ -> ok
    end,
    cpool:get_connection(Name).

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

