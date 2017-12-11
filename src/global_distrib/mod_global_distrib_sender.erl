-module(mod_global_distrib_sender).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([start/2, stop/1, send/2, get_process_for/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec send(ejabberd:lserver() | pid(), {jid(), jid(), mongoose_acc:t()}) -> ok.
send(Server, Packet) when is_binary(Server) ->
    Worker = get_process_for(Server),
    send(Worker, Packet);
send(Worker, {From, _To, _Acc} = Packet) ->
    BinPacket = term_to_binary(Packet),
    BinFrom = mod_global_distrib_utils:recipient_to_worker_key(From, opt(global_host)),
    Data = <<(byte_size(BinFrom)):16, BinFrom/binary, BinPacket/binary>>,
    Stamp = erlang:monotonic_time(),
    ok = mod_global_distrib_utils:cast_or_call(Worker, {data, Stamp, Data}).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    Opts = [{listen_port, 5555},
            {connections_per_endpoint, 1},
            {endpoint_refresh_interval, 60},
            {disabled_gc_interval, 60} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: ejabberd:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start() -> any().
start() ->
    opt(tls_opts), %% Check for required tls_opts
    ConnsSup = mod_global_distrib_outgoing_conns_sup,
    ChildSpec = #{
      id => ConnsSup,
      start => {ConnsSup, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => supervisor,
      modules => [ConnsSup]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

-spec stop() -> any().
stop() ->
    ConnsSup = mod_global_distrib_outgoing_conns_sup,
    supervisor:terminate_child(ejabberd_sup, ConnsSup),
    supervisor:delete_child(ejabberd_sup, ConnsSup).

-spec get_process_for(ejabberd:lserver()) -> pid().
get_process_for(Server) ->
    mod_global_distrib_outgoing_conns_sup:get_connection(Server).

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

