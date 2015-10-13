-module(mod_mam_con_ca_sup).
-author('arcusfelis@gmail.com').

-behaviour(supervisor).

-export([start/2, stop/1, start_link/2, init/1]).

start(Host, Config) ->
    supervisor:start_child(ejabberd_sup, supervisor_spec(Host, Config)).

stop(Host) ->
    Tag = {mod_mam_con_ca_sup, Host},
    supervisor:terminate_child(ejabberd_sup, Tag),
    supervisor:delete_child(ejabberd_sup, Tag).

start_link(Host, Config) ->
    supervisor2:start_link({local, ?MODULE}, ?MODULE, [Host, Config]).

supervisor_spec(Host, Config) ->
    {{mod_mam_con_ca_sup, Host},
     {?MODULE, start_link, [Host, Config]},
     permanent,
     infinity,
     supervisor,
     [?MODULE]}.

worker_spec(Host, Addr, Port, WorkerNumber, ClientOptions) ->
    {{Host, Addr, Port, WorkerNumber},
     {mod_mam_con_ca_arch, start_link, [Host, Addr, Port, ClientOptions]},
     {permanent, 10}, %% Delay is 10 seconds
     infinity,
     worker,
     [mod_mam_con_ca_arch]}.

worker_specs(Host, Servers, ClientOptions) ->
    [worker_spec(Host, Addr, Port, WorkerNumber, ClientOptions)
     || {Addr, Port, WorkerCount} <- Servers,
        WorkerNumber <- lists:seq(1, WorkerCount)].

init([Host, ClientOptions]) ->
    Servers = proplists:get_value(servers, ClientOptions),
    Specs = worker_specs(Host, Servers, ClientOptions),
    {ok, {{one_for_one, 10, 1}, Specs}}.

