-module(mod_mam_con_ca_sup).
-author('arcusfelis@gmail.com').

-behaviour(supervisor).

-export([start/2, stop/1, start_link/2, init/1]).

start(Host, Addresses) ->
    supervisor:start_child(ejabberd_sup, supervisor_spec(Host, Addresses)).

stop(Host) ->
    Tag = {mod_mam_con_ca_sup, Host},
    supervisor:terminate_child(ejabberd_sup, Tag),
    supervisor:delete_child(ejabberd_sup, Tag).

start_link(Host, Addresses) ->
    supervisor2:start_link({local, ?MODULE}, ?MODULE, [Host, Addresses]).

supervisor_spec(Host, Addresses) ->
    {{mod_mam_con_ca_sup, Host},
     {?MODULE, start_link, [Host, Addresses]},
     permanent,
     infinity,
     supervisor,
     [?MODULE]}.

worker_spec(Host, Addr, Port, WorkerNumber) ->
    {{Host, Addr, Port, WorkerNumber},
     {mod_mam_con_ca_arch, start_link, [Host, Addr, Port]},
     {permanent, 10}, %% Delay is 10 seconds
     infinity,
     worker,
     [mod_mam_con_ca_arch]}.

worker_specs(Host, Addresses) ->
    [worker_spec(Host, Addr, Port, WorkerNumber)
     || {Addr, Port, WorkerCount} <- Addresses,
        WorkerNumber <- lists:seq(1, WorkerCount)].

init([Host, Addresses]) ->
    Specs = worker_specs(Host, Addresses),
    {ok, {{one_for_one, 10, 1}, Specs}}.

