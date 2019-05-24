-module(mongoose_wpool_ldap).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4, stop/2]).

init() ->
    ok.

start(Host, Tag, WpoolOpts, ConnOpts) ->
    WorkerSpec = {mongoose_ldap_worker, ConnOpts},
    Name = mongoose_wpool:make_pool_name(ldap, Host, Tag),
    mongoose_wpool:start_sup_pool(ldap, Name, [{worker, WorkerSpec} | WpoolOpts]).

stop(_Host, _Tag) ->
    ok.
