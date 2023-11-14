-module(mongoose_node_address).
-export([init/0, remember_addresses/1]).

init() ->
    ets:new(?MODULE, [named_table, public]).

%% We also should send our IPs to other nodes on nodeup
remember_addresses(Pairs) ->
    ets:insert(?MODULE, Pairs).
