-module(mongoose_epmd).
-export([
    start/0,
    start_link/0,
    stop/0,
    port_please/2, port_please/3,
    listen_port_please/2,
    names/0, names/1,
    register_node/2, register_node/3,
    address_please/3,
    open/0, open/1, open/2
]).
-include_lib("kernel/include/logger.hrl").

start() -> erl_epmd:start().
start_link() -> erl_epmd:start_link().
stop() -> erl_epmd:stop().
port_please(A, B) -> erl_epmd:port_please(A, B).
port_please(A, B, C) -> erl_epmd:port_please(A, B, C).
listen_port_please(A, B) -> erl_epmd:listen_port_please(A, B).
names() -> erl_epmd:names().
names(A) -> erl_epmd:names(A).
register_node(A, B) -> erl_epmd:register_node(A, B).
register_node(A, B, C) -> erl_epmd:register_node(A, B, C).
open() -> erl_epmd:open().
open(A) -> erl_epmd:open(A).
open(A, B) -> erl_epmd:open(A, B).

address_please(Name, Host, AddressFamily) ->
    Node = list_to_atom(Name ++ "@" ++ Host),
    case mongoose_node_address:lookup(Node) of
        {ok, _IP} = Res ->
            Res;
        _ ->
            %% Fallback to the default behaviour
            inet:getaddr(Host, AddressFamily)
    end.
