%% EPMD implementation which redefines how name lookups work
%% There is no behaviour for erl_epmd
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

%% For debugging
-export([lookup_ip/1]).
-export([match_node_name/2]).

-type lookup_error() ::
    {no_ip_in_db, node()}
  | {cannot_connect_to_epmd, node(), inet:ip_address()}
  | {no_record_for_node, node()}
  | mongoose_node_address_ets_table_not_found.

-ignore_xref([
    start/0,
    start_link/0,
    stop/0,
    port_please/2, port_please/3,
    listen_port_please/2,
    names/0, names/1,
    register_node/2, register_node/3,
    address_please/3,
    open/0, open/1, open/2,
    lookup_ip/1, match_node_name/2
]).

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
    Node = list_to_binary(Name ++ "@" ++ Host),
    case lookup_ip(Node) of
        {ok, _IP} = Res ->
            Res;
        _ ->
            %% Fallback to the default behaviour
            inet:getaddr(Host, AddressFamily)
    end.

%% This function waits for the IP address to appear.
%% It only works well in case net_kernel does not contact a lot of
%% dead nodes.
%% Generally, we only expect calls for nodes that are alive
%% and the connect is issued by CETS (but CETS checks that node is
%% reachable first) or by connect_all feature in the global module of OTP.
-spec lookup_ip(binary()) -> {ok, inet:ip_address()} | {error, lookup_error()}.
lookup_ip(Node) ->
    try
        cets_discovery:wait_for_get_nodes(mongoose_cets_discovery, 3000),
        cets_discovery:system_info(mongoose_cets_discovery)
    of
        #{backend_state := BackendState} ->
            match_node_name(BackendState, Node)
    catch _:_ ->
        {error, failed_to_get_disco_state}
    end.

match_node_name(#{address_pairs := Pairs}, Node) ->
    case Pairs of
        #{Node := <<>>} ->
            %% The caller should try DNS.
            {error, {no_ip_in_db, Node}};
        #{Node := Bin} ->
            inet:parse_address(binary_to_list(Bin));
        #{} ->
            {error, {no_record_for_node, Node}}
    end;
match_node_name(BackendState, Node) ->
    %% Could happen if CETS backend is not mongoose_cets_discovery_rdbms
    ?LOG_ERROR(#{what => cets_no_address_pairs_set,
                 backend_state => BackendState,
                 remote_node => Node}),
    {error, no_address_pairs_set}.
