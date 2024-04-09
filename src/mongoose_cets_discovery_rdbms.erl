%% @doc MongooseIM RDBMS backend for cets_discovery.
-module(mongoose_cets_discovery_rdbms).
-behaviour(cets_discovery).
-export([init/1, get_nodes/1]).

%% these functions are exported for testing purposes only.
-export([select/1, insert_new/5, update_existing/3, delete_node_from_db/1,
         cluster_name_with_vsn/1]).
-ignore_xref([select/1, insert_new/5, update_existing/3, delete_node_from_db/1,
              cluster_name_with_vsn/1]).

-include("mongoose_logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type opts() :: #{cluster_name := binary(), node_name_to_insert := binary(),
                  last_query_info => map(), expire_time => non_neg_integer(),
                  node_ip_binary => binary(),
                  any() => any()}.

-type state() :: #{cluster_name := binary(), node_name_to_insert := binary(),
                   last_query_info := map(), expire_time := non_neg_integer(),
                   node_ip_binary := binary(), address_pairs := #{binary() => binary()}}.

-spec init(opts()) -> state().
init(Opts = #{cluster_name := ClusterName, node_name_to_insert := Node})
       when is_binary(ClusterName), is_binary(Node) ->
    Keys = [node_name_to_insert, expire_time, last_query_info, node_ip_binary],
    StateOpts = maps:merge(defaults(), maps:with(Keys, Opts)),
    StateOpts#{cluster_name => cluster_name_with_vsn(ClusterName)}.

cluster_name_with_vsn(ClusterName) ->
    {ok, CetsVsn} = application:get_key(cets, vsn),
    [MajorVsn, MinorVsn | _] = string:tokens(CetsVsn, "."),
    iolist_to_binary([ClusterName, $-, MajorVsn, $., MinorVsn]).

defaults() ->
    #{expire_time => 60 * 60 * 1, %% 1 hour in seconds
      last_query_info => #{},
      node_ip_binary => <<>>,
      address_pairs => #{}}.

-spec get_nodes(state()) -> {cets_discovery:get_nodes_result(), state()}.
get_nodes(State = #{cluster_name := ClusterName, node_name_to_insert := Node}) ->
    case is_rdbms_running() of
        true ->
            try try_register(ClusterName, Node, State) of
                {Num, Nodes, Info, AddrPairs} ->
                    mongoose_node_num:set_node_num(Num),
                    {{ok, [binary_to_atom(N) || N <- Nodes]},
                     State#{last_query_info => Info, address_pairs => AddrPairs}}
            catch Class:Reason:Stacktrace ->
                ?LOG_ERROR(#{what => discovery_failed_select, class => Class,
                             reason => Reason, stacktrace => Stacktrace}),
                {{error, Reason}, State}
            end;
        false ->
            {{error, rdbms_not_running}, State}
    end.

is_rdbms_running() ->
    try mongoose_wpool:get_worker(rdbms, global) of
         {ok, _} -> true;
         _ -> false
    catch _:_ ->
         false
    end.

try_register(ClusterName, Node, State = #{node_ip_binary := Address})
    when is_binary(Node), is_binary(ClusterName) ->
    prepare(),
    Timestamp = timestamp(),
    {selected, Rows} = select(ClusterName),
    Nodes = [element(1, Row) || Row <- Rows],
    Nums = [element(2, Row) || Row <- Rows],
    Addresses = [element(3, Row) || Row <- Rows],
    AddrPairs = maps:from_list(lists:zip(Nodes, Addresses)),
    AlreadyRegistered = lists:member(Node, Nodes),
    NodeNum =
        case AlreadyRegistered of
            true ->
                 update_existing(Node, Address, Timestamp),
                 {value, {_, Num, _Addr, _TS}} = lists:keysearch(Node, 1, Rows),
                 Num;
            false ->
                 Num = first_free_num(lists:usort(Nums)),
                 delete_node_from_db(Node), % Delete node if it was a member of another cluster
                 %% Could fail with duplicate node_num reason.
                 %% In this case just wait for the next get_nodes call.
                 case insert_new(ClusterName, Node, Num, Address, Timestamp) of
                     {error, _} -> 0; %% return default node num
                     {updated, 1} -> Num
                 end
        end,
    RunCleaningResult = run_cleaning(Timestamp, Rows, State),
    %% This could be used for debugging
    Info = #{already_registered => AlreadyRegistered, timestamp => Timestamp,
             address => Address,
             node_num => Num, last_rows => Rows, run_cleaning_result => RunCleaningResult},
    {NodeNum, skip_expired_nodes(Nodes, RunCleaningResult), Info, AddrPairs}.

skip_expired_nodes(Nodes, {removed, ExpiredNodes}) ->
    (Nodes -- ExpiredNodes).

run_cleaning(Timestamp, Rows, State) ->
    #{expire_time := ExpireTime, node_name_to_insert := CurrentNode} = State,
    ExpiredNodes = [DbNode || {DbNode, _Num, _Addr, DbTS} <- Rows,
                              is_expired(DbTS, Timestamp, ExpireTime),
                              DbNode =/= CurrentNode],
    [delete_node_from_db(DbNode) || DbNode <- ExpiredNodes],
    case ExpiredNodes of
        [] -> ok;
        [_ | _] ->
            ?LOG_WARNING(#{what => cets_expired_nodes,
                           text => <<"Expired nodes are detected in discovery_nodes table">>,
                           expired_nodes => ExpiredNodes})
    end,
    {removed, ExpiredNodes}.

is_expired(DbTS, Timestamp, ExpireTime) when is_integer(Timestamp),
                                             is_integer(ExpireTime),
                                             is_integer(DbTS) ->
    (Timestamp - DbTS) > ExpireTime. %% compare seconds

prepare() ->
    T = discovery_nodes,
    mongoose_rdbms_timestamp:prepare(),
    mongoose_rdbms:prepare(cets_disco_select, T, [cluster_name], select()),
    mongoose_rdbms:prepare(cets_disco_insert_new, T,
                           [cluster_name, node_name, node_num, address, updated_timestamp], insert_new()),
    mongoose_rdbms:prepare(cets_disco_update_existing, T,
                           [updated_timestamp, address, node_name], update_existing()),
    mongoose_rdbms:prepare(cets_delete_node_from_db, T,
                           [node_name], delete_node_from_db()).

select() ->
    <<"SELECT node_name, node_num, address, updated_timestamp FROM discovery_nodes WHERE cluster_name = ?">>.

select(ClusterName) ->
    mongoose_rdbms:execute_successfully(global, cets_disco_select, [ClusterName]).

insert_new() ->
    <<"INSERT INTO discovery_nodes (cluster_name, node_name, node_num, address, updated_timestamp)"
      " VALUES (?, ?, ?, ?, ?)">>.

insert_new(ClusterName, NodeName, NodeNum, Address, UpdatedTimestamp) ->
    mongoose_rdbms:execute(global, cets_disco_insert_new,
                           [ClusterName, NodeName, NodeNum, Address, UpdatedTimestamp]).

update_existing() ->
    <<"UPDATE discovery_nodes SET updated_timestamp = ?, address = ? WHERE node_name = ?">>.

update_existing(NodeName, Address, UpdatedTimestamp) ->
    mongoose_rdbms:execute(global, cets_disco_update_existing, [UpdatedTimestamp, Address, NodeName]).

delete_node_from_db() ->
    <<"DELETE FROM discovery_nodes WHERE node_name = ?">>.

delete_node_from_db(Node) ->
    mongoose_rdbms:execute_successfully(global, cets_delete_node_from_db, [Node]).

%% in seconds
timestamp() ->
    % We could use Erlang timestamp os:system_time(second).
    % But we use the database server time as a central source of truth.
    mongoose_rdbms_timestamp:select().

%% Returns a next free node id based on the currently registered ids
first_free_num(Nums) ->
    %% 0 is default node_num, so lets start from 1
    [FirstFreeNum | _] = lists:seq(1, length(Nums)+1) -- Nums,
    FirstFreeNum.

-ifdef(TEST).

jid_to_opt_binary_test_() ->
    [?_assertEqual(1, first_free_num([])),
     ?_assertEqual(3, first_free_num([1, 2, 5])),
     ?_assertEqual(1, first_free_num([2, 5])),
     ?_assertEqual(3, first_free_num([1, 2]))].

-endif.
