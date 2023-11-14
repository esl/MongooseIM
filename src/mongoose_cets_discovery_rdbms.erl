%% @doc MongooseIM RDBMS backend for cets_discovery.
-module(mongoose_cets_discovery_rdbms).
-behaviour(cets_discovery).
-export([init/1, get_nodes/1]).

%% these functions are exported for testing purposes only.
-export([select/1, insert_new/5, update_existing/4, delete_node_from_db/2]).
-ignore_xref([select/1, insert_new/5, update_existing/4, delete_node_from_db/2]).

-include("mongoose_logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type opts() :: #{cluster_name := binary(), node_name_to_insert := binary(),
                  last_query_info => map(), expire_time => non_neg_integer(),
                  any() => any()}.

-type state() :: #{cluster_name := binary(), node_name_to_insert := binary(),
                   last_query_info := map(), expire_time := non_neg_integer()}.

-spec init(opts()) -> state().
init(Opts = #{cluster_name := _, node_name_to_insert := _}) ->
    Keys = [cluster_name, node_name_to_insert, last_query_info, expire_time],
    maps:with(Keys, maps:merge(defaults(), Opts)).

defaults() ->
    #{expire_time => 60 * 60 * 1, %% 1 hour in seconds
      last_query_info => #{}}.

-spec get_nodes(state()) -> {cets_discovery:get_nodes_result(), state()}.
get_nodes(State = #{cluster_name := ClusterName, node_name_to_insert := Node}) ->
    case is_rdbms_running() of
        true ->
            try try_register(ClusterName, Node, State) of
                {Num, Nodes, Info} ->
                    mongoose_node_num:set_node_num(Num),
                    {{ok, [binary_to_atom(N) || N <- Nodes]},
                     State#{last_query_info => Info}}
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

try_register(ClusterName, Node, State) when is_binary(Node), is_binary(ClusterName) ->
    prepare(),
    Timestamp = timestamp(),
    {selected, Rows} = select(ClusterName),
    Nodes = [element(1, Row) || Row <- Rows],
    Nums = [element(2, Row) || Row <- Rows],
    AlreadyRegistered = lists:member(Node, Nodes),
    Address = os:getenv("POD_IP", ""),
    NodeNum =
        case AlreadyRegistered of
            true ->
                 update_existing(ClusterName, Node, Timestamp, Address),
                 {value, {_, Num, _Addr, _TS}} = lists:keysearch(Node, 1, Rows),
                 Num;
            false ->
                 Num = first_free_num(lists:usort(Nums)),
                 %% Could fail with duplicate node_num reason.
                 %% In this case just wait for the next get_nodes call.
                 case insert_new(ClusterName, Node, Timestamp, Num, Address) of
                     {error, _} -> 0; %% return default node num
                     {updated, 1} -> Num
                 end
        end,
    RunCleaningResult = run_cleaning(ClusterName, Timestamp, Rows, State),
    %% This could be used for debugging
    Info = #{already_registered => AlreadyRegistered, timestamp => Timestamp,
             address => Address,
             node_num => Num, last_rows => Rows, run_cleaning_result => RunCleaningResult},
    {NodeNum, skip_expired_nodes(Nodes, RunCleaningResult), Info}.

skip_expired_nodes(Nodes, {removed, ExpiredNodes}) ->
    (Nodes -- ExpiredNodes).

run_cleaning(ClusterName, Timestamp, Rows, State) ->
    #{expire_time := ExpireTime, node_name_to_insert := CurrentNode} = State,
    ExpiredNodes = [DbNode || {DbNode, _Num, _Addr, DbTS} <- Rows,
                              is_expired(DbTS, Timestamp, ExpireTime),
                              DbNode =/= CurrentNode],
    [delete_node_from_db(ClusterName, DbNode) || DbNode <- ExpiredNodes],
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
                           [updated_timestamp, address, cluster_name, node_name], update_existing()),
    mongoose_rdbms:prepare(cets_delete_node_from_db, T,
                           [cluster_name, node_name], delete_node_from_db()).

select() ->
    <<"SELECT node_name, node_num, address, updated_timestamp FROM discovery_nodes WHERE cluster_name = ?">>.

select(ClusterName) ->
    mongoose_rdbms:execute_successfully(global, cets_disco_select, [ClusterName]).

insert_new() ->
    <<"INSERT INTO discovery_nodes (cluster_name, node_name, node_num, address, updated_timestamp)"
      " VALUES (?, ?, ?, ?, ?)">>.

insert_new(ClusterName, Node, Timestamp, Num, Address) ->
    mongoose_rdbms:execute(global, cets_disco_insert_new, [ClusterName, Node, Num, Address, Timestamp]).

update_existing() ->
    <<"UPDATE discovery_nodes SET updated_timestamp = ?, address = ? WHERE cluster_name = ? AND node_name = ?">>.

update_existing(ClusterName, Node, Timestamp, Address) ->
    mongoose_rdbms:execute(global, cets_disco_update_existing, [Timestamp, Address, ClusterName, Node]).

delete_node_from_db() ->
    <<"DELETE FROM discovery_nodes WHERE cluster_name = ? AND node_name = ?">>.

delete_node_from_db(ClusterName, Node) ->
    mongoose_rdbms:execute_successfully(global, cets_delete_node_from_db, [ClusterName, Node]).

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
