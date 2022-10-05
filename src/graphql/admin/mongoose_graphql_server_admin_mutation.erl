-module(mongoose_graphql_server_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).
-export([await_execution/4]).

-import(mongoose_graphql_helper, [make_error/2]).
-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(#{method := cli}, server, <<"joinCluster">>, #{<<"node">> := Node}) ->
    case mongoose_server_api:join_cluster(binary_to_list(Node)) of
        {mnesia_error, _} = Error ->
            make_error(Error, #{cluster => Node});
        {error, Message} ->
            make_error({internal_server_error, io_lib:format("~p", [Message])},
                       #{cluster => Node});
        {pang, String} ->
            make_error({timeout_error, String}, #{cluster => Node});
        {_, String} ->
            {ok, String}
    end;
execute(#{method := http}, server, <<"joinCluster">>, #{<<"node">> := Node}) ->
    spawn(?MODULE, await_execution,
          [1000, mongoose_server_api, join_cluster, [binary_to_list(Node)]]),
    {ok, "JoinCluster scheduled"};

execute(#{method := cli}, server, <<"removeFromCluster">>, #{<<"node">> := Node}) ->
    case mongoose_server_api:remove_from_cluster(binary_to_list(Node)) of
        {ok, _} = Result ->
            Result;
        Error ->
            make_error(Error, #{node => Node})
    end;
execute(#{method := http}, server, <<"removeFromCluster">>, #{<<"node">> := Node}) ->
    spawn(?MODULE, await_execution,
          [1000, mongoose_server_api, remove_from_cluster, [binary_to_list(Node)]]),
    {ok, "RemoveFromCluster scheduled"};

execute(#{method := cli}, server, <<"leaveCluster">>, #{}) ->
    case mongoose_server_api:leave_cluster() of
        {error, Message} ->
            make_error({internal_server_error, io_lib:format("~p", [Message])}, #{});
        {not_in_cluster, String} ->
            make_error({not_in_cluster_error, String}, #{});
        {_, String} ->
            {ok, String}
    end;
execute(#{method := http}, server, <<"leaveCluster">>, #{}) ->
    spawn(?MODULE, await_execution, [1000, mongoose_server_api, leave_cluster, []]),
    {ok, "LeaveCluster scheduled"};

execute(_Ctx, server, <<"removeNode">>, #{<<"node">> := Node}) ->
    mongoose_server_api:remove_node(binary_to_list(Node));

execute(_Ctx, server, <<"setLoglevel">>, #{<<"level">> := LogLevel}) ->
    mongoose_server_api:set_loglevel(LogLevel);
execute(_Ctx, server, <<"stop">>, #{}) ->
    spawn(mongoose_server_api, stop, []),
    {ok, "Stop scheduled"};
execute(_Ctx, server, <<"restart">>, #{}) ->
    spawn(mongoose_server_api, restart, []),
    {ok, "Restart scheduled"}.

%% Helpers

await_execution(Timeout, Module, Fun, Args) ->
    timer:sleep(Timeout),
    apply(Module, Fun, Args).
