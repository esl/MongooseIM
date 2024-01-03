-module(mongoose_cets_discovery).
-export([start_link/1]).
-export([supervisor_specs/0]).

-ignore_xref([start_link/1]).

start_link(DiscoOpts) ->
    Res = cets_discovery:start_link(DiscoOpts),
    %% Ensure metrics are added after the disco start
    mongoose_metrics_probe_cets:start(),
    Res.

-include("mongoose_logger.hrl").

supervisor_specs() ->
    supervisor_specs(mongoose_config:get_opt([internal_databases, cets], disabled)).

supervisor_specs(disabled) ->
    [];
supervisor_specs(#{backend := DiscoBackend, cluster_name := ClusterName} = Opts) ->
    DiscoFile =
        case {DiscoBackend, Opts} of
            {file, #{node_list_file := NodeFile}} ->
                NodeFile;
            {file, _} ->
                ?LOG_CRITICAL(#{what => node_list_file_option_is_required,
                                text => <<"Specify internal_databases.cets.node_list_file option">>}),
                error(node_list_file_option_is_required);
            _ ->
                undefined
        end,
    DiscoOpts = #{
        backend_module => disco_backend_to_module(DiscoBackend),
        cluster_name => atom_to_binary(ClusterName),
        node_name_to_insert => atom_to_binary(node(), latin1),
        node_ip_binary => get_node_ip_binary(),
        name => mongoose_cets_discovery, disco_file => DiscoFile},
    CetsDisco = #{
        id => cets_discovery,
        start => {?MODULE, start_link, [DiscoOpts]},
        restart => permanent,
        type => worker,
        shutdown => infinity,
        modules => [cets_discovery]},
    [CetsDisco].

disco_backend_to_module(rdbms) -> mongoose_cets_discovery_rdbms;
disco_backend_to_module(file) -> cets_discovery_file.

get_node_ip_binary() ->
    list_to_binary(os:getenv("MIM_NODE_IP", "")).
