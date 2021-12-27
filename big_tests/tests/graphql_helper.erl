-module(graphql_helper).

-import(distributed_helper, [mim/0, rpc/4]).

-export([execute/3, get_listener_port/1, get_listener_config/1]).

-spec execute(atom(), binary(), {binary(), binary()} | undefined) ->
    {Status :: tuple(), Data :: map()}.
execute(EpName, Body, Creds) ->
    Request =
      #{port => get_listener_port(EpName),
        role => {graphql, atom_to_binary(EpName)},
        method => <<"POST">>,
        return_maps => true,
        creds => Creds,
        path => "/graphql",
        body => Body},
    rest_helper:make_request(Request).

-spec get_listener_port(binary()) -> integer().
get_listener_port(EpName) ->
    {PortIpNet, ejabberd_cowboy, _Opts} = get_listener_config(EpName),
    element(1, PortIpNet).

-spec get_listener_config(binary()) -> tuple().
get_listener_config(EpName) ->
    Listeners = rpc(mim(), mongoose_config, get_opt, [listen]),
    [{_, ejabberd_cowboy, _} = Config] =
        lists:filter(fun(Config) -> is_graphql_config(Config, EpName) end, Listeners),
    Config.

%% Internal

is_graphql_config({_PortIpNet, ejabberd_cowboy, Opts}, EpName) ->
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Opts),
    lists:any(fun({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                      atom_to_binary(EpName) == proplists:get_value(schema_endpoint, Args);
                 (_) -> false
              end, Modules);
is_graphql_config(_, _EpName) ->
    false.
