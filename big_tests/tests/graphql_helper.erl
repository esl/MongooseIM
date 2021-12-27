-module(graphql_helper).

-import(distributed_helper, [mim/0, rpc/4]).

-export([execute/3, execute_auth/2, get_listener_port/1, get_listener_config/1]).
-export([init_admin_handler/1]).

-include_lib("common_test/include/ct.hrl").

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

execute_auth(Body, Config) ->
    Ep = ?config(schema_endpoint, Config),
    Opts = get_listener_opts(Ep),
    User = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    execute(Ep, Body, {User, Password}).

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

init_admin_handler(Config) ->
    Endpoint = admin,
    Opts = get_listener_opts(Endpoint),
    case proplists:is_defined(username, Opts) of
        true ->
            [{schema_endpoint, Endpoint}, {listener_opts, Opts} | Config];
        false ->
            {skipped, <<"Admin credentials are not defined in config">>}
    end.

get_listener_opts(EpName) ->
    {_, ejabberd_cowboy, Opts} = get_listener_config(EpName),
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Opts),
    [Opts2] = lists:filtermap(
        fun
            ({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                {true, Args};
            (_) ->
                false
        end, Modules),
    Opts2.

%% Internal

is_graphql_config({_PortIpNet, ejabberd_cowboy, Opts}, EpName) ->
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Opts),
    lists:any(fun({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                      atom_to_binary(EpName) == proplists:get_value(schema_endpoint, Args);
                 (_) -> false
              end, Modules);
is_graphql_config(_, _EpName) ->
    false.
