-module(graphql_helper).

-import(distributed_helper, [mim/0, rpc/4]).

-export([execute/3, execute_auth/2, get_listener_port/1, get_listener_config/1]).
-export([init_admin_handler/1]).
-export([get_ok_value/2, get_err_msg/1, make_creds/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").

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
    #{port := Port} = get_listener_config(EpName),
    Port.

-spec get_listener_config(binary()) -> map().
get_listener_config(EpName) ->
    Listeners = rpc(mim(), mongoose_config, get_opt, [listen]),
    [Config] =
        lists:filter(fun(Config) -> is_graphql_config(Config, EpName) end, Listeners),
    Config.

init_admin_handler(Config) ->
    Endpoint = admin,
    Opts = get_listener_opts(Endpoint),
    case proplists:is_defined(username, Opts) of
        true ->
            [{schema_endpoint, Endpoint}, {listener_opts, Opts} | Config];
        false ->
            ct:fail(<<"Admin credentials are not defined in config">>)
    end.

get_listener_opts(EpName) ->
    #{modules := Modules} = get_listener_config(EpName),
    [Opts2] = lists:filtermap(
        fun
            ({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                {true, Args};
            (_) ->
                false
        end, Modules),
    Opts2.

-spec get_err_msg(#{errors := [#{message := binary()}]}) -> binary().
get_err_msg(Resp) ->
    get_ok_value([errors, message], Resp).

-spec get_ok_value([atom()], {tuple(), map()}) -> binary().
get_ok_value([errors | Path], {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Error]}}) ->
    get_value(Path, Error);
get_ok_value(Path, {{<<"200">>, <<"OK">>}, Data}) ->
    get_value(Path, Data).

make_creds(#client{props = Props} = Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    Password = proplists:get_value(password, Props),
    {JID, Password}.

%% Internal

% Gets a nested value given a path
get_value([], Data) -> Data;
get_value([Field | Fields], Data) ->
    BinField = atom_to_binary(Field),
    Data2 = maps:get(BinField, Data),
    get_value(Fields, Data2).

is_graphql_config(#{module := ejabberd_cowboy, modules := Modules}, EpName) ->
    lists:any(fun({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                      atom_to_binary(EpName) == proplists:get_value(schema_endpoint, Args);
                 (_) -> false
              end, Modules);
is_graphql_config(_, _EpName) ->
    false.
