-module(graphql_helper).

-import(distributed_helper, [mim/0, rpc/4]).

-export([execute/3, execute_auth/2, execute_domain_auth/2, execute_user/3]).
-export([init_admin_handler/1, init_domain_admin_handler/1, end_domain_admin_handler/1]).
-export([get_listener_port/1, get_listener_config/1]).
-export([get_ok_value/2, get_err_msg/1, get_err_msg/2, get_err_code/1, make_creds/1,
         user_to_bin/1, user_to_full_bin/1, user_to_jid/1, user_to_lower_jid/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").

-spec execute(atom(), binary(), {binary(), binary()} | undefined) ->
    {Status :: tuple(), Data :: map()}.
execute(EpName, Body, Creds) ->
    Request =
      #{port => get_listener_port(EpName),
        role => {graphql, EpName},
        method => <<"POST">>,
        return_maps => true,
        creds => Creds,
        path => "/graphql",
        body => Body},
    rest_helper:make_request(Request).

execute_auth(Body, Config) ->
    Ep = ?config(schema_endpoint, Config),
    #{username := Username, password := Password} = get_listener_opts(Ep),
    execute(Ep, Body, {Username, Password}).

execute_domain_auth(Body, Config) ->
    Ep = ?config(schema_endpoint, Config),
    Creds = ?config(domain_admin, Config),
    execute(Ep, Body, Creds).

execute_user(Body, User, Config) ->
    Ep = ?config(schema_endpoint, Config),
    Creds = make_creds(User),
    execute(Ep, Body, Creds).

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
    case maps:is_key(username, Opts) of
        true ->
            [{schema_endpoint, Endpoint}, {listener_opts, Opts} | Config];
        false ->
            ct:fail(<<"Admin credentials are not defined in config">>)
    end.

init_domain_admin_handler(Config) ->
    Domain = domain_helper:domain(),
    Password = base16:encode(crypto:strong_rand_bytes(8)),
    Creds = {<<"admin@", Domain/binary>>, Password},
    ok = domain_helper:set_domain_password(mim(), Domain, Password),
    [{domain_admin, Creds}, {schema_endpoint, domain_admin} | Config].

end_domain_admin_handler(Config) ->
    {JID, _} = ?config(domain_admin, Config),
    Domain = escalus_utils:get_server(JID),
    domain_helper:delete_domain_password(mim(), Domain).

get_listener_opts(EpName) ->
    #{handlers := [Opts]} = get_listener_config(EpName),
    Opts.

get_err_code(Resp) ->
    get_ok_value([errors, 1, extensions, code], Resp).

-spec get_err_msg(#{errors := [#{message := binary()}]}) -> binary().
get_err_msg(Resp) ->
    get_ok_value([errors, 1, message], Resp).

-spec get_err_msg(pos_integer(), #{errors := [#{message := binary()}]}) -> binary().
get_err_msg(N, Resp) ->
    get_ok_value([errors, N, message], Resp).

-spec get_ok_value([atom()], {tuple(), map()}) -> binary().
get_ok_value([errors, N | Path], {{<<"200">>, <<"OK">>}, #{<<"errors">> := Errors}}) ->
    get_value(Path, lists:nth(N, Errors));
get_ok_value(Path, {{<<"200">>, <<"OK">>}, Data}) ->
    get_value(Path, Data).

make_creds(#client{props = Props} = Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    Password = proplists:get_value(password, Props),
    {JID, Password}.

user_to_full_bin(#client{} = Client) -> escalus_client:full_jid(Client);
user_to_full_bin(Bin) when is_binary(Bin) -> Bin.

user_to_bin(#client{} = Client) -> escalus_client:short_jid(Client);
user_to_bin(Bin) when is_binary(Bin) -> Bin;
user_to_bin(null) -> null.

user_to_jid(#client{jid = JID}) -> jid:to_bare(jid:from_binary(JID));
user_to_jid(Bin) when is_binary(Bin) -> jid:to_bare(jid:from_binary(Bin)).

user_to_lower_jid(#client{} = C) ->
    jid:from_binary(escalus_utils:jid_to_lower(escalus_client:short_jid(C)));
user_to_lower_jid(Bin) when is_binary(Bin) ->
    jid:to_bare(jid:from_binary(escalus_utils:jid_to_lower(Bin))).

%% Internal

% Gets a nested value given a path
get_value([], Data) -> Data;
get_value([Field | Fields], Data) ->
    BinField = atom_to_binary(Field),
    Data2 = maps:get(BinField, Data),
    get_value(Fields, Data2).

is_graphql_config(#{module := ejabberd_cowboy, handlers := Handlers}, ExpEpName) ->
    lists:any(fun(#{module := mongoose_graphql_cowboy_handler, schema_endpoint := EpName}) ->
                      ExpEpName =:= EpName;
                 (_) -> false
              end, Handlers);
is_graphql_config(_, _EpName) ->
    false.
