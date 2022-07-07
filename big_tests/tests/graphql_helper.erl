-module(graphql_helper).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).

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

execute_command(Category, Command, Args, Config) ->
    Protocol = ?config(protocol, Config),
    execute_command(Category, Command, Args, Config, Protocol).

execute_command(Category, Command, Args, Config, http) ->
    {ok, Doc} = rpc(mim(), mongoose_graphql_commands, find_document, [Category, Command]),
    execute_auth(#{query => Doc, variables => Args}, Config);
execute_command(Category, Command, Args, Config, cli) ->
    VarsJSON = jiffy:encode(Args),
    {Result, Code} = mongooseimctl_helper:mongooseimctl(Category, [Command, VarsJSON], Config),
    {{exit_status, Code}, rest_helper:decode(Result, #{return_maps => true})}.

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
            [{protocol, http}, {schema_endpoint, Endpoint}, {listener_opts, Opts} | Config];
        false ->
            ct:fail(<<"Admin credentials are not defined in config">>)
    end.

init_admin_cli(Config) ->
    [{protocol, cli} | Config].

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
    get_value([extensions, code], get_error(1, Resp)).

get_err_msg(Resp) ->
    get_err_msg(1, Resp).

get_err_msg(N, Resp) ->
    get_value([message], get_error(N, Resp)).

get_error(N, {Code, #{<<"errors">> := Errors}}) ->
    assert_response_code(error, Code),
    lists:nth(N, Errors).

get_ok_value(Path, {Code, Data}) ->
    assert_response_code(ok, Code),
    get_value(Path, Data).

assert_response_code(_, {<<"200">>, <<"OK">>}) -> ok;
assert_response_code(error, {exit_status, 1}) -> ok;
assert_response_code(ok, {exit_status, 0}) -> ok;
assert_response_code(Type, Code) ->
    error(#{what => invalid_response_code, expected_type => Type, response_code => Code}).

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
