-module(graphql_helper).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").

-spec execute(atom(), binary(), {binary(), binary()} | undefined) ->
    {Status :: tuple(), Data :: map()}.
execute(EpName, Body, Creds) ->
    #{node := Node} = mim(),
    execute(Node, EpName, Body, Creds).

-spec execute(node(), atom(), binary(), {binary(), binary()} | undefined) ->
    {Status :: tuple(), Data :: map()}.
execute(Node, EpName, Body, Creds) ->
    Request = build_request(Node, EpName, Body, Creds),
    rest_helper:make_request(Request).

build_request(Node, EpName, Body, Creds) ->
    #{port => get_listener_port(Node, EpName),
      role => {graphql, EpName},
      method => <<"POST">>,
      return_maps => true,
      creds => Creds,
      path => "/graphql",
      body => Body}.

execute_sse(EpName, Params, Creds) ->
    #{node := Node} = mim(),
    execute_sse(Node, EpName, Params, Creds).

execute_sse(Node, EpName, Params, Creds) ->
    Port = get_listener_port(Node, EpName),
    Path = "/api/graphql/sse",
    QS = uri_string:compose_query([{atom_to_binary(K), encode_sse_value(V)}
                                   || {K, V} <- maps:to_list(Params)]),
    sse_helper:connect_to_sse(Port, [Path, "?", QS], Creds, #{}).

encode_sse_value(M) when is_map(M) -> jiffy:encode(M);
encode_sse_value(V) when is_binary(V) -> V.

execute_user_command(Category, Command, User, Args, Config) ->
    Doc = get_doc(Category, Command),
    execute_user(#{query => Doc, variables => Args}, User, Config).

execute_user_command_sse(Category, Command, User, Args, Config) ->
    Doc = get_doc(Category, Command),
    execute_user_sse(#{query => Doc, variables => Args}, User, Config).

execute_command(Category, Command, Args, Config) ->
    #{node := Node} = mim(),
    Protocol = ?config(protocol, Config),
    execute_command(Node, Category, Command, Args, Config, Protocol).

execute_command(Node, Category, Command, Args, Config) ->
    Protocol = ?config(protocol, Config),
    execute_command(Node, Category, Command, Args, Config, Protocol).

execute_command_sse(Category, Command, Args, Config) ->
    Doc = get_doc(Category, Command),
    execute_auth_sse(#{query => Doc, variables => Args}, Config).

%% Admin commands can be executed as GraphQL over HTTP or with CLI (mongooseimctl)
execute_command(Node, Category, Command, Args, Config, http) ->
    Doc = get_doc(Category, Command),
    execute_auth(Node, #{query => Doc, variables => Args}, Config);
execute_command(Node, Category, Command, Args, Config, cli) ->
    CLIArgs = encode_cli_args(Args),
    {Result, Code}
        = mongooseimctl_helper:mongooseimctl(Node, Category, [Command | CLIArgs], Config),
    {{exit_status, Code}, rest_helper:decode(Result, #{return_maps => true})}.

get_doc(Category, Command) ->
    #{Category := #{commands := #{Command := #{doc := Doc}}}} = get_specs(),
    Doc.

encode_cli_args(Args) ->
    lists:flatmap(fun({Name, Value}) -> encode_cli_arg(Name, Value) end, maps:to_list(Args)).
encode_cli_arg(_Name, null) ->
    [];
encode_cli_arg(Name, Value) ->
    [<<"--", (arg_name_to_binary(Name))/binary>>, arg_value_to_binary(Value)].

arg_name_to_binary(Name) when is_atom(Name) -> atom_to_binary(Name);
arg_name_to_binary(Name) when is_binary(Name) -> Name.

arg_value_to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
arg_value_to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
arg_value_to_binary(Value) when is_binary(Value) -> Value;
arg_value_to_binary(Value) when is_list(Value);
                                is_map(Value) -> iolist_to_binary(jiffy:encode(Value)).

execute_auth(Body, Config) ->
    #{node := Node} = mim(),
    execute_auth(Node, Body, Config).

execute_auth(Node, Body, Config) ->
    Ep = ?config(schema_endpoint, Config),
    execute(Node, Ep, Body, make_admin_creds(Ep, Config)).

execute_auth_sse(Body, Config) ->
    #{node := Node} = mim(),
    execute_auth_sse(Node, Body, Config).

execute_auth_sse(Node, Body, Config) ->
    Ep = ?config(schema_endpoint, Config),
    execute_sse(Node, Ep, Body, make_admin_creds(Ep, Config)).

make_admin_creds(admin = Ep, _Config) ->
    #{username := Username, password := Password} = get_listener_opts(Ep),
    {Username, Password};
make_admin_creds(domain_admin, Config) ->
    ?config(domain_admin, Config).

execute_user(Body, User, Config) ->
    Ep = ?config(schema_endpoint, Config),
    Creds = make_creds(User),
    #{node := Node} = mim(),
    execute(Node, Ep, Body, Creds).

execute_user_sse(Body, User, Config) ->
    Ep = ?config(schema_endpoint, Config),
    Creds = make_creds(User),
    #{node := Node} = mim(),
    execute_sse(Node, Ep, Body, Creds).

-spec get_listener_port(binary()) -> integer().
get_listener_port(EpName) ->
    #{node := Node} = mim(),
    get_listener_port(Node, EpName).

-spec get_listener_port(node(), binary()) -> integer().
get_listener_port(Node, EpName) ->
    #{port := Port} = get_listener_config(Node, EpName),
    Port.

-spec get_listener_config(node(), binary()) -> map().
get_listener_config(Node, EpName) ->
    Listeners = rpc(#{node => Node}, mongoose_config, get_opt, [listen]),
    [Config] =
        lists:filter(fun(Config) -> is_graphql_config(Config, EpName) end, Listeners),
    Config.

init_admin_handler(Config) ->
    Endpoint = admin,
    Opts = get_listener_opts(Endpoint),
    case maps:is_key(username, Opts) of
        true ->
            add_specs([{protocol, http}, {schema_endpoint, Endpoint}, {listener_opts, Opts}
                      | Config]);
        false ->
            ct:fail(<<"Admin credentials are not defined in config">>)
    end.

init_admin_cli(Config) ->
    add_specs([{protocol, cli}, {schema_endpoint, admin} | Config]).

init_user(Config) ->
    add_specs([{schema_endpoint, user} | Config]).

init_domain_admin_handler(Config) ->
    init_domain_admin_handler(Config, domain_helper:domain()).

init_domain_admin_handler(Config, Domain) ->
    case mongoose_helper:is_rdbms_enabled(Domain) of
        true ->
            Password = base16:encode(crypto:strong_rand_bytes(8)),
            Creds = {<<"admin@", Domain/binary>>, Password},
            domain_helper:set_domain_password(mim(), Domain, Password),
            add_specs([{protocol, http}, {domain_admin, Creds}, {schema_endpoint, domain_admin}
                      | Config]);
        false -> {skip, require_rdbms}
    end.

add_specs(Config) ->
    EpName = ?config(schema_endpoint, Config),
    Specs = rpc(mim(), mongoose_graphql_commands, build_specs, [EpName]),
    persistent_term:put(graphql_specs, Specs),
    Config.

get_specs() ->
    persistent_term:get(graphql_specs).

clean() ->
    persistent_term:erase(graphql_specs).

end_domain_admin_handler(Config) ->
    {JID, _} = ?config(domain_admin, Config),
    Domain = escalus_utils:get_server(JID),
    domain_helper:delete_domain_password(mim(), Domain).

get_listener_opts(EpName) ->
    #{node := Node} = mim(),
    #{handlers := [Opts]} = get_listener_config(Node, EpName),
    Opts.

get_not_loaded(Resp) ->
    ?assertEqual(<<"deps_not_loaded">>, get_err_code(Resp)),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Resp), <<"Some of the required">>)).

get_err_code(Resp) ->
    get_value([extensions, code], get_error(1, Resp)).

get_err_msg(Resp) ->
    get_err_msg(1, Resp).

get_unauthorized({Code, #{<<"errors">> := Errors}}) ->
    [#{<<"extensions">> := #{<<"code">> := _ErrorCode}}] = Errors,
    assert_response_code(unauthorized, Code).

get_bad_request({Code, _Msg}) ->
    assert_response_code(bad_request, Code).

get_method_not_allowed({Code, _Msg}) ->
    assert_response_code(method_not_allowed, Code).

get_coercion_err_msg({Code, #{<<"errors">> := [Error]}}) ->
    assert_response_code(bad_request, Code),
    ?assertEqual(<<"input_coercion">>, get_value([extensions, code], Error)),
    get_value([message], Error).

get_err_msg(N, Resp) ->
    get_value([message], get_error(N, Resp)).

get_error(N, {Code, #{<<"errors">> := Errors}}) ->
    assert_response_code(error, Code),
    lists:nth(N, Errors).

%% Expect both errors and successful responses
get_err_value(Path, {Code, Data}) ->
    assert_response_code(error, Code),
    get_value(Path, Data).

get_ok_value(Path, {Code, Data}) ->
    assert_response_code(ok, Code),
    get_value(Path, Data).

assert_response_code(bad_request, {<<"400">>, <<"Bad Request">>}) -> ok;
assert_response_code(unauthorized, {<<"401">>, <<"Unauthorized">>}) -> ok;
assert_response_code(method_not_allowed, {<<"405">>, <<"Method Not Allowed">>}) -> ok;
assert_response_code(error, {<<"200">>, <<"OK">>}) -> ok;
assert_response_code(ok, {<<"200">>, <<"OK">>}) -> ok;
assert_response_code(bad_request, 400) -> ok;
assert_response_code(unauthorized, 401) -> ok;
assert_response_code(method_not_allowed, 405) -> ok;
assert_response_code(error, 200) -> ok;
assert_response_code(ok, 200) -> ok;
assert_response_code(bad_request, {exit_status, 1}) -> ok;
assert_response_code(error, {exit_status, 1}) -> ok;
assert_response_code(ok, {exit_status, 0}) -> ok;
assert_response_code(Type, Code) ->
    error(#{what => invalid_response_code, expected_type => Type, response_code => Code}).

skip_null_fields(M) when is_map(M) ->
    M1 = maps:filter(fun(_K, V) -> V =/= null end, M),
    maps:map(fun(_K, V) -> skip_null_fields(V) end, M1);
skip_null_fields(L) when is_list(L) ->
    [skip_null_fields(Item) || Item <- L];
skip_null_fields(V) ->
    V.

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
    lists:any(fun(#{module := mongoose_graphql_handler, schema_endpoint := EpName}) ->
                      ExpEpName =:= EpName;
                 (_) -> false
              end, Handlers);
is_graphql_config(_, _EpName) ->
    false.
