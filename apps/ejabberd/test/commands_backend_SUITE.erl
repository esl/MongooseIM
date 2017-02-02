%% @doc This suite tests both old ejabberd_commands module, which is slowly getting deprecated,
%% and the new mongoose_commands implementation.
-module(commands_backend_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").

-define(PORT, 5288).
-define(HOST, "localhost").
-define(IP,  {127,0,0,1}).

%% Error messages
-define(ARGS_LEN_ERROR, <<"Bad parameters length.">>).
-define(ARGS_SPEC_ERROR, <<"Bad name of the parameter.">>).
-type method() :: string().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

client_module() ->
    mongoose_api_client.

backend_module() ->
    mongoose_api_admin.

all() ->
    [
     {group, simple_backend},
     {group, get_advanced_backend},
     {group, post_advanced_backend},
     {group, delete_advanced_backend},
      {group, simple_client}
    ].

groups() ->
    [
     {simple_backend, [sequence],
      [
       get_simple,
       post_simple,
       delete_simple,
       put_simple
      ]
     },
     {get_advanced_backend, [sequence],
      [
       get_two_args,
       get_wrong_path,
       get_wrong_arg_number,
       get_no_command,
       get_wrong_arg_type
      ]
     },
     {post_advanced_backend, [sequence],
      [
       post_simple_with_subcategory,
       post_different_arg_order,
       post_wrong_arg_number,
       post_wrong_arg_name,
       post_wrong_arg_type,
       post_no_command
      ]
     },
     {delete_advanced_backend, [sequence],
      [
       delete_wrong_arg_order,
       delete_wrong_arg_types
      ]
     },
     {put_advanced_backend, [sequence],
      [
       put_wrong_type,
       put_wrong_param_type,
       put_wrong_bind_type,
       put_different_params_order,
       put_wrong_binds_order,
       put_too_less_params,
       put_too_less_binds,
       put_wrong_bind_name,
       put_wrong_param_name
      ]
     },
     {simple_client, [sequence],
      [
       get_simple_client,
       get_two_args_client,
       get_bad_auth,
       post_simple_client,
       put_simple_client,
       delete_simple_client
      ]
     }
    ].

setup(Module) ->
    meck:unload(),
    meck:new(supervisor, [unstick, passthrough, no_link]),
    meck:new(ejabberd_hooks, []),
    meck:new(ejabberd_auth, []),
    %% you have to meck some stuff to get it working....
    meck:expect(ejabberd_hooks, add, fun(_, _, _, _, _) -> ok end),
    meck:expect(ejabberd_hooks, run_fold, fun(_, _, _, _) -> ok end),
    spawn(fun mc_holder/0),
    meck:expect(supervisor, start_child,
        fun(ejabberd_listeners, {_, {_, start_link, [_]}, transient,
            infinity, worker, [_]}) -> {ok, self()};
            (A,B) -> meck:passthrough([A,B])
        end),
    %% HTTP API config
    Opts = [{num_acceptors, 10},
        {max_connections, 1024},
        {modules, [{"localhost", "/api", Module, []}]}],
    ejabberd_cowboy:start_listener({?PORT, ?IP, tcp}, Opts).

teardown() ->
    cowboy:stop_listener(ejabberd_cowboy:ref({?PORT, ?IP, tcp})),
    mongoose_commands:unregister(commands_new()),
    meck:unload(ejabberd_auth),
    meck:unload(ejabberd_hooks),
    meck:unload(supervisor),
    mc_holder_proc ! stop,
    ok.

init_per_suite(C) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(stringprep),
    application:ensure_all_started(fusco),
    application:ensure_all_started(lager),
    ok = mnesia:start(),
    C.

end_per_suite(C) ->
    stopped = mnesia:stop(),
    mnesia:delete_schema([node()]),
    application:stop(lager),
    application:stop(fusco),
    application:stop(cowboy),
    C.

init_per_group(_, C) ->
    C.

end_per_group(_, C) ->
    C.

init_per_testcase(_, C) ->
    C.

end_per_testcase(_, C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Backend side tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_simple(_Config) ->
    Arg = {arg1, <<"bob@localhost">>},
    Base = "/api/users",
    ExpectedBody = get_simple_command(element(2, Arg)),
    {ok, Response} = request(create_path_with_binds(Base, [Arg]), "GET", admin),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

delete_simple(_Config) ->
    Arg1 = {arg1, <<"ala_ma_kota">>},
    Arg2 = {arg2, 2},
    Base = "/api/music",
    {ok, Response} = request(create_path_with_binds(Base, [Arg1, Arg2]), "DELETE", admin),
    check_status_code(Response, 204).

post_simple(_Config) ->
    Arg1 = {arg1, 10},
    Arg2 = {arg2, 2},
    Args = [Arg1, Arg2],
    Path = <<"/api/weather">>,
    Result = binary_to_list(post_simple_command(element(2, Arg1), element(2, Arg2))),
    {ok, Response} = request(Path, "POST", Args, admin),
    check_status_code(Response, 201),
    check_location_header(Response, list_to_binary(build_path_prefix()++"/api/weather/" ++ Result)).

post_simple_with_subcategory(_Config) ->
    Arg1 = {arg1, 10},
    Arg2 = {arg2, 2},
    Args = [Arg2],
    Path = <<"/api/weather/10/subcategory">>,
    Result = binary_to_list(post_simple_command(element(2, Arg1), element(2, Arg2))),
    {ok, Response} = request(Path, "POST", Args, admin),
    check_status_code(Response, 201),
    check_location_header(Response, list_to_binary(build_path_prefix()++"/api/weather/10/subcategory/" ++ Result)).

put_simple(_Config) ->
    Binds = [{arg1, <<"username">>}, {arg2,<<"localhost">>}],
    Args = [{arg3, <<"newusername">>}],
    Base = "/api/users",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Args, admin),
    check_status_code(Response, 204).

get_two_args(_Config) ->
    Arg1 = {arg1, 1},
    Arg2 = {arg2, 2},
    Base = "/api/animals",
    ExpectedBody = get_two_args_command(element(2, Arg1), element(2, Arg2)),
    {ok, Response} = request(create_path_with_binds(Base, [Arg1, Arg2]), "GET", admin),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_two_args_different_types(_Config) ->
    Arg1 = {one, 1},
    Arg2 = {two, <<"mybin">>},
    Base = "/api/books",
    ExpectedBody = get_two_args2_command(element(2, Arg1), element(2, Arg2)),
    {ok, Response} = request(create_path_with_binds(Base, [Arg1, Arg2]), "GET", admin),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_wrong_path(_Config) ->
    Path = <<"/api/animals2/1/2">>,
    {ok, Response} = request(Path, "GET", admin),
    check_status_code(Response, 404).

get_wrong_arg_number(_Config) ->
    Path = <<"/api/animals/1/2/3">>,
    {ok, Response} = request(Path, "GET", admin),
    check_status_code(Response, 404).

get_no_command(_Config) ->
    Path = <<"/api/unregistered_command/123123">>,
    {ok, Response} = request(Path, "GET", admin),
    check_status_code(Response, 404).

get_wrong_arg_type(_Config) ->
    Path = <<"/api/animals/1/wrong">>,
    {ok, Response} = request(Path, "GET", admin),
    check_status_code(Response, 400).

post_wrong_arg_number(_Config) ->
    Args = [{arg1, 10}, {arg2,2}, {arg3, 100}],
    Path = <<"/api/weather">>,
    {ok, Response} = request(Path, "POST", Args, admin),
    check_status_code(Response, 404).

post_wrong_arg_name(_Config) ->
    Args = [{arg11, 10}, {arg2,2}],
    Path = <<"/api/weather">>,
    {ok, Response} = request(Path, "POST", Args, admin),
    check_status_code(Response, 400).

post_wrong_arg_type(_Config) ->
    Args = [{arg1, 10}, {arg2,<<"weird binary">>}],
    Path = <<"/api/weather">>,
    {ok, Response} = request(Path, "POST", Args, admin),
    check_status_code(Response, 400).

post_different_arg_order(_Config) ->
    Arg1 = {arg1, 10},
    Arg2 = {arg2, 2},
    Args = [Arg2, Arg1],
    Path = <<"/api/weather">>,
    Result = binary_to_list(post_simple_command(element(2, Arg1), element(2, Arg2))),
    {ok, Response} = request(Path, "POST", Args, admin),
    check_status_code(Response, 201),
    check_location_header(Response, list_to_binary(build_path_prefix() ++"/api/weather/" ++ Result)).

post_no_command(_Config) ->
    Args = [{arg1, 10}, {arg2,2}],
    Path = <<"/api/weather/10">>,
    {ok, Response} = request(Path, "POST", Args, admin),
    check_status_code(Response, 404).


delete_wrong_arg_order(_Config) ->
    Arg1 = {arg1, <<"ala_ma_kota">>},
    Arg2 = {arg2, 2},
    Base = "/api/music",
    {ok, Response} = request(create_path_with_binds(Base, [Arg2, Arg1]), "DELETE", admin),
    check_status_code(Response, 400).

delete_wrong_arg_types(_Config) ->
    Arg1 = {arg1, 2},
    Arg2 = {arg2, <<"ala_ma_kota">>},
    Base = "/api/music",
    {ok, Response} = request(create_path_with_binds(Base, [Arg1, Arg2]), "DELETE", admin),
    check_status_code(Response, 400).

put_wrong_param_type(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"domain">>}],
    Parameters = [{age, <<"23">>}, {kids, 10}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 400).

put_wrong_bind_type(_Config) ->
    Binds = [{username, <<"username">>}, {domain, 123}],
    Parameters = [{age, 23}, {kids, 10}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 400).

put_different_params_order(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"domain">>}],
    Parameters = [{kids, 2}, {age, 45}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 200).

put_wrong_binds_order(_Config) ->
    Binds = [{domain, <<"domain">>}, {username, <<"username">>}],
    Parameters = [{kids, 2}, {age, 30}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 400).

put_too_less_params(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"domain">>}],
    Parameters = [{kids, 3}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 400).

put_too_less_binds(_Config) ->
    Binds = [{username, <<"username">>}],
    Parameters = [{age, 20}, {kids, 3}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 404).

put_wrong_bind_name(_Config) ->
    Binds = [{usersrejm, <<"username">>}, {domain, <<"localhost">>}],
    Parameters = [{age, 20}, {kids, 3}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 404).

put_wrong_param_name(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"localhost">>}],
    Parameters = [{age, 20}, {srids, 3}],
    Base = "/api/dragons",
    {ok, Response} = request(create_path_with_binds(Base, Binds), "PUT", Parameters, admin),
    check_status_code(Response, 404).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Client side tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_simple_client(_Config) ->
    Arg = {arg1, <<"bob@localhost">>},
    Base = "/api/clients",
    Username = <<"username@localhost">>,
    Auth = {binary_to_list(Username), "secret"},
    ExpectedBody = get_simple_client_command(Username, element(2, Arg)),
    {ok, Response} = request(create_path_with_binds(Base, [Arg]), "GET", {Auth, true}),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_two_args_client(_Config) ->
    Arg1 = {other, <<"bob@localhost">>},
    Arg2 = {limit, 10},
    Base = "/api/message",
    Username = <<"alice@localhost">>,
    Auth = {binary_to_list(Username), "secret"},
    ExpectedBody = get_two_args_client_command(Username, element(2, Arg1), element(2, Arg2)),
    {ok, Response} = request(create_path_with_binds(Base, [Arg1, Arg2]), "GET", {Auth, true}),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_bad_auth(_Config) ->
    Arg = {arg1, <<"bob@localhost">>},
    Base = "/api/clients",
    Username = <<"username@localhost">>,
    Auth = {binary_to_list(Username), "secret"},
    get_simple_client_command(Username, element(2, Arg)),
    {ok, Response} = request(create_path_with_binds(Base, [Arg]), "GET", {Auth, false}),
    check_status_code(Response, 401).

post_simple_client(_Config) ->
    Arg1 = {title, <<"Juliet's despair">>},
    Arg2 = {content, <<"If they do see thee, they will murder thee!">>},
    Base = <<"/api/ohmyromeo">>,
    Username = <<"username@localhost">>,
    Auth = {binary_to_list(Username), "secret"},
    Result = binary_to_list(post_simple_client_command(Username, element(2, Arg1), element(2, Arg2))),
    {ok, Response} = request(Base, "POST", [Arg1, Arg2], {Auth, true}),
    check_status_code(Response, 201),
    check_location_header(Response, list_to_binary(build_path_prefix() ++"/api/ohmyromeo/" ++ Result)).

put_simple_client(_Config) ->
    Arg = {password, <<"ilovepancakes">>},
    Base = <<"/api/superusers">>,
    Username = <<"joe@localhost">>,
    Auth = {binary_to_list(Username), "secretpassword"},
    put_simple_client_command(Username, element(2, Arg)),
    {ok, Response} = request(Base, "PUT", [Arg], {Auth, true}),
    check_status_code(Response, 204).

delete_simple_client(_Config) ->
    Arg = {name, <<"giant">>},
    Base = "/api/bikes",
    Username = <<"username@localhost">>,
    Auth = {binary_to_list(Username), "secret"},
    get_simple_client_command(Username, element(2, Arg)),
    {ok, Response} = request(create_path_with_binds(Base, [Arg]), "DELETE", {Auth, true}),
    check_status_code(Response, 204).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commands_client() ->
    [
     [
      {name, get_simple_client},
      {category, <<"clients">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, get_simple_client_command},
      {action, read},
      {identifiers, []},
      {security_policy, [user]},
      {args, [{caller, binary}, {arg1, binary}]},
      {result, {result, binary}}
     ],
     [
      {name, get_two_args_client},
      {category, <<"message">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, get_two_args_client_command},
      {action, read},
      {identifiers, []},
      {security_policy, [user]},
      {args, [{caller, binary}, {other, binary}, {limit, integer}]},
      {result, {result, binary}}
     ],
     [
      {name, post_simple_client},
      {category, <<"ohmyromeo">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, post_simple_client_command},
      {action, create},
      {identifiers, []},
      {security_policy, [user]},
      {args, [{caller, binary}, {title, binary}, {content, binary}]},
      {result, {result, binary}}
     ],
     [
      {name, put_simple_client},
      {category, <<"superusers">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, put_simple_client_command},
      {action, update},
      {identifiers, [caller]},
      {security_policy, [user]},
      {args, [{caller, binary}, {password, binary}]},
      {result, ok}
     ],
     [
      {name, delete_simple_client},
      {category, <<"bikes">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, delete_simple_client_command},
      {action, delete},
      {identifiers, []},
      {security_policy, [user]},
      {args, [{caller, binary}, {name, binary}]},
      {result, ok}
     ]
    ].

commands_admin() ->
    [
     [
      {name, get_simple},
      {category, <<"users">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, get_simple_command},
      {action, read},
      {identifiers, []},
      {args, [{arg1, binary}]},
      {result, {result, binary}}
     ],
     [
      {name, get_advanced},
      {category, <<"animals">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, get_two_args_command},
      {action, read},
      {identifiers, []},
      {args, [{arg1, integer}, {arg2, integer}]},
      {result, {result, binary}}
     ],
     [
      {name, get_advanced2},
      {category, <<"books">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, get_two_args2_command},
      {action, read},
      {identifiers, []},
      {args, [{one, integer}, {two, binary}]},
      {result, {result, integer}}
     ],
     [
      {name, post_simple},
      {category, <<"weather">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, post_simple_command},
      {action, create},
      {identifiers, []},
      {args, [{arg1, integer}, {arg2, integer}]},
      {result, {result, binary}}
     ],
     [
      {name, post_simple2},
      {category, <<"weather">>},
      {subcategory, <<"subcategory">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, post_simple_command},
      {action, create},
      {identifiers, [arg1]},
      {args, [{arg1, integer}, {arg2, integer}]},
      {result, {result, binary}}
     ],
     [
      {name, delete_simple},
      {category, <<"music">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, delete_simple_command},
      {action, delete},
      {identifiers, []},
      {args, [{arg1, binary}, {arg2, integer}]},
      {result, ok}
     ],
     [
      {name, put_simple},
      {category, <<"users">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, put_simple_command},
      {action, update},
      {args, [{arg1, binary}, {arg2, binary}, {arg3, binary}]},
      {identifiers, [arg1, arg2]},
      {result, ok}
     ],
     [
      {name, put_advanced},
      {category, <<"dragons">>},
      {desc, <<"do nothing and return">>},
      {module, ?MODULE},
      {function, put_advanced_command},
      {action, update},
      {args, [{username, binary},
              {domain, binary},
              {age, integer},
              {kids, integer}]},
      {identifiers, [username, domain]},
      {result, ok}
     ]
    ].

commands_new() ->
    commands_admin() ++ commands_client().


%% admin command funs
get_simple_command(<<"bob@localhost">>) ->
    <<"bob is OK">>.

get_two_args_command(1, 2) ->
    <<"all is working">>.

get_two_args2_command(X, B) when is_integer(X) and is_binary(B) ->
    100.

post_simple_command(_X, 2) ->
    <<"new_resource">>.

delete_simple_command(Binary, 2) when is_binary(Binary) ->
    10.

put_simple_command(_Arg1, _Arg2, _Arg3) ->
    ok.

put_advanced_command(Arg1, Arg2, Arg3, Arg4) when is_binary(Arg1) and is_binary(Arg2)
                                             and is_integer(Arg3) and is_integer(Arg4) ->
    ok.

%% clients command funs
get_simple_client_command(_Caller, _SomeBinary) ->
    <<"client bob is OK">>.

get_two_args_client_command(_Caller, _SomeBinary, _SomeInteger) ->
    <<"client2 bob is OK">>.

post_simple_client_command(_Caller, _Title, _Content) ->
    <<"new_resource">>.

put_simple_client_command(_Username, _Password) ->
    changed.

delete_simple_client_command(_Username, _BikeName) ->
    changed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_path_prefix() ->
    "http://" ++ ?HOST ++ ":" ++ integer_to_list(?PORT).

maybe_add_body([]) ->
    [];
maybe_add_body(Args) ->
    jiffy:encode(maps:from_list(Args)).

maybe_add_accepted_headers("POST") ->
    accepted_headers();
maybe_add_accepted_headers("PUT") ->
    accepted_headers();
maybe_add_accepted_headers(_) ->
    [].

accepted_headers() ->
    [{<<"Content-Type">>, <<"application/json">>}, {<<"Accept">>, <<"application/json">>}].

maybe_add_auth_header({User, Password}) ->
    Basic = list_to_binary("basic " ++ base64:encode_to_string(User ++ ":"++ Password)),
    [{<<"authorization">>, Basic}];
maybe_add_auth_header(admin) ->
    [].

-spec create_path_with_binds(string(), list()) -> binary().
create_path_with_binds(Base, ArgList) when is_list(ArgList) ->
    list_to_binary(
        lists:flatten(Base ++ ["/" ++ to_list(ArgValue)
                               || {ArgName, ArgValue} <- ArgList])).

to_list(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_list(Float) when is_float(Float) ->
    float_to_list(Float);
to_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Other) ->
    Other.

-spec request(binary(), method(), admin | {{binary(), binary()}, boolean()}) -> any.
request(Path, "GET", Entity) ->
    request(Path, "GET", [], Entity);
request(Path, "DELETE", Entity) ->
    request(Path, "DELETE", [], Entity).

-spec request(binary(), method(), list({atom(), any()}),
              {headers, list()} | admin | {{binary(), binary()}, boolean()}) -> any.
do_request(Path, Method, Body, {headers, Headers}) ->
    {ok, Pid} = fusco:start_link("http://"++ ?HOST ++ ":" ++ integer_to_list(?PORT), []),
    R = fusco:request(Pid, Path, Method, Headers, Body, 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

request(Path, Method, BodyData, {{_User, _Pass} = Auth, Authorized}) ->
    setup(client_module()),
    meck:expect(ejabberd_auth, check_password, fun(_, _, _) -> Authorized end),
    Body = maybe_add_body(BodyData),
    AuthHeader = maybe_add_auth_header(Auth),
    AcceptHeader = maybe_add_accepted_headers(Method),
    do_request(Path, Method, Body, {headers, AuthHeader ++ AcceptHeader});
request(Path, Method, BodyData, admin) ->
    ct:pal("~p, ~p, ~p", [Path, Method, BodyData]),
    setup(backend_module()),
    Body = maybe_add_body(BodyData),
    AcceptHeader = maybe_add_accepted_headers(Method),
    do_request(Path, Method, Body, {headers, AcceptHeader}).

mc_holder() ->
    erlang:register(mc_holder_proc, self()),
    mongoose_commands:init(),
    mongoose_commands:register(commands_new()),
    receive
        _ -> ok
    end,
    erlang:unregister(mc_holder_proc).

check_status_code(Response, Code) when is_integer(Code) ->
    {{ResCode, _}, _, _, _, _} = Response,
    ?assertEqual(Code, binary_to_integer(ResCode));
check_status_code(_R, Code) ->
    ?assertEqual(Code, not_a_number).

check_response_body(Response, ExpectedBody) ->
    {_, _, Body, _ , _} = Response,
    ?assertEqual(binary_to_list(Body), "\"" ++ binary_to_list(ExpectedBody) ++ "\"").

check_location_header(Response, Path) ->
    {_, Headers, _, _ , _} = Response,
    Location = proplists:get_value(<<"location">>, Headers),
    ?assertEqual(Path, Location).
