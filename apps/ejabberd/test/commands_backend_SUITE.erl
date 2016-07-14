%% @doc This suite tests both old ejabberd_commands module, which is slowly getting deprecated,
%% and the new mongoose_commands implementation.
-module(commands_backend_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/mongoose_commands.hrl").

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).
-define(PORT, 5288).
-define(HOST, "localhost").
-define(IP,  {127,0,0,1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
        {group, simple},
        {group, get_advanced},
        {group, post_advanced},
        {group, delete_advanced}
    ].

groups() ->
    [
        {simple, [sequence],
            [
                get_simple,
                post_simple,
                delete_simple
            ]
        },
        {get_advanced, [sequence],
            [
                get_two_args,
                get_wrong_path,
                get_wrong_arg_number,
                get_no_command,
                get_wrong_arg_type
            ]
        },
        {post_advanced, [sequence],
            [
                post_wrong_arg_number,
                post_wrong_arg_name,
                post_wrong_arg_type,
                post_wrong_arg_order,
                post_no_command
            ]
        },
        {delete_advanced, [sequence],
            [
                delete_wrong_arg_order,
                delete_wrong_arg_types
            ]
        }
    ].

setup() ->
    meck:unload(),
    spawn(fun mc_holder/0),
    meck:new(supervisor, [unstick, passthrough, no_link]),
    %% you have to meck some stuff to get it working....
    meck:expect(supervisor, start_child,
        fun(ejabberd_listeners, {_, {_, start_link, [_]}, transient,
            infinity, worker, [_]}) -> {ok, self()};
            (A,B) -> meck:passthrough([A,B])
        end),
    %% HTTP API config
    Opts = [{num_acceptors, 10},
        {max_connections, 1024},
        {modules, [{"localhost", "/api", mongoose_api_backend, []}]}],
    ejabberd_cowboy:start_listener({?PORT, ?IP, tcp}, Opts).



teardown() ->
    ejabberd_cowboy:stop(ejabberd_cowboy:handler({?PORT, ?IP, tcp})),
    mongoose_commands:unregister(commands_new()),
    meck:unload(),
    ok.

init_per_suite(C) ->
    application:ensure_all_started(stringprep),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(fusco),
    application:ensure_all_started(lager),
    ok = mnesia:start(),
    ok = acl:start(),
    acl:add(global, coder, {user, <<"zenek">>}),
    C.

end_per_suite(C) ->
    application:stop(lager),
    application:stop(fusco),
    application:stop(stringprep),
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
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_simple(_Config) ->
    Arg = {arg1, <<"bob@localhost">>},
    Base = "/api/users",
    ExpectedBody = get_simple_command(element(2, Arg)),
    {ok, Response} = get_request(create_path_with_args(Base, [Arg])),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

delete_simple(_Config) ->
    Arg1 = {arg1, <<"ala_ma_kota">>},
    Arg2 = {arg2, 2},
    Base = "/api/music",
    {ok, Response} = delete_request(create_path_with_args(Base, [Arg1, Arg2])),
    check_status_code(Response, 200).

post_simple(_Config) ->
    Args = [{arg1, 10}, {arg2,2}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 201).

get_two_args(_Config) ->
    Arg1 = {arg1, 1},
    Arg2 = {arg2, 2},
    Base = "/api/animals",
    ExpectedBody = get_two_args_command(element(2, Arg1), element(2, Arg2)),
    {ok, Response} = get_request(create_path_with_args(Base, [Arg1, Arg2])),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_wrong_path(_Config) ->
    Path = <<"/api/animals/1/2">>,
    {ok, Response} = get_request(Path),
    check_status_code(Response, 404).

get_wrong_arg_number(_Config) ->
    Path = <<"/api/animals/arg1/1/arg2/2/arg3/3">>,
    {ok, Response} = get_request(Path),
    check_status_code(Response, 404).

get_no_command(_Config) ->
    Path = <<"/api/unregistered_command/123123">>,
    {ok, Response} = get_request(Path),
    check_status_code(Response, 404).

get_wrong_arg_type(_Config) ->
    Path = <<"/api/animals/arg1/1/arg2/wrong">>,
    {ok, Response} = get_request(Path),
    check_status_code(Response, 400).

post_wrong_arg_number(_Config) ->
    Args = [{arg1, 10}, {arg2,2}, {arg3, 100}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 400).

post_wrong_arg_name(_Config) ->
    Args = [{arg11, 10}, {arg2,2}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 400).

post_wrong_arg_type(_Config) ->
    Args = [{arg1, 10}, {arg2,<<"weird binary">>}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 400).

post_wrong_arg_order(_Config) ->
    Args = [{arg2, 2}, {arg1, 10}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 201).

post_no_command(_Config) ->
    Args = [{arg1, 10}, {arg2,2}],
    Path = <<"/api/weather/10">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 404).


delete_wrong_arg_order(_Config) ->
    Arg1 = {arg1, <<"ala_ma_kota">>},
    Arg2 = {arg2, 2},
    Base = "/api/music",
    {ok, Response} = delete_request(create_path_with_args(Base, [Arg2, Arg1])),
    check_status_code(Response, 404).

delete_wrong_arg_types(_Config) ->
    Arg1 = {arg1, 2},
    Arg2 = {arg2, <<"ala_ma_kota">>},
    Base = "/api/music",
    {ok, Response} = delete_request(create_path_with_args(Base, [Arg1, Arg2])),
    check_status_code(Response, 400).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commands_new() ->
    [
        [
            {name, get_simple},
            {category, users},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, get_simple_command},
            {action, read},
            {identifiers, []},
            {args, [{arg1, binary}]},
            {result, {result, binary}}
        ],
        [
            {name, get_advanced},
            {category, animals},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, get_two_args_command},
            {action, read},
            {identifiers, []},
            {args, [{arg1, integer}, {arg2, integer}]},
            {result, {result, binary}}
        ],
        [
            {name, post_simple},
            {category, weather},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, post_simple_command},
            {action, create},
            {identifiers, []},
            {args, [{arg1, integer}, {arg2, integer}]},
            {result, {result, integer}}
        ],
        [
            {name, delete_simple},
            {category, music},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, delete_simple_command},
            {action, delete},
            {identifiers, []},
            {args, [{arg1, binary}, {arg2, integer}]},
            {result, ok}
        ]
    ].

get_simple_command(<<"bob@localhost">>) ->
    <<"bob is OK">>.

get_two_args_command(1, 2) ->
    <<"all is working">>.

post_simple_command(X, 2) ->
    X.

delete_simple_command(Binary, 2) when is_binary(Binary) ->
    10.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post_headers() ->
    [{<<"Content-Type">>, <<"application/json">>}, {<<"Accept">>, <<"application/json">>}].
create_path_with_args(Base, ArgList) when is_list(ArgList) ->
    list_to_binary(lists:flatten(Base ++
    ["/" ++ to_list(ArgName) ++ "/" ++ to_list(ArgValue) || {ArgName, ArgValue} <- ArgList])).

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


-spec get_request(binary()) -> any().
get_request(Path) ->
    setup(),
    {ok, Pid} = fusco:start_link("http://localhost:5288", []),
    R = fusco:request(Pid, Path, "GET", [], [], 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

-spec post_request(binary(), [{atom(), any()}]) -> any().
post_request(Path, Args) ->
    setup(),
    Body = jiffy:encode(maps:from_list(Args)),
    {ok, Pid} = fusco:start_link("http://localhost:5288", []),
    R = fusco:request(Pid, Path, "POST", post_headers(), Body, 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

-spec delete_request(binary()) -> any().
delete_request(Path) ->
    setup(),
    {ok, Pid} = fusco:start_link("http://localhost:5288", []),
    R = fusco:request(Pid, Path, "DELETE", [], [], 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.


mc_holder() ->
    mongoose_commands:init(),
    mongoose_commands:register(commands_new()),
    receive
        _ -> ok
    end.

check_status_code(Response, Code) when is_integer(Code) ->
    {{ResCode, _}, _, _, _, _} = Response,
    ?assertEqual(Code, binary_to_integer(ResCode));
check_status_code(_R, Code) ->
    ?assertEqual(Code, not_a_number).

check_response_body(Response, ExpectedBody) ->
    {_, _, Body, _ , _} = Response,
    ?assertEqual(binary_to_list(Body), "\"" ++ binary_to_list(ExpectedBody) ++ "\"").


