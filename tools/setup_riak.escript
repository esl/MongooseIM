#!/usr/bin/env escript
-mode(compile).

main(_) ->
    %% Set current directory to script directory
    file:set_cwd(filename:dirname(escript:script_name())),
    Cookie = riak,
    io:format("~nsetup_riak START~n", []),
    {ok, _} = net_kernel:start([node_name(setup_riak)]),
    erlang:set_cookie(node(), Cookie),
    RiakNode = node_name(riak),
    case net_adm:ping(RiakNode) of
        pang ->
            io:format("Failed to ping ~p~n", [RiakNode]),
            init:stop(1);
        pong ->
            io:format("Riak node ~p~n", [RiakNode]),
            try setup_riak_node(RiakNode)
            catch Class:Reason ->
                %% This script runs inside michalwski/docker-riak, which still uses OTP 20.3
                Stacktrace = erlang:get_stacktrace(),
                io:format("Failed ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
                init:stop(1)
            end,
        io:format("setup_riak DONE~n", [])
    end.

node_name(ShortName) ->
    list_to_atom(atom_to_list(ShortName) ++ "@" ++ format_ip(local_ip_v4())).

format_ip({A,B,C,D}) ->
    integer_to_list(A) ++ "." ++
    integer_to_list(B) ++ "." ++
    integer_to_list(C) ++ "." ++
    integer_to_list(D).

local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).

setup_riak_node(RiakNode) ->
    application:ensure_all_started(inets),
    setup_schemas(),
    setup_indexes(),
    setup_access(RiakNode),
    setup_types(RiakNode).

setup_access(RiakNode) ->
    User = "ejabberd",
    Password = "mongooseim_secret",
    ok = rpc:call(RiakNode, riak_core_console, security_enable, [[]]),
    ok = rpc:call(RiakNode, riak_core_console, add_user, [[User, "password=" ++ Password]]),
    ok = rpc:call(RiakNode, riak_core_console, add_source, [["all", "127.0.0.1/32", "password"]]),
    ok = rpc:call(RiakNode, riak_core_console, add_source, [[User, "0.0.0.0/0", "password"]]),
    ok = rpc:call(RiakNode, riak_core_console, grant, [[permissions(), "on", "any", "to", User]]),
    ok = rpc:call(RiakNode, riak_core_console, ciphers, [[ciphers()]]),
    ok.

ciphers() ->
    "AES256-SHA:DHE-RSA-AES128-SHA256".

permissions() ->
    "riak_kv.get,riak_kv.put,riak_kv.delete,riak_kv.index,"
    "riak_kv.list_keys,riak_kv.list_buckets,riak_kv.mapreduce,"
    "riak_core.get_bucket,riak_core.set_bucket,riak_core.set_bucket_type,"
    "riak_core.get_bucket_type,search.admin,search.query".

setup_types(RiakNode) ->
    Def = [{last_write_wins, true}, {dvv_enabled, false}],
    Map = [{datatype, map}],
    Set = [{datatype, set}],
    create_and_activate_bucket_type(RiakNode, <<"users">>, Map),
    create_and_activate_bucket_type(RiakNode, <<"rosters">>, Map),
    create_and_activate_bucket_type(RiakNode, <<"roster_versions">>, Def),
    create_and_activate_bucket_type(RiakNode, <<"private">>, Def),
    create_and_activate_bucket_type(RiakNode, <<"vcard">>, [{search_index, <<"vcard">>}|Def]),
    create_and_activate_bucket_type(RiakNode, <<"mam_yz">>, [{search_index, <<"mam">>}|Map]),
    create_and_activate_bucket_type(RiakNode, <<"last">>, Def),
    create_and_activate_bucket_type(RiakNode, <<"offline">>, Def),
    create_and_activate_bucket_type(RiakNode, <<"privacy_defaults">>, Def),
    create_and_activate_bucket_type(RiakNode, <<"privacy_lists_names">>, Set),
    create_and_activate_bucket_type(RiakNode, <<"privacy_lists">>, Def),
    ok.


create_and_activate_bucket_type(Node, Type, Props) ->
    ok = rpc:call(Node, riak_core_bucket_type, create, [Type, Props]),
    wait_until_bucket_type_status(Type, ready, Node),
    ok = rpc:call(Node, riak_core_bucket_type, activate, [Type]),
    wait_until_bucket_type_status(Type, active, Node).

wait_until_bucket_type_status(Type, ExpectedStatus, Nodes) when is_list(Nodes) ->
    [wait_until_bucket_type_status(Type, ExpectedStatus, Node) || Node <- Nodes];
wait_until_bucket_type_status(Type, ExpectedStatus, Node) ->
    F = fun() ->
                ActualStatus = rpc:call(Node, riak_core_bucket_type, status, [Type]),
                ExpectedStatus =:= ActualStatus
        end,
    ok = wait_until(F, 30, 100).


%% @doc Retry `Fun' until it returns true.
%% Repeat maximum `Retry' times.
%% Wait `Delay' milliseconds between retries.
wait_until(Fun, Retry, Delay) when Retry > 0 ->
    Res = Fun(),
    case Res of
        true ->
            ok;
        _ when Retry == 1 ->
            {fail, Res};
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry-1, Delay)
    end.


setup_schemas() ->
    do_put_file("/search/schema/vcard", "vcard_search_schema.xml"),
    do_put_file("/search/schema/mam", "mam_search_schema.xml"),
    wait_for_code("/search/schema/vcard", 200),
    wait_for_code("/search/schema/mam", 200),
    ok.

setup_indexes() ->
    do_put_json("/search/index/vcard", "{\"schema\":\"vcard\"}"),
    do_put_json("/search/index/mam", "{\"schema\":\"mam\"}"),
    wait_for_code("/search/index/vcard", 200),
    wait_for_code("/search/index/mam", 200),
    ok.

wait_for_code(Path, Code) ->
    F = fun() -> check_result_code(do_get(Path), Code) end,
    ok = wait_until(F, 10, 2000).

do_get(Path) ->
    Url = riak_url(Path),
    Headers = [],
    Request = {Url, Headers},
    %% request(Method, Request, HTTPOptions, Options)
    Result = httpc:request(get, Request, [], []),
    io:format("GET to ~p returns:~n~p~n", [Path, Result]),
    Result.

do_put_file(Path, File) ->
    Url = riak_url(Path),
    Headers = [],
    ContentType = "application/xml",
    Body = get_body_from_file(File),
    Request = {Url, Headers, ContentType, Body},
    %% request(Method, Request, HTTPOptions, Options)
    Result = httpc:request(put, Request, [], []),
    io:format("~nPUT to ~p returns:~n~p~n", [Path, Result]),
    Result.

do_put_json(Path, JsonBody) ->
    Url = riak_url(Path),
    Headers = [],
    ContentType = "application/json",
    Request = {Url, Headers, ContentType, JsonBody},
    %% request(Method, Request, HTTPOptions, Options)
    Result = httpc:request(put, Request, [], []),
    io:format("~nPUT to ~p returns:~n~p~n", [Path, Result]),
    Result.

check_result_code({ok, {{_HttpVer, Code, _Reason}, _Headers, _Body}}, Code) ->
    true;
check_result_code(Result, Code) ->
    false.

riak_url(Path) ->
    riak_host() ++ Path.

riak_host() ->
    "http://localhost:" ++ integer_to_list(riak_port()).

riak_port() ->
    case os:getenv("RIAK_PORT") of
        false ->
            8098;
        Value ->
            list_to_integer(Value)
    end.

get_body_from_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            binary_to_list(Bin);
        {error, Reason} ->
            error({get_body_from_file_failed, Reason, File})
    end.
