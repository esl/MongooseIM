%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 10:16
%%%-------------------------------------------------------------------
-module(mongoose_api_common).
-author("ludwikbukowski").
-include("mongoose_api.hrl").
-include("ejabberd.hrl").
%%
%% @doc MongooseIM REST API backend
%%
%% This module handles the client HTTP REST requests, then respectively convert
%% them to Commands from mongoose_commands and execute with `admin` privileges.
%% It supports responses with appropriate HTTP Status codes returned to the
%% client.
%% This module implements behaviour introduced in ejabberd_cowboy which is
%% %% built on top of the cowboy library.
%% The method supported: GET, POST, PUT, DELETE. Only JSON format.
%% The library "jiffy" used to serialize and deserialized JSON data.
%%
%% REQUESTS
%%
%% The module is based on mongoose_commands registry.
%% The root http path for a command is build based on the "category" field.
%% %% It's always used as path a prefix.
%% The commands are translated to HTTP API in the following manner:
%%
%% command of action "read" will be called by GET request
%% command of action "create" will be called by POST request
%% command of action "update" will be called by PUT request
%% command of action "delete" will be called by DELETE request
%%
%% The args of the command will be filled with the values provided in path
%% %% bindings or body parameters, depending of the method type:
%% - for command of action "read" or "delete" all the args are pulled from the
%% path bindings. The path should be constructed of pairs "/arg_name/arg_value"
%% so that it could match the {arg_name, type} %% pattern in the command
%% registry. E.g having the record of category "users" and args:
%% [{username, binary}, {domain, binary}] we will have to make following GET
%% request %% path: http://domain:port/api/users/username/Joe/domain/localhost
%% and the command will be called with arguments "Joe" and "localhost"
%%
%% - for command of action "create" or "update" args are pulled from the body
%% JSON, except those that are on the "identifiers" list of the command. Those
%% go to the path bindings.
%% E.g having the record of category "animals", action "update" and args:
%% [{species, binary}, {name, binary}, {age, integer}]
%% and identifiers:
%% [species, name]
%% we can set the age for our elephant Ed in the PUT request:
%% path: http://domain:port/api/species/elephant/name/Ed
%% body: {"age" : "10"}
%% and then the command will be called with arguments ["elephant", "Ed" and 10].
%%
%% RESPONSES
%%
%% The API supports some of the http status code like 200, 201, 400, 404 etc
%% depending on the return value of the command execution and arguments checks.
%% Additionally, when the command's action is "create" and it returns a value,
%% it is concatenated to the path and return to the client in header "location"
%% with response code 201 so that it could represent now a new created resource.
%% If error occured while executing the command, the appropriate reason is
%% returned in response body.

%% API
-export([create_admin_url_path/1,
         create_user_url_path/1,
         error_response/3,
         error_response/4,
         action_to_method/1,
         method_to_action/1,
         parse_request_body/1,
         get_allowed_methods/1,
         process_request/4,
         reload_dispatches/1]).


%% @doc Reload all ejabberd_cowboy listeners.
%% When a command is registered or unregistered, the routing paths that
%% cowboy stores as a "dispatch" must be refreshed.
%% Read more http://ninenines.eu/docs/en/cowboy/1.0/guide/routing/
reload_dispatches(drop) ->
    drop;
reload_dispatches(_Command) ->
    Listeners = supervisor:which_children(ejabberd_listeners),
    CowboyListeners = [Child || {_Id, Child, _Type, [ejabberd_cowboy]}  <- Listeners],
    [ejabberd_cowboy:reload_dispatch(Child) || Child <- CowboyListeners],
    drop.


-spec create_admin_url_path(mongoose_commands:t()) -> ejabberd_cowboy:path().
create_admin_url_path(Command) ->
    ["/", mongoose_commands:category(Command),
          maybe_add_bindings(Command, admin), maybe_add_subcategory(Command)].

-spec create_user_url_path(mongoose_commands:t()) -> ejabberd_cowboy:path().
create_user_url_path(Command) ->
    ["/", mongoose_commands:category(Command), maybe_add_bindings(Command, user)].

-spec process_request(method(), mongoose_commands:t(), any(), http_api_state()) ->
                      {any(), any(), http_api_state()}.
process_request(Method, Command, Req, #http_api_state{bindings = Binds, entity = Entity} = State)
    when ((Method == <<"POST">>) or (Method == <<"PUT">>)) ->
    {Params, Req2} = Req,
    {QVals, _} = cowboy_req:qs_vals(Req2),
    QV = [{binary_to_existing_atom(K, utf8), V} || {K, V} <- QVals],
    Params2 = Binds ++ Params ++ QV ++ maybe_add_caller(Entity),
    handle_request(Method, Command, Params2, Req2, State);
process_request(Method, Command, Req, #http_api_state{bindings = Binds, entity = Entity}=State)
    when ((Method == <<"GET">>) or (Method == <<"DELETE">>)) ->
    {QVals, _} = cowboy_req:qs_vals(Req),
    QV = [{binary_to_existing_atom(K, utf8), V} || {K, V} <- QVals],
    BindsAndVars = Binds ++ QV ++ maybe_add_caller(Entity),
    handle_request(Method, Command, BindsAndVars, Req, State).

-spec handle_request(method(), mongoose_commands:t(), args_applied(), term(), http_api_state()) ->
                     {any(), any(), http_api_state()}.
handle_request(Method, Command, Args, Req, #http_api_state{entity = Entity} = State) ->
    ConvertedArgs = check_and_extract_args(mongoose_commands:args(Command),
                                           mongoose_commands:optargs(Command), Args),
    Result = execute_command(ConvertedArgs, Command, Entity),
    handle_result(Method, Result, Req, State).

-type correct_result() :: ok | {ok, any()}.
-type error_result() ::  mongoose_commands:failure() |
                         {error, bad_request, any()}.
-type expected_result() :: correct_result() | error_result().

-spec handle_result(Method, Result, Req, State) -> Return when
      Method :: method(),
      Result :: expected_result(),
      Req :: cowboy_req:req(),
      State :: http_api_state(),
      Return :: {any(), cowboy_req:req(), http_api_state()}.
handle_result(Verb, ok, Req, State) ->
    handle_result(Verb, {ok, nocontent}, Req, State);
%%    {ok, Req2} = cowboy_req:reply(200, Req),
%%    {halt, Req2, State};
handle_result(<<"GET">>, {ok, Result}, Req, State) ->
    {jiffy:encode(Result), Req, State};
handle_result(<<"POST">>, {ok, nocontent}, Req, State) ->
    {Path, Req2} = cowboy_req:url(Req),
    Req3 = maybe_add_location_header(nocontent, binary_to_list(Path), Req2),
    {halt, Req3, State};
handle_result(<<"POST">>, {ok, Res}, Req, State) ->
    {Path, Req2} = cowboy_req:url(Req),
    Req3 = cowboy_req:set_resp_body(Res, Req2),
    Req4 = maybe_add_location_header(Res, binary_to_list(Path), Req3),
    {halt, Req4, State};
%% Ignore the returned value from a command for DELETE methods
handle_result(<<"DELETE">>, {ok, _Res}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(204, Req),
    {halt, Req2, State};
handle_result(<<"PUT">>, {ok, nocontent}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(204, Req),
    {halt, Req2, State};
handle_result(<<"PUT">>, {ok, Res}, Req, State) ->
    Req2 = cowboy_req:set_resp_body(Res, Req),
    {ok, Req3} = cowboy_req:reply(201, Req2),
    {halt, Req3, State};
handle_result(_, {error, Error, Reason}, Req, State) when is_binary(Reason) ->
    error_response(Error, Reason, Req, State);
handle_result(_, {error, Error, _R}, Req, State) ->
    error_response(Error, Req, State);
% handle_result(_, {error, Error}, Req, State) ->
%     error_response(Error, Req, State);
handle_result(no_call, _, Req, State) ->
    error_response(not_implemented, Req, State).


-spec parse_request_body(any()) -> {args_applied(), cowboy_req:req()} | {error, any()}.
parse_request_body(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Data} = jiffy:decode(Body),
    try
        Params = create_params_proplist(Data),
        {Params, Req2}
    catch
        error:Err ->
            {error, Err}
    end.

%% @doc Checks if the arguments are correct. Return the arguments that can be applied to the
%% execution of command.
-spec check_and_extract_args(arg_spec_list(), optarg_spec_list(), args_applied()) ->
                             map() | {error, atom(), any()}.
check_and_extract_args(ReqArgs, OptArgs, RequestArgList) ->
    try
        AllArgs = ReqArgs ++ [{N, T} || {N, T, _} <- OptArgs],
        AllArgVals = [{N, T, proplists:get_value(N, RequestArgList)} || {N, T} <- AllArgs],
        ConvArgs = [{N, convert_arg(T, V)} || {N, T, V} <- AllArgVals, V =/= undefined],
        maps:from_list(ConvArgs)
    catch
        _:R ->
            {error, bad_request, R}
    end.


-spec execute_command(list({atom(), any()}) | map() | {error, atom(), any()},
                      mongoose_commands:t(), admin | binary()) ->
                      correct_result() | error_result().
execute_command({error, _Type, _Reason} = Err, _, _) ->
    Err;
execute_command(ArgMap, Command, Entity) ->
    try
        do_execute_command(ArgMap, Command, Entity)
    catch
        _:R ->
            {error, bad_request, R}
    end.

-spec do_execute_command(map(), mongoose_commands:t(), admin|binary()) -> ok | {ok, any()}.
do_execute_command(ArgMap, Command, Entity) ->
    mongoose_commands:execute(Entity, mongoose_commands:name(Command), ArgMap).

-spec maybe_add_caller(admin | binary()) -> list() | list({caller, binary()}).
maybe_add_caller(admin) ->
    [];
maybe_add_caller(JID) ->
    [{caller, JID}].

-spec maybe_add_location_header(binary() | list() | nocontent, list(), any())
    -> any().
maybe_add_location_header(Result, ResourcePath, Req) when is_binary(Result) ->
    add_location_header(binary_to_list(Result), ResourcePath, Req);
maybe_add_location_header(Result, ResourcePath, Req) when is_list(Result) ->
    add_location_header(Result, ResourcePath, Req);
maybe_add_location_header(_, _Path, Req) ->
    {ok, Req2} = cowboy_req:reply(204, [], Req),
    Req2.

add_location_header(Result, ResourcePath, Req) ->
    Path = [ResourcePath, "/", Result],
    Header = {<<"location">>, Path},
    {ok, Req2} = cowboy_req:reply(201, [Header], Req),
    Req2.

-spec convert_arg(atom(), any()) -> integer() | float() | binary() | string() | {error, bad_type}.
convert_arg(binary, Binary) when is_binary(Binary) ->
    Binary;
convert_arg(integer, Binary) when is_binary(Binary) ->
    binary_to_integer(Binary);
convert_arg(integer, Integer) when is_integer(Integer) ->
    Integer;
convert_arg(float, Binary) when is_binary(Binary) ->
    binary_to_float(Binary);
convert_arg(float, Float) when is_float(Float) ->
    Float;
convert_arg(_, _Binary) ->
    throw({error, bad_type}).

-spec create_params_proplist(list({binary(), binary()})) -> args_applied().
create_params_proplist(ArgList) ->
    lists:sort([{to_atom(Arg), Value} || {Arg, Value} <- ArgList]).

%% @doc Returns list of allowed methods.
-spec get_allowed_methods(admin | user) -> list(method()).
get_allowed_methods(Entity) ->
    Commands = mongoose_commands:list(Entity),
    [action_to_method(mongoose_commands:action(Command)) || Command <- Commands].

-spec maybe_add_bindings(mongoose_commands:t(), admin|user) -> iolist().
maybe_add_bindings(Command, Entity) ->
    Action = mongoose_commands:action(Command),
    Args = mongoose_commands:args(Command),
    BindAndBody = both_bind_and_body(Action),
    case BindAndBody of
        true ->
            Ids = mongoose_commands:identifiers(Command),
            Bindings = [El || {Key, _Value} = El <- Args, true =:= proplists:is_defined(Key, Ids)],
            add_bindings(Bindings, Entity);
        false ->
            add_bindings(Args, Entity)
    end.

maybe_add_subcategory(Command) ->
    SubCategory = mongoose_commands:subcategory(Command),
    case SubCategory of
        undefined ->
            [];
        _ ->
            ["/", SubCategory]
    end.

-spec both_bind_and_body(mongoose_commands:action()) -> boolean().
both_bind_and_body(update) ->
    true;
both_bind_and_body(create) ->
    true;
both_bind_and_body(read) ->
    false;
both_bind_and_body(delete) ->
    false.

add_bindings(Args, Entity) ->
    [add_bind(A, Entity) || A <- Args].

%% skip "caller" arg for frontend command
add_bind({caller, _}, user) ->
    "";
add_bind({ArgName, _}, _Entity) ->
    lists:flatten(["/:", atom_to_list(ArgName)]);
add_bind(Other, _) ->
    throw({error, bad_arg_spec, Other}).

-spec to_atom(binary() | atom()) -> atom().
to_atom(Bin) when is_binary(Bin) ->
    erlang:binary_to_existing_atom(Bin, utf8);
to_atom(Atom) when is_atom(Atom) ->
    Atom.

%%--------------------------------------------------------------------
%% HTTP utils
%%--------------------------------------------------------------------
-spec error_response(integer() | atom(), any(), http_api_state()) ->
                     {halt, any(), http_api_state()}.
error_response(Code, Req, State) when is_integer(Code) ->
    {ok, Req1} = cowboy_req:reply(Code, Req),
    {halt, Req1, State};
error_response(ErrorType, Req, State) ->
    error_response(error_code(ErrorType), Req, State).

-spec error_response(any(), any(), any(), http_api_state()) -> {halt, any(), http_api_state()}.
error_response(Code, Reason, Req, State) when is_integer(Code) ->
    {ok, Req1} = cowboy_req:reply(Code, [], Reason, Req),
    {halt, Req1, State};
error_response(ErrorType, Reason, Req, State) ->
    error_response(error_code(ErrorType), Reason, Req, State).


%% HTTP status codes
error_code(denied) -> 403;
error_code(not_implemented) -> 501;
error_code(bad_request) -> 400;
error_code(type_error) -> 400;
error_code(not_found) -> 404;
error_code(internal) -> 500.

action_to_method(read) -> <<"GET">>;
action_to_method(update) -> <<"PUT">>;
action_to_method(delete) -> <<"DELETE">>;
action_to_method(create) -> <<"POST">>;
action_to_method(_) -> undefined.

method_to_action(<<"GET">>) -> read;
method_to_action(<<"POST">>) -> create;
method_to_action(<<"PUT">>) -> update;
method_to_action(<<"DELETE">>) -> delete.

%%--------------------------------------------------------------------
%% Authorization
%%--------------------------------------------------------------------

get_creds(Req) ->
    case get_auth_details(Req) of
	{ok, undefined, _} ->
	    undefined,
	{ok, {_AuthMethod, Creds}, _Req2} ->
	   Creds
    end.

get_auth_method(Req) ->
    case get_auth_details(Req) of
	{ok, undefined, _} ->
	    undefined,
	{ok, {AuthMethod, _Creds}, _Req2} ->
	   AuthMethod
    end.

get_http_method(Req) ->
    {M, _} = cowboy_req:method(Req),
    M.

get_auth_details(Req) ->
    cowboy_req:parse_header(<<"authorization">>, Req).

is_known_auth_method(<<"basic">>) -> true;
is_known_auth_method(_) -> false.

make_unauthorized_response(Req, State) ->
        {{false, <<"Basic realm=\"mongooseim\"">>}, Req, State}.
