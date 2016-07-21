%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 10:16
%%%-------------------------------------------------------------------
-module(mongoose_api_utils).
-author("ludwikbukowski").
-include("mongoose_api.hrl").
-include("ejabberd.hrl").

%% API
-export([create_params_proplist/1,
    create_admin_url_path/1,
    error_response/3,
    error_response/4,
    action_to_method/1,
    method_to_action/1,
    create_user_url_path/1,
    get_allowed_methods/1,
    process_request/4,
    reload_dispatches/1]).


%% @doc Reload all ejabberd_cowboy listeners.
%% When a command is registered or unregistered, the routing paths that
%% cowboy stores as a "dispatch" must be refreshed.
%% Read more http://ninenines.eu/docs/en/cowboy/1.0/guide/routing/
reload_dispatches(_Command) ->
    Listeners = supervisor:which_children(ejabberd_listeners),
    CowboyListeners = [Child || {_Id, Child, _Type, [ejabberd_cowboy]}  <- Listeners],
    [ejabberd_cowboy:reload_dispatch(Child) || Child <- CowboyListeners].


create_params_proplist(ArgList) ->
    lists:sort([{to_atom(Arg), Value} || {Arg, Value} <- ArgList]).

-spec create_admin_url_path(mongoose_command()) -> ejabberd_cowboy:path().
create_admin_url_path(Command) ->
    "/" ++ category_to_resource(?COMMANDS_ENGINE:category(Command))
        ++ maybe_add_bindings(Command, admin).

-spec create_user_url_path(mongoose_command()) -> ejabberd_cowboy:path().
create_user_url_path(Command) ->
    "/" ++ category_to_resource(?COMMANDS_ENGINE:category(Command))
        ++ maybe_add_bindings(Command, user).


category_to_resource(Category) when is_atom(Category) ->
    atom_to_list(Category);
category_to_resource(Category) when is_list(Category) ->
    Category.


%% @doc Returns list of allowed methods.
-spec get_allowed_methods(admin | user) -> list(method()).
get_allowed_methods(Entity) ->
    Commands = ?COMMANDS_ENGINE:list(Entity),
    [action_to_method(?COMMANDS_ENGINE:action(Command)) || {_Name, Command} <- Commands].

-spec maybe_add_bindings(mongoose_command(), admin|user) -> string().
maybe_add_bindings(Command, Entity) ->
    Action = ?COMMANDS_ENGINE:action(Command),
    Args = ?COMMANDS_ENGINE:args(Command),
    case Action of
        read ->
            add_bindings(Args, Entity);
        update ->
            Ids = ?COMMANDS_ENGINE:identifiers(Command),
            Bindings = [El || {Key, _Value} = El <- Args, true =:= proplists:is_defined(Key, Ids)],
            add_bindings(Bindings, Entity);
        delete ->
            add_bindings(Args, Entity);
        _ ->
            ""
    end.


add_bindings(Args, Entity) ->
    lists:flatten([add_bind(A, Entity) || A <- Args]).

%% skip "caller" arg for frontend command
add_bind({caller, _}, user) ->
    "";
add_bind({ArgName, _}, _Entity) ->
    "/" ++ atom_to_list(ArgName) ++ "/:" ++ atom_to_list(ArgName);
add_bind(Other, _) ->
    throw({error, bad_arg_spec, Other}).

to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin));
to_atom(List) when is_list(List) ->
    list_to_atom(List);
to_atom(Atom) when is_atom(Atom) ->
    Atom.

%%--------------------------------------------------------------------
%% Method handlers
%%--------------------------------------------------------------------

-spec process_request(method(), mongoose_command(), any(), #backend_state{}) -> {any(), any(), #backend_state{}}.
process_request(<<"POST">>, Command, Req, #backend_state{entity = Entity} = State) ->
    case parse_request_body(Req) of
        {error, _R}->
            error_response(bad_request, ?BODY_MALFORMED , Req, State);
        {Params, Req2} ->
            Params2 = Params ++ maybe_add_caller(Entity),
            handle_request(Command, Params2, Req2, State)
    end;
process_request(<<"PUT">>, Command, Req, #backend_state{bindings = Binds, entity = Entity} = State) ->
    BindsReversed = lists:reverse(Binds),
    case parse_request_body(Req) of
        {error, _R}->
            error_response(bad_request, ?BODY_MALFORMED , Req, State);
        {Params, Req2} ->
            Args = BindsReversed ++ maybe_add_caller(Entity) ++ Params,
            handle_request(Command, Args, Req2, State)
    end;
process_request(_Method, Command, Req, #backend_state{bindings = Binds, entity = Entity}=State) ->
    BindsReversed = lists:reverse(Binds) ++ maybe_add_caller(Entity),
    handle_request(Command, BindsReversed, Req, State).


-spec handle_request(mongoose_command(), args_applied(), term(), #backend_state{}) ->
    {any(), any(), #backend_state{}}.
handle_request(Command, Args, Req, #backend_state{entity = Entity} = State) ->
    Method = action_to_method(?COMMANDS_ENGINE:action(Command)),
    ConvertedArgs = check_and_extract_args(?COMMANDS_ENGINE:args(Command), Args),
    Result = execute_command(ConvertedArgs, Command, Entity),
    handle_result(Method, Result, Req, State).

-spec handle_result(method(),
    {ok, any()} | failure() | {error, errortype()},
    any(), #backend_state{}) -> {any(), any(), #backend_state{}}.
handle_result(<<"GET">>, {ok, Result}, Req, State) ->
    {jiffy:encode(Result), Req, State};
handle_result(<<"POST">>, {ok, Res}, Req, State) ->
    {Path, Req2} = cowboy_req:url(Req),
    Req3 = maybe_add_location_header(Res, binary_to_list(Path), Req2),
    {halt, Req3, State};
handle_result(<<"DELETE">>, {ok, _Res}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {halt, Req2, State};
handle_result(_, ok, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {halt, Req2, State};
handle_result(_, {error, Error, Reason}, Req, State) when is_list(Reason) ->
    error_response(Error, Reason, Req, State);
handle_result(_, {error, Error, _R}, Req, State) ->
    error_response(Error, Req, State);
handle_result(_, {error, Error}, Req, State) ->
    error_response(Error, Req, State);
handle_result(no_call, _, Req, State) ->
    error_response(not_implemented, Req, State).


-spec parse_request_body(any()) -> {args_applied(), any()} | {error, atom(), any()}.
parse_request_body(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Data} = jiffy:decode(Body),
    Params = try
                 mongoose_api_utils:create_params_proplist(Data)
             catch
                 error:Err ->
                     {error, Err}
             end,
    {Params, Req2}.


%% @doc Checks if the arguments are correct. Return the arguments that can be applied to the execution of command.
-spec check_and_extract_args(arg_spec_list(), args_applied()) ->
    arg_values() | {error, atom(), any()}.
check_and_extract_args(CommandsArgList, RequestArgList) ->
    try
        Res1 = check_args_length({CommandsArgList, RequestArgList}),
        compare_names_extract_args(Res1)
    catch
        throw:Err ->
            Err
    end.

check_args_length({CommandsArgList, RequestArgList} = Acc) ->
    if
        length(CommandsArgList) =/= length(RequestArgList) ->
            throw({error, bad_request, ?ARGS_LEN_ERROR});
        true ->
            Acc
    end.

-spec compare_names_extract_args({arg_spec_list(), args_applied()}) -> arg_values().
compare_names_extract_args({CommandsArgList, RequestArgProplist}) ->
    Keys = lists:sort([K || {K, _V} <- RequestArgProplist]),
    ExpectedKeys = lists:sort([Key || {Key, _Type} <- CommandsArgList]),
    ZippedKeys = lists:zip(Keys, ExpectedKeys),
    case lists:member(false, [ReqKey =:= ExpKey || {ReqKey, ExpKey} <- ZippedKeys]) of
        true ->
            throw({error, bad_request, ?ARGS_SPEC_ERROR});
        _ ->
            do_extract_args(CommandsArgList, RequestArgProplist)
    end.

-spec do_extract_args(arg_spec_list(), args_applied()) -> arg_values().
do_extract_args(CommandsArgList, RequestArgList) ->
    [element(2, lists:keyfind(Key, 1, RequestArgList)) || {Key, _Type} <- CommandsArgList].

-spec execute_command(list({atom(), any()}) | map() | {error, atom(), any()}, mongoose_command(), admin|binary()) ->
    {ok, term()} | failure().
execute_command({error, _Type, _Reason} = Err, _, _) ->
    Err;
execute_command(Args, Command, Entity) ->
    try
        do_execute_command(Args, Command, Entity)
    catch
        _:R ->
            {error, bad_request, R}
    end.
-spec do_execute_command(arg_values(), mongoose_command(), admin|binary()) -> ok | {ok, any()}.
do_execute_command(Args, Command, Entity) ->
    Types = [Type || {_Name, Type} <- ?COMMANDS_ENGINE:args(Command)],
    ConvertedArgs = [convert_arg(Type, Arg) || {Type, Arg} <- lists:zip(Types, Args)],
    ?COMMANDS_ENGINE:execute(Entity, ?COMMANDS_ENGINE:name(Command), ConvertedArgs).

maybe_add_caller(admin) ->
    [];
maybe_add_caller(JID) ->
    [{caller, JID}].

maybe_add_location_header(Result, ResourcePath, Req) when is_binary(Result) ->
    add_location_header(binary_to_list(Result), ResourcePath, Req);
maybe_add_location_header(Result, ResourcePath, Req) when is_list(Result) ->
    add_location_header(Result, ResourcePath, Req);
maybe_add_location_header(_R, _R, Req) ->
    {ok, Req2} = cowboy_req:reply(200, [], Req),
    Req2.

add_location_header(Result, ResourcePath, Req) ->
    Path = ResourcePath ++ "/" ++ Result,
    Header = {<<"location">>, Path},
    {ok, Req2} = cowboy_req:reply(201, [Header], Req),
    Req2.



-spec convert_arg(atom(), any()) -> integer() | float() | binary() | string() | {error, bad_type}.
convert_arg(binary, Binary) when is_binary(Binary) ->
    Binary;
convert_arg(string, Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
convert_arg(string, String) when is_list(String) ->
    String;
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

%%--------------------------------------------------------------------
%% HTTP utils
%%--------------------------------------------------------------------

error_response(Code, Req, State) when is_integer(Code) ->
    {ok, Req1} = cowboy_req:reply(Code, Req),
    {halt, Req1, State};
error_response(ErrorType, Req, State) ->
    error_response(error_code(ErrorType), Req, State).

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