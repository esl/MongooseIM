%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2016 12:59
%%%-------------------------------------------------------------------
%%
%% @doc MongooseIM REST API backend
%%
%%
%%
-module(mongoose_api_backend).
-author("ludwikbukowski").
-record(backend_state, {allowed_methods, bindings, parameters, command_category}).

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% cowboy_rest exports
-export([allowed_methods/2,
         content_types_provided/2,
         rest_terminate/2,
         init/3,
         rest_init/2,
         content_types_accepted/2,
         delete_resource/2]).

%% local callbacks
-export([to_json/2,
         from_json/2]).


-define(COMMANDS_ENGINE, mongoose_commands).
-include("mongoose_commands.hrl").
-include("ejabberd.hrl").
-type method() ::binary().
-type arg_name() :: atom().
-type arg_value() :: any().
-type arg_spec_list() :: list(argspec()).
-type args_applied() :: list({arg_name(), arg_value()}).
-type arg_values() :: list(arg_value()).
-type mongoose_command() :: #mongoose_command{}.

%% Error messages
-define(ARGS_LEN_ERROR, "Bad parameters length.").
-define(ARGS_SPEC_ERROR, "Bad name of the parameter.").
-define(BODY_MALFORMED, "The request body is malformed.").

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------
%% @doc This is implementation of ejabberd_cowboy callback. Returns list of all available http paths.
-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
    ejabberd_cowboy:implemented_result() | ejabberd_cowboy:default_result().
cowboy_router_paths(Base, _Opts) ->
        try
            Commands = ?COMMANDS_ENGINE:list(admin),
            [handler_path(Base, Command) || Command <- Commands]
        catch
            _:Err ->
                ?ERROR_MSG("Error occured when getting the commands list: ~p~n", [Err]),
                []
        end.


%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------

init({_Transport, _}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
    {Bindings, Req1} = cowboy_req:bindings(Req),
    CommandCategory =
        case lists:keytake(command_category, 1, Opts) of
            {value, {command_category, Name},  _Opts1} ->
                Name;
            false ->
                undefined
        end,
    State = #backend_state{allowed_methods = get_allowed_methods(),
        bindings = Bindings, command_category = CommandCategory},
    {ok, Req1, State}.

allowed_methods(Req, #backend_state{command_category = Name} = State) ->
    CommandList = ?COMMANDS_ENGINE:list(admin, Name),
    AllowedMethods = [action_to_method(?COMMANDS_ENGINE:action(Command)) || Command <- CommandList],
    {AllowedMethods, Req, State}.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {CTP, Req, State}.

content_types_accepted(Req, State) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, from_json}],
    {CTA, Req, State}.

rest_terminate(_Req, _State) ->
    ok.

%% @doc Called for a method of type "DELETE"
delete_resource(Req, State) ->
    extract_and_handle(<<"DELETE">>, Req, State).

%%--------------------------------------------------------------------
%% internal callbacks
%%--------------------------------------------------------------------
%% @doc Called for a method of type "GET"
to_json(Req, State) ->
    extract_and_handle(<<"GET">>, Req, State).

%%--------------------------------------------------------------------
%% @doc Called for a method of type "POST" and "PUT"
from_json(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    extract_and_handle(Method, Req2, State).

%%--------------------------------------------------------------------
%% Method handlers
%%--------------------------------------------------------------------

-spec extract_and_handle(method(), any(), #backend_state{}) -> {any(), any(), #backend_state{}}.
extract_and_handle(<<"POST">> = Method, Req, #backend_state{command_category = Category} = State) ->
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(Method)),
    case parse_request_body(Req) of
    {error, _R}->
            error_response(bad_request, ?BODY_MALFORMED , Req, State);
    {Params, Req2} ->
        handle_request(Command, Params, Req2, State)
    end;
extract_and_handle(<<"PUT">> = Method, Req, #backend_state{bindings = Binds, command_category = Category} = State) ->
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(Method)),
    BindsReversed = lists:reverse(Binds),
    case parse_request_body(Req) of
        {error, _R}->
            error_response(bad_request, ?BODY_MALFORMED , Req, State);
        {Params, Req2} ->
            Args = BindsReversed ++ Params,
            handle_request(Command, Args, Req2, State)
    end;
extract_and_handle(Method, Req, #backend_state{bindings = Binds, command_category = Category}=State) ->
    BindsReversed = lists:reverse(Binds),
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(Method)),
    handle_request(Command, BindsReversed, Req, State).

-spec handle_request(mongoose_command(), args_applied(), term(), #backend_state{}) ->
    {any(), any(), #backend_state{}}.
handle_request(Command, Args, Req, State) ->
    Method = action_to_method(?COMMANDS_ENGINE:action(Command)),
    ConvertedArgs = check_and_extract_args(?COMMANDS_ENGINE:args(Command), Args),
    Result = execute_command(ConvertedArgs, Command),
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

%%--------------------------------------------------------------------
%% internals
%%--------------------------------------------------------------------

-spec parse_request_body(any()) -> {args_applied(), any()} | {error, atom(), any()}.
parse_request_body(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Data} = jiffy:decode(Body),
    Params = try
        create_params_proplist(Data)
    catch
        error:Err ->
            {error,Err}
    end,
    {Params, Req2}.

-spec handler_path(ejabberd_cowboy:path(), mongoose_command()) -> ejabberd_cowboy:path().
handler_path(Base, Command) ->
    {[Base, create_url_path(Command)],
        ?MODULE, [{command_category, ?COMMANDS_ENGINE:category(Command)}]}.

%% @doc Returns list of allowed methods.
-spec get_allowed_methods() -> list(method()).
get_allowed_methods() ->
    Commands = ?COMMANDS_ENGINE:list(admin),
    [action_to_method(?COMMANDS_ENGINE:action(Command)) || {_Name, Command} <- Commands].

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
    Keys = [K || {K, _V} <- RequestArgProplist],
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

-spec execute_command(list({atom(), any()}) | map() | {error, atom(), any()}, mongoose_command()) ->
    {ok, term()} | failure().
execute_command({error, _Type, _Reason} = Err, _) ->
    Err;
execute_command(Args, Command) ->
    try
        do_execute_command(Args, Command)
    catch
        _:R ->
            {error, bad_request, R}
    end.
-spec do_execute_command(arg_values(), mongoose_command()) -> ok | {ok, any()}.
do_execute_command(Args, Command) ->
    Types = [Type || {_Name, Type} <- ?COMMANDS_ENGINE:args(Command)],
    ConvertedArgs = [convert_arg(Type, Arg) || {Type, Arg} <- lists:zip(Types, Args)],
    ?COMMANDS_ENGINE:execute(admin, ?COMMANDS_ENGINE:name(Command), ConvertedArgs).

create_params_proplist(ArgList) ->
    lists:sort([{to_atom(Arg), Value} || {Arg, Value} <- ArgList]).

-spec create_url_path(mongoose_command()) -> ejabberd_cowboy:path().
create_url_path(Command) ->
    "/" ++ category_to_resource(?COMMANDS_ENGINE:category(Command))
        ++ maybe_add_bindings(Command).

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

-spec maybe_add_bindings(mongoose_command()) -> string().
maybe_add_bindings(Command) ->
    Action = ?COMMANDS_ENGINE:action(Command),
    Args = ?COMMANDS_ENGINE:args(Command),
    case Action of
        read ->
            add_bindings(Args);
        update ->
            Ids = ?COMMANDS_ENGINE:identifiers(Command),
            Bindings = [El || {Key, _Value} = El <- Args, true =:= proplists:is_defined(Key, Ids)],
            add_bindings(Bindings);
        delete ->
            add_bindings(Args);
        _ ->
            ""
    end.

-spec add_bindings(arg_spec_list()) -> string().
add_bindings(Args) ->
    lists:flatten([add_bind(A) || A <- Args]).

-spec add_bind(argspec()) -> string().
add_bind({ArgName, _}) ->
    "/" ++ atom_to_list(ArgName) ++ "/:" ++ atom_to_list(ArgName);
add_bind(Other) ->
    throw({error, bad_arg_spec, Other}).

category_to_resource(Category) when is_atom(Category) ->
    atom_to_list(Category);
category_to_resource(Category) when is_list(Category) ->
    Category.

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

to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin));
to_atom(List) when is_list(List) ->
    list_to_atom(List);
to_atom(Atom) when is_atom(Atom) ->
    Atom.