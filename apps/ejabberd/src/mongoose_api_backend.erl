%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2016 12:59
%%%-------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
    ejabberd_cowboy:implemented_result() | ejabberd_cowboy:default_result().
cowboy_router_paths(Base, _Opts) ->
    Commands =
        try
            ?COMMANDS_ENGINE:list(admin)
        catch
            _:Err ->
                ?ERROR_MSG("Error occured when getting the commands list: ~p~n", [Err]),
                undefined
        end,
    [handler_path(Base, Command) || Command <- Commands].

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

delete_resource(Req, State) ->
    extract_and_handle(<<"DELETE">>, Req, State).

%%--------------------------------------------------------------------
%% internal callbacks
%%--------------------------------------------------------------------

to_json(Req, State) ->
    extract_and_handle(<<"GET">>, Req, State).

%%--------------------------------------------------------------------
from_json(Req, State) ->
    extract_and_handle(<<"POST">>, Req, State).

%%--------------------------------------------------------------------
%% internal funs
%%--------------------------------------------------------------------

-spec extract_and_handle(method(), any(), #backend_state{}) -> {any(), any(), #backend_state{}}.
extract_and_handle(<<"POST">> = Method, Req, #backend_state{command_category = Category} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Args} = jiffy:decode(Body),
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(Method)),
    handle_request(Command, Args, Req2, State);
extract_and_handle(Method, Req, #backend_state{bindings = Bindings, command_category = Category}=State) ->
    Args = extract_bindings(Bindings),
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(Method)),
    PairedArgs = [{ArgName, Value} || {Value, {ArgName, _Type}} <- lists:zip(Args, ?COMMANDS_ENGINE:args(Command))],
    handle_request(Command, PairedArgs, Req, State).

-spec handle_request(#mongoose_command{}, list({atom(), binary()}), term(), #backend_state{}) ->
    {any(), any(), #backend_state{}}.
handle_request(Command, Args, Req, State) ->
    Method = action_to_method(?COMMANDS_ENGINE:action(Command)),
    ConvertedArgs = check_and_extract_args(?COMMANDS_ENGINE:args(Command), Args),
    Result = execute_command(ConvertedArgs, Command),
    handle_result(Method, Result, Req, State).

-spec handle_result(method(), {ok, any()} | failure(), any(), #backend_state{}) -> {any(), any(), #backend_state{}}.
handle_result(<<"GET">>, {ok, Result}, Req, State) ->
    {jiffy:encode(Result), Req, State};
handle_result(<<"POST">>, {ok, _Res}, Req, State) ->
    %% TODO When POST add resource created to header "location"
%%    {ok, Req2} = cowboy_req:reply(201, [{<<"location">>, ResourcePath}], Req),
    {ok, Req2} = cowboy_req:reply(201, Req),
    {halt, Req2, State};
handle_result(<<"DELETE">>, {ok, _Res}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {halt, Req2, State};
handle_result(_, ok, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {halt, Req2, State};
handle_result(_, {error, Error, _Reason}, Req, State) ->
    error_response(Error, Req, State);
handle_result(no_call, _, Req, State) ->
    error_response(not_implemented, Req, State).

-spec handler_path(ejabberd_cowboy:path(), #mongoose_command{}) -> ejabberd_cowboy:path().
handler_path(Base, Command) ->
    {[Base, create_url_path(Command)],
        ?MODULE, [{command_category, ?COMMANDS_ENGINE:category(Command)}]}.

-spec get_allowed_methods() -> list(method()).
get_allowed_methods() ->
    Commands = ?COMMANDS_ENGINE:list(admin),
    [action_to_method(?COMMANDS_ENGINE:action(Command)) || {_Name, Command} <- Commands].

check_and_extract_args(CommandsArgList, RequestArgList) ->
    if
        length(CommandsArgList) =/= length(RequestArgList) ->
            {error, bad_request, bad_args_len};
        true ->
            RequestArgAtomized = lists:sort([{to_atom(Arg), Value} || {Arg, Value} <- RequestArgList]),
            GivenKeyList = [K || {K, _V} <- RequestArgAtomized],
            ExptectedKeyList = lists:sort([Key || {Key, _Type} <- CommandsArgList]),
            Zipped = lists:zip(GivenKeyList, ExptectedKeyList),
            case lists:member(false, [ReqKey =:= ExpKey || {ReqKey, ExpKey} <- Zipped]) of
                true ->
                    {error, bad_request, bad_arg_spec};
                _ ->
                    do_extract_args(CommandsArgList, RequestArgAtomized)
            end
    end.

do_extract_args(CommandsArgList, GivenArgList) ->
    [element(2, lists:keyfind(Key, 1, GivenArgList)) || {Key, _Type} <- CommandsArgList].

-spec execute_command(map() | {error, atom(), any()}, #mongoose_command{}) -> {ok, term()} | failure().
execute_command({error, _Type, _Reason} = Err, _) ->
    Err;
execute_command(Args, Command) ->
    try
        do_execute_command(Args, Command)
    catch
        _:R ->
            {error, bad_request, R}
    end.

do_execute_command(Args, Command) ->
    Types = [Type || {_Name, Type} <- ?COMMANDS_ENGINE:args(Command)],
    ConvertedArgs = [convert_arg(Type, Arg) || {Type, Arg} <- lists:zip(Types, Args)],
    ?COMMANDS_ENGINE:execute(admin, ?COMMANDS_ENGINE:name(Command), ConvertedArgs).

-spec create_url_path(#mongoose_command{}) -> ejabberd_cowboy:path().
create_url_path(Command) ->
    "/" ++ category_to_resource(?COMMANDS_ENGINE:category(Command))
        ++ maybe_add_bindings(Command).

%% for now, might be GET of form http://api/users/:domain/:username
%% instead of                    http://api/users/domain/:domain/username/:username
-spec maybe_add_bindings(list({atom(), any()})) -> string().
maybe_add_bindings(Command) ->
    Action = ?COMMANDS_ENGINE:action(Command),
    Args = ?COMMANDS_ENGINE:args(Command),
    case Action of
        read ->
            add_bindings(Args);
        update ->
            add_bindings(Args);
        delete ->
            add_bindings(Args);
        _ ->
            ""
    end.

-spec add_bindings(list({atom(), any()})) -> string().
add_bindings(Args) ->
    lists:flatten([add_bind(A) || A <- Args]).

-spec add_bind({atom(), any()}) -> string().
add_bind({ArgName, _}) ->
    "/" ++ atom_to_list(ArgName) ++ "/:" ++ atom_to_list(ArgName);
add_bind(Other) ->
    throw({error, bad_arg_spec, Other}).

%% Bindings are in reverse order by default
extract_bindings(Bindings) ->
    lists:reverse([Bind || {_BindingName, Bind} <- Bindings]).

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
error_response(Reason, Req, State) ->
    error_response(error_code(Reason), Req, State).


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