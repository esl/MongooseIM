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
-export([allowed_methods/2, content_types_provided/2, rest_terminate/2, init/3, rest_init/2, content_types_accepted/2,
    allow_missing_post/2]).

%% local callbacks
-export([to_json/2, from_json/2]).


-define(COMMANDS_ENGINE, mongoose_commands).
-include("mongoose_commands.hrl").
-include("ejabberd.hrl").

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

cowboy_router_paths(Base, _Opts) ->
    Commands =
        try
    ?COMMANDS_ENGINE:list(admin)
    catch
        _:Err ->
            ?ERROR_MSG("Error occured when getting the commands list: ~p~n", [Err]),
            undefined
    end,
    [register_handler(Base, Command) || Command <- Commands].

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------
init({ssl, _}, Req, Opts) ->
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

allow_missing_post(Req, State)->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% internal callbacks
%%--------------------------------------------------------------------

to_json(Req, State) ->
    handle_request(mongoose_api_json, Req, State).

%%--------------------------------------------------------------------
from_json(Req, State) ->
    handle_body(mongoose_api_json, Req, State).

%%--------------------------------------------------------------------
%% internal funs
%%--------------------------------------------------------------------

handle_request(Serializer, Req, #backend_state{bindings=Bindings, command_category = Name}=State) ->
    %% should we introduce filtering not only by category?
    Commands = ?COMMANDS_ENGINE:list(admin, Name),
    [Command] = [C || C <- Commands, ?COMMANDS_ENGINE:action(C) =:= read],
    Result = execute_command(extract_bindings(Bindings), Command),
    handle_result(Result, Serializer, Req, State).

handle_body(Deserializer, Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    case Deserializer:deserialize(Body) of
        {ok, Data} ->
            handle_body(Method, Data, Req2, State);
        {error, _Reason} ->
            error_response(bad_request, Req2, State)
    end.

handle_body(Method, Data, Req, #backend_state{command_category = Category}=State) ->
    Action = method_to_action(Method),
    Commands = ?COMMANDS_ENGINE:list(admin, Category),
    [Command] = [C || C <- Commands, ?COMMANDS_ENGINE:action(C) =:= Action],
    case check_and_extract_args(?COMMANDS_ENGINE:args(Command), Data) of
        {ok, Args} ->
            Result = execute_command(Args, Command),
            handle_result(Result, Req, State);
        {error, Type} ->
            error_response(Type, Req, State)
    end.

handle_result({ok, Result}, Serializer, Req, State) ->
    serialize(Result, Serializer, Req, State);
handle_result(Other, _Serializer, Req, State) ->
    handle_result(Other, Req, State).

handle_result({ok, _Res}, Req, State) ->
    %% TODO When POST add resource created to headers
    %% TODO return appropriate status code
    {ok, Req2} = cowboy_req:reply(201, [{<<"location">>, <<"http://localhost:5288/api/user">>}], Req),
    {halt, Req2, State};
handle_result({error, Error}, Req, State) ->
    error_response(Error, Req, State);
handle_result(no_call, Req, State) ->
    error_response(not_implemented, Req, State).

serialize(Data, Serializer, Req, State) ->
    {Serializer:serialize(Data), Req, State}.

register_handler(Base, Command) ->
    {[Base, create_url_path(Command)],
      ?MODULE, [{command_category, ?COMMANDS_ENGINE:category(Command)}]}.

get_allowed_methods() ->
    Commands = ?COMMANDS_ENGINE:list(admin),
    [action_to_method(?COMMANDS_ENGINE:action(Command)) || {_Name, Command} <- Commands].

execute_command(Args, Command) ->
    ?COMMANDS_ENGINE:execute(admin, ?COMMANDS_ENGINE:name(Command), Args).

create_url_path(Command) ->
    "/" ++ maybe_add_bindings(category_to_resource(?COMMANDS_ENGINE:category(Command)), Command).

%% for now, might be GET of form http://api/users/:domain/:username
%% instead of                    http://api/users/domain/:domain/username/:username
maybe_add_bindings(Base, Command) ->
    Action = ?COMMANDS_ENGINE:action(Command),
    Args = ?COMMANDS_ENGINE:args(Command),
    case Action of
        read ->
            add_bindings(Base, Args);
        update ->
            add_bindings(Base, Args);
        delete ->
            add_bindings(Base, Args);
        _ ->
            Base
    end.

add_bindings(Base, Args) ->
    Suffix = ["/:" ++ atom_to_list(ArgName)  || {ArgName, _} <- Args],
    lists:flatten(Base ++ Suffix).

extract_bindings(Bindings) ->
    [Bind || {_BindingName, Bind} <- Bindings].

category_to_resource(Category) when is_atom(Category) ->
    atom_to_list(Category);
category_to_resource(Category) when is_list(Category) ->
    Category.

check_and_extract_args(CommandsArgList, RequestArgList) ->
    if
        length(CommandsArgList) =/= length(RequestArgList) ->
            {error, bad_request};
        true ->
            RequestArgAtomized = [{list_to_atom(binary_to_list(Arg)), Value} || {Arg, Value} <- RequestArgList],
            GivenKeyList = [K || {K, _V} <- RequestArgAtomized ],
            ExptectedKeyList = [Key || {Key, _Type} <- CommandsArgList],
            Zipped = lists:zip(lists:sort(GivenKeyList), lists:sort(ExptectedKeyList)),
            case lists:member(false, [ReqKey =:= ExpKey || {ReqKey, ExpKey} <- Zipped]) of
                true ->
                    {error, bad_request};
                _ ->
                    {ok, do_extract_args(CommandsArgList, RequestArgAtomized)}
            end
    end.

%% Arguments must be sorted due to args order in mongoose_commands module
%% because order in request could differ
do_extract_args(CommandsArgList, GivenArgList) ->
    [element(2, lists:keyfind(Key, 1, GivenArgList)) || {Key, _Type} <- CommandsArgList].

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