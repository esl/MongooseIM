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
-export([allowed_methods/2, content_types_provided/2, rest_terminate/2, init/3, rest_init/2, content_types_accepted/2]).

%% local callbacks
-export([to_json/2, from_json/2, delete_resource/2]).


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
    handle_request(<<"DELETE">>, Req, State).

%%--------------------------------------------------------------------
%% internal callbacks
%%--------------------------------------------------------------------

to_json(Req, State) ->
    handle_request(<<"GET">>, Req, State).

%%--------------------------------------------------------------------
from_json(Req, State) ->
    handle_request(<<"POST">>, Req, State).

%%--------------------------------------------------------------------
%% internal funs
%%--------------------------------------------------------------------

handle_request(Method, Req, #backend_state{bindings=Bindings, command_category = Category}=State) ->
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(Method)),
    Result = execute_command(extract_bindings(Bindings), Command),
    handle_result(Method, Result, Req, State).

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
handle_result(_, {error, Error, _Reason}, Req, State) ->
    error_response(Error, Req, State);
handle_result(no_call, _, Req, State) ->
    error_response(not_implemented, Req, State).

handler_path(Base, Command) ->
    {[Base, create_url_path(Command)],
        ?MODULE, [{command_category, ?COMMANDS_ENGINE:category(Command)}]}.

get_allowed_methods() ->
    Commands = ?COMMANDS_ENGINE:list(admin),
    [action_to_method(?COMMANDS_ENGINE:action(Command)) || {_Name, Command} <- Commands].

execute_command(Args, Command) ->
    try
        ?COMMANDS_ENGINE:execute(admin, ?COMMANDS_ENGINE:name(Command), Args)
    catch
        _:R ->
            {error, bad_request, R}
    end.

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
    "/:" ++ atom_to_list(ArgName);
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