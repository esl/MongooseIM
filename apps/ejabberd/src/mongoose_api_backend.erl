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
-record(backend_state, {allowed_methods, bindings, parameters, command}).

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% cowboy_rest exports
-export([allowed_methods/2, content_types_provided/2, rest_terminate/2, init/3, rest_init/2]).

%% local callbacks
-export([to_json/2]).


-define(COMMANDS_ENGINE, mongoose_commands).
-include("mongoose_commands.hrl").

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

cowboy_router_paths(Base, Opts) ->
    Commands = ?COMMANDS_ENGINE:list(admin),
    [pa:bind(fun register_handler/2, Base), Commands].

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------
init({_Transport, http}, Req, _Opts) ->
    {upgrade, protocol, cowboy_rest, Req,
        #backend_state{allowed_methods = get_allowed_methods()}}.

rest_init(Req, Opts) ->
    {Bindings, Req1} = cowboy_req:bindings(Req),
    State = #backend_state{bindings=Bindings, command = Opts},
    {ok, Req1, State}.


allowed_methods(Req, State) ->
    {State#backend_state.allowed_methods, Req, State}.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {CTP, Req, State}.

rest_terminate(_Req, _State) ->
    ok.
%%--------------------------------------------------------------------
%% internal callbacks
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    handle_request(Method, mongoose_api_json, Req2, State).

%%--------------------------------------------------------------------
%% internal funs
%%--------------------------------------------------------------------

%% All parameters sent in bindings
handle_request(<<"GET">>, Serializer, Req, #backend_state{bindings=Bindings, command = Name}=State) ->
    CommandRecord = ?COMMANDS_ENGINE:get_command(admin, Name),
    Result = execute_command(extract_bindings(Bindings), CommandRecord),
    handle_result(Result, Serializer, Req, State).

handle_result({ok, Result}, Serializer, Req, State) ->
    serialize(Result, Serializer, Req, State);
handle_result(Other, _Serializer, Req, State) ->
    handle_result(Other, Req, State).

handle_result(ok, Req, State) ->
    {true, Req, State};
handle_result({error, Error}, Req, State) ->
    error_response(Error, Req, State);
handle_result(no_call, Req, State) ->
    error_response(not_implemented, Req, State).

serialize(Data, Serializer, Req, State) ->
    {Serializer:serialize(Data), Req, State}.

register_handler(Base, Command) ->
    {[Base, create_url_path(Base, Command#mongoose_command.category)],
      ?MODULE, [Command#mongoose_command.name]}.

get_allowed_methods() ->
    Commands = ?COMMANDS_ENGINE:list(admin),
    [translate_action(Command#mongoose_command.action) || Command <- Commands].

-spec execute_command(list(), #mongoose_command{}) -> any().
execute_command(Args, #mongoose_command{name = Name}) ->
    ?COMMANDS_ENGINE:execute(admin,Name, Args).

create_url_path(Base, Command) ->
    maybe_add_bindings(Base ++ category_to_resource(Command#mongoose_command.category), Command).

%% for now, might be GET of form http://api/users/:domain/:username
%% instead of                    http://api/users/domain/:domain/username/:username
maybe_add_bindings(Base, #mongoose_command{action = Action, args = Args} = Command) ->
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
    Base ++ Suffix.

extract_bindings(Bindings) ->
    [Bind || {_BindingName, Bind} <- Bindings].

category_to_resource(Category) when is_atom(Category) ->
    atom_to_list(Category) ++ "/";
category_to_resource(Category) when is_list(Category) ->
    Category ++"/".


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
error_code(type_error) -> 400;
error_code(internal) -> 500.

translate_action(get) -> <<"GET">>;
translate_action(set) -> <<"POST">>;
translate_action(delete) -> <<"DELETE">>;
translate_action(send) -> <<"POST">>;
translate_action(_) -> undefined.





