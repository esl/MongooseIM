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
%% This module handles the client HTTP REST requests, then respectively convert them to Commands from mongoose_commands
%% and execute with `admin` privileges.
%% It supports responses with appropriate HTTP Status codes returned to the client.
%% This module implements behaviour introduced in ejabberd_cowboy which is built on top of the cowboy library.
%% The method supported: GET, POST, PUT, DELETE. Only JSON format.
%% The library "jiffy" used to serialize and deserialized JSON data.
%%
%% %% Handling the requests:
%%
%% - GET method:
%%
%% all bindings from path are mapped to the "args" field in mongoose_commands` record.
%% Example:
%% reqired headers: [Content-Type: application/json]
%% path: http://localhost:5288/api/users/username/joe/domain/example.com
%% body: []
%% will execute the command with record specified as:
%% {action, read},
%% {category, users},
%% {args, [{username, binary}, {domain, binary}]},
%% {identifiers, []}
%%
%% - DELETE method:
%%
%% reqired headers: [Content-Type: application/json]
%% path: http://localhost:5288/api/animals/name/elephant/region/india
%% body: {}
%% will execute the command with record specified as:
%% {action, delete},
%% {category, animals},
%% {args, [{name, binary}, {region, binary}]},
%% {identifiers, []}
%%
%% - POST method:
%%
%% all the parameters from body's JSON are mapped to the "args" field in mongoose_commands' record.
%% Example:
%% reqired headers: [Content-Type: application/json, Accept: application/json]
%% path: http://localhost:5288/api/users
%% body: {"username" : "andres", "domain" : "example.com", "age" : "22"}
%% will execute the command with record specified as:
%% {action, create}
%% {category, users},
%% {args, [{username, binary}, {domain, binary}, {age, integer}]},
%% {identifiers, []}
%%
%% - PUT method:
%%
%% the arguments marked as "identifiers" in mongoose_commands's record are the bindings (part of the path) and the
%% rest belongs to the body JSON.
%% Example:
%% reqired headers: [Content-Type: application/json, Accept: application/json]
%% path: http://localhost:5228/movies/title/the_shining/director/kubrick
%% body: {"new_title", : "My little pony"}
%% will execute the command with record specified as:
%% {action, update}
%% {category, movies},
%% {args, [{title, binary}, {director, binary}, {new_title, binary}]},
%% {identifiers, [title, director]}
%%

-module(mongoose_api_backend).
-author("ludwikbukowski").


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
-export([to_json/2, from_json/2]).
-include("mongoose_api.hrl").
-include("ejabberd.hrl").

-import(mongoose_api_utils, [error_response/3, error_response/4, action_to_method/1, method_to_action/1, error_code/1]).

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

%% @doc This is implementation of ejabberd_cowboy callback. Returns list of all available http paths.
-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
    ejabberd_cowboy:implemented_result() | ejabberd_cowboy:default_result().
cowboy_router_paths(Base, _Opts) ->
    ejabberd_hooks:add(register_command, global, mongoose_api_utils, reload_dispatches, 50),
    ejabberd_hooks:add(unregister_command, global, mongoose_api_utils, reload_dispatches, 50),
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
    State = #backend_state{allowed_methods = mongoose_api_utils:get_allowed_methods(admin),
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
delete_resource(Req, #backend_state{command_category = Category} = State) ->
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(<<"DELETE">>)),
    mongoose_api_utils:process_request(<<"DELETE">>, Command, Req, State).

%%--------------------------------------------------------------------
%% internal callbacks
%%--------------------------------------------------------------------

%% @doc Called for a method of type "GET"
to_json(Req, #backend_state{command_category = Category} = State) ->
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(<<"GET">>)),
    mongoose_api_utils:process_request(<<"GET">>, Command, Req, State).


%% @doc Called for a method of type "POST" and "PUT"
from_json(Req, #backend_state{command_category = Category} = State) ->
    {Method, Req2} = cowboy_req:method(Req),
    [Command] = ?COMMANDS_ENGINE:list(admin, Category, method_to_action(Method)),
    mongoose_api_utils:process_request(Method, Command, Req2, State).


%%--------------------------------------------------------------------
%% internals
%%--------------------------------------------------------------------

-spec handler_path(ejabberd_cowboy:path(), mongoose_command()) -> ejabberd_cowboy:path().
handler_path(Base, Command) ->
    {[Base, mongoose_api_utils:create_admin_url_path(Command)],
        ?MODULE, [{command_category, ?COMMANDS_ENGINE:category(Command)}]}.


